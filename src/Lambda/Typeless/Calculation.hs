module Lambda.Typeless.Calculation where

import Control.Lens
import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.List as L
import qualified Data.Set as S

data Cause
  = Result
    -- ^ Normal calculation finished
  | Error String
    -- ^ Error because term malformed
  deriving Show

data Stop = Stop
  { _stopCause :: Cause
  , _stopInner :: Term
    -- ^ Inner term calculation was stopped in
  } deriving Show

makeLenses ''Stop

type CalcStep = Either Stop

-- | Call by name stepper (like in Haskell). Before calculation term must be
-- sanitized
fullReduce :: Term -> CalcStep Term
fullReduce t = case t of
  TVar _ ->
    Left $ Stop Result t
  TAbs v term -> TAbs v <$> fullReduce term
  TApp abs arg -> case abs of
    TAbs v term -> pure $ replaceVar v arg term
    _           -> case fullReduce abs of
      Right nuAbs -> Right $ TApp nuAbs arg
      Left stop -> case stop ^. stopCause of
        Result -> TApp abs <$> fullReduce arg
        _      -> Left stop

replaceVar
  :: Var
     -- ^ var to replace
  -> Term
     -- ^ replace with
  -> Term
     -- ^ replace where
  -> Term
replaceVar v trepl twhere = case twhere of
  TVar lv
    | lv == v   -> trepl
    | otherwise -> twhere
  TAbs lv term ->
    TAbs lv $ replaceVar v trepl term
  TApp abs arg -> TApp (replaceVar v trepl abs) (replaceVar v trepl arg)

nonUniq :: Var -> String
nonUniq (Var v) = "Variable already in scope: " ++ show v


-- | Returns either error message or list of free variables. Closed term have no
-- free variables
sanitizeTerm :: Term -> CalcStep (Set Var)
sanitizeTerm = go S.empty
  where
    go scope t = case t of
      TVar v
        | S.notMember v scope -> Right $ S.singleton v
        | otherwise           -> Right $ S.empty
      TAbs v term
        | S.member v scope -> Left $ Stop (Error (nonUniq v)) t
        | otherwise        -> go (S.insert v scope) term
      TApp abs arg -> S.union <$> go scope abs <*> go scope arg

-- | Generates potentially infinite list of calculation steps
calculation :: (Term -> CalcStep Term) -> Term -> [CalcStep Term]
calculation step term = Right term:rest
  where
    rest = case step term of
      Right t   -> calculation step t
      Left stop -> case stop ^. stopCause of
        Result -> []  -- bcos previous term is the same
        _      -> [Left stop]

calcDetails :: Term -> [CalcStep Term]
calcDetails t = case sanitizeTerm t of
  Left s -> [Left s]
  Right _ -> take 10 $  calculation fullReduce t

justCalc :: Term -> CalcStep (Int, Term)
justCalc t = case sanitizeTerm t of
  Left s  -> Left s
  Right _ ->
    let (f, l) = L.splitAt 1000 $ calculation fullReduce t
    in case l of
      []    -> case last f of
        Right a   -> Right (length f, a)
        Left stop -> Left stop
      (x:_) -> Left $ Stop (Error "Too much stpes") $ x ^?! _Right
