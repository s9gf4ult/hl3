module Lambda.Typeless.Calculation where

import Control.Lens
import Data.Map.Strict (Map)
import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.List as L
import qualified Data.Map.Strict as M
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

-- | Assumes all binded variables are uniqe
fullReduce :: Term -> CalcStep Term
fullReduce t = case t of
  TVar _ ->
    Left $ Stop Result t
  TAbs v term -> TAbs v <$> fullReduce term
  TApp abst arg -> case abst of
    TAbs v term -> pure $ replaceVar v arg term
    _           -> case fullReduce abst of
      Right nuAbs -> Right $ TApp nuAbs arg
      Left stop -> case stop ^. stopCause of
        Result -> TApp abst <$> fullReduce arg
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
  TApp abst arg -> TApp (replaceVar v trepl abst) (replaceVar v trepl arg)

-- | Returns list of free variables. Closed term have no free variables
freeVars :: Term -> Set Var
freeVars = go S.empty
  where
    go scope t = case t of
      TVar v
        | S.notMember v scope -> S.singleton v
        | otherwise           -> S.empty
      TAbs v term   -> go (S.insert v scope) term
      TApp abst arg -> S.union (go scope abst) (go scope arg)

renameVars :: Term -> GTerm
renameVars term = go (freeVars term) M.empty term
  where
    go :: Set Var -> Map Var Var -> Term -> GTerm
    go free repVar t = case t of
      TVar


-- | Generates potentially infinite list of calculation steps
calculation :: (Term -> CalcStep Term) -> Term -> [CalcStep Term]
calculation step term = case sanitizeTerm term of
  Left stop -> [Left stop]
  Right vars -> go vars term         -- to track that free fariables stay same
  where
    go vars sanTerm =
      let
        rest = case step sanTerm of
          Left stop -> [Left stop]
          Right nextTerm -> case sanitizeTerm nextTerm of
            Left stop -> [Right nextTerm, Left stop]
            Right newVars
              | newVars == vars -> go vars nextTerm
              | otherwise -> [Left $ Stop (Error "free variables differ") nextTerm ]
      in Right sanTerm:rest

justCalc :: Term -> CalcStep (Int, Term)
justCalc t =
  let (f, l) = L.splitAt 1000 $ calculation fullReduce t
  in case l of
    [] -> case last f of
      Right a   -> Right (length f, a)
      Left stop -> Left stop
    (x:_) -> either Left (Left . Stop (Error "Too much steps")) x
