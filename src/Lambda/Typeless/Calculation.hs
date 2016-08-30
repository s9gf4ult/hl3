module Lambda.Typeless.Calculation where

import Control.Lens
import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.Set as S

data Cause
  = Result
    -- ^ Normal calculation finished
  | Stuck
    -- ^ Calculation stuck because term is not closed
  | Error String
    -- ^ Error because term malformed
  deriving Show

data Stop = Stop
  { _stopCause :: Cause
  , _stopInner :: Term
    -- ^ Inner term calculation was stopped in
  , _stopOuter :: Term
    -- ^ Outer term containing inner one
  } deriving Show

makeLenses ''Stop

type CalcStep = Either Stop

crMap :: (Term -> Term) -> CalcStep Term -> CalcStep Term
crMap f = \case
  Right t -> Right $ f t
  Left c  -> Left $ over stopOuter f c

-- | Call by name stepper (like in Haskell). Before calculation term must be
-- sanitized
callByName :: Term -> CalcStep Term
callByName t = case t of
  TVar _ ->
    Left $ Stop Result t t
  TAbs v term -> crMap (TAbs v) $ callByName term
  TApp abs arg -> case abs of
    TAbs v term -> pure $ replaceVar v arg term
    _           -> crMap (`TApp` arg) $ callByName abs

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
sanitizeTerm :: Term -> Either String (Set Var)
sanitizeTerm = go S.empty
  where
    go scope = \case
      TVar v
        | S.notMember v scope -> Right $ S.singleton v
        | otherwise           -> Right $ S.empty
      TAbs v term
        | S.member v scope -> Left (nonUniq v)
        | otherwise        -> go (S.insert v scope) term
      TApp abs arg -> S.union <$> go scope abs <*> go scope arg

-- | Generates potentially infinite list of calculation steps
calculation :: (Term -> CalcStep Term) -> Term -> [Either Stop Term]
calculation step term = Right term:rest
  where
    rest = case step term of
      Right t   -> calculation step t
      Left stop -> case stop ^. stopCause of
        Result -> []  -- bcos previous term is the same
        _      -> [Right $ stop ^. stopOuter, Left stop]
