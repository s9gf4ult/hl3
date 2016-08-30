module Lambda.Typeless.Calculation where

import Control.Lens
import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.Set as S

data CalcStep = Either Stop

data Stop = Stop
  { _stopCause :: Cause
  , _stopInner :: Term
    -- ^ Inner term calculation was stopped in
  , _stopOuter :: Term
    -- ^ Outer term containing inner one
  } deriving Show

makeLenses ''Stop

data Cause
  = Result
    -- ^ Normal calculation finished
  | Stuck
    -- ^ Calculation stuck because term is not closed
  | Error String
    -- ^ Error because term malformed
  deriving Show

crMap :: (Term -> Term) -> CalcStep Term -> CalcStep Term
crMap f = \case
  Right t   -> Right $ f t
  Left c -> Left

-- | Call by name stepper (like in Haskell)
callByName :: Term -> CalcStep Term
callByName = go S.empty
  where
    go
      :: Set Var
         -- ^ Scope of current calculation
      -> Term
      -> CalcStep Term
    go scope = \case
      TVar v ->
        let cause = if S.member v scope
                    then Result
                    else Stuck
        in CStop cause $ TVar v
      t@(TAbs v term) ->
        if S.member v scope
        then CStop (Error $ nonUniq v) t
        else crMap (TAbs v) $ go (S.insert v scope) term
      t@(TApp abs arg) -> case abs of
        TAbs v term ->
          if S.member v scope
          then CStop (Error $ nonUniq v) t
          else replaceVar (S.insert v scope) term v arg
        term -> crMap (`TApp` arg) $ go scope term


    replaceVar :: Set Var -> Term -> Var -> Term -> CalcStep Term
    replaceVar scope lwhere v lwhat = case lwhere of
      TVar lv ->
        if | lv == v -> CStep lwhat
           | S.member lv scope -> CStep lwhere
           | otherwise -> CStop Stuck lwhere
      TAbs lv term ->
        if S.member lv scope
        then CStop (Error $ nonUniq lv) lwhere
        else crMap (TAbs lv) $ replaceVar (S.insert lv scope) term v lwhat
      TApp abs arg -> TApp
        <$> replaceVar scope abs v lwhat
        <*> replaceVar scope arg v lwhat

nonUniq :: Var -> String
nonUniq (Var v) = "Variable already in scope: " ++ show v


-- | Returns either error message or list of free variables. Closed term have no
-- free variables
checkTerm :: Term -> Either String (Set Var)
checkTerm = go S.empty
  where
    go scope = \case
      TVar v
        | S.notMember v scope -> Right $ S.singleton v
        | otherwise -> Right $ S.empty
      TAbs v term
        | S.member v scope -> Teft (nonUniq v)
        | otherwise -> go (S.insert v scope) term
      TApp abs arg -> S.union <$> go scope abs <*> go scope arg

-- | Generates potentially infinite list of calculation steps
calculation :: (Term -> CalcStep Term) -> Term -> [Either String Term]
calculation step term = Right term:rest
  where
    rest = case step term of
      CStep t        -> calculation step t
      CStop Result t -> []      -- bcos previous term is the same
      CStop e t      -> [Right t, Left $ show e]
