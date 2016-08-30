module Lambda.Typeless.Calculation where

import Data.Set (Set)
import Lambda.Typeless.Language

import qualified Data.Set as S

data CalcResult a
  = CStep a
  | CStop Cause LTerm
  deriving (Functor)

instance Applicative CalcResult where
  pure = CStep
  (<*>) = \case
    CStep f -> \case
      CStep a -> CStep $ f a
      CStop e t -> CStop e t
    CStop e t -> const $ CStop e t

data Cause
  = Result
    -- ^ Normal calculation finished
  | Stuck
    -- ^ Calculation stuck because term is not closed
  | Error String
    -- ^ Error because term malformed
  deriving Show

crMap :: (LTerm -> LTerm) -> CalcResult LTerm -> CalcResult LTerm
crMap f = \case
  CStep t   -> CStep $ f t
  CStop c t -> CStop c $ f t

-- | Call by name stepper (like in Haskell)
callByName :: LTerm -> CalcResult LTerm
callByName = go S.empty
  where
    go
      :: Set Var
         -- ^ Scope of current calculation
      -> LTerm
      -> CalcResult LTerm
    go scope = \case
      LVar v ->
        let cause = if S.member v scope
                    then Result
                    else Stuck
        in CStop cause $ LVar v
      t@(LAbs v term) ->
        if S.member v scope
        then CStop (Error $ nonUniq v) t
        else crMap (LAbs v) $ go (S.insert v scope) term
      t@(LApp abs arg) -> case abs of
        LAbs v term ->
          if S.member v scope
          then CStop (Error $ nonUniq v) t
          else replaceVar (S.insert v scope) term v arg
        term -> crMap (`LApp` arg) $ go scope term


    replaceVar :: Set Var -> LTerm -> Var -> LTerm -> CalcResult LTerm
    replaceVar scope lwhere v lwhat = case lwhere of
      LVar lv ->
        if | lv == v -> CStep lwhat
           | S.member lv scope -> CStep lwhere
           | otherwise -> CStop Stuck lwhere
      LAbs lv term ->
        if S.member lv scope
        then CStop (Error $ nonUniq lv) lwhere
        else crMap (LAbs lv) $ replaceVar (S.insert lv scope) term v lwhat
      LApp abs arg -> LApp
        <$> replaceVar scope abs v lwhat
        <*> replaceVar scope arg v lwhat

nonUniq :: Var -> String
nonUniq (Var v) = "Variable already in scope: " ++ show v


-- | Returns either error message or list of free variables. Closed term have no
-- free variables
checkTerm :: LTerm -> Either String (Set Var)
checkTerm = go S.empty
  where
    go scope = \case
      LVar v
        | S.notMember v scope -> Right $ S.singleton v
        | otherwise -> Right $ S.empty
      LAbs v term
        | S.member v scope -> Left (nonUniq v)
        | otherwise -> go (S.insert v scope) term
      LApp abs arg -> S.union <$> go scope abs <*> go scope arg

-- | Generates potentially infinite list of calculation steps
calculation :: (LTerm -> CalcResult LTerm) -> LTerm -> [Either String LTerm]
calculation step term = Right term:rest
  where
    rest = case step term of
      CStep t        -> calculation step t
      CStop Result t -> []      -- bcos previous term is the same
      CStop e t      -> [Right t, Left $ show e]
