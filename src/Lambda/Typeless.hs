module Lambda.Typeless where

import Control.Monad.State
import Data.Set (Set)

import qualified Data.Set as S

newtype Var = Var
  { unVar :: Int
  } deriving (Ord, Eq, Num, Enum)

-- Повторения имен переменных не допускаются.
data LTerm
  = LVar Var
  | LAbs Var LTerm
  | LApp LTerm LTerm

instance Show LTerm where
  showsPrec _ = showString . printTerm

printTerm :: LTerm -> String
printTerm = \case
  LVar (Var v)      -> show v
  LAbs (Var v) term -> "\\" ++ show v ++ ".(" ++ printTerm term ++ ")"
  LApp t1 t2        -> printTerm t1 ++ " " ++ printTerm t2

type TGen = State Var

runTGen :: TGen a -> a
runTGen g = evalState g 0

-- | considered not to use directly
nextVar :: TGen Var
nextVar = do
  v <- get
  put $ succ v
  return v

lam :: (Var -> TGen LTerm) -> TGen LTerm
lam f = do
  v <- nextVar
  t <- f v
  return $ LAbs v t

var :: Var -> TGen LTerm
var v = LVar <$> pure v

app :: TGen LTerm -> TGen LTerm -> TGen LTerm
app abs arg = LApp <$> abs <*> arg

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

crMap :: (LTerm -> LTerm) -> CalcResult LTerm -> CalcResult LTerm
crMap f = \case
  CStep t   -> CStep $ f t
  CStop c t -> CStop c $ f t

calcStep :: LTerm -> CalcResult LTerm
calcStep = go S.empty
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
        then CStop (nonUniq v) t
        else crMap (LAbs v) $ go (S.insert v scope) term
      t@(LApp abs arg) -> case abs of
        LAbs v term ->
          if S.member v scope
          then CStop (nonUniq v) t
          else replaceVar (S.insert v scope) term v arg
        term -> crMap (`LApp` arg) $ go scope term

    nonUniq (Var v) = Error $ "Variable already in scope: " ++ show v

    replaceVar :: Set Var -> LTerm -> Var -> LTerm -> CalcResult LTerm
    replaceVar scope lwhere v lwhat = case lwhere of
      LVar lv ->
        if | lv == v -> CStep lwhat
           | S.member lv scope -> CStep lwhere
           | otherwise -> CStop Stuck lwhere
      LAbs lv term ->
        if S.member lv scope
        then CStop (nonUniq lv) lwhere
        else crMap (LAbs lv) $ replaceVar (S.insert lv scope) term v lwhat
      LApp abs arg -> LApp
        <$> replaceVar scope abs v lwhat
        <*> replaceVar scope arg v lwhat

-- | Generates potentially infinite list of calculation steps
calculation :: (LTerm -> CalcResult LTerm) -> LTerm -> [LTerm]
calculation step term = term:rest
  where
    rest = case step term of
      CStep t   -> calculation step t
      CStop _ t -> [t]
