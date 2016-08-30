module Lambda.Typeless.Language where

import Control.Monad.State

newtype Var = Var
  { unVar :: Int
  } deriving (Ord, Eq, Num, Enum)

data Term
  = TVar Var
  | TAbs Var Term
  | TApp Term Term

instance Show Term where
  showsPrec _ = showString . printTerm

printTerm :: Term -> String
printTerm = \case
  TVar (Var v)      -> show v
  TAbs (Var v) term -> "\\" ++ show v ++ ".(" ++ printTerm term ++ ")"
  TApp t1 t2        -> "(" ++ printTerm t1 ++ ") (" ++ printTerm t2 ++ ")"


type TGen = State Var

type GTerm = TGen Term

runTGen :: TGen a -> a
runTGen g = evalState g 0

-- | considered not to use directly
nextVar :: TGen Var
nextVar = do
  v <- get
  put $ succ v
  return v

lam :: (Var -> GTerm) -> GTerm
lam f = do
  v <- nextVar
  t <- f v
  return $ TAbs v t

var :: Var -> GTerm
var v = TVar <$> pure v

app :: GTerm -> GTerm -> GTerm
app abs arg = TApp <$> abs <*> arg

-- | Apply many args to first elem
apps :: GTerm -> [GTerm] -> GTerm
apps t = \case
  []         -> t
  (arg:args) -> apps (app t arg) args

-- | Compose many functions by applying them to given argument. Head is the most
-- nested function to be applied
compose :: [GTerm] -> GTerm -> GTerm
compose [] a       = a
compose (f:fs) arg = compose fs (app f arg)
