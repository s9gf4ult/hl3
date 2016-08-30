module Lambda.Typeless.Language where

import Control.Monad.State

newtype Var = Var
  { unVar :: Int
  } deriving (Ord, Eq, Num, Enum)

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
  LApp t1 t2        -> "(" ++ printTerm t1 ++ ") (" ++ printTerm t2 ++ ")"


type TGen = State Var

type GTerm = TGen LTerm

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
