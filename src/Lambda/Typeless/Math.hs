module Lambda.Typeless.Math where

import Lambda.Typeless.Language

chTrue :: GTerm
chTrue = lam $ \t -> lam $ \f -> var t

chFalse :: GTerm
chFalse = lam $ \t -> lam $ \f -> var f

-- | And for two church bools
chAnd :: GTerm -> GTerm -> GTerm
chAnd a b = apps a [b, chFalse]

chOr :: GTerm -> GTerm -> GTerm
chOr a b = apps a [chTrue, b]

chNat :: Int -> GTerm
chNat w = lam $ \s -> lam $ \n -> compose (replicate w $ var s) $ var n

chPlus :: GTerm -> GTerm -> GTerm
chPlus a b = lam $ \s -> lam $ \n ->
  let bnum = apps b [var s, var n]
  in apps a [var s, bnum]
