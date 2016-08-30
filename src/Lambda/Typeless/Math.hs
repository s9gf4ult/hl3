module Lambda.Typeless.Math where

import Lambda.Typeless.Language

chTrue :: GTerm
chTrue = lam $ \t -> lam $ \f -> var t

chFalse :: GTerm
chFalse = lam $ \t -> lam $ \f -> var f

-- | Two argument function taking two church bools
chAnd :: GTerm
chAnd = lam $ \a -> lam $ \b -> apps (var a) [var b, chFalse]

chOr :: GTerm
chOr = lam $ \a -> lam $ \b -> apps (var a) [chTrue, var b]

chNat :: Int -> GTerm
chNat w = lam $ \s -> lam $ \n -> compose (replicate w $ var s) $ var n

chAdd :: GTerm
chAdd = lam $ \a -> lam $ \b -> lam $ \s -> lam $ \n ->
  let bnum = apps (var b) [var s, var n]
  in apps (var a) [var s, bnum]

chMul :: GTerm
chMul = lam $ \a -> lam $ \b -> apps (var a) [app chAdd (var b), chNat 0]
