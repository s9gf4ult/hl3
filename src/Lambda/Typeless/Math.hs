module Lambda.Typeless.Math where

import Lambda.Typeless.Language

chTrue :: GTerm
chTrue = lam $ \t -> lam $ \f -> var t

chFalse :: GTerm
chFalse = lam $ \t -> lam $ \f -> var f

-- | And for two church bools
chAnd :: GTerm -> GTerm -> GTerm
chAnd a b = app (app a b) chFalse

chOr :: GTerm -> GTerm -> GTerm
chOr a b = app (app a chTrue) b
