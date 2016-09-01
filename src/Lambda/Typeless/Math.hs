module Lambda.Typeless.Math where

import Lambda.Typeless.Language

chTrue :: GTerm
chTrue = lam $ \t -> lam $ \_ -> var t

chFalse :: GTerm
chFalse = lam $ \_ -> lam $ \f -> var f

-- | Two argument function taking two church bools
chAnd :: GTerm
chAnd = lam $ \a -> lam $ \b -> apps (var a) [var b, chFalse]

chOr :: GTerm
chOr = lam $ \a -> lam $ \b -> apps (var a) [chTrue, var b]

chPair :: GTerm
chPair = lam $ \a -> lam $ \b -> lam $ \f -> apps (var f) [var a, var b]

chFst :: GTerm
chFst = lam $ \p -> app (var p) getFst
  where
    getFst = lam $ \a -> lam $ \_ -> var a

chSnd :: GTerm
chSnd = lam $ \p -> app (var p) getSnd
  where
    getSnd = lam $ \_ -> lam $ \b -> var b

chNat :: Int -> GTerm
chNat w = lam $ \s -> lam $ \n -> compose (replicate w $ var s) $ var n

chAdd :: GTerm
chAdd = lam $ \a -> lam $ \b -> lam $ \s -> lam $ \n ->
  let bnum = apps (var b) [var s, var n]
  in apps (var a) [var s, bnum]

chMul :: GTerm
chMul = lam $ \a -> lam $ \b -> apps (var a) [app chAdd (var b), chNat 0]

chInc :: GTerm
chInc = lam $ \a -> lam $ \s -> lam $ \n ->
  app (var s) (apps (var a) [var s, var n])

upPair :: GTerm
upPair = lam $ \p -> apps chPair
  [ app chSnd (var p), app chInc $ app chSnd (var p) ]

chDec :: GTerm
chDec = lam $ \a ->
  let zz = apps chPair [chNat 0, chNat 0]
      f = upPair
  in app chFst $ apps (var a) [f, zz]

-- | :: Nat -> Bool
chNull :: GTerm
chNull = lam $ \a -> apps (var a) [lam $ \_ -> chFalse, chTrue]

chFix :: GTerm
chFix = lam $ \f ->
  let lx = lam $ \x -> app (var f) (app (var x) (var x))
  in app lx lx

-- | Nat -> Nat
chFactorial :: GTerm
chFactorial =
  app chFix $ lam $ \fac ->
  lam $ \a -> apps (app chNull $ var a)
              [ chNat 1
              , apps chMul [ var a
                           , compose [chDec, var fac] $ var a ] ]
