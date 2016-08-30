module Lambda.Typeless.Detection where

import Lambda.Typeless.Language

detectBool :: Term -> Maybe Bool
detectBool = \case
  TAbs a (TAbs b (TVar v))
    | v == a    -> Just True
    | v == b    -> Just False
    | otherwise -> Nothing
  _ -> Nothing


detectNat :: Term -> Maybe Int
detectNat = \case
  TAbs s (TAbs n term) -> go term
    where
      go = \case
        TVar nn
          | nn == n   -> Just 0
          | otherwise -> Nothing
        TApp (TVar ss) nested
          | ss == s -> (1 +) <$> go nested
          | otherwise -> Nothing
        _ -> Nothing
