module Main where

import Control.Lens
import Lambda.Typeless
import System.Environment

main :: IO ()
main = getArgs >>= \case
  [a] -> do
    let var = read a
    print $ over (_Right . _2) detectNat $ justCalc $ runTGen $ app chFactorial $ chNat var
