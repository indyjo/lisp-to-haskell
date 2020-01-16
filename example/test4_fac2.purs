module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Control.Monad.Loops

fac = \n -> (if ((==) (n) (0 :: Int)) then (1 :: Int) else ((*) (n) ((fac) ((-) (n) (1 :: Int)))))
main = (log) ((show) ((fac) (6 :: Int)))

