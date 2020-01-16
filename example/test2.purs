module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Control.Monad.Loops

data Employee = Employee (String) (Int)
name (Employee v _) = v
salary (Employee _ v) = v

e = (Employee) ("Jonas") (1000000 :: Int)
main = do
    (log) ("Employee name:")
    (log) ((name) (e))
    (log) ("Employee salary:")
    (log) ((show) ((salary) (e)))
  

