module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Control.Monad.Loops

main = do
    r <- (new) (1 :: Int)
    n <- (new) (6 :: Int)
    (whileM_) (do
        v <- (read) (n)
        (pure) ((>) (v) (1 :: Int))
      ) (do
        v <- (read) (n)
        (modify_) ((*) (v)) (r)
        (modify_) ((+) ((-1 :: Int))) (n)
      )
    f <- (read) (r)
    (log) ((show) (f))
  
