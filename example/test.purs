module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref
import Control.Monad.Loops

main = do
    n <- (new) (0 :: Int)
    (whileM_) (do
        x <- (read) (n)
        (pure) ((<) (x) (3 :: Int))
      ) (do
        (log) ("Your name: ")

-- The following line doesn't work because there is no getLine function in PureScript :-)
        name <- getLine

        (log) ((<>) ("Hello, ") (name))
        (modify_) ((+) (1 :: Int)) (n)
      )
  
