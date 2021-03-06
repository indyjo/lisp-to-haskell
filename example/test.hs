-- Prelude --
import Data.IORef
import Data.Semigroup ((<>))
while :: (Monad m) => m Bool -> m a -> m ()
while p f = go
  where go = do
           x <- p
           if x
             then f >> go
             else return ()
-- Start of actual program --
main = do
    n <- (newIORef) (0 :: Int)
    (while) (do
        x <- (readIORef) (n)
        (return) ((<) (x) (3 :: Int))
      ) (do
        (putStrLn) ("Your name: ")
        name <- getLine
        (putStrLn) ((<>) ("Hello, ") (name))
        (modifyIORef) (n) ((+) (1 :: Int))
      )
  
