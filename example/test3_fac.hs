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
    r <- (newIORef) (1 :: Int)
    n <- (newIORef) (6 :: Int)
    (while) (do
        v <- (readIORef) (n)
        (return) ((>) (v) (1 :: Int))
      ) (do
        v <- (readIORef) (n)
        (modifyIORef) (r) ((*) (v))
        (modifyIORef) (n) ((+) ((-1 :: Int)))
      )
    f <- (readIORef) (r)
    (putStrLn) ((show) (f))
  
