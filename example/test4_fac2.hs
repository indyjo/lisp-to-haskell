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
fac = \n -> (if ((==) (n) (0 :: Int)) then (1 :: Int) else ((*) (n) ((fac) ((-) (n) (1 :: Int)))))
main = (putStrLn) ((show) ((fac) (6 :: Int)))
