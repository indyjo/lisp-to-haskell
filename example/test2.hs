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
data Employee = Employee { name :: String, salary :: Int }
e = (Employee) ("Jonas") (1000000 :: Int)
main = do
    (putStrLn) ("Employee name:")
    (putStrLn) ((name) (e))
    (putStrLn) ("Employee salary:")
    (putStrLn) ((show) ((salary) (e)))
  
