import Data.IORef
import Control.Monad.Loops
run = do {n <- (newIORef) (0 :: Int); cond_action <- (return) (do {nv <- (readIORef) (n); (return) ((<) (nv) (3 :: Int))}); (whileM_) (cond_action) (do {(putStrLn) ("Your name: "); name <- getLine; (putStrLn) ((<>) ("Hello, ") (name)); (modifyIORef) (n) ((+) (1 :: Int))})}
