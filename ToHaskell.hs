import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import SExpr

transform :: SExpr -> String
transform (SAtom a)  = tf a
  where tf (ASymbol s) = s
        tf (AInt n) = show n ++ " :: Int"
        tf (AString s) = show s

transform (SList []) = "()"
transform (SList (SAtom (ASymbol "do") : xs)) = "do {" ++ doTuples ++ "}"
  where
    doTuple (SList [(SAtom (ASymbol "_")), s]) = transform s
    doTuple (SList [(SAtom (ASymbol v)), s]) = v ++ " <- " ++ transform s
    doTuples = intercalate "; " $ map doTuple xs
transform (SList (SAtom (ASymbol s) : xs))
   | s == "alloc"        = replace "newIORef"
   | s == "get"          = replace "readIORef"
   | s == "set"          = replace "writeIORef"
   | s == "update"       = replace "modifyIORef"
   | s == "compute"      = replace "return"
   | s == "while"        = replace "whileM_"
  where replace s' = transform $ SList $ (SAtom $ ASymbol s') : xs
transform (SList xs) = unwords $ map (\s -> "(" ++ (transform s) ++ ")") xs

main = do source <- getContents
          let presult = readP_to_S parseSExpr source
--          hPutStrLn stderr $ show presult
          case presult of
            ((res, "") : _) -> do putStrLn $ "import Data.IORef"
                                  putStrLn $ "import Control.Monad.Loops"
                                  putStrLn $ "run = " ++ (transform res)
            otherwise      -> do hPutStrLn stderr "Error:"
                                 hPutStrLn stderr $ show presult

