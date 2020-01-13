import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import SExpr

transform :: String -> SExpr -> String
transform _ (SAtom a) = tf a
  where tf (ASymbol s) = s
        tf (AInt n) = show n ++ " :: Int"
        tf (AString s) = show s

transform _ (SList []) = "()"

transform indent (SList (SAtom (ASymbol "do") : xs)) = "do \n" ++ doTuples
  where
    doTuple (SList [(SAtom (ASymbol "_")), s]) = indent ++ transform ("  " ++ indent) s ++ "\n"
    doTuple (SList [(SAtom (ASymbol v)), s]) = indent ++ v ++ " <- " ++ transform ("  " ++ indent) s ++ "\n"
    doTuples = intercalate "" $ map doTuple xs

transform indent (SList (SAtom (ASymbol "if") : xs)) = case xs of
   (_cond : _then : _else : []) -> "if (" ++ transform indent _cond ++ ") then (" ++ transform indent _then ++ ") else (" ++ transform indent _else ++ ")"
   _ -> error "if must have three arguments"

transform indent (SList (SAtom (ASymbol s) : xs))
   | s == "alloc"        = replace "new"
   | s == "get"          = replace "read"
   | s == "set"          = replace "write"
--   | s == "update"       = replace "modify_"
   | s == "compute"      = replace "pure"
   | s == "while"        = replace "whileM_"
  where replace s' = transform indent $ SList $ (SAtom $ ASymbol s') : xs

transform indent (SList (SAtom (ASymbol "update") : xs)) = case xs of 
  (r : f : []) -> transform indent $ SList $ (SAtom $ ASymbol "modify_") : f : r : []
  _            -> error "update requires two arguments"

transform indent (SList xs) = unwords $ map (\s -> "(" ++ (transform indent s) ++ ")") xs

main = do source <- getContents
          let presult = readP_to_S parseProgram source
          case presult of
            ((res, "") : _) -> do putStrLn $ "module Main where"
                                  putStrLn $ "import Prelude"
                                  putStrLn $ "import Effect (Effect)"
                                  putStrLn $ "import Effect.Console (log)"
                                  putStrLn $ "import Effect.Ref"
                                  putStrLn $ "import Control.Monad.Loops"
                                  putStrLn $ "main = " ++ (transform "  " res)
            otherwise      -> do hPutStrLn stderr "Error:"
                                 hPutStrLn stderr $ show presult

