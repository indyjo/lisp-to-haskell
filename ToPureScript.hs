import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import SExpr

-- Transform atoms (symbos, ints, strings) into corresponding literals.
transform :: String -> SExpr -> String
transform _ (SAtom a) = tf a
  where tf (ASymbol s) = s
        tf (AInt n) = show n ++ " :: Int"
        tf (AString s) = show s


-- PureScript doesn't know '()' syntax, use 'unit' instead.
transform _ (SList []) = "unit"

-- (do ...) blocks. Empty blocks are prohibited.
-- Use four spaces of indentation for commands, two for closing parens.
transform indent (SList (SAtom (ASymbol "do") : [])) = error "Empty do not allowed"
transform indent (SList (SAtom (ASymbol "do") : xs)) = "do" ++ doTuples ++ "\n  " ++ indent
  where
    doTuple (SList [(SAtom (ASymbol "_")), s]) = transform ("    " ++ indent) s
    doTuple (SList [(SAtom (ASymbol v)), s]) = v ++ " <- " ++ transform ("    " ++ indent) s
    doTuples = intercalate "" $ map ((("\n    " ++ indent) ++ ) . doTuple) xs

-- (if ...) statements. Only the three-branch version is allowed.
transform indent (SList (SAtom (ASymbol "if") : xs)) = case xs of
    (_cond : _then : _else : []) -> "if (" ++ t _cond ++ ") then (" ++ t _then ++ ") else (" ++ t _else ++ ")"
    _ -> error "if must have three arguments"
  where
    t e = transform indent e

-- Some functions have corresponding PureScript versions
transform indent (SList (SAtom (ASymbol s) : xs))
   | s == "alloc"        = replace "new"
   | s == "get"          = replace "read"
   | s == "set"          = replace "write"
   | s == "compute"      = replace "pure"
   | s == "while"        = replace "whileM_"
  where replace s' = transform indent $ SList $ (SAtom $ ASymbol s') : xs

-- (update f f) maps to modify_ f r
transform indent (SList (SAtom (ASymbol "update") : xs)) = case xs of 
  (r : f : []) -> transform indent $ SList $ (SAtom $ ASymbol "modify_") : f : r : []
  _            -> error "update requires two arguments"

-- Trivial transformation for every other SExpr
transform indent (SList xs) = unwords $ map (\s -> "(" ++ (transform indent s) ++ ")") xs

main = do
  source <- getContents
  let presult = readP_to_S parseProgram source
  case presult of
    ((res, "") : _) -> do
      putStrLn "module Main where"
      putStrLn ""
      putStrLn "import Prelude"
      putStrLn "import Effect (Effect)"
      putStrLn "import Effect.Console (log)"
      putStrLn "import Effect.Ref"
      putStrLn "import Control.Monad.Loops"
      putStrLn ""
      putStrLn ("main = " ++ (transform "" res))
    otherwise -> do
      hPutStrLn stderr "Error:"
      hPutStrLn stderr (show presult)
