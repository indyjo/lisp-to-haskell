import Control.Monad (forM_)
import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import SExpr


-- Context for transformation
data Context = Ctx { indent :: String }
initContext = Ctx { indent = "" }

-- Record that wraps field information
data Field = Field { fieldName :: String, fieldType :: SExpr }

-- Transform while in declarative context (decl ...)
transformDecl :: SExpr -> String

-- Transform "define" declarations.
transformDecl (SList ((SAtom (ASymbol "define")) : (SAtom (ASymbol name)) : value : [])) =
  name ++ " = " ++ transform initContext value
transformDecl (SList ((SAtom (ASymbol "define")) : _)) =
  error "Invalid syntax for define (exactly two arguments needed)"

-- Transform "record" declarations

transformDecl (SList ((SAtom (ASymbol "record")) : (SAtom (ASymbol name)) : fieldExprs)) =
    "data " ++ name ++ " = " ++ name ++ " " ++ tFieldTypes ++ "\n" ++ tAccessors ++ "\n"
  where
    fields = map toField fieldExprs
    toField (SList ( (SAtom (ASymbol fieldName)) : typeExpr : [] )) =
      Field fieldName typeExpr
    tFieldTypes = unwords $ map transformField fields
    transformField field = "(" ++ (transform initContext $ fieldType field) ++ ")"
    tAccessors = intercalate "\n" $ map genAccessor fields
    genAccessor field = fieldName field ++ " (" ++ name ++ " " ++ patterns ++ ") = v"
      where
        patterns = unwords $ map (\f -> if (fieldName f == fieldName field) then "v" else "_") fields


-- Transform atoms (symbos, ints, strings) into corresponding literals.
transform :: Context -> SExpr -> String
transform _ (SAtom a) = tf a
  where tf (ASymbol s) = s
        tf (AInt n) = show n ++ " :: Int"
        tf (AString s) = show s


-- PureScript doesn't know '()' syntax, use 'unit' instead.
transform _ (SList []) = "unit"

-- (do ...) blocks. Empty blocks are prohibited.
-- Use four spaces of indentation for commands, two for closing parens.
transform _ (SList (SAtom (ASymbol "do") : [])) = error "Empty do not allowed"
transform c@Ctx{ indent = indent } (SList (SAtom (ASymbol "do") : xs)) = "do" ++ doTuples ++ "\n  " ++ indent
  where
    doTuple (SList [(SAtom (ASymbol "_")), s]) = transform newc s
    doTuple (SList [(SAtom (ASymbol v)), s]) = v ++ " <- " ++ transform newc s
    newc = c{ indent = "    " ++ indent }
    doTuples = intercalate "" $ map ((("\n    " ++ indent) ++ ) . doTuple) xs

-- (if ...) statements. Only the three-branch version is allowed.
transform c (SList (SAtom (ASymbol "if") : xs)) = case xs of
    (_cond : _then : _else : []) -> "if (" ++ t _cond ++ ") then (" ++ t _then ++ ") else (" ++ t _else ++ ")"
    _ -> error "if must have three arguments"
  where
    t e = transform c e

-- Some functions have corresponding PureScript versions
transform c (SList (SAtom (ASymbol s) : xs))
   | s == "alloc"        = replace "new"
   | s == "get"          = replace "read"
   | s == "set"          = replace "write"
   | s == "compute"      = replace "pure"
   | s == "while"        = replace "whileM_"
  where replace s' = transform c $ SList $ (SAtom $ ASymbol s') : xs

-- (update f f) maps to modify_ f r
transform c (SList (SAtom (ASymbol "update") : xs)) = case xs of 
  (r : f : []) -> transform c $ SList $ (SAtom $ ASymbol "modify_") : f : r : []
  _            -> error "update requires two arguments"

-- Trivial transformation for every other SExpr
transform c (SList xs) = unwords $ map (\s -> "(" ++ (transform c s) ++ ")") xs

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
      case res of SList (SAtom (ASymbol "decl") : decls) -> putStrLn $ unlines $ fmap transformDecl decls
                  otherwise              -> putStrLn ("main = " ++ (transform initContext res))
    otherwise -> do
      hPutStrLn stderr "Error:"
      hPutStrLn stderr (show presult)
