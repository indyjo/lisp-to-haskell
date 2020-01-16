import Data.List (concatMap, intercalate)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
import SExpr

-- Context for transformation
data Context = Ctx { indent :: String }
initContext = Ctx { indent = "" }

-- Transform declarations
transformDecl :: Context -> SExpr -> String

-- Transform (record T (n_1 T_1) ...) declarations
transformDecl c (SList (SAtom (ASymbol "record") : xs)) = case xs of
  (SAtom (ASymbol typeName) : fields)
    -> let tfields = intercalate ", " $ map transformField fields
           transformField (SList (SAtom (ASymbol fieldName) : typeExpr : [])) =
             fieldName ++ " :: " ++ transform c typeExpr
        in "data " ++ typeName ++ " = " ++ typeName ++ " { " ++ tfields ++ " }"
  otherwise
    -> error "Invalid record"

-- Transform (define name value) declarations
transformDecl c (SList (SAtom (ASymbol "define") : xs)) = case xs of
  (SAtom (ASymbol name) : expr : []) -> name ++ " = " ++ transform c expr
  otherwise                          -> error "Invalid define"

-- Transform expressions
transform :: Context -> SExpr -> String

-- Transform atoms
transform _ (SAtom a)  = tf a
  where tf (ASymbol s) = s
        tf (AInt n) = show n ++ " :: Int"
        tf (AString s) = show s

-- Transform ()
transform _ (SList []) = "()"

-- Transform (do ...) combinator
transform c@Ctx{ indent=indent } (SList (SAtom (ASymbol "do") : xs)) = "do\n" ++ doTuples ++ indent ++ "  "
  where
    doTuple (SList [(SAtom (ASymbol "_")), s]) = ts s 
    doTuple (SList [(SAtom (ASymbol v)), s]) = v ++ " <- " ++ ts s
    ts e = transform c{ indent = "    " ++ indent } e
    doTuples :: String
    doTuples = concatMap (\s -> "    " ++ indent ++ s ++ "\n") $ map doTuple xs

-- Transform (if _cond _then _else) expressions
transform c (SList (SAtom (ASymbol "if") : xs)) = case xs of
   (_cond : _then : _else : [])
     -> let t = transform c in
       "if (" ++ t _cond ++ ") then (" ++ t _then ++ ") else (" ++ t _else ++ ")"
   _ -> error "if must have three arguments"
    where t = transform c

-- Transform (fun (...) ...) lambdas
transform c (SList (SAtom (ASymbol "fun") : xs)) = case xs of
  ((SList vars) : expr : []) -> let
      tvars = unwords $ map (\e -> case e of (SAtom (ASymbol name)) -> name) vars
    in "\\" ++ tvars ++ " -> (" ++ transform c expr ++ ")"
  otherwise                  -> error "Invalid lambda"

-- Transform functions with corresponding Haskell functions
transform c (SList (SAtom (ASymbol s) : xs))
   | s == "alloc"        = replace "newIORef"
   | s == "get"          = replace "readIORef"
   | s == "set"          = replace "writeIORef"
   | s == "update"       = replace "modifyIORef"
   | s == "compute"      = replace "return"
   | s == "log"          = replace "putStrLn"
  where replace s' = transform c $ SList $ (SAtom $ ASymbol s') : xs

-- Transform remaining functions
transform c (SList xs) = unwords $ map (\s -> "(" ++ (transform c s) ++ ")") xs

main = do source <- getContents
          let presult = readP_to_S parseProgram source
--          hPutStrLn stderr $ show presult
          case presult of
            ((prg, "") : _) -> do putStrLn $ "-- Prelude --"
                                  putStrLn $ "import Data.IORef"
                                  putStrLn $ "import Data.Semigroup ((<>))"
                                  putStrLn $ "while :: (Monad m) => m Bool -> m a -> m ()"
                                  putStrLn $ "while p f = go"
                                  putStrLn $ "  where go = do"
                                  putStrLn $ "           x <- p"
                                  putStrLn $ "           if x"
                                  putStrLn $ "             then f >> go"
                                  putStrLn $ "             else return ()"
                                  putStrLn $ "-- Start of actual program --"
                                  putStrLn $ case prg of
                                    (SList (SAtom (ASymbol "decl") : decls))
                                      -> intercalate "\n" $ map (transformDecl initContext) decls
                                    otherwise
                                      -> "main = " ++ (transform initContext prg)
            otherwise      -> do hPutStrLn stderr "Error:"
                                 hPutStrLn stderr $ show presult

