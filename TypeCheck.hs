module Main where
import Control.Monad.Trans.Except
import System.Exit
import Text.ParserCombinators.ReadP (readP_to_S)
import Types
import SExpr

type Check = Except String

check :: SExpr -> Check Type
check prg = case prg of
  (SList ((SAtom (ASymbol "decl")) : decls)) -> throwE "decl not supported yet"
  otherwise                                  -> except $ typeOf initEnv prg

report :: Check Type -> IO ()
report r = case runExcept r of
  Left e  -> die e
  Right t -> putStrLn $ "The program passed type check. Its type is: " ++ show t

main = do
  source <- getContents
  let presult = readP_to_S parseProgram source
  case presult of
    ((prg, "") : _) -> report $ check prg
    otherwise       -> die $ "Parse error: " ++ show presult


