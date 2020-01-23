module Types where
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.State.Lazy (State, get, runState)
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import SExpr (SExpr)
import qualified SExpr as S

data Type = TParam  [Type]        -- parametric type application (T1 ... TN), also for "->"
          | TForall String Type   -- forall <name> . <type>
          | TSymbol String        -- <name>, both for variables and for type constructors (Double, Char...)
            deriving (Ord, Eq)
instance Show Type where
  show (TSymbol s) = s
  show (TParam ts) = "(" ++ (unwords $ map show ts) ++ ")"
  show (TForall n t) = "forall " ++ n ++ "." ++ show t

-- Some definitions for types.

-- arrow is the parametric type of functions
arrow = TSymbol "->"
tFunc ts = TParam $ arrow : ts

tBool = TSymbol "Bool"
tInt = TSymbol "Int"
tChar = TSymbol "Char"
tDouble = TSymbol "Double"
tList t = TParam $ TSymbol "List" : [ t ]
tString = tList tChar

-- Given a type expression, return any unbound names in the order of use. Names might appear twice.
freeVars :: Type -> [String]
freeVars (TSymbol n)   = [n]
freeVars (TParam ts)   = concat $ map freeVars ts
freeVars (TForall n t) = filter (/= n) (freeVars t)

type Env = Map String Type
type Error = String

-- The monad that typeof will operate in.
-- Supports exceptions and passing the environment as state.
type Infer = ExceptT Error (State Env)

lookupEnv :: String -> Infer Type
lookupEnv s = do
  env <- get
  case Map.lookup s env of
    Just t  -> return t
    Nothing -> throwE $ "No type known for symbol " ++ s


typeof :: SExpr -> Infer Type
typeof (S.SAtom (S.AInt _))    = return $ TSymbol "Int"
typeof (S.SAtom (S.AString _)) = return $ TParam $ [TSymbol "List", TSymbol "Char"] -- a List of Char
typeof (S.SAtom (S.ASymbol s)) = lookupEnv s

-- Type of function application (f x y)
-- This function uses the fact that Either is a monad, and thus "do" notation can be used.
typeof exp@(S.SList (func:params)) = go
  where
    go = result `catchE` \err -> throwE $ "Error in " ++ show exp ++ ": " ++ err
    
    result :: Infer Type
    result = do
      -- Get the type of the function
      funcType <- typeof func
      -- The function should be of type (-> T1 ... TN R).
      -- Extract those type parameters, i.e. the expected types of the function's arguments
      -- and the function's return type
      funcTypeParams <- getFuncTypeParams funcType
      -- Get the types of the actual arguments
      actualParamTypes <- forM params typeof
      -- Check that formal and actual arguments have same size
      let nExp = (length funcTypeParams) - 1
          nAct = length actualParamTypes
        in verify (nExp == nAct)
                  ("Function expects " ++ show nExp ++ " arguments, but only " ++ show nAct ++ " given.")
      -- Match formal and actual argument types
      forM_ (zip funcTypeParams actualParamTypes) (\(a,b)->matchTypes a b)
      -- Result type is determined by the function's last function type parameter
      return $ last funcTypeParams
    
    verify :: Bool -> Error -> Infer ()
    verify b e = if b then return () else (throwE e)

    getFuncTypeParams :: Type -> Infer [Type]
    getFuncTypeParams t = 
      case t of
        TParam (arrow : funcTypeParams) -> return funcTypeParams
        otherwise -> throwE $ "Not a function. Its type is: " ++ show t
    
    matchTypes :: Type -> Type -> Infer ()
    matchTypes a b = verify (a == b) ("Types don't match: " ++ show a ++ " /= " ++ show b)

exampleEnv = Map.fromList
  [ ("n", tInt)
  , ("m", tInt)
  , ("true", tBool)
  , ("false", tBool)
  , ("s", tString)
  , ("ss", tList tString)
  , ("f", tFunc [tInt, tInt, tInt])
  , ("id", TForall "a" $ tFunc [TSymbol "a", TSymbol "a"])
  , ("unwords", tFunc [ tList tString, tString ])
  , ("show", TForall "a" $ tFunc [TSymbol "a", tString])
  ]

exampleExprs :: [SExpr]
exampleExprs =
  [ S.SAtom $ S.AInt 42
  , S.SAtom $ S.AString "hello"
  , S.SAtom $ S.ASymbol "unknown"
  , S.SAtom $ S.ASymbol "n"
  , S.SAtom $ S.ASymbol "true"
  , S.SAtom $ S.ASymbol "s"
  , S.SAtom $ S.ASymbol "f"
  , S.SList [ S.SAtom $ S.ASymbol "g", S.SAtom $ S.ASymbol "h" ]
  , S.SList [ S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "m" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "true" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "m" ]
  , S.SList [ S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "n" ]
  ]

exampleTypes = map (\e -> (\(t, _) -> t) $ runState (runExceptT $ typeof e) exampleEnv) exampleExprs

example = putStrLn $ unlines $ map show $ zip exampleExprs exampleTypes

