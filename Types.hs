module Types (Type, Env, Error, Infer, typeOf) where
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

-- type variable must begin with lower-case letters
isTypeVar :: String -> Bool
isTypeVar (n:ns) = n >= 'a' && n <= 'z'
isTypeVar _ = False

-- Given a type expression, return any unbound type variable names in the order of use. Names might appear twice.
freeVars :: Type -> [String]
freeVars (TSymbol n) | isTypeVar n = [n]
                     | otherwise   = []
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


typeofM :: SExpr -> Infer Type

-- type of an integer literal
typeofM (S.SAtom (S.AInt _))    = return $ TSymbol "Int"

-- Type of a string literal
typeofM (S.SAtom (S.AString _)) = return $ TParam $ [TSymbol "List", TSymbol "Char"] -- a List of Char

-- Type of a symbol
typeofM (S.SAtom (S.ASymbol s)) = lookupEnv s

-- Type of ()
typeofM (S.SList []) = return $ TSymbol "()"

-- type of a redundant pair of parentheses (e)
typeofM (S.SList [e]) = typeofM e

-- Type of function application (f x) or (f x y) ...
typeofM exp@(S.SList (func:params)) = go
  where
    go = result `catchE` \err -> throwE $ "Error in " ++ show exp ++ ": " ++ err
    
    result :: Infer Type
    result = do
      -- Get the type of the function
      funcType <- typeofM func
      -- The function should be of type (-> T1 ... TN R).
      -- Extract those type parameters, i.e. the expected types of the function's arguments
      -- and the function's return type
      funcTypeParams <- getFuncTypeParams funcType
      -- Get the types of the actual arguments
      actualParamTypes <- forM params typeofM
      -- Check that formal and actual arguments have same size
      let nExp = (length funcTypeParams) - 1
          nAct = length actualParamTypes
        in verify (nExp == nAct)
                  ("Function expects " ++ show nExp ++ " arguments, but only " ++ show nAct ++ " given.")
      -- Match formal and actual argument types
      forM_ (zip funcTypeParams actualParamTypes) (\(a,b) -> unify a b)
      -- Result type is determined by the function's last function type parameter
      return $ last funcTypeParams
    
    verify :: Bool -> Error -> Infer ()
    verify b e = if b then return () else (throwE e)

    getFuncTypeParams :: Type -> Infer [Type]
    getFuncTypeParams t = 
      case t of
        TParam (arrow : funcTypeParams) -> return funcTypeParams
        otherwise -> throwE $ "Not a function. Its type is: " ++ show t

-- non-monadic wrapper around typeofM
typeOf :: Env -> SExpr -> Either Error Type
typeOf env expr = result
  where
    (result, _) = runState (runExceptT (typeofM expr)) env

unify :: Type -> Type -> Infer ()
unify (TParam (TSymbol "->" : xs)) (TParam (TSymbol "->" : ys)) = do
  forM_ (zip xs ys) (\(x, y) -> unify x y)

unify x y | x == y    = return ()


unify x y = throwE $ "Types don't match: " ++ show x ++ " <-> " ++ show y

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

exampleTypes = map (typeOf exampleEnv) exampleExprs

example = putStrLn $ unlines $ map show $ zip exampleExprs exampleTypes

