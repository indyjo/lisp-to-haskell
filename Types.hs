module Types (Type, Env, Error, Infer, typeOf) where
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.State.Lazy (State, get, put, modify, runState)
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

-- Given a type that contains free type variables, returns a closed corresponding forall type.
closeOver :: Type -> Type
closeOver t = case freeVars t of
  (n:_)     -> closeOver (TForall n t)
  otherwise -> t

type Env = Map String Type
type Subst = Map String Type
type Error = String

initSubst :: Subst
initSubst = Map.empty

data InferState = InferState { env :: Env, count :: Int, subst :: Subst }

-- The monad that typeof will operate in.
-- Supports exceptions and passing the environment as state.
type Infer = ExceptT Error (State InferState)

lookupEnv :: String -> Infer Type
lookupEnv s = do
  InferState { env = env } <- get
  case Map.lookup s env of
    Just t  -> return t
    Nothing -> throwE $ "No type known for symbol " ++ s

newTypeVar :: Infer String
newTypeVar = do
  s <- get
  let n  = "t" ++ show (count s)
      s' = s { count = count s + 1 }
  put s'
  return n

replaceVar :: String -> Type -> Type -> Type
replaceVar oldN newT t = case t of
    TParam ts    -> TParam $ map recurse ts
    TForall n t' -> if n == oldN then t else TForall n (recurse t')
    TSymbol n    -> if n == oldN then newT else t
  where
    recurse = replaceVar oldN newT

-- When encountering a Forall type, we have to substitute its type variables by free variables
eliminateForall :: Type -> Infer Type
eliminateForall (TForall n t) = do
  n' <- newTypeVar
  let t' = replaceVar n (TSymbol n') t
  eliminateForall t'

eliminateForall t = return t

typeofM :: SExpr -> Infer Type

-- type of an integer literal
typeofM (S.SAtom (S.AInt _))    = return $ TSymbol "Int"

-- Type of a string literal
typeofM (S.SAtom (S.AString _)) = return $ TParam $ [TSymbol "List", TSymbol "Char"] -- a List of Char

-- Type of a symbol. Eliminate any Forall types.
typeofM (S.SAtom (S.ASymbol s)) = do
  t <- lookupEnv s
  eliminateForall t

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
    (preresult, InferState { subst = substitutions }) = runState (runExceptT (typeofM expr)) $ InferState { env = env, count = 0, subst = initSubst }
    result = fmap (closeOver . applySubstitutions substitutions) preresult
    applySubstitutions s t = Map.foldrWithKey (\name subst accum -> replaceVar name subst accum) t s

constrain :: String -> Type -> Infer ()
constrain n t = do
  InferState { subst = s } <- get
  case Map.lookup n s of
    Nothing -> do
      modify (\s -> s { subst = Map.insert n t (subst s) })
    Just t' -> do
      unify t t'

unify :: Type -> Type -> Infer ()
unify (TParam xs) (TParam ys) = do
  forM_ (zip xs ys) (\(x, y) -> unify x y)

unify x y | x == y = return ()

unify (TSymbol n) t | isTypeVar n && not (n `elem` freeVars t) = constrain n t
unify t (TSymbol n) | isTypeVar n && not (n `elem` freeVars t) = constrain n t

unify x y = throwE $ "Can't unify types: " ++ show x ++ " <-> " ++ show y

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
  , ("ff", TForall "a" $ tFunc [TSymbol "a", TSymbol "a", TSymbol "a"])
  , ("magic", TForall "a" $ TSymbol "a")
  , ("fx", TForall "a" $ tFunc [TSymbol "a", TSymbol "Int"])
  , ("fy", TForall "a" $ tFunc [TSymbol "Int", TSymbol "a"])
  ]

exampleExprs :: [SExpr]
exampleExprs =
  [ S.SAtom $ S.AInt 42
  , S.SAtom $ S.AString "hello"
  , S.SAtom $ S.ASymbol "unknown"
  , S.SAtom $ S.ASymbol "n"
  , S.SAtom $ S.ASymbol "m"
  , S.SAtom $ S.ASymbol "true"
  , S.SAtom $ S.ASymbol "s"
  , S.SAtom $ S.ASymbol "ss"
  , S.SAtom $ S.ASymbol "f"
  , S.SAtom $ S.ASymbol "ff"
  , S.SAtom $ S.ASymbol "fx"
  , S.SAtom $ S.ASymbol "fy"
  , S.SAtom $ S.ASymbol "id"
  , S.SAtom $ S.ASymbol "magic"
  , S.SList [ S.SAtom $ S.ASymbol "g", S.SAtom $ S.ASymbol "h" ]
  , S.SList [ S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "m" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "true" ]
  , S.SList [ S.SAtom $ S.ASymbol "f", S.SAtom $ S.ASymbol "n", S.SAtom $ S.ASymbol "m" ]
  , S.SList [ S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "n" ]
  , S.SList [ S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "f" ]
  , S.SList [ S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "id" ]
  , S.SList [ S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "magic" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.AInt 23, S.SAtom $ S.AInt 42]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.AInt 23, S.SAtom $ S.AString "hello" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.AInt 23, S.SAtom $ S.ASymbol "magic" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "id" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.ASymbol "id", S.SAtom $ S.ASymbol "magic" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.ASymbol "magic", S.SAtom $ S.ASymbol "id" ]
  , S.SList [ S.SAtom $ S.ASymbol "ff", S.SAtom $ S.ASymbol "fx", S.SAtom $ S.ASymbol "fy" ]
  , S.SList [ S.SAtom $ S.ASymbol "fun", S.SList [S.SAtom $ S.ASymbol "x"], S.SAtom $ S.ASymbol "x"]
  ]

exampleTypes = map (typeOf exampleEnv) exampleExprs

example = putStrLn $ unlines $ map print $ zip exampleExprs exampleTypes
  where print (_, Left err) = err
        print (expr, Right typ) = show expr ++ " :: " ++ show typ

