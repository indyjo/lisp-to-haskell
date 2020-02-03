module Types (Type, Env, Error, Infer, typeOf) where
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Except (ExceptT, catchE, except, runExcept, runExceptT, throwE)
import Control.Monad.State.Strict (State, get, put, modify, runState, withState)
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Desugar (desugar)
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

action = TSymbol "Action"
tAction t = TParam $ [action, t]

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

-- Type of lambda expressions
typeofM exp@(S.SList (S.SAtom (S.ASymbol "fun") : (S.SList args) : body : [])) = go
  where
    go = result `catchE` \err -> throwE $ "Error in lambda expression " ++ show exp ++ ": " ++ err
    result = do
      locals <- createLocals
      -- create an environment in which formal arguments are bound
      InferState { env = oldEnv } <- get
      modify (\state -> state { env = Map.fromList locals `Map.union` oldEnv } )
      resultType <- typeofM body
      -- restore the original environment
      modify (\state -> state { env = oldEnv })
      return ( tFunc (map (\(_, t) -> t) locals ++ [resultType]) )
    createLocals :: Infer [(String, Type)]
    createLocals = forM args $ \arg -> case arg of
      S.SAtom (S.ASymbol name) -> do
        tvar <- newTypeVar
        return (name, TSymbol tvar)
      otherwise                -> throwE $ show arg ++ " found in list of formal arguments"

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
      -- Get the types of the actual arguments
      actualParamTypes <- forM params typeofM
      -- The function should be of type (-> T1 ... TN R).
      -- Extract those type parameters, i.e. the expected types of the function's arguments
      -- and the function's return type
      funcTypeParams <- getFuncTypeParams funcType actualParamTypes
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

    getFuncTypeParams :: Type -> [Type] -> Infer [Type]
    getFuncTypeParams t ts =
      case t of
        TParam (arrow : funcTypeParams) -> return funcTypeParams
        TSymbol n | isTypeVar n         -> do
          retTypeVar <- newTypeVar
          let coerced = tFunc (ts ++ [TSymbol retTypeVar])
          unify t coerced
          forM (ts ++ [TSymbol retTypeVar]) applySubstitutionsM
        otherwise                       -> throwE $ "Not a function. Its type is: " ++ show t

-- non-monadic wrapper around typeofM0
typeOf :: Env -> SExpr -> Either Error Type
typeOf env expr = runExcept $ do
  desugared <- desugar expr
  -- run type checker on desugared version of expression
  let initState = InferState { env = env, count = 0, subst = initSubst }
      typecheck = typeofM desugared
      r :: Either Error Type
      (r, InferState { subst = substitutions }) = runState (runExceptT typecheck) initState
  -- check for error and unwrap r (it's an Either Error Type)
  r <- except r
  -- apply type substitutions and add "forall" for every free variable.
  return $ closeOver $ applySubstitutions substitutions $ r

-- Given the map of substitutions, apply it to the given type and return a new type.
applySubstitutions :: Subst -> Type -> Type
applySubstitutions s t = Map.foldrWithKey (\name subst accum -> replaceVar name subst accum) t s

applySubstitutionsM :: Type -> Infer Type
applySubstitutionsM t = do
  InferState { subst = s } <- get
  return $ applySubstitutions s t

-- Constrain a type variable to a type t
constrain :: String -> Type -> Infer ()
constrain n t = do
  InferState { subst = s } <- get
  -- Unify with any previous constraint (i.e., constrain further)
  forM_ (Map.lookup n s) $ unify t
  -- If that worked, replace t with the unifier
  InferState { subst = s } <- get
  let t' = applySubstitutions s t
  modify (\state -> state { subst = Map.insert n t' s })

-- Unify takes two types and either finds a unifier by applying substitutions, or throws an error.
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
  , ("compute", TForall "a" $ tFunc[TSymbol "a", tAction $ TSymbol "a"])
  , (">>=", TForall "m" $ TForall "a" $ TForall "b" $
            tFunc [
              TParam [TSymbol "m", TSymbol "a"],
              tFunc [TSymbol "a", TParam [TSymbol "m", TSymbol "b"]],
              TParam [TSymbol "m", TSymbol "b"]])
  ]

eSym s = S.SAtom (S.ASymbol s)
eInt n = S.SAtom (S.AInt n)
eStr s = S.SAtom (S.AString s)
eLst l = S.SList l

exampleExprs :: [SExpr]
exampleExprs =
  [ eInt 42
  , eStr "hello"
  , eSym "unknown"
  , eSym "n"
  , eSym "m"
  , eSym "true"
  , eSym "s"
  , eSym "ss"
  , eSym "f"
  , eSym "ff"
  , eSym "fx"
  , eSym "fy"
  , eSym "id"
  , eSym "magic"
  , eSym "compute"
  , eSym ">>="
  , eLst [ eSym "g", eSym "h" ]
  , eLst [ eSym "n", eSym "m" ]
  , eLst [ eSym "f", eSym "n" ]
  , eLst [ eSym "f", eSym "n", eSym "true" ]
  , eLst [ eSym "f", eSym "n", eSym "m" ]
  , eLst [ eSym "id", eSym "n" ]
  , eLst [ eSym "id", eSym "f" ]
  , eLst [ eSym "id", eSym "id" ]
  , eLst [ eSym "id", eSym "magic" ]
  , eLst [ eSym "ff", eInt 23, eInt 42]
  , eLst [ eSym "ff", eInt 23, eStr "hello" ]
  , eLst [ eSym "ff", eInt 23, eSym "magic" ]
  , eLst [ eSym "ff", eSym "id", eSym "id" ]
  , eLst [ eSym "ff", eSym "id", eSym "magic" ]
  , eLst [ eSym "ff", eSym "magic", eSym "id" ]
  , eLst [ eSym "ff", eSym "fx", eSym "fy" ]
  , eLst [ eSym "fun", eLst [eSym "x"], eSym "x"]
  , eLst [ eSym "fun", eLst [eSym "x"], eSym "n"]
  , eLst [ eSym "fun", eLst [eSym "x"], eLst [ eSym "fx", eSym "x"]]
  , eLst [ eSym "fun", eLst [eSym "x"], eLst [ eSym "fy", eSym "x"]]
  , eLst [ eSym "fun", eLst [eSym "f"], eLst [ eSym "f", eSym "n" ]]
  , eLst [ eSym "fun", eLst [eSym "f"], eLst [ eSym "f", eSym "f" ]]
  , eLst [ eSym "fun", eLst [eSym "f", eSym "g"], eLst [eSym "fun", eLst [eSym "x"],  eLst [ eSym "f", eLst [eSym "g", eSym "x" ]]]]
  , eLst [ eSym "do", eLst [eSym "_", eLst [eSym "compute", eInt 42]]]
  , eLst [ eSym "do",
           eLst [eSym "n", eLst [eSym "compute", eInt 42]],
           eLst [eSym "_", eLst [eSym "compute", eLst [eSym "f", eSym "n", eInt 23]]]]
  , eLst [ eSym "do",
           eLst [eSym "f", eLst [eSym "compute", eStr "id"]],
           eLst [eSym "_", eLst [eSym "compute", eLst [eSym "f", eSym "f"]]]]
  ]

exampleTypes = map (typeOf exampleEnv) exampleExprs

example = putStrLn $ unlines $ map print $ zip exampleExprs exampleTypes
  where print (_, Left err) = err
        print (expr, Right typ) = show expr ++ " :: " ++ show typ

desugarExample = putStrLn $ unlines $ map (show . runExcept . desugar) exampleExprs
