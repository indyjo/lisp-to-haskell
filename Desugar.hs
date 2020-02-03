module Desugar (desugar) where
import Control.Monad.Trans.Except
import SExpr

type Error = String
type Desugar = Except Error

desugar :: SExpr -> Desugar SExpr

-- desugar "do" combinators
desugar expr@(SList (SAtom (ASymbol "do") : stmts)) = do
    if null stmts then throwE "Invalid empty (do)" else return ()
    go stmts `catchE` \e -> throwE $ "Error in expression " ++ show expr ++ ": " ++ e
  where
    -- go takes a list of statements in (n action) form and transforms to the corresponding chain
    -- (>>= ...) operations.
    go :: [SExpr] -> Desugar SExpr
    -- Last statement (n expr) -> expr
    go (SList (SAtom (ASymbol _) : expr : []) : []) = return expr
    -- Any statement  (n expr) -> xform to (>>= expr (fun (n) ...))
    go (SList (SAtom (ASymbol n) : expr : []) : ss) = do
      next <- go ss
      return $ SList [
        SAtom (ASymbol ">>="),
        expr,
        SList [SAtom (ASymbol "fun"), SList [SAtom $ ASymbol n], next]]
    go bad = throwE $ "Invalid statement " ++ show bad

desugar expr = return expr

