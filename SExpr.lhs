> module SExpr where
> import GHC.Unicode (isSpace)
> import Text.ParserCombinators.ReadP

Let's define an Atom as either a symbol (like this-symbol), an int (like 42) or a string (like "this").

> data Atom = ASymbol String
>           | AInt Int
>           | AString String

Implement Show to yield an output equivalent to the input.

> instance Show Atom where
>   show (ASymbol s) = s
>   show (AInt n) = show n
>   show (AString s) = ['"'] ++ s ++ ['"']

Define an SExpr to be either an Atom or a parenthesized list of SExprs.

> data SExpr = SAtom Atom | SList [SExpr]

Again, implement Show to yield an equivalent output.

> instance Show SExpr where
>   show (SAtom a) = show a
>   show (SList l) = "(" ++ (unwords $ map show l) ++ ")"

Now define parsers for the different Atom cases.

> parseASymbol :: ReadP Atom
> parseASymbol = do c1 <- satisfy $ \c -> not $ (isSpace c) || any (==c) "()0123456789\""
>                   s <- many $ satisfy $ \c -> not $ (isSpace c) || any (==c) "()\""
>                   return $ ASymbol $ c1 : s

> parseAInt :: ReadP Atom
> parseAInt = do sign <- option '+' $ char '-'
>                digits <- munch1 (\c -> c >= '0' && c <= '9')
>                return $ AInt $ if sign == '+' then (read $ digits) else -(read $ digits)

> parseAString :: ReadP Atom
> parseAString = do char '"'
>                   s <- munch (/= '"')
>                   char '"'
>                   return $ AString s

And a parser for Atoms in every case. Note how ambiguities are resolved using the <++ operator.

> parseAtom :: ReadP Atom
> parseAtom = do skipSpaces
>                choice [parseAInt <++ parseAString <++ parseASymbol]

Now define parsers for the two cases of SExpr and for SExpr itself.

> parseSAtom :: ReadP SExpr
> parseSAtom = do a <- parseAtom
>                 return $ SAtom a

> parseSList :: ReadP SExpr
> parseSList = do char '('
>                 l <- sepBy parseSExpr $ munch1 isSpace
>                 skipSpaces
>                 char ')'
>                 return $ SList l

> parseSExpr :: ReadP SExpr
> parseSExpr = do skipSpaces
>                 choice [parseSAtom, parseSList]

> parseProgram :: ReadP SExpr
> parseProgram = do s <- parseSExpr
>                   skipSpaces
>                   return s

