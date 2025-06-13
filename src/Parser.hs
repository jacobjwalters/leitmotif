module Parser where

import Control.Applicative ((<|>))
import Text.Trifecta

import Types

(?>) :: Parsing m => String -> m a -> m a
name ?> parser = parser <?> name

-- Identifiers
keywords :: [String]
keywords =
  [ "lambda", "λ"
  , "Int", "Char"
  , "letdata", "in"
  , "match", "with"
  , "strLen", "strHead", "strTail"
  , "__getLine", "__print", "__readFile"
  ]

valueId :: Parser ValueIdentifier
valueId = "value identifier" ?> do
  c <- lower
  cs <- many alphaNum
  _ <- whiteSpace
  if (c:cs) `notElem` keywords
    then pure (c:cs)
    else unexpected "keyword"

tconId :: Parser TConIdentifier
tconId = "type constructor" ?> do
  c <- upper
  cs <- many alphaNum
  _ <- whiteSpace
  if (c:cs) `notElem` keywords
    then pure (c:cs)
    else unexpected $ "keyword " ++ c:cs

vconId :: Parser VConIdentifier
vconId = "value constructor" ?> tconId

-- Types
fn :: Parser (Type, [TypeBinding])
fn = do
  ar <- sepBy1 parseType' (symbol "->")
  --guard (length ar >= 2)
  let (args,  ret)  = (init ar, last ar)
      (args', ret') = (map fst args, fst ret)
      bindings = map snd args
  pure $ if args == [] then ret else (Fn args' ret', concat bindings)

tcon :: Parser Type
tcon = TCon <$> tconId

-- strictly non-left recursive
parseType' :: Parser (Type, [TypeBinding])
parseType' = choice
  [ try $ parens tvar
  , parens parseType
  , try $ do
      tc <- tcon
      pure (tc, [])
  , symbol "Int"  *> pure (TInt,  [])
  , symbol "Char" *> pure (TChar, [])
  ]

parseType :: Parser (Type, [TypeBinding])
parseType = "type" ?> choice
  [ try fn
  , parseType'
  ]

-- x : type
tvar :: Parser (Type, [TypeBinding])
tvar = do
  name <- valueId
  _ <- symbol ":"
  (ty, bindings) <- parseType
  pure (ty, (name, ty) : bindings)

-- Parses a type signature that introduces bindings:
-- (x : Int) -> Int -> (y : String) -> (_ : Bool) -> Float
-- --> (Fn [Int, Int, String, Bool] Float, [(x, Int), (y, String)])
bindingTypeSig :: Parser (Type, [TypeBinding])
bindingTypeSig = do
  pure undefined

-- Patterns
pintlit, pcharlit, pstrlit :: Parser Pattern
pintlit  = PIntLit  <$> integer
pcharlit = PCharLit <$> charLiteral
pstrlit  = PStrLit  <$> stringLiteral

pvar :: Parser Pattern
pvar = PVar <$> valueId

pcon :: Parser Pattern
pcon = PCon <$> vconId
            <*> many pattern

pattern :: Parser Pattern
pattern = "pattern" ?> choice
  [ symbol "_" *> pure PWildcard
  , pintlit, pcharlit, pstrlit
  , pcon
  , pvar
  , parens pattern
  ]


-- Expressions
nullaryPrim, unaryPrim, binaryPrim :: Parser Expr
nullaryPrim = Prim <$> choice
  [ -- IO
    symbol "__getLine"  *> pure (IOP GetLine)
  ]
unaryPrim = Prim <$> choice
  [ -- Strings
    symbol "strLen"  *> pure StrLen
  , symbol "strHead" *> pure StrHead
  , symbol "strTail" *> pure StrTail

    -- IO
  , symbol "__print"    *> pure (IOP Print)
  , symbol "__readFile" *> pure (IOP ReadFile)
  ]
binaryPrim = Prim <$> choice
  [ -- Numerics
    symbol "+" *> pure Plus
  , symbol "*" *> pure Mult
  , symbol "-" *> pure Minus

    -- IO
  , symbol "__IOSeq" *> pure (IOP IOSeq)
  ]

nullaryOp, unaryOp, binaryOp :: Parser Expr
nullaryOp = nullaryPrim

unaryOp = do
  op <- unaryPrim
  e <- expr
  pure $ App op [e]

binaryOp = do
  l <- expr'
  op <- binaryPrim
  r <- expr
  pure $ App op [l, r]

lambda :: Parser Expr
lambda = "lambda" ?> do
  _ <- symbol "\\" <|> symbol "lambda" <|> symbol "λ"
  args <- sepBy1 tvar (comma <|> space)
  let bindings = concat $ map snd args

  _ <- symbol "." <|> symbol "->" <|> symbol "=>"
  body <- expr
  pure $ Abs bindings body

-- data ::= letdata TCon = VCon1 type* | VCon2 type* | ... in e
letdata :: Parser Expr
letdata = "let data" ?> do
  _ <- symbol "letdata"
  name <- tconId
  _ <- symbol "="
  branches <- sepBy1 (branch $ TCon name) (symbol "|")
  _ <- symbol "in"
  body <- expr
  pure $ LetData name branches body
  where branch :: Type -> Parser (VConIdentifier, Type)
        branch ret = do
          name <- vconId
          args <- many parseType
          pure (name, if args /= [] then Fn (map fst args) ret else ret)

{-
match s with
| pattern => expr
| ...
;
-}

match :: Parser Expr
match = "match" ?> do
  _ <- symbol "match"
  scrutinee <- expr
  _ <- symbol "with"
  cases <- some parseCase
  _ <- symbol ";"
  pure $ Match scrutinee cases
  where parseCase :: Parser (Pattern, Expr)
        parseCase = "match branch" ?> do
          _ <- symbol "|"
          p <- pattern
          _ <- symbol "=>"
          b <- expr
          pure (p, b)

appable :: Parser Expr
appable = choice
  [ parens expr
  , Var <$> valueId
  , EVCon <$> vconId
  ]

-- Strictly non-left recursive
expr' :: Parser Expr
expr' = "expression (prime)" ?> choice
  [ LInt  <$> integer
  , LChar <$> charLiteral
  , LStr  <$> stringLiteral
  , nullaryOp
  , unaryOp
  , lambda
  , letdata
  , match
  , appable
  ]

expr :: Parser Expr
expr = "expression" ?> choice
  [ try binaryOp
  , try $ App <$> appable <*> some expr
  , expr'
  ]

adtDecl :: Parser Decl
adtDecl = "data declaration" ?> do
  _ <- symbol "data"
  name <- tconId
  _ <- symbol "="
  branches <- sepBy1 (branch $ TCon name) (symbol "|")
  _ <- newline
  pure $ ADTDecl name branches

  where branch :: Type -> Parser (VConIdentifier, Type)
        branch ret = do
          name <- vconId
          args <- many parseType
          pure (name, if args /= [] then Fn (map fst args) ret else ret)

fnDecl :: Parser Decl
fnDecl = "function declaration" ?> do
  name <- valueId
  _ <- symbol ":"
  (ty, bindings) <- parseType
  _ <- symbol "="
  body <- expr
  pure $ FnDecl name ty bindings body

decl :: Parser Decl
decl = "declaration" ?> (adtDecl <|> fnDecl)

program :: Parser Program
program = many decl <* eof

parseExpr :: String -> Result Expr
parseExpr = parseString (expr <* eof) mempty

parseProgram :: String -> Result Program
parseProgram = parseString program mempty
