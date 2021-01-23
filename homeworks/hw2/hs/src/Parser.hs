module Parser where

import           AST
import           Lexer

import           Text.Parsec             hiding ( parse )
import           Text.Parsec.Indent
import           Text.Parsec.Expr
import           Control.Monad

-- ANum - trivial
num :: IParser ANum
num = liftM ANum integer

-- AVar - trivial
var :: IParser AVar
var = liftM AVar identifier

-- AExpr - we use an expression parser
expr :: IParser AExpr
expr = buildExpressionParser ops exprTerm

ops =
  [ [Infix (reservedOp "*" >> return (Op Prod)) AssocLeft]
  , [ Infix (reservedOp "+" >> return (Op Sum))  AssocLeft
    , Infix (reservedOp "-" >> return (Op Diff)) AssocLeft
    ]
  ]

exprTerm :: IParser AExpr
exprTerm =
  parens expr
    <|> liftM (NumExpr . ANum) integer
    <|> liftM (VarExpr . AVar) identifier

-- In ADecl or AStmt, due to the inherent issues in the original
-- syntax, sequencing the terms is done via layout. More specifically,
-- S in "var x_1..S", S_i in "if e = 0 then .." etc. have the extent
-- defined in a similar fashion to Haskell layout rules, i.e. that
-- AStmt and ADecl extends as far as the level is the same.
-- For example,
--   while e != 0 do S_1, or     while e != 0 do
--                   S_2           S_1
--                                 S_2
-- is interpreted as
--   while e != 0 do {S_1; S_2}
cont :: IParser a -> ([a] -> a) -> IParser a
cont part ctor = do
  sameOrIndented
  withPos $ do
    parts <- block part
    return (unravel ctor parts)

-- When conjoining S_1.. S_n, if n=1, then there's
-- no need to wrap it further around Seq
unravel :: ([a] -> a) -> [a] -> a
unravel seqCtor xs = if (length xs) /= 1 then seqCtor xs else head xs

-- Parser for S_1; ..; S_n where S_i are parsed by s
compact :: IParser a -> ([a] -> a) -> IParser a
compact part ctor = do
  parts <- part `sepBy` semi
  return (unravel ctor parts)

-- ADecl

-- "All" ADecl (i.e. only "var x_1..S")
decl :: IParser ADecl
decl = setterDecl

-- Contiguous block of ADecl
contDecl :: IParser ADecl
contDecl = cont decl DeclSeq

-- Parser for "var x_1 set to x_2 by S"
setterDecl :: IParser ADecl
setterDecl = withPos $ do
  reserved "var"
  x_1 <- var
  reserved "set to"
  x_2 <- var
  reserved "by"
  s <- contStmt

  return (SetterDecl x_1 x_2 s)

-- AStmt

-- Compact AStmt
compactStmt :: IParser AStmt
compactStmt = setReal <|> setMeta

-- "All" AStmt (aside from S_1; S_2 which is handled automatically)
stmt :: IParser AStmt
stmt = compactStmt <|> ifStmt <|> whileStmt <|> beginStmt
-- stmt = compactStmt

-- Contiguous block of AStmt
contStmt :: IParser AStmt
contStmt = cont stmt StmtSeq

-- Parser for x := e
setReal :: IParser AStmt
setReal = do
  x <- var
  reservedOp ":="
  e <- expr

  return (SetReal x e)

-- Parser for var := e
setMeta :: IParser AStmt
setMeta = do
  reserved "var"
  reservedOp ":="
  e <- expr

  return (SetMeta e)

-- Parser for "if e = 0 then S_1 else S_2"
ifStmt :: IParser AStmt
ifStmt = withPos $ do
  reserved "if"
  e <- expr
  reservedOp "= 0"
  reserved "then"
  s_t <- contStmt
  reserved "else"
  s_f <- contStmt

  return (If e s_t s_f)

-- Parser for "while e != 0 do S"
whileStmt :: IParser AStmt
whileStmt = withPos $ do
  reserved "while"
  e <- expr
  reservedOp "!= 0"
  reserved "do"
  s <- contStmt

  return (While e s)

-- Parser for "begin d; S end"
beginStmt :: IParser AStmt
beginStmt = withPos $ do
  reserved "begin"
  d <- contDecl
  s <- contStmt
  reserved "end"

  return (Begin d s)

-- Utility parser wrappers

-- Wraps a parser, so that it removes leading whitespace,
-- ensures it's top level, parses proper, and ensures
-- entire text has been parsed
parser' :: IParser a -> IParser a
parser' p = do
  whiteSpace
  topLevel
  ast <- p
  eof
  return ast

-- Parser for Stmt
parser :: IParser AStmt
parser = parser' stmt

-- Given parser, file and code, parse it
parse' :: IParser a -> String -> String -> Either ParseError a
parse' parser file code = runIndentParser parser () file code

-- parse' for Stmt
parse :: SourceName -> String -> Either ParseError AStmt
parse = parse' stmt
