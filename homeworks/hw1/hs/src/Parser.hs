module Parser where

import           AST
import           Lexer

import           Text.Parsec             hiding ( parse )
import           Text.Parsec.Indent
import           Text.Parsec.Expr
import           Control.Monad

num = liftM Num integer

var = liftM Var identifier

trid = liftM TrId identifier

arith = buildExpressionParser arithOps arithTerm

arithOps =
  [ [Infix (reservedOp "*" >> return (Op Prod)) AssocLeft]
  , [ Infix (reservedOp "+" >> return (Op Sum))  AssocLeft
    , Infix (reservedOp "-" >> return (Op Diff)) AssocLeft
    ]
  ]

arithTerm =
  parens arith
    <|> liftM (NumExpr . Num) integer
    <|> liftM (VarExpr . Var) identifier

bool = buildExpressionParser boolOps boolTerm

boolOps =
  [ [Prefix (reservedOp "not" >> return Neg)]
  , [Infix (reservedOp "and" >> return And) AssocLeft]
  ]

boolTerm =
  parens bool
    <|> (reserved "true" >> return (Literal True))
    <|> (reserved "false" >> return (Literal False))
    <|> arithRel

arithRel = do
  e1 <- arith
  op <- rel
  e2 <- arith
  return $ ARel op e1 e2

rel =
  (reservedOp "<" >> return Less)
    <|> (reservedOp "==" >> return Equal)

instr = block'

makeSeq instrs =
  if length instrs == 1 then head instrs else Seq instrs

seq' = do
  instrs <- sepBy1 cont' semi
  return $ makeSeq instrs

block' = do
  block seq' >>= return . makeSeq

cont' = braces instr <|> primitive

primitive =
  if' <|> while' <|> try' <|> assign <|> fail' <|> commit <|> debug

assign = do
  x <- var
  reservedOp ":="
  e <- arith
  return $ Assign x e

ifHeader = do
  reserved "if"
  b <- bool
  reserved "then"
  return b

blockOp :: (a -> AInstr -> c) -> IParser a -> IParser c
blockOp ctor hdr =
  let ctor' h ix = ctor h (makeSeq ix)
  in  withBlock ctor' hdr ((indented >> block') <|> cont')

if' = blockOp If ifHeader

whileHeader = do
  reserved "while"
  b <- bool
  reserved "do"
  return b

while' = blockOp While whileHeader

tryHeader = do
  reserved "try"
  t <- trid
  reservedOp ":"
  return t

try' = blockOp Try tryHeader

fail' = do
  reserved "fail"
  t <- trid
  return $ Fail t

commit = do
  reserved "commit"
  return Commit

debug = do
  reserved "debug"
  return Debug

parser = whiteSpace >> topLevel >> instr

parse = runIndentParser parser ()
