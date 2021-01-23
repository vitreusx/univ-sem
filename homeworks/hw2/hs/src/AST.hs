module AST where

-- n ::= 0 | 1 | -1 | ...
newtype ANum = ANum Integer
               deriving Show

-- x ::= x | y | ...
newtype AVar = AVar String
               deriving Show

-- e ::= n | x | e_1 + e_2 | e_1 * e_2 | e_1 - e_2
data AExprOp = Sum | Prod | Diff
             deriving Show

data AExpr = NumExpr ANum
           | VarExpr AVar
           | Op AExprOp AExpr AExpr
           deriving Show

-- d ::= var x_1 set to x_2 by S | d_1; d_2 | \varepsilon
data ADecl = SetterDecl AVar AVar AStmt
           | DeclSeq [ADecl]
           deriving Show

-- S ::= x := e | var := e | S_1; S_2 | if e = 0 then S_1 else S_2 |
--       while e != 0 do S | begin d; S end
data AStmt = SetReal AVar AExpr
           | SetMeta AExpr
           | StmtSeq [AStmt]
           | If AExpr AStmt AStmt
           | While AExpr AStmt
           | Begin ADecl AStmt
           deriving Show
