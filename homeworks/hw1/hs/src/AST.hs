module AST where

newtype ANum = Num Integer
               deriving (Show)

newtype AVar = Var String
               deriving (Show)

newtype ATrId = TrId String
                deriving (Show)

data ArithOp = Sum | Diff | Prod
             deriving (Show)

data AExpr = NumExpr ANum
           | VarExpr AVar
           | Op ArithOp AExpr AExpr
           deriving (Show)

data ArithRel = Less | Equal
              deriving (Show)

data ABExpr = Literal Bool
            | ARel ArithRel AExpr AExpr
            | And ABExpr ABExpr
            | Neg ABExpr
            deriving (Show)

data AInstr = Assign AVar AExpr
            | Seq [AInstr]
            | If ABExpr AInstr
            | While ABExpr AInstr
            | Try ATrId AInstr
            | Fail ATrId
            | Commit
            | Debug
            deriving (Show)
