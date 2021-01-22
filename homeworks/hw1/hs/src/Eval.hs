module Eval where

import           Trans
import           AST
import           Control.Monad
import           Data.Text.Lazy


evalArithOp Sum  = pure (+)
evalArithOp Diff = pure (-)
evalArithOp Prod = pure (*)

evalExpr (NumExpr (Num     n)) = pure n

evalExpr (VarExpr (AST.Var x)) = do
  ensureVar x
  val x

evalExpr (Op op e1 e2) = do
  ve1 <- evalExpr e1
  ve2 <- evalExpr e2
  vop <- evalArithOp op
  pure $ vop ve1 ve2

evalArithRel Less  = pure (<)
evalArithRel Equal = pure (==)

evalBExpr (Literal b     ) = pure b

evalBExpr (ARel rel e1 e2) = do
  ve1  <- evalExpr e1
  ve2  <- evalExpr e2
  vrel <- evalArithRel rel
  pure $ vrel ve1 ve2

evalBExpr (And b1 b2) = do
  vb1 <- evalBExpr b1
  vb2 <- evalBExpr b2
  pure $ vb1 && vb2

evalBExpr (Neg b) = do
  vb <- evalBExpr b
  pure (not vb)

whileM_ b xs = do
  vb <- b
  when vb (xs >> whileM_ b xs)

evalInstr (Assign (AST.Var x) e) = do
  ensureVar x
  ve <- evalExpr e
  setVar x ve

evalInstr (Seq instrs) = forM_ instrs evalInstr

evalInstr (If b i    ) = do
  vb <- evalBExpr b
  when vb (evalInstr i)

evalInstr (While b        i) = whileM_ (evalBExpr b) (evalInstr i)

evalInstr (Try   (TrId t) i) = do
  try t $ evalInstr i

evalInstr (Fail (TrId t)) = do
  Trans.fail t

evalInstr (Commit) = commit

evalInstr (Debug ) = do
  state <- get
  error $ show state
