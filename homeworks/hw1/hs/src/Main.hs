module Main where

import           Parser
import           Eval
import           Trans                          ( eval
                                                , dump
                                                )
import           Control.Lens
import           System.Environment

showcase progAST = do
  let prog = dump $ evalInstr progAST
  let res  = eval prog
  print res

main = do
  args <- getArgs
  let sourceName = head args
  sourceText <- readFile sourceName
  case parse sourceName sourceText of
    Left  err     -> print err >> fail "parser error"
    Right progAST -> showcase progAST
