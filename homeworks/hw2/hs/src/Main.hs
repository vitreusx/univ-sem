module Main where

import           AST
import           Lexer
import           Parser
import           Eval                           ( run )
import           System.Environment

-- Given a parser, file and code, try to parse the code
-- and if it fails, yield the error; otherwise, show the AST
showParsed :: Show a => IParser a -> String -> String -> IO ()
showParsed p file code = do
  let parsed = parse' (parser' p) file code
  case parsed of
    Left  err -> print err >> fail "parser error"
    Right ast -> print ast

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", codeType, file] -> do
      code <- readFile file
      let short' parser = showParsed parser file code
      case codeType of
        "Num"     -> short' num
        "Var"     -> short' var
        "Expr"    -> short' expr
        "Decl"    -> short' decl
        "Stmt"    -> short' stmt
        otherwise -> fail "invalid code type"
    ["run", file] -> do
      code <- readFile file
      case parse file code of
        Left  err -> print err >> fail "parser error"
        Right ast -> print (run ast)
    otherwise -> fail "invalid args"
