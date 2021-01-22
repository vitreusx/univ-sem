{-# LANGUAGE FlexibleContexts #-}

module Lexer where

import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Indent
import qualified Text.Parsec.Token             as Tok
import           Control.Monad.Identity

type IParser a = ParsecT String () (IndentT Identity) a

langDef :: GenLanguageDef String u (IndentT Identity)
langDef = Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum
  , Tok.opStart         = oneOf "+-*<=>:!&"
  , Tok.opLetter        = oneOf "+-*<=>:!&"
  , Tok.reservedNames   = [ "true"
                          , "false"
                          , "if"
                          , "then"
                          , "while"
                          , "do"
                          , "try"
                          , "fail"
                          , "commit"
                          , "debug"
                          ]
  , Tok.reservedOpNames = ["+", "-", "*", "&&", "<", "==", ">", "!", ":=", ":"]
  , Tok.caseSensitive   = False
  }

lexer = Tok.makeTokenParser langDef
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
braces = Tok.braces lexer
integer = Tok.integer lexer
semi = Tok.semi lexer
whiteSpace = Tok.whiteSpace lexer
