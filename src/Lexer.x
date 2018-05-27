{
module Lexer (lex) where

import Prelude hiding (lex)
import Token
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-
  $white+                         ;
  fun                             { \_ -> TokenFun }
  \\                              { \_ -> TokenFun }
  λ                               { \_ -> TokenFun }
  if                              { \_ -> TokenIf }
  then                            { \_ -> TokenThen }
  else                            { \_ -> TokenElse }
  true                            { \_ -> TokenTrue }
  false                           { \_ -> TokenFalse }
  succ                            { \_ -> TokenSucc }
  pred                            { \_ -> TokenPred }
  is_zero                         { \_ -> TokenIsZero }
  =>                              { \_ -> TokenEqGt }
  \(                              { \_ -> TokenLParen }
  \)                              { \_ -> TokenRParen }
  [0-9]+                          { \s -> TokenNat (read s) }
  $alpha [$alpha $digit \_ \']*   { \s -> TokenVar s }

{
lex :: String -> [Token]
lex = alexScanTokens
}