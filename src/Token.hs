module Token where

import           Numeric.Natural

data Token
  = TokenFun
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenTrue
  | TokenFalse
  | TokenSucc
  | TokenPred
  | TokenIsZero
  | TokenNat Natural
  | TokenVar String
  | TokenEqGt
  | TokenLParen
  | TokenRParen
  deriving (Eq, Show)
