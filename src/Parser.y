{
module Parser (parse) where

import           Bound
import           Numeric.Natural
import           Syntax
import           Token

}

%name parse
%tokentype { Token }
%error { parseError }

%token 
  fun             { TokenFun }
  if              { TokenIf }
  then            { TokenThen }
  else            { TokenElse }
  true            { TokenTrue }
  false           { TokenFalse }
  succ            { TokenSucc }
  pred            { TokenPred }
  is_zero         { TokenIsZero }
  nat             { TokenNat $$ }
  var             { TokenVar $$ }
  "=>"            { TokenEqGt }
  '('             { TokenLParen }
  ')'             { TokenRParen }

%right "then" "else"
%left APP

%%

Expr :: { Expr String }
  : NoApp { $1 }
  | Expr NoApp %prec APP { App $1 $2 }

NoApp :: { Expr String }
  : '(' Expr ')' { $2 }
  | if Expr then Expr else Expr { If $2 $4 $6 }
  | true { True_ }
  | false { False_ }
  | succ NoApp { Succ $2 }
  | pred NoApp { Pred $2 }
  | is_zero NoApp { IsZero $2 }
  | var { Var $1 }
  | nat { iterate Succ Zero !! fromIntegral $1 }
  | fun Vars1 "=>" Expr { foldr (\b e -> Lam b (abstract1 b e)) $4 $2 }

Vars1 :: { [String] }
  : var       { [$1] }
  | var Vars1 { $1 : $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}