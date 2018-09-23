{
module Parser (parse) where

import           Bound
import           Data.Void
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
  "->"            { TokenMinusGt }
  ':'             { TokenColon }
  ','             { TokenComma }
  '('             { TokenLParen }
  ')'             { TokenRParen }

%right "ite" "lam"
%right if then else fun true false succ pred is_zero nat var '('

%%

Expr :: { TExpr Void String }
  : NoApp { $1 }
  | Expr NoApp { App $1 $2 }

NoApp :: { TExpr Void String }
  : '(' Expr ')' { $2 }
  | if Expr then Expr else Expr %prec "ite" { If $2 $4 $6 }
  | true { True_ }
  | false { False_ }
  | succ NoApp { Succ $2 }
  | pred NoApp { Pred $2 }
  | is_zero NoApp { IsZero $2 }
  | var { Var $1 }
  | nat { iterate Succ Zero !! fromIntegral $1 }
  | fun Vars1 "=>" Expr %prec "lam" { foldr (\(b,t) e -> Lam b t (abstract1 b e)) $4 $2 }

Vars1 :: { [(String, Type Void)] }
  : var ':' Type      { [($1,$3)] }
  | var ':' Type ',' Vars1 { ($1,$3) : $5 }

Type :: { Type Void }
  : '(' Type ')' { $2 }
  | Type "->" Type { Arrow $1 $3 }
  | var { if $1 == "Nat" 
          then TyNat
          else if $1 == "Bool"
          then TyBool
          else parseError [] }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}