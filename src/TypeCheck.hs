module TypeCheck where

import Syntax

data Constraint b
  = C (Type b) (Type b)
--  | Subst (Scope () Type b)

constraints :: UExpr b -> [Constraint (Var () b)]
constraints = go 
  where
    go (App f a) = 