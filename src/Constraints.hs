{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Constraints where

import           Bound
import           Control.Monad.Trans.RWS.Strict
import           Syntax
import           Data.Void

type UVar = Int

type Constraint = (Type UVar, Type UVar)

type ConstraintM = RWS () [Constraint] Int

freshUVar :: ConstraintM UVar
freshUVar = state (\n -> (n, n + 1))

noteConstraint :: Constraint -> ConstraintM ()
noteConstraint = tell . (: [])

constraints :: (v -> Type Void) -> TExpr Void v -> (Type UVar, [Constraint])
constraints env = evalRWS' () 0 . go (pure . fmap absurd . env)
 where
  evalRWS' r s m = evalRWS m r s
  go
    :: forall v
     . (v -> ConstraintM (Type UVar))
    -> TExpr Void v
    -> ConstraintM (Type UVar)
  go env (Var v  ) = env v
  go env (App f a) = do
    tF <- go env f
    tA <- go env a
    tX <- TyVar <$> freshUVar
    noteConstraint (tF, Arrow tA tX)
    pure tX
  go env (Lam _ ty (Scope e)) = do
    let env' (B _ ) = pure (fmap absurd ty)
        env' (F e') = go env e'
    Arrow (fmap absurd ty) <$> go @(Var () (TExpr Void v)) env' e
  go env True_      = pure TyBool
  go env False_     = pure TyBool
  go env (If b t e) = do
    tB <- go env b
    tT <- go env t
    tE <- go env e
    noteConstraint (tB, TyBool)
    noteConstraint (tT, tE)
    pure tT
  go env Zero       = pure TyNat
  go env (IsZero e) = do
    t <- go env e
    noteConstraint (t, TyNat)
    pure TyBool
  go env (Succ e) = do
    t <- go env e
    noteConstraint (t, TyNat)
    pure TyNat
