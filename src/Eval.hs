{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import Bound
import Syntax

-- prop> whnf e == Just v ==> isValue v
whnf :: Expr ty b -> Maybe (Expr ty b)
whnf = \case
  e@Var{} -> Nothing -- or maybe Just e?
  e@Lam{} -> Just e
  e@True_{} -> Just e
  e@False_{} -> Just e
  e@Zero{} -> Just e
  App f a -> whnf f >>= \case
    Lam _ _ body -> whnf (instantiate1 a body)
    _ -> Nothing
  Succ n -> whnf n >>= \case
    e@Succ{} -> Just (Succ e)
    Zero -> Just (Succ Zero)
    _ -> Nothing
  Pred n -> whnf n >>= \case
    Succ n' -> Just n'
    Zero -> Just Zero
    _ -> Nothing
  IsZero n -> whnf n >>= \case
    Zero -> Just True_
    Succ{} -> Just False_
    _ -> Nothing
  If b t e -> whnf b >>= \case
    True_ -> whnf t
    False_ -> whnf e
    _ -> Nothing

nf :: Expr ty b -> Maybe (Expr ty b)
nf = \case
  e@Var{} -> Nothing -- or maybe Just e?
  e@Lam{} -> Just e
  e@True_{} -> Just e
  e@False_{} -> Just e
  e@Zero{} -> Just e
  App f a -> whnf f >>= \case
    Lam _ _ body -> nf (instantiate1 a body)
    _ -> Nothing
  Succ n -> whnf n >>= \case
    e@Succ{} -> Just (Succ e)
    Zero -> Just (Succ Zero)
    _ -> Nothing
  Pred n -> whnf n >>= \case
    Succ n' -> Just n'
    Zero -> Just Zero
    _ -> Nothing
  IsZero n -> whnf n >>= \case
    Zero -> Just True_
    Succ{} -> Just False_
    _ -> Nothing
  If b t e -> whnf b >>= \case
    True_ -> nf t
    False_ -> nf e
    _ -> Nothing