{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Unification where

import           Constraints
import           Control.Monad                  ( when )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Syntax
import qualified Data.IntMap.Strict            as IntMap
import           Data.IntMap.Strict             ( IntMap )
import           Data.Maybe                     ( fromMaybe )
import           Data.Foldable                  ( traverse_ )

newtype Substitution = Subst (IntMap (Type UVar))

idSubst :: Substitution
idSubst = Subst IntMap.empty

lookupSubst :: UVar -> Substitution -> Maybe (Type UVar)
lookupSubst x (Subst m) = IntMap.lookup x m

extendSubst :: UVar -> Type UVar -> Substitution -> Substitution
extendSubst x t (Subst m) = Subst (IntMap.insert x t m)

applySubst :: Substitution -> Type UVar -> Type UVar
applySubst s (Arrow a b) = Arrow (applySubst s a) (applySubst s b)
applySubst s TyNat       = TyNat
applySubst s TyBool      = TyBool
applySubst s v@(TyVar n) = fromMaybe v (applySubst s <$> lookupSubst n s)

data UnificationError
  = OccursCheckFailure UVar (Type UVar)
  | TypeMismatch (Type UVar) (Type UVar)
  deriving Show

newtype UnifyM a
  = UnifyM (StateT Substitution (Except UnificationError) a)
  deriving (Functor, Applicative, Monad)

runUnifyM :: UnifyM () -> Either UnificationError Substitution
runUnifyM (UnifyM m) = runExcept (execStateT m idSubst)

throw :: UnificationError -> UnifyM a
throw = UnifyM . lift . throwE

lookupVar :: UVar -> UnifyM (Maybe (Type UVar))
lookupVar = UnifyM . gets . lookupSubst

bind :: UVar -> Type UVar -> UnifyM ()
bind x t = UnifyM (modify (extendSubst x t))

occursCheck :: UVar -> Type UVar -> UnifyM ()
occursCheck x t = when (x `occursIn` t) (throw (OccursCheckFailure x t))
 where
  occursIn x (TyVar y)   = x == y
  occursIn x TyNat       = False
  occursIn x TyBool      = False
  occursIn x (Arrow s t) = occursIn x s || occursIn x t

unify :: [Constraint] -> Either UnificationError Substitution
unify = runUnifyM . traverse_ go
 where
  go (l, r) = case (l, r) of
    (TyNat      , TyNat      ) -> pure r
    (TyBool     , TyBool     ) -> pure r
    (Arrow a1 b1, Arrow a2 b2) -> Arrow <$> go (a1, a2) <*> go (b1, b2)
    (TyVar x, TyVar y)
      | x == y -> pure r
      | otherwise -> do
        ml <- lookupVar x
        mr <- lookupVar y
        case (ml, mr) of
          (Just l', Just r') -> do
            occursCheck x r'
            occursCheck y l'
            t <- go (l', r')
            bind y t
            bind x r
            pure r
          (Just _, Nothing) -> do
            bind y l
            pure l
          _ -> do
            bind x r
            pure r
    (TyVar x, _) -> do
      occursCheck x r
      t <- lookupVar x >>= \case
        Nothing -> pure r
        Just l' -> go (l', r)
      bind x t
      pure l
    (_, TyVar y) -> do
      occursCheck y l
      t <- lookupVar y >>= \case
        Nothing -> pure l
        Just r' -> go (l, r')
      bind y t
      pure r
    _ -> throw (TypeMismatch l r)
