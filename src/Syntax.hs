{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Syntax where

import           Bound
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Type.Equality
import           Data.Void

data Expr ty b
  = Var b
  | App (Expr ty b) (Expr ty b)
  | Lam String ty (Scope () (Expr ty) b)
  | True_
  | False_
  | If (Expr ty b) (Expr ty b) (Expr ty b) | Zero
  | Succ (Expr ty b)
  | Pred (Expr ty b)
  | IsZero (Expr ty b)
  deriving (Functor, Foldable, Traversable)

makeBound ''Expr
instance (Eq ty, Eq a) => Eq (Expr ty a) where (==) = eq1
instance Eq ty => Eq1 (Expr ty) where liftEq = $(makeLiftEq ''Expr)
instance (Ord ty, Ord a) => Ord (Expr ty a) where compare = compare1
instance Ord ty => Ord1 (Expr ty) where liftCompare = $(makeLiftCompare ''Expr)
instance (Read ty, Read a) => Read (Expr ty a) where readsPrec = readsPrec1 
instance Read ty => Read1 (Expr ty) where liftReadsPrec = $(makeLiftReadsPrec ''Expr)
instance (Show ty, Show a) => Show (Expr ty a) where showsPrec = showsPrec1 
instance Show ty => Show1 (Expr ty) where liftShowsPrec = $(makeLiftShowsPrec ''Expr)

closed :: Expr ty b -> Maybe (Expr ty x)
closed = Bound.closed

isValue :: Expr ty Void -> Bool
isValue Lam{} = True
isValue True_ = True
isValue False_ = True
isValue e = isNumericValue e

isNumericValue :: Expr ty b -> Bool
isNumericValue Zero = True
isNumericValue (Succ n) = isNumericValue n
isNumericValue _ = False

data BaseType
  = TInt
  | TBool
  deriving (Eq, Ord, Read, Show)

data Type b
  = Base BaseType
  | Arrow (Type b) (Type b)
  | TyVar b
  deriving (Functor, Foldable, Traversable)

makeBound ''Type
deriveEq1 ''Type
deriveOrd1 ''Type
deriveRead1 ''Type
deriveShow1 ''Type
instance Eq b => Eq (Type b) where (==) = eq1
instance Ord b => Ord (Type b) where compare = compare1
instance Read b => Read (Type b) where readsPrec = readsPrec1
instance Show b => Show (Type b) where showsPrec = showsPrec1 

type UExpr b = Expr () b
type TExpr tys b = Expr (Type tys) b
