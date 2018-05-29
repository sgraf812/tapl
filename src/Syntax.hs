{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Syntax where

import           Bound
import           Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1,
                                       deriveShow1)
import           Data.Functor.Classes
import           Data.Type.Equality
import           Data.Void

data Expr b
  = Var b
  | App (Expr b) (Expr b)
  | Lam String (Scope () Expr b)
  | True_
  | False_
  | If (Expr b) (Expr b) (Expr b)
  | Zero
  | Succ (Expr b)
  | Pred (Expr b)
  | IsZero (Expr b)
  deriving (Functor, Foldable, Traversable)

makeBound ''Expr
deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveShow1 ''Expr
deriveRead1 ''Expr

instance Eq a => Eq (Expr a) where (==) = eq1
instance Ord a => Ord (Expr a) where compare = compare1
instance Read a => Read (Expr a) where readsPrec = readsPrec1
instance Show a => Show (Expr a) where showsPrec = showsPrec1

closed :: Expr b -> Maybe (Expr x)
closed = Bound.closed

isValue :: Expr Void -> Bool
isValue Lam{} = True
isValue True_ = True
isValue False_ = True
isValue e = isNumericValue e

isNumericValue :: Expr b -> Bool
isNumericValue Zero = True
isNumericValue (Succ n) = isNumericValue n
isNumericValue _ = False

data BaseType
  = TInt
  | TBool
  deriving (Eq, Show)

data Type
  = Arrow Type Type
  | Base BaseType
  deriving (Eq, Show)