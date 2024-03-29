module Language.Kydo.Core where

type Sym = String

data Expr
   = Var Sym
   | App Expr Expr
   | Lam Sym Type Expr
   | TLam Sym Kind Expr
   | TApp Expr Type
   deriving (Eq, Read, Show)

data Type 
   = Arrow Type Type
   | Base
   | TVar Sym
   deriving (Eq, Read, Show)

data Kind
   = KArrow Type Type
   | Star
   deriving (Eq, Read, Show)

