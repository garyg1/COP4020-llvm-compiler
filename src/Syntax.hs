--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Syntax where


type Name = String
data Type = TypeBool | TypeFloat | TypeInt | TypeVoid deriving (Eq, Ord, Show)
type Declaration = (Type, Name)

data Expr
  = Int Integer
  | Float Double
  | Bool Bool
  | Var String
  | Call Name [Expr]
  | Function Declaration [Declaration] Expr
  | Extern Declaration [Declaration]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | Let Declaration Expr Expr
  deriving (Eq, Ord, Show)