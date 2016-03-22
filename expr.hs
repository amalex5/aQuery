module Expr
where

data WrapperFxn = WrapperFxn (String,String)
   deriving (Show)

data Expr = Val Int
		   | Var [Char] -- :: [Char] -> Expr
		   | Neg Expr
           | Add Expr Expr
           | Mul Expr Expr
           | Sub Expr Expr
           | Pow Expr Expr -- a^b
           | Div Expr Expr
           | Fxn [Char] Expr
           | Error [Char]
           deriving (Show,Eq)
