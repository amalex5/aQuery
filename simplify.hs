module Simplify
where

import Expr

simplify :: Expr -> Expr
simplify (Mul (Val x) (Val y)) = Val (x*y)
simplify (Add (Val x) (Val y)) = Val (x+y)
simplify (Sub (Val x) (Val y)) = Val (x-y)
simplify (Pow (Val x) (Val y)) = Val (x^y)
simplify (Neg (Val x)) = Val (-x)
simplify (Neg (Neg x)) =  simplify x
simplify (Neg x) = Neg (simplify x)
simplify (Add x y)
  | simplify x == (Val 0) = simplify y
  | simplify y == (Val 0) = simplify x
  | simplify x == simplify y = Mul (Val 2) (simplify x)
  | otherwise = simplify (Add (simplify x) (simplify y) ) 
simplify (Mul x y)
  | simplify x == (Val 1) = simplify y
  | simplify y == (Val 1) = simplify x
  | simplify x == (Val 0) = Val 0
  | simplify y == (Val 0) = Val 0
  | simplify x == simplify y = (Pow x (Val 2))
  | otherwise = simplify (Mul (simplify x) (simplify y) )
simplify (Sub x y)
  | simplify x == (Val 0) = simplify (Neg y)
  | simplify y == (Val 0) = simplify x
  | simplify x == simplify y = Val 0
  | otherwise = simplify (Sub (simplify x) (simplify y) ) 
simplify (Div x y)
  | simplify y == (Val 1) = simplify x
  | simplify x == (Val 0) = Val 0
  | simplify y == (Val 0) = error "no division by zero!"
  | simplify x == simplify y = Val 1
  | otherwise = (Div (simplify x) (simplify y) )
simplify (Pow x y)
  | y == (Val 1) = x
  | y == (Val 0) = Val 1
  | otherwise = simplify (Pow (simplify x) (simplify y))
simplify (Fxn f y) = Fxn f (simplify y)
simplify (Var c) = Var c
simplify x = x