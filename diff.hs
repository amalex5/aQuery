module Diff
where

import Expr

diff :: [Char] -> Expr -> Expr 
--[Char] is the variable we differentiate against
-- written "v" below
diff v (Val _) = Val 0
diff x (Var y)
  | x == y = Val 1
  | otherwise = Val 0
diff v (Neg x) = Neg (diff v x)
diff v (Add x y) = Add (diff v x) (diff v y)
diff v (x `Mul` y) = ( (diff v x) `Mul` y ) `Add`  (x `Mul` (diff v y) )
diff v (Sub x y) = diff v (Add x (Neg y))
diff v (Pow x y)
  | x == Var v = Mul x (Pow x (Sub y (Val 1)) )
  | x == Var "e" = diff v (Fxn "exp" x)
  | otherwise = Add (Mul (Mul (Pow x y) (diff v y) ) (Fxn "ln" x) ) (Mul (Mul (Pow x (Sub y (Val 1))) (y)) (diff v x) ) 
  -- the last one is the very general form
--diff v (Pow (Var v) (Val n)) = Mul (Val n) (Pow x (Val (n - 1) ) )
--diff v (Pow (Var "e") x ) = diff v (Fxn "exp" x)
diff v (Div x y) = diff v (Mul x (Pow y (Val (-1)) ) )
diff v (Fxn name arg) = case name of
	"sin" -> Mul (Fxn "cos" arg) (diff v arg)
	"cos" -> Mul (Neg (Fxn "sin" arg)) (diff v arg)
	"tan" -> Mul (Div (Val 1) (Mul (Fxn "cos" arg) (Fxn "cos" arg))) (diff v arg)
	"exp" -> Mul (Fxn "exp" arg) (diff v arg)
	"ln"  -> Mul (Div (Val 1) (arg)) (diff v arg)
	otherwise -> Mul (Fxn (name ++ "\'") arg) (diff v arg) -- generic chain rule
