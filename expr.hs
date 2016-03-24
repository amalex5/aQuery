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
           | Div Expr Expr
           | Pow Expr Expr -- a^b
           | Fxn [Char] Expr
           | Error [Char]
           deriving (Show,Eq)


-- GOOD LORD THIS IS GROSS WON'T YOU PLEASE FACTOR THIS OUT
-- INTO A SIMPLER DATA TYPE
-- I CAN'T EVEN MAKE IT INTO A STUPID FUNCTOR B/C IT'S THE WRONG KIND
-- maybe some of these are nicer: https://www.haskell.org/hoogle/?hoogle=%28a+-%3E+a%29+-%3E+a+-%3E+a
exprMap :: (Expr -> Expr) -> Expr -> Expr
exprMap f (Neg x)   =  Neg ( f x)
exprMap f (Add x y) =  Add ( f x) ( f y)
exprMap f (Mul x y) =  Mul ( f x) ( f y)
exprMap f (Sub x y) =  Sub ( f x) ( f y)
exprMap f (Div x y) =  Div ( f x) ( f y)
exprMap f (Pow x y) =  Pow ( f x) ( f y)
exprMap f (Fxn c y) =  Fxn c ( f y)
exprMap f (Val x )  =  Val x
exprMap f (Var x)   =  Var x 
exprMap f z = z
