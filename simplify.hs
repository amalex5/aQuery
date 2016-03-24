module Simplify (simplify,sumTree,prodTree)
where

import Expr

simplify :: Expr -> Expr
simplify inp = case simplify' inp of
  inp -> simplify' inp
  otherwise -> simplify (simplify' inp) 

simplify' :: Expr -> Expr
--simplify' (Mul (Val x) (Val y)) = Val (x*y)
--simplify' (Add (Val x) (Val y)) = Val (x+y)
simplify' (Sub (Val x) (Val y)) = Val (x-y)
simplify' (Pow x (Val 0)) = Val 1
simplify' (Pow (Val x) (Val y)) = Val (x^y)
simplify' (Neg (Val x)) = Val (-x)
simplify' (Neg (Neg x)) =  simplify' x
simplify' (Neg x) = Neg (simplify' x)
simplify' (Add x y)
  | simplify' x == (Val 0) = simplify' y
  | simplify' y == (Val 0) = simplify' x
  | simplify' x == simplify' y = Mul (Val 2) (simplify' x)
  | otherwise = sumTree (Add (simplify' x) (simplify' y) )
--simplify' (Mul x (Add y z)) = Add (Mul x y) (Mul x z)
--simplify' (Mul (Add x y) z) = Add (Mul x z) (Mul y z)
simplify' (Mul x y)
  | simplify' x == (Val 1) = simplify' y
  | simplify' y == (Val 1) = simplify' x
  | simplify' x == (Val 0) = Val 0
  | simplify' y == (Val 0) = Val 0
  | simplify' x == simplify' y = (Pow x (Val 2))
  | otherwise = prodTree (Mul (simplify' x) (simplify' y) )
simplify' (Sub x y)
  | simplify' x == (Val 0) = simplify' (Neg y)
  | simplify' y == (Val 0) = simplify' x
  | simplify' x == simplify' y = Val 0
  | otherwise =  (Sub (simplify' x) (simplify' y) ) 
simplify' (Div x y)
  | simplify' y == (Val 1) = simplify' x
  | simplify' x == (Val 0) = Val 0
  | simplify' y == (Val 0) = error "no division by zero!"
  | simplify' x == simplify' y = Val 1
  | otherwise = (Div (simplify' x) (simplify' y) )
simplify' (Pow x y)
  | y == (Val 1) = x
  | y == (Val 0) = Val 1
  | otherwise =  (Pow (simplify' x) (simplify' y))
simplify' (Fxn f y) = Fxn f (simplify' y)
simplify' (Var c) = Var c
simplify' x = x






-- sumTree takes a tree whose top-level node is an Add node
-- and adds up all the values, respecting order of operations
sumTree :: Expr -> Expr
sumTree t = rebuildSumTree1 $ bubbleSums t

rebuildSumTree1 :: (Int,[Expr]) -> Expr
rebuildSumTree1 (x,y) = rebuildSumTree2 (Val x) y

rebuildSumTree2 :: Expr -> [Expr] -> Expr
rebuildSumTree2 x [] = x
rebuildSumTree2 x (y:[]) = Add x y
rebuildSumTree2 x (y:ys) = rebuildSumTree2 (Add x y) ys

-- bubble sums takes a tree whose top-level node is an Add node
-- and returns a tuple whose first element
bubbleSums :: Expr -> (Int,[Expr])
bubbleSums = bubbleSums' 0 []

bubbleSums' :: Int -> [Expr] -> Expr -> (Int,[Expr])
bubbleSums' acc rst (Add (Val x) (Val y )) = (acc + x + y, rst)
bubbleSums' acc rst (Add (Val x)    y    ) = bubbleSums' (acc + x) rst y
bubbleSums' acc rst (Add     x   (Val y )) = bubbleSums' (acc + y) rst x
bubbleSums' acc rst (Add     x     y     ) = (i+j        , m++n)
     where 
     	(i,m) = bubbleSums' acc rst x
     	(j,n) = bubbleSums' 0 [] y --let's not double the accumulators
bubbleSums' acc rst           z            = (acc        , z:rst)










prodTree :: Expr -> Expr
prodTree t = prodTree1 $ bubbleProds t

prodTree1 :: (Int,[Expr]) -> Expr
prodTree1 (x,y) = prodTree2 (Val x) y

prodTree2 :: Expr -> [Expr] -> Expr
prodTree2 x [] = x
prodTree2 x (y:[]) = Mul x y
prodTree2 x (y:ys) = prodTree2 (Mul x y) ys

bubbleProds :: Expr -> (Int,[Expr])
bubbleProds = bubbleProds' 1 []

bubbleProds' :: Int -> [Expr] -> Expr -> (Int,[Expr])
bubbleProds' acc rst (Mul (Val x) (Val y )) = (acc * x * y, rst)
bubbleProds' acc rst (Mul (Val x)    y    ) = bubbleProds' (acc * x) rst y
bubbleProds' acc rst (Mul     x   (Val y )) = bubbleProds' (acc * y) rst x
bubbleProds' acc rst (Mul     x     y     ) = (i*j        , m++n)
     where 
     	(i,m) = bubbleProds' acc rst x
     	(j,n) = bubbleProds' 1 [] y --let's not double the accumulators
bubbleProds' acc rst           z            = (acc        , z:rst)



distribute :: Expr -> Expr
distribute (Mul (Add x y) z) = undefined
distribute (Mul x (Add y z)) = undefined
distribute z = z



