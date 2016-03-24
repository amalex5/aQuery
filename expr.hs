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

data Ops a = Add' a 
          | Mul' a 
          | Sub' a
          | Div' a
          | Neg' a
          | Pow' a
          | Fxn' [Char] a
  deriving Show

data Expr' = Val' Int
           | Var' [Char]
           | Op (Ops [Expr'])
           | Error' [Char]
           deriving Show

--diff' v (Val _) = Val 0
--diff' v (Var x)
--  | v == x = Val 1
--  | otherwise = Val 0
--diff' v (Op x) = case x of
--	Mul 

--addEq :: Expr -> Expr -> Bool
--addEq x y = sort (listOfAdds x) == sort (listOfAdds y)

-- sumTree takes a tree whose top-level node is an Add node
-- and adds up all the values, respecting order of operations
sumTree :: Expr -> Expr
sumTree t = sumTree1 $ bubbleSums t

sumTree1 :: (Int,[Expr]) -> Expr
sumTree1 (x,y) = sumTree2 (Val x) y

sumTree2 :: Expr -> [Expr] -> Expr
sumTree2 x [] = x
sumTree2 x (y:[]) = Add x y
sumTree2 x (y:ys) = sumTree2 (Add x y) ys

-- bubble sums takes a tree whose top-level node is an Add node
-- and returns a tuple whose first element
bubbleSums :: Expr -> (Int,[Expr])
bubbleSums = bubbleSums' 0 []

bubbleSums' :: Int -> [Expr] -> Expr -> (Int,[Expr])
bubbleSums' acc rst (Add (Val x) (Val y )) = (acc + x + y,rst)
bubbleSums' acc rst (Add (Val x)    y    ) = bubbleSums' (acc + x) rst y
bubbleSums' acc rst (Add     x   (Val y )) = bubbleSums' (acc + y) rst x
bubbleSums' acc rst (Add     x     y     ) = (i+j , m++n)
     where 
     	(i,m) = bubbleSums' acc rst x
     	(j,n) = bubbleSums' 0 [] y --let's not double the accumulators
bubbleSums' acc rst           z            = (acc, z:rst)




listOfAdds :: Expr -> [Expr]
listOfAdds = listOfAdds' []

listOfAdds' :: [Expr] -> Expr -> [Expr]
listOfAdds' l (Add x y) = l ++ (listOfAdds x) ++ (listOfAdds y)
listOfAdds' l z = z:l

--test0 = Add (Val 1) (Mul (Val 5) (Var "c"))
--test1 = Add test0 (Add (Var "b") (Val 7))


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
