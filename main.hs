-- symdiff

---import System.IO

data Expr = Val Int
		   | Const [Char] -- :: [Char] -> Expr
		   | Symbol
		   | Neg Expr
           | Add Expr Expr
           | Mul Expr Expr
           | Sub Expr Expr
           | Pow Expr Expr
           | Div Expr Expr
           | Exp Expr
           | Log Expr
           | Sin Expr 
           | Cos Expr
           | Fxn [Char] Expr
           deriving (Show,Eq)

-- how do i factor out the binary and unary operations into seperate subtypes?

diff :: Expr -> Expr
diff (Val _ ) = Val 0
diff (Const _ ) = Val 0
diff (Symbol) = Val 1
diff (Neg x) = Neg (diff x)
diff (Add x y) = Add (diff x) (diff y)
diff (x `Mul` y) = ( (diff x) `Mul` y ) `Add`  (x `Mul` (diff y) )
diff (Sub x y) = diff (Add x (Neg y))
diff (Pow x (Val n)) = Mul (Val n) (Pow x (Val (n - 1) ) )
diff (Div x y) = diff (Mul x (Pow y (Val (-1)) ) )
diff (Exp x) = Mul (Exp x) (diff x)
diff (Sin x) = Mul (Cos x) (diff x)
diff (Cos x) = Neg (Mul (Sin x) (diff  x) )
--diff (Fxn xs y) = Mul (Fxn (xs ++ "\'") y) (diff y)
diff (Fxn name arg) = case name of
	"sin" -> Mul (Fxn "cos" arg) (diff arg)
    "cos" -> Mul (Neg (Fxn "sin" arg)) (diff arg) 
    otehrwise -> Mul (Fxn (name ++ "\'") arg) (diff arg) -- generic chain rule

simplify :: Expr -> Expr
simplify (Mul (Val x) (Val y)) = Val (x*y)
simplify (Add (Val x) (Val y)) = Val (x+y)
simplify (Neg (Val 0)) = Val 0
simplify (Neg (Val x)) = Val (-x)
simplify (Neg (Neg x)) =  simplify x
simplify (Neg x) = Neg (simplify x)
simplify (Add x y)
  | simplify x == (Val 0) = simplify y
  | simplify y == (Val 0) = simplify x
  | simplify x == simplify y = Mul (Val 2) (simplify x)
  | otherwise = (Add (simplify x) (simplify y) ) 
simplify (Mul x y)
  | simplify x == (Val 1) = simplify y
  | simplify y == (Val 1) = simplify x
  | simplify x == (Val 0) = Val 0
  | simplify y == (Val 0) = Val 0
  | simplify x == simplify y = (Pow x (Val 2))
  | otherwise = (Mul (simplify x) (simplify y) )
simplify (Sub x y)
  | simplify x == (Val 0) = simplify (Neg y)
  | simplify y == (Val 0) = simplify x
  | simplify x == simplify y = Val 0
  | otherwise = (Sub (simplify x) (simplify y) ) 
simplify (Div x y)
  | simplify y == (Val 1) = simplify x
  | simplify x == (Val 0) = Val 0
  | simplify y == (Val 0) = error "no division by zero!"
  | simplify x == simplify y = Val 1
  | otherwise = (Div (simplify x) (simplify y) )
simplify (Pow x y)
  | y == (Val 1) = x
  | y == (Val 0) = Val 1
  | otherwise = Pow x y
simplify (Cos x) = Cos (simplify x)
simplify (Sin x) = Sin (simplify x)
simplify (Fxn f y) = Fxn f (simplify y)
simplify x = x


-- redo this so it's some pretty tree un-parsing with operational priority!
-- in particular, need to do this in a non-hacky way...
pprint :: Expr -> [Char]
pprint (Neg x) = "-" ++ pprint x
pprint (Add x y) = pprint x ++ "+" ++ pprint y
pprint (Mul Symbol (Val x)) = show x ++ "x"
pprint (Mul (Val x) Symbol) = show x ++ "x"
pprint (Mul x y) = pprint x ++ "*" ++ pprint y
pprint (Pow x y) = pprint x ++ "^" ++ pprint y
pprint Symbol = "x"
pprint (Const x) = x
pprint (Val x) = show x
pprint (Cos x) = "cos(" ++ pprint x ++ ")"
pprint (Sin x) =  "sin(" ++ pprint x ++ ")"
pprint (Exp x) = "e^(" ++ pprint x ++ ")"
pprint (Fxn xs y) = xs ++ "(" ++ pprint y ++ ")"

test0 :: Expr
test0 = (Add (Pow Symbol (Val 2)) (Mul (Exp Symbol) (Sin ( Mul (Val 5) Symbol))))

test1 :: Expr
test1 = (Add (Sin (Mul (Val 5) (Pow Symbol (Val 7)))) (Mul (Val 4) (Neg Symbol)) )

test2 :: Expr
test2 = (Neg (Cos (Mul Symbol (Val 5))))

test3 :: Expr
test3 = (Fxn "f" (Mul (Exp (Mul (Val 5) Symbol)) (Pow Symbol (Val 7))))




--display  :: Expr -> IO ()
--display es  =  do result <- ((pprint . simplify . diff) es)
--                  putStr "result is: "
--                  putStr result
--                  putStr "\n"

--main :: IO ()
--main = do hSetBuffering stdout NoBuffering
--          putStrLn "\nandrew's symbolic differentiator!"
--          putStrLn "-----------------------------\n"
--          putStr "enter an expression! : "
--          ns <- readLn
--          display ns

