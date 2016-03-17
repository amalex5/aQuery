

diff :: [Char] -> Expr -> Expr --[Char] is our variable to differentiate against
diff _ (Val _) = Val 0
diff x (Var y) = case y of
	y == x -> Val 1
	otherwise -> Val 0
diff _ (Neg x) = Neg (diff x)
diff _ (Add x y) = Add (diff x) (diff y)
diff _ (x `Mul` y) = ( (diff x) `Mul` y ) `Add`  (x `Mul` (diff y) )
diff _ (Sub x y) = diff (Add x (Neg y))
diff _ (Exp x (Val n)) = Mul (Val n) (Pow x (Val (n - 1) ) )
diff _ (Div x y) = diff (Mul x (Pow y (Val (-1)) ) )
diff _ (Fxn name arg) = case name of
	"sin" -> Mul (Fxn "cos" arg) (diff arg)
    "cos" -> Mul (Neg (Fxn "sin" arg)) (diff arg)
    "tan" -> Mul (Div (1) (Mul (Fxn "cos" arg) (Fxn "cos" arg))) (diff arg)
    "exp" -> Mul (Fxn "exp" x) (diff x)
    "ln"  -> Mul ( (Div (Val 1) (arg)) (diff arg) )
    otherwise -> Mul (Fxn (name ++ "\'") arg) (diff arg) -- generic chain rule
