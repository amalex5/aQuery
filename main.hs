-- symdiff

import System.IO
import Expr
import Parser
import Diff

ddx = diff "x"

main :: IO ()
main = runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
--evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString expr = return $ pprint . simplify . ddx $ parseExpr (expr)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "sd>>> ") evalAndPrint

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
simplify (Fxn f y) = Fxn f (simplify y)
simplify x = x


-- redo this so it's some pretty tree un-parsing with operational priority!
-- in particular, need to do this in a non-hacky way...
pprint :: Expr -> [Char]
pprint (Neg x) = "-" ++ pprint x
pprint (Add x y) = pprint x ++ "+" ++ pprint y
--pprint (Mul Symbol (Val x)) = show x ++ "x"
--pprint (Mul (Val x) Symbol) = show x ++ "x"
pprint (Sub x y) = pprint x ++ "-" ++ pprint y
pprint (Mul x y) = pprint x ++ "*" ++ pprint y
pprint (Div x y) = pprint x ++ "/" ++ pprint y
pprint (Pow x (Val y) ) = pprint x ++ "^" ++ show y
pprint (Pow x y) = pprint x ++ "^(" ++ pprint y ++ ")"
pprint (Var x) = x
pprint (Val x) = show x
pprint (Fxn "exp" x) = "e^(" ++ pprint x ++ ")"
pprint (Fxn xs y) = xs ++ "(" ++ pprint y ++ ")"

test0 :: Expr
test0 = (Add (Pow (Var "x") (Val 2)) (Mul (Fxn "exp" (Var "x")) (Fxn "sin" ( Mul (Val 5) (Var "x")))))

test1 :: Expr
test1 = (Add (Fxn "sin" (Mul (Val 5) (Pow (Var "x") (Val 7)))) (Mul (Val 4) (Neg (Var "x"))) )

test2 :: Expr
test2 = (Neg (Fxn "cos" (Mul (Var "x") (Val 5))))

test3 :: Expr
test3 = (Fxn "f" (Mul (Fxn "exp" (Mul (Val 5) (Var "x"))) (Pow (Var "x") (Val 7))))

testEmAll = map ddx [test0,test1,test2,test3]


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

