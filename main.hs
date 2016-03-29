-- symdiff

import System.IO
import Expr
import Parser
import Diff
import Simplify
import Control.Monad.State

main :: IO ()
main = putStrLn welcomeString >> runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hSetBuffering stdin LineBuffering --hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
--evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString expr = return $ id (pprint . evalWrapper . parseWrappedExpressions $ expr)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  (evalString expr) >>= putStrLn

welcomeString :: String
welcomeString = "welcome to aQuery!\n\"quit\" to quit."

promptString :: String
promptString = "aQuery>> "

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt promptString) evalAndPrint



evalWrapper :: [WrapperFxn] -> Expr
evalWrapper = foldl foldingFxn (Val 0)
  where 
    foldingFxn _ (WrapperFxn ("$",        y))   = (parseExpr y)
    foldingFxn b (WrapperFxn ("diff",     y)) = diffn 1 y b
    foldingFxn b (WrapperFxn ("simplify", _)) = simplify b
    foldingFxn b (WrapperFxn ("eval",     y)) = evalExpr (parseEquals y) b
    foldingFxn b (WrapperFxn ("showAST",  _)) = Var (show b)
    foldingFxn b (WrapperFxn ("add",      y)) = (Add b (parseExpr y) )
    foldingFxn b (WrapperFxn ("sub",      y)) = (Sub b (parseExpr y) )
    foldingFxn b (WrapperFxn ("mul",      y)) = (Mul b (parseExpr y) )
    foldingFxn b (WrapperFxn ("div",      y)) = (Div b (parseExpr y) )
    foldingFxn b (WrapperFxn ("pow",      y)) = (Pow b (parseExpr y) )
    -- deal with errors. 

-- in the expression e, replace all occurances of a with b
evalExpr :: (Expr,Expr) -> Expr -> Expr
evalExpr (a,b) e = simplify ( if e == a then b else exprMap (evalExpr (a,b)) e )

-- redo this so it's some pretty tree un-parsing with operational priority!
-- in particular, need to do this in a non-hacky way...
pprint :: Expr -> [Char]
pprint (Neg x) = "-" ++ pprint x
pprint (Add x y) = pprint x ++ "+" ++ pprint y
pprint (Sub x y) = pprint x ++ "-" ++ pprint y
pprint (Mul (Var c) (Val x)) = c ++ show x
pprint (Mul (Val x) (Var c)) = c ++ show x
pprint (Mul x y) = pprint x ++ "*" ++ pprint y
pprint (Div x y) = pprint x ++ "/" ++ pprint y
pprint (Pow x y) = case y of
  Val n -> pprint x ++ "^" ++ show n
  Var n -> pprint x ++ "^" ++ show n
  otherwise -> pprint x ++ "^(" ++ pprint y ++ ")"
pprint (Var x) = x
pprint (Val x) = show x
--pprint (Fxn "exp" x) = "e^(" ++ pprint x ++ ")"
pprint (Fxn c y) = c ++ "(" ++ pprint y ++ ")"
pprint (Error x) =  show x
pprint z = show z
