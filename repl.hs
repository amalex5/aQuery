-- REPL!!!
-- basically all of this code is stolen from WRITE YOU A SCHEMEL:
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL

module Repl
where

import System.IO
import Diff

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
--evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString expr = return $ pprint . simplify . ddx expr

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