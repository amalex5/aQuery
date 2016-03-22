
module Parser (parseExpr,parseWrappedExpressions)
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Expr 

--data Expr = Val Integer 
--           | Var [Char]
--           | Exp Expr Expr
--           | Mul Expr Expr 
--           | Add Expr Expr
--           | Sub Expr Expr
--           | Div Expr Expr
--           | Fxn [Char] Expr
--          deriving (Show,Eq)

parseWrappedExpressions :: String -> [WrapperFxn]
parseWrappedExpressions inp = case parse manyWrappers "wrapped!" inp of
  Left err -> [WrapperFxn ("Error","Error")] -- talk about shitty error handling!
  Right val -> val


parseExpr :: String -> Expr
parseExpr inp = case   parse expr "parser!" inp of
  Left err -> Error "the parser failed! :("
  Right val -> val

expr :: Parser Expr
expr = buildExpressionParser table factor <?> "expression"

table :: [[ Operator Char st Expr ]]
table = [
    [ binary "^" Pow AssocRight], -- can't add "**" as an alternative because it conflicts with "*" as the multiplication operator. not sure how to easily fix without writing all my own parsing machinery...
    [ binary "*" Mul AssocLeft, binary "/" Div AssocLeft ],
    [ binary "+" Add AssocLeft, binary "-" Sub AssocLeft ]
    ]
  where
     binary  name fun assoc = Infix (do{ string name; return fun }) assoc
     prefix  name fun       = Prefix (do{ string name; return fun })
     postfix name fun       = Postfix (do{ string name; return fun })


factor = 
   try function -- "try" consumes no input, whereas <|> does. 
   <|> parens
   <|> number
   <|> variable
   <?> "simple expression"

function :: Parser Expr
function = do 
           name <- many1 letter
           char '('
           argument <- expr
           char ')'
           return $ Fxn name argument

parens :: Parser Expr
parens = do
         char '('
         x <- expr
         char ')'
         return x

number :: Parser Expr
number = do { ds <- many1 digit; return (Val $ read ds) } <?> "number"

variable :: Parser Expr
variable = many letter >>= return . Var 


wrapper :: Parser WrapperFxn
wrapper = do 
           name <-  (many1 letter) <|> (string "$")
           char '['
           contents <- anyChar `manyTill` (char ']') --probably Expr, but don't parse it yet
           --char ']'
           return $ WrapperFxn (name,contents)

manyWrappers :: Parser [WrapperFxn]
manyWrappers = wrapper `sepBy` (char '.')





