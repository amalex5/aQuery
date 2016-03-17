
module Parser
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Expr = Val Integer 
           | Var [Char]
           | Exp Expr Expr
           | Mul Expr Expr 
           | Add Expr Expr
           | Sub Expr Expr
           | Div Expr Expr
           | Fxn [Char] Expr
          deriving (Show,Eq)

parseExpr inp = parse expr "parser!" inp

expr :: Parser Expr
expr = buildExpressionParser table factor <?> "expression"

table :: [[ Operator Char st Expr ]]
table = [
    [ binary "^" Exp AssocRight], -- can't add "**" as an alternative because it conflicts with "*" as the multiplication operator. not sure how to easily fix without writing all my own parsing machinery...
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