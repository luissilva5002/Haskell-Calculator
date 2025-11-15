{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for expressions
-- made up from integer numbers, + and *
--

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Sub Expr Expr
          | Mod Expr Expr
          | Var String
          deriving Show

data Command = Assign String Expr | Expr Expr

type Pair = [(String, Integer)]

lookupVar :: String -> Pair -> Integer
lookupVar x pair = case lookup x pair of
                      Just v  -> v
                      Nothing -> error ("Variable not defined: " ++ x)

-- a recursive evaluator for expressions
--

eval :: Pair -> Expr -> Integer
eval pair (Num n) = n
eval pair (Var x) = lookupVar x pair
eval pair (Add e1 e2) = eval pair e1 + eval pair e2
eval pair (Mul e1 e2) = eval pair e1 * eval pair e2
eval pair (Sub e1 e2) = eval pair e1 - eval pair e2
eval pair (Div e1 e2) = eval pair e1 `div` eval pair e2
eval pair (Mod e1 e2) = eval pair e1 `mod` eval pair e2

-----------------------------------------------------------------------------------

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | ’-’ term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | ’/’ factor termCont | ’%’ factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               -- <|> return acc

               -- add minus
               <|> do
                  char '-'
                  t <- term
                  exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
               <|> do
                  char '/'
                  f <- factor
                  exprCont (Div acc f)
               <|> do 
                  char '%'
                  f <- factor
                  exprCont (Mod acc f)
               <|> return acc

factor :: Parser Expr
factor = do v <- variable
            return (Var v)
         <|>
         do n <- natural
            return (Num n)
         <|>
         do char '('
            e <- expr
            char ')'
            return e

command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|>
          do e <- expr
             return (Expr e)

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit) -- many1 reads next chars and puts them into string xs as long as the next char is int
             return (read xs) -- read converts string to int

variable :: Parser String
variable = many1 (satisfy isAlpha)

-- Hierarchy: expr -> term -> factor
-- Factor is first priority (last being called in recursion) ex: a * b
-- Term is second priority ex: a + b
-- expr is the full line 

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       runCommands[] (lines txt)

execute :: Pair -> String -> (String, Pair)
execute pair input =
    case parse command input of
        [(cmd, "")] -> runCommand pair cmd
        _           -> error "Parse error"

runCommand :: Pair -> Command -> (String, Pair)
runCommand pair (Expr e) = (show v, pair)
    where v = eval pair e
    
runCommand pair (Assign x e) = (show v, pair')
    where 
        v = eval pair e
        pair' = (x,v) : filter ((/= x) . fst) pair
    

runCommands :: Pair -> [String] -> IO ()
runCommands _ [] = return ()
runCommands pair (l:ls) = do putStrLn out
                             runCommands pair' ls 
                        where (out, pair') = execute pair l