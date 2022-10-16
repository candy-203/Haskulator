module Main where

import Control.Applicative (Alternative (..))   
import Parser (Parser (..), char, paren, parseIntAsDouble, parseDouble, token)
import Expression (Expr (..), Op (..), eval)
import Error (MaybeError(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)

expr :: Parser Expr
expr = token (term >>= exprRest)

exprRest :: Expr -> Parser Expr
exprRest e1 = do
        op <- char '+' <|> char '-'
        e2 <- term
        exprRest (Bin (if op == '+' then Plus else Minus) e1 e2)
    <|> return e1

term :: Parser Expr
term = token (factor >>= termRest)

termRest :: Expr -> Parser Expr
termRest e1 = do
        op <- char '*' <|> char '/'
        e2 <- factor
        termRest (Bin (if op == '*' then Mult else Div) e1 e2) 
    <|> return e1

factor :: Parser Expr
factor = token (powterm >>= factorRest)

factorRest :: Expr -> Parser Expr
factorRest e1 = do
        op <- char '^'
        e2 <- powterm
        factorRest (Bin Pow e1 e2)
    <|> return e1

powterm :: Parser Expr
powterm = token (parseNum <|> paren expr <|> parseMinus parseNum <|> paren (parseMinus parseNum))


parseMinus :: Parser Expr -> Parser Expr
parseMinus p = do
  char '-'
  Bin Mult (Number (-1)) <$> p

parseNum :: Parser Expr
parseNum = Number <$> (parseDouble <|> parseIntAsDouble)
 
calculate' :: String -> Maybe (Expr, String)
calculate' = run expr

calculate'' :: Maybe (Expr, String) -> (Maybe Expr, MaybeError Double)
calculate'' (Just (expr, [])) = (Just expr, Success (eval expr))
calculate'' (Just (expr, s)) = (Just expr, Error ("Unexpected input: " ++ s))
calculate'' Nothing = (Nothing, Error "Syntax error")

calculate :: String -> (Maybe Expr, MaybeError Double)
calculate = calculate'' . calculate'

main :: IO ()
main = do
    rawargs <- getArgs
    let args = concat rawargs 
    case args of
        "" -> putStrLn "Usage: ./Haskulator <expression>"
        s -> do
            let (expr, result) = calculate s
            case result of
                Success r -> putStrLn $ "Result: " ++ show r
                Error e -> putStrLn $ "Error: " ++ e
        