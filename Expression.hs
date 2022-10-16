module Expression where

data Expr = Number Double | Bin Op Expr Expr
  deriving (Show)

data Op = Plus | Minus | Div | Mult | Pow
  deriving (Show)

eval :: Expr -> Double
eval (Number x) = x
eval (Bin Plus x y) = eval x + eval y
eval (Bin Minus x y) = eval x - eval y
eval (Bin Mult x y) = eval x * eval y
eval (Bin Div x y) = eval x / eval y
eval (Bin Pow x y) = eval x ** eval y