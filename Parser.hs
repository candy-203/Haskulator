module Parser
  ( Parser,
    getc,
    char,
    string,
    skipSpace,
    parseInt,
    parseDouble,
    run,
    paren,
    parseIntAsDouble,
    token
  )
where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser {run :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (x, s') <- p s
    return (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  (Parser p) <*> (Parser q) = Parser $ \s -> do
    (f, s') <- p s
    (x, s'') <- q s'
    return (f x, s'')

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \s -> do
    (x, s') <- p s
    run (f x) s'

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

instance Alternative Parser where
  empty = fail ""
  (Parser p) <|> (Parser q) = Parser $ \s ->
    case p s of
      Just x -> Just x
      Nothing -> q s

getc :: Parser Char
getc = Parser f
  where
    f [] = Nothing
    f (x : xs) = Just (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- getc
  if p x then return x else fail ""

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string = mapM char

space :: Parser Char
space = char ' ' <|> char '\n' <|> char '\r' <|> char '\t'

skipSpace :: Parser [Char]
skipSpace = many space

parseInt' :: Parser Char
parseInt' =
  char '0' <|> char '1' <|> char '2' <|> char '3'
    <|> char '4'
    <|> char '5'
    <|> char '6'
    <|> char '7'
    <|> char '8'
    <|> char '9'

parseInt :: Parser Int
parseInt = read <$> some parseInt'

parseIntAsDouble :: Parser Double
parseIntAsDouble = read <$> some parseInt'

parseDouble :: Parser Double  -- TODO: fix this description:  
parseDouble = do
  x <- parseInt
  char '.'
  y <- parseInt
  return (fromIntegral x + fromIntegral y / 10 ^ length (show y))

paren :: Parser a -> Parser a
paren p = do
  char '('
  x <- p
  char ')'
  return x

token :: Parser b -> Parser b
token p = skipSpace >> p

