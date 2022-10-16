module Error 
(
    MaybeError(..)
)
where

data MaybeError a = Error String| Success a
    deriving (Show, Eq)

instance Functor MaybeError where
  fmap f (Success a) = Success (f a)
  fmap f (Error s) = Error s

instance Applicative MaybeError where
  pure = Success
  Error s <*> _ = Error s
  Success f <*> x = fmap f x

instance Monad MaybeError where
  return = Success
  (Success a) >>= f = f a
  (Error s) >>= f = Error s
