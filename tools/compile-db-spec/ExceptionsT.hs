module ExceptionsT (ExceptionsT, runExceptionsT, toss, liftE) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow       (right)
import Data.Monoid         (Monoid(mappend))

-----------------------------------------------------------
-- Exceptions Transformer ---------------------------------
-----------------------------------------------------------

newtype ExceptionsT i m a = E (m (Either i a))

runExceptionsT :: ExceptionsT i m a -> m (Either i a)
runExceptionsT (E m) = m

instance Functor m => Functor (ExceptionsT i m) where
  fmap f (E m) = E (right f <$> m)

instance (Monoid i, Applicative m) => Applicative (ExceptionsT i m) where
  pure x = E (pure (Right x))
  E mf <*> E mx = E (merge <$> mf <*> mx)
    where
    merge (Right f) (Right x) = Right (f x)
    merge (Left xs) (Left ys) = Left (mappend xs ys)
    merge (Left xs) (Right _) = Left xs
    merge (Right _) (Left ys) = Left ys

instance Monad m => Monad (ExceptionsT i m) where
  return x = E (return (Right x))
  m >>= f = E (either (return . Left) (runExceptionsT . f) =<< runExceptionsT m)

liftE :: Functor m => m a -> ExceptionsT i m a
liftE m = E (Right <$> m)
                    
toss :: Applicative m => i -> ExceptionsT i m a
toss i = E (pure (Left i))
