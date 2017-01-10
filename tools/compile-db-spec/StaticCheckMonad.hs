{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module StaticCheckMonad (M, run, mbGetKey, except) where
-----------------------------------------------------------
-- File format parser monad -------------------------------
-----------------------------------------------------------

import Control.Applicative
import MonadLib
import ExceptionsT
import Data.Map (Map)
import qualified Data.Map as Map

data ValueData v = VD
  { value :: v
  , used  :: Bool
  }

mkVD v = VD { value = v, used = False }
markUsed v = v { used = True }
unusedKeys m = [ k | (k,v) <- Map.toList m, not (used v)]


newtype M k v a = MkM (ExceptionsT [String] (StateT (Map k (ValueData v)) Id) a)
  deriving (Functor, Applicative, Monad)

run :: Map k v -> M k v a -> (Either [String] a, [k])
run st (MkM m) = let (res, st') = runId $ runStateT (fmap mkVD st) $ runExceptionsT m
                 in (res, unusedKeys st')

mbGetKey :: Ord k => k -> M k v (Maybe v)
mbGetKey key = MkM $ liftE $ do
  m <- get
  let (res, m') = Map.updateLookupWithKey (\ _ -> Just . markUsed) key m
  set m'
  return (fmap value res)

availableKeys :: Ord k => M k v [k]
availableKeys = MkM (liftE (fmap Map.keys get))

except :: String -> M k v a
except s = MkM (toss [s])
