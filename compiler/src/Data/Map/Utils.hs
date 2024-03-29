module Data.Map.Utils
  ( fromKeys,
    fromKeysA,
    fromValues,
    any,
  )
where

import Data.Map qualified as Map
import Data.Map.Internal (Map (..))
import Prelude hiding (any)

-- FROM KEYS

fromKeys :: (Ord k) => (k -> v) -> [k] -> Map.Map k v
fromKeys toValue keys =
  Map.fromList $ map (\k -> (k, toValue k)) keys

fromKeysA :: (Applicative f, Ord k) => (k -> f v) -> [k] -> f (Map.Map k v)
fromKeysA toValue keys =
  Map.fromList <$> traverse (\k -> (,) k <$> toValue k) keys

fromValues :: (Ord k) => (v -> k) -> [v] -> Map.Map k v
fromValues toKey values =
  Map.fromList $ map (\v -> (toKey v, v)) values

-- ANY

any :: (v -> Bool) -> Map.Map k v -> Bool
any isGood = go
  where
    go Tip = False
    go (Bin _ _ v l r) = isGood v || go l || go r
