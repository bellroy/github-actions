{-# LANGUAGE ScopedTypeVariables #-}

module Language.Github.Actions.Internal (inverseMap) where

import qualified Data.Map as Map

-- | Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#inverseMap>
inverseMap ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  (a -> k) ->
  (k -> Maybe a)
inverseMap f = (`Map.lookup` dict)
  where
    dict :: Map.Map k a
    dict = Map.fromList ((\a -> (f a, a)) <$> [minBound .. maxBound])
