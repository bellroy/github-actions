{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Github.Actions.Internal
-- Description : Internal utility functions for GitHub Actions library
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides internal utility functions used throughout the GitHub Actions
-- library. These functions are not intended for external use but are exported for
-- testing and internal module dependencies.
--
-- The main utility is 'inverseMap' which creates bidirectional mappings between
-- enumeration values and their string representations, commonly used for JSON
-- serialization of activity types and other enumerated values.
module Language.Github.Actions.Internal
  ( inverseMap,
  )
where

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
    dict = Map.fromList (fmapToFst f (universe @a))

-- | Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#fmapToFst>
fmapToFst :: (Functor f) => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

-- | Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#universe>
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

-- | Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#toFst>
toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
