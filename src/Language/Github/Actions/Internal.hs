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

-- | Create an inverse mapping from keys back to enumeration values.
--
-- This function takes a function that converts enumeration values to keys
-- and returns a function that can look up the original enumeration value
-- from a key. This is commonly used for parsing JSON representations of
-- enumeration types.
--
-- Example usage:
--
-- @
-- data Color = Red | Green | Blue
--  deriving (Bounded, Enum, Eq, Ord, Show)
--
-- colorToText :: Color -> Text
-- colorToText Red = "red"
-- colorToText Green = "green"
-- colorToText Blue = "blue"
--
-- textToColor :: Text -> Maybe Color
-- textToColor = inverseMap colorToText
--
-- -- Usage:
-- textToColor "red"   == Just Red
-- textToColor "green" == Just Green
-- textToColor "blue"  == Just Blue
-- textToColor "yellow" == Nothing
-- @
--
-- Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#inverseMap>
inverseMap ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  -- | Function from enumeration values to keys
  (a -> k) ->
  -- | Function from keys to enumeration values
  (k -> Maybe a)
inverseMap f = (`Map.lookup` dict)
  where
    dict :: Map.Map k a
    dict = Map.fromList (fmapToFst f (universe @a))

-- | Map a function over a functor, creating tuples with the result as the first element.
--
-- Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#fmapToFst>
fmapToFst :: (Functor f) => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

-- | Generate all values of a bounded enumeration type.
--
-- Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#universe>
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

-- | Apply a function and create a tuple with the result as the first element.
--
-- Based on: <https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#toFst>
toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
