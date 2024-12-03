module Language.Github.Actions.Internal
  ( inverseMap,
  )
where

import Data.Function ((.))
import Data.Functor (Functor, fmap)
import Data.Map qualified as Map
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import GHC.Enum (Bounded, Enum, maxBound, minBound)

-- | https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#inverseMap
inverseMap ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  (a -> k) ->
  (k -> Maybe a)
inverseMap f = (`Map.lookup` dict)
  where
    dict :: Map.Map k a
    dict = Map.fromList (fmapToFst f (universe @a))

-- | https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#fmapToFst
fmapToFst :: (Functor f) => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst

-- | https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Enum.html#universe
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

-- | https://hackage.haskell.org/package/relude-1.2.2.0/docs/src/Relude.Extra.Tuple.html#toFst
toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
