{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FiberBundle.Section
  ( -- * Bundle Sections
    BundleSection
  , value
    -- * Construction
  , insert
  , insertLeft
  , insertRight
  , fromList
  , fromListLeft
  , fromListRight
    -- * Transformation
  , map
  , mapMonotonic
    -- * Destruction
  , toList
  ) where

import Data.FiberBundle

import Data.Foldable (foldl')
import Data.Group (Group (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Prelude hiding (map)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge

--------------------------------------------------------------------------------
-- Bundle Section

-- | A 'BundleSection' corresponds to a function @f :: Base a -> a@ such that
-- @base . f = id@. In this encoding we only store values that are different
-- from a 'unit'.
newtype BundleSection a = BundleSection (Map (Base a) a)

instance (Show a, Show (Base a)) => Show (BundleSection a) where
  show (BundleSection m) = show m

instance (Eq a, Eq (Base a)) => Eq (BundleSection a) where
  (BundleSection m1) == (BundleSection m2) = m1 == m2
  (BundleSection m1) /= (BundleSection m2) = m1 /= m2

instance (MonoidBundle a, Ord (Base a), Eq a) => Semigroup (BundleSection a) where
  (<>) = sectionCombine

instance (MonoidBundle a, Ord (Base a), Eq a) => Monoid (BundleSection a) where
  mempty = sectionUnit

instance (GroupBundle a, Ord (Base a), Eq a) => Group (BundleSection a) where
  invert = sectionInvert

-- | The underlying function of the 'BundleSection'.
value :: (MonoidBundle a, Ord (Base a)) => BundleSection a -> Base a -> a
value (BundleSection m) b = fromMaybe (unit b) (Map.lookup b m)

-- | Insert a value in a 'BundleSection', by combining the given value to the
-- value that is present in the corresponding fiber. Synonym for 'insertLeft',
-- simplified for when the operation is commutative.
insert ::
  (AbelianBundle a, MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
insert = insertLeft

-- | Insert a value in a 'BundleSection', by combining the given value to the
-- value that is present in the corresponding fiber. The new value is combined
-- from the left: @new <> old@.
insertLeft ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
insertLeft a (BundleSection m) =
  let
    b = base a
  in
    if a == unit b
      then BundleSection m
      else BundleSection (Map.alter f (base a) m)
  where
    f Nothing = Just a
    f (Just a') = combineNotNull a a'

-- | Insert a value in a 'BundleSection', by combining the given value to the
-- value that is present in the corresponding fiber. The new value is combined
-- from the right: @old <> new@.
insertRight ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
insertRight a (BundleSection m) =
  let
    b = base a
  in
    if a == unit b
      then BundleSection m
      else BundleSection (Map.alter f (base a) m)
  where
    f Nothing = Just a
    f (Just a') = combineNotNull a' a

-- | Combine the two elements, returning 'Nothing' is the result is the unit.
combineNotNull :: (MonoidBundle a, Eq a) => a -> a -> Maybe a
combineNotNull x y =
  case combine x y of
    Nothing -> Nothing
    Just z ->
      if z == unitOf x
        then Nothing
        else Just z

-- | Build a 'BundleSection' from a list of elements. Elements belonging to the
-- same fiber are combined. This is a synonym for 'fromListLeft', simplified
-- for when the operation is commutative.
fromList ::
  (AbelianBundle a, MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
fromList = fromListLeft

-- | Build a 'BundleSection' from a list of elements. Elements belonging to the
-- same fiber are combined. The new value is combined from the left: @new <>
-- old@.
fromListLeft ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
fromListLeft = foldl' (flip insertLeft) sectionUnit

-- | Build a 'BundleSection' from a list of elements. Elements belonging to the
-- same fiber are combined. The new value is combined from the right: @old <>
-- new@.
fromListRight ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
fromListRight = foldl' (flip insertRight) sectionUnit

-- | Retrieve all the non-unit values for the 'BundleSection'.
toList :: BundleSection a -> [a]
toList (BundleSection m) = Map.elems m

-- | Maps a 'BundleSection' from a 'MonoidBundle' to another. The function @map
-- g f@ only makes sense if @g@ and @f@ constitute a /fiber bundle morphism/.
-- That is, the mapping preserves fibers:
--
-- @
--     g . base = base . f
--
--              f
--      a ------------> b
--      |               |
-- base |               | base
--      V               V
--   Base a --------> Base b
--              g
-- @
--
-- Furthermore, we expect the function @f@ to be an /abelian monoid morphism/
-- in each fiber. The "abelian" part is due to the fact that there is no
-- guarantee on the order on which fibers are combined.
--
-- WARNING: This function can error if the fiber bundle morphism precondition
-- is not satisfied.
map ::
  (AbelianBundle b, MonoidBundle b, Ord (Base b), Eq b) =>
  (Base a -> Base b) -> (a -> b) -> BundleSection a -> BundleSection b
map b f (BundleSection m) =
    BundleSection $
    Map.filter (not . isUnit) $
    Map.mapKeysWith unsafeCombine b $
    Map.map f m

-- | The same as 'map', but for base mappings which are monotonic. In this case
-- the mapping between fibers is injective. Therefore, the "abelian" requisite
-- can be dropped.
mapMonotonic ::
  (MonoidBundle b, Ord (Base b), Eq b) =>
  (Base a -> Base b) -> (a -> b) -> BundleSection a -> BundleSection b
mapMonotonic b f (BundleSection m) =
    BundleSection $
    Map.mapKeysMonotonic b $
    Map.mapMaybe f' m
  where
    f' x =
      let
        y = f x
      in
        if isUnit y then Nothing else Just y

--------------------------------------------------------------------------------
-- Instance helpers

-- | The unit section, where all values are mapped to their corresponding 'unit'.
sectionUnit :: MonoidBundle a => BundleSection a
sectionUnit = BundleSection Map.empty

-- | Combine two 'BundleSection's into a single one, combining elements in the
-- same fiber.
sectionCombine ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  BundleSection a -> BundleSection a -> BundleSection a
sectionCombine (BundleSection m1) (BundleSection m2) = BundleSection m
  where
    m =
      Map.Merge.merge
        Map.Merge.preserveMissing
        Map.Merge.preserveMissing
        (Map.Merge.zipWithMaybeMatched (const combineNotNull))
        m1
        m2

-- | Invert elements on all the fibers of the 'BundleSection'.
sectionInvert ::
  (GroupBundle a, Ord (Base a)) =>
  BundleSection a -> BundleSection a
sectionInvert (BundleSection m) = BundleSection (fmap inverse m)
