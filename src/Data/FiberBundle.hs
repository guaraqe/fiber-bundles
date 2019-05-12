{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FiberBundle
  ( -- * Fiber Bundle
    FiberBundle (..)
    -- * Semigroup Bundle
  , SemigroupBundle (..)
    -- * Monoid Bundle
  , MonoidBundle (..)
  , unitOf
    -- * Group Bundle
  , GroupBundle (..)
    -- * Trivial Bundle
  , TrivialBundle (..)
    -- * Bundle Section
  , BundleSection
  , sectionValue
  , sectionUnit
  , sectionInsert
  , sectionLeftInsert
  , sectionRightInsert
  , sectionFromList
  , sectionFromListLeft
  , sectionFromListRight
  , sectionToList
    -- * QuickCheck Properties
  , prop_SemigroupBundle_combine_base
  , prop_SemigroupBundle_combine_associative
  , prop_MonoidBundle_unit_left
  , prop_MonoidBundle_unit_right
  , prop_GroupBundle_inverse_base
  , prop_GroupBundle_inverse_combine_left
  , prop_GroupBundle_inverse_combine_right
  ) where

import Data.Foldable (foldl')
import Data.Group (Group (..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge

--------------------------------------------------------------------------------
-- Fiber Bundle

-- | A 'FiberBundle' is a type @a@ together with another type @'Base' a@ such
-- that there is a mapping from any element of @a@ to an element of @'Base' a@.
-- Therefore, one can see @a@ as a set of /fibers/: for each @b :: 'Base' a@
-- there is a set @[a | 'base' a == b]@, the /fiber/ at @b@.
class FiberBundle a where
  type Base a :: *
  base :: a -> Base a

--------------------------------------------------------------------------------
-- Semigroup Bundle

-- | A 'SemigroupBundle' is a 'FiberBundle' whose fibers have a 'Semigroup'
-- structure. We represent this structure by a partial function 'combine' that
-- has the following semantics: given @x@ and @y@, if they belong to the same
-- fiber, we return 'Just' their combination, otherwise, we return
-- 'Nothing'.
class FiberBundle a => SemigroupBundle a where
  combine :: a -> a -> Maybe a

-- | Checks that 'combine' returns a result if and only if the arguments belong
-- to the same fiber.
prop_SemigroupBundle_combine_base ::
  (SemigroupBundle a, Eq (Base a)) =>
  a -> a -> Bool
prop_SemigroupBundle_combine_base x y =
    case combine x y of
      Nothing ->
        base x /= base y
      Just _ ->
        base x == base y

-- | Checks that 'combine' is associative.
prop_SemigroupBundle_combine_associative ::
  (SemigroupBundle a, Eq a) =>
  a -> a -> a -> Bool
prop_SemigroupBundle_combine_associative x y z =
    (combine x y >>= flip combine z) ==
    (combine x =<< combine y z)

--------------------------------------------------------------------------------
-- Monoid Bundle

-- | A 'MonoidBundle' is a 'FiberBundle' whose fibers have a 'Monoid'
-- structure. That is, for each fiber we have an 'unit' element that is the
-- neutral element for 'combine'.
class SemigroupBundle a => MonoidBundle a where
  unit :: Base a -> a

-- | Get the unit element on the fiber of the given element.
unitOf :: MonoidBundle a => a -> a
unitOf = unit . base

-- | Checks that 'unit' is a left unit for 'combine'.
prop_MonoidBundle_unit_left ::
  (MonoidBundle a, Eq a) =>
  a -> Bool
prop_MonoidBundle_unit_left x =
    combine (unitOf x) x == Just x

-- | Checks that 'unit' is a right unit for 'combine'.
prop_MonoidBundle_unit_right ::
  (MonoidBundle a, Eq a) =>
  a -> Bool
prop_MonoidBundle_unit_right x =
    combine x (unitOf x) == Just x

--------------------------------------------------------------------------------
-- Group Bundle

-- | A 'GroupBundle' is a 'FiberBundle' whose fibers have a 'Group' structure.
-- That is, we can 'inverse' any element, keeping it on the same fiber. This
-- inversed element is the inverse with relation to 'combine'.
class MonoidBundle a => GroupBundle a where
  inverse :: a -> a

-- | Check that 'inverse' preserves the fiber.
prop_GroupBundle_inverse_base ::
  (GroupBundle a, Eq (Base a)) =>
  a -> Bool
prop_GroupBundle_inverse_base x =
    base (inverse x) == base x

-- | Check that 'inverse' is a left inverse for 'combine'.
prop_GroupBundle_inverse_combine_left ::
  (GroupBundle a, Eq a) =>
  a -> Bool
prop_GroupBundle_inverse_combine_left x =
    combine (inverse x) x == Just (unitOf x)

-- | Check that 'inverse' is a right inverse for 'combine'.
prop_GroupBundle_inverse_combine_right ::
  (GroupBundle a, Eq a) =>
  a -> Bool
prop_GroupBundle_inverse_combine_right x =
    combine x (inverse x) == Just (unitOf x)

--------------------------------------------------------------------------------
-- Trivial Bundle

-- | A 'TrivialBundle' is a 'FiberBundle' that is the product of two types. The
-- 'base' function is given by projecting on the first coordinate.
data TrivialBundle b a = TrivialBundle !b !a
  deriving (Show, Eq)

instance FiberBundle (TrivialBundle b a) where
  type Base (TrivialBundle b a) = b
  base (TrivialBundle b _) = b

instance (Eq b, Semigroup a) => SemigroupBundle (TrivialBundle b a) where
  combine (TrivialBundle b1 a1) (TrivialBundle b2 a2) =
      if b1 == b2
        then Just $ TrivialBundle b1 (a1 <> a2)
        else Nothing

instance (Eq b, Monoid a) => MonoidBundle (TrivialBundle b a) where
  unit b = TrivialBundle b mempty

instance (Eq b, Group a) => GroupBundle (TrivialBundle b a) where
  inverse (TrivialBundle b a) = TrivialBundle b (invert a)

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

-- | The unit section, where all values are mapped to their corresponding 'unit'.
sectionUnit :: MonoidBundle a => BundleSection a
sectionUnit = BundleSection Map.empty

-- | The underlying function of the 'BundleSection'.
sectionValue :: (MonoidBundle a, Ord (Base a)) => BundleSection a -> Base a -> a
sectionValue (BundleSection m) b = fromMaybe (unit b) (Map.lookup b m)

-- | Insert a value in a 'BundleSection', by combining the given value to the
-- value that is present in the corresponding fiber. Synonym for
-- 'sectionLeftInsert', simplified for when the operation is commutative.
sectionInsert ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
sectionInsert = sectionLeftInsert

-- | Insert a value in a 'BundleSection', by combining the given value to the
-- value that is present in the corresponding fiber. The new value is combined
-- from the left: @new <> old@.
sectionLeftInsert ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
sectionLeftInsert a (BundleSection m) =
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
sectionRightInsert ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  a -> BundleSection a -> BundleSection a
sectionRightInsert a (BundleSection m) =
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
-- same fiber are combined. This is a synonym for 'sectionFromListLeft',
-- simplified for when the operation is commutative.
sectionFromList ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
sectionFromList = sectionFromListLeft

-- | Build a 'BundleSection' from a list of elements. Elements belonging to the
-- same fiber are combined. The new value is combined from the left: @new <>
-- old@.
sectionFromListLeft ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
sectionFromListLeft = foldl' (flip sectionLeftInsert) sectionUnit

-- | Build a 'BundleSection' from a list of elements. Elements belonging to the
-- same fiber are combined. The new value is combined from the right: @old <>
-- new@.
sectionFromListRight ::
  (MonoidBundle a, Ord (Base a), Eq a) =>
  [a] -> BundleSection a
sectionFromListRight = foldl' (flip sectionRightInsert) sectionUnit

-- | Retrieve all the non-unit values for the 'BundleSection'.
sectionToList :: BundleSection a -> [a]
sectionToList (BundleSection m) = Map.elems m

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
