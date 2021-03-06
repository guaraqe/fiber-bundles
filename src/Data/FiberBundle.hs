{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-| This library defines /fiber bundles/. Fiber bundles can be seen as a set of
    /fibers/ that have similar structure. These fibers are indexed by a set,
    called the /base space/.

    This is a structure that is well adapted to many cases where one almost
    have an algebraic structure, except for some information in the type, which
    must match for the algebraic operation to happen.

    A good example is money. One can add @1 EUR@ to @1 EUR@, but not to @1
    USD@. That is, one can add the quantities, provided that the currencies are
    equals. This means that one can give money the structure of a fiber bundle
    whose base space is a set of currencies.
-}

module Data.FiberBundle
  ( -- * Fiber Bundle
    FiberBundle (..)
    -- * Semigroup Bundle
  , SemigroupBundle (..)
    -- * Monoid Bundle
  , MonoidBundle (..)
  , unitOf
  , isUnit
    -- * Group Bundle
  , GroupBundle (..)
    -- * Abelian Bundle
  , AbelianBundle
    -- * Bundle Morphism
  , BundleMorphism (..)
  , monoidBundleMorphism
    -- * QuickCheck Properties
  , prop_SemigroupBundle_combine_base
  , prop_SemigroupBundle_combine_associative
  , prop_MonoidBundle_unit_left
  , prop_MonoidBundle_unit_right
  , prop_GroupBundle_inverse_base
  , prop_GroupBundle_inverse_combine_left
  , prop_GroupBundle_inverse_combine_right
  , prop_AbelianBundle_combine_commutative
  , prop_BundleMorphism_fiber_preserving
  , prop_BundleMorphism_semigroup
  , prop_BundleMorphism_monoid
  , prop_BundleMorphism_group
  ) where

import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
-- Fiber Bundle

-- | A 'FiberBundle' is composed of:
--
-- - a type @a@, called the /fiber space/
-- - a type @'Base' a@, called the /base space/
-- - a mapping @'base' : a -> 'Base'@ that maps each point to its corresponding
--   base.
--
-- The /fiber/ at @b :: 'Base' a@ is the set of all elements @x :: a@ such that
-- @base x = b@.
class FiberBundle a where
  type Base a :: *
  base :: a -> Base a

instance (FiberBundle a, FiberBundle b) => FiberBundle (a,b) where
  type Base (a,b) = (Base a, Base b)
  base (x, y) = (base x, base y)

instance (FiberBundle a, FiberBundle b) => FiberBundle (Either a b) where
  type Base (Either a b) = Either (Base a) (Base b)
  base (Left x) = Left (base x)
  base (Right x) = Right (base x)

--------------------------------------------------------------------------------
-- Semigroup Bundle

-- | A 'SemigroupBundle' is a 'FiberBundle' whose fibers have a 'Semigroup'
-- structure. We represent this structure by a partial function 'combine' that
-- has the following semantics: given @x@ and @y@, if they belong to the same
-- fiber, we return 'Just' their combination, otherwise, we return
-- 'Nothing'.
class FiberBundle a => SemigroupBundle a where
  combine :: a -> a -> Maybe a

  -- | An unsafe version of 'combine' that assumes that both arguments are in
  -- the same fiber. Errors otherwise.
  unsafeCombine :: a -> a -> a
  unsafeCombine x y = fromJust (combine x y)

  {-# MINIMAL combine #-}

instance (SemigroupBundle a, SemigroupBundle b) => SemigroupBundle (a,b) where
  combine (a1,b1) (a2,b2) = (,) <$> combine a1 a2 <*> combine b1 b2
  unsafeCombine (a1,b1) (a2,b2) = (unsafeCombine a1 a2, unsafeCombine b1 b2)

instance (SemigroupBundle a, SemigroupBundle b) => SemigroupBundle (Either a b) where
  combine (Left a) (Left b) = Left <$> combine a b
  combine (Right a) (Right b) = Right <$> combine a b
  combine _ _ = Nothing

  unsafeCombine (Left a) (Left b) = Left $ unsafeCombine a b
  unsafeCombine (Right a) (Right b) = Right $ unsafeCombine a b
  unsafeCombine _ _ = error "unsafeCombine: not in the same fiber"

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

instance (MonoidBundle a, MonoidBundle b) => MonoidBundle (a,b) where
  unit (a,b) = (unit a, unit b)

instance (MonoidBundle a, MonoidBundle b) => MonoidBundle (Either a b) where
  unit (Left a) = Left (unit a)
  unit (Right a) = Right (unit a)

-- | Get the unit element on the fiber of the given element.
unitOf :: MonoidBundle a => a -> a
unitOf = unit . base

isUnit :: (MonoidBundle a, Eq a) => a -> Bool
isUnit a = a == unitOf a

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

instance (GroupBundle a, GroupBundle b) => GroupBundle (a,b) where
  inverse (a,b) = (inverse a, inverse b)

instance (GroupBundle a, GroupBundle b) => GroupBundle (Either a b) where
  inverse (Left a) = Left (inverse a)
  inverse (Right a) = Right (inverse a)

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
-- Abelian Bundle

-- | A 'AbelianBundle' is a 'FiberBundle' whose fibers have an abelian
-- 'Semigroup' structure. That is, the 'combine' operation is commutative.
class SemigroupBundle a => AbelianBundle a where

-- | Checks that 'combine' is commutative.
prop_AbelianBundle_combine_commutative ::
  (AbelianBundle a, Eq a) =>
  a -> a -> Bool
prop_AbelianBundle_combine_commutative x y =
    combine x y == combine y x

--------------------------------------------------------------------------------
-- Fiber Bundle Morphisms

-- | A morphism between two 'FiberBundle's is a pair @'BundleMorphism' f g@
-- that preserves fibers. That is, the following diagram commute:
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
--
-- This morphism can have extra properties, such as preserving the 'Semigroup',
-- 'Monoid' or 'Group' structure of fibers. See the corresponding QuickCheck
-- properties below for more details.
--
-- One of the uses of 'BundleMorphism's to map between
-- 'Data.FiberBundle.Section.Section's.
data BundleMorphism a b = BundleMorphism (a -> b) (Base a -> Base b)

-- | In a 'MonoidBundle' any function @a -> b@ has a corresponding function
-- @Base a -> Base b@, namely @base . f . unit@. This pair corresponds to a
-- lawful 'BundleMorphism' if:
--
-- @
-- base . f . unitOf = base . f
-- @
monoidBundleMorphism ::
  (MonoidBundle a, FiberBundle b) => (a -> b) -> BundleMorphism a b
monoidBundleMorphism f = BundleMorphism f (base . f . unit)

-- | Checks that the 'BundleMorphism' preserves fibers.
prop_BundleMorphism_fiber_preserving ::
  (FiberBundle a, FiberBundle b, Eq (Base b)) =>
  BundleMorphism a b -> a -> Bool
prop_BundleMorphism_fiber_preserving (BundleMorphism f g) x =
  g (base x) == base (f x)

-- | Checks that the 'BundleMorphism' preserves 'combine'.
prop_BundleMorphism_semigroup ::
  (SemigroupBundle a, SemigroupBundle b, Eq b) =>
  BundleMorphism a b -> a -> a -> Bool
prop_BundleMorphism_semigroup (BundleMorphism f _) x y =
  combine (f x) (f y) == fmap f (combine x y)

-- | Checks that the 'BundleMorphism' preserves the 'unit' of each fiber.
prop_BundleMorphism_monoid ::
  (MonoidBundle a, MonoidBundle b, Eq b) =>
  BundleMorphism a b -> Base a -> Bool
prop_BundleMorphism_monoid (BundleMorphism f g) x =
  f (unit x) == unit (g x)

-- | Checks that the 'BundleMorphism' preserves 'inverse'.
prop_BundleMorphism_group ::
  (GroupBundle a, GroupBundle b, Eq b) =>
  BundleMorphism a b -> a -> Bool
prop_BundleMorphism_group (BundleMorphism f _) x =
  inverse (f x) == f (inverse x)
