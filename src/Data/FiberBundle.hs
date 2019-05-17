{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
    -- * QuickCheck Properties
  , prop_SemigroupBundle_combine_base
  , prop_SemigroupBundle_combine_associative
  , prop_MonoidBundle_unit_left
  , prop_MonoidBundle_unit_right
  , prop_GroupBundle_inverse_base
  , prop_GroupBundle_inverse_combine_left
  , prop_GroupBundle_inverse_combine_right
  , prop_AbelianBundle_combine_commutative
  ) where

import Data.Maybe (fromJust)

--------------------------------------------------------------------------------
-- Fiber Bundle

-- | A 'FiberBundle' is a type @a@ together with another type @'Base' a@ such
-- that there is a mapping from any element of @a@ to an element of @'Base' a@.
-- Therefore, one can see @a@ as a set of /fibers/: for each @b :: 'Base' a@
-- there is a set @[a | 'base' a == b]@, the /fiber/ at @b@.
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


