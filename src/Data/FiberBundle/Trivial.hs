{-# LANGUAGE TypeFamilies #-}

module Data.FiberBundle.Trivial
  ( TrivialBundle (..)
  ) where

import Data.FiberBundle
import Data.Group

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
