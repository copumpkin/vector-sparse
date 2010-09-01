module Instances where

import Control.Applicative
import Test.QuickCheck

import Data.SparseVector

import qualified Data.Vector as V
import Data.Vector (Vector)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap V.fromList arbitrary

instance Arbitrary a => Arbitrary (SparseVector a) where
  arbitrary = liftA2 (fromList . abs) arbitrary arbitrary