{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Instances where

import Prelude as P

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.SparseVector

import qualified Data.Vector as V
import Data.Vector (Vector)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fmap V.fromList arbitrary

-- The mod is to prevent it from generating huge ranges that slow down our tests when we're converting to flat vectors
instance Arbitrary a => Arbitrary (SparseVector a) where
  arbitrary = liftA2 (fromList . (`mod` 10000) . abs) arbitrary arbitrary


{-  
newtype NonOverlappingRange v a = NonOverlappingRange [(Index, v a)] 
  deriving (Show)


nonOverlapping :: (v a -> Int) -> [(NonNegative Index, v a)] -> [(NonNegative Index, v a)]
nonOverlapping _ [] = []
nonOverlapping len xs = scanl1 (\(NonNegative x, y) (NonNegative a, b) -> (NonNegative $ x + a + len y, b)) xs

deNonNegative :: [(NonNegative a, b)] -> [(a, b)]
deNonNegative = map (\(NonNegative i, xs) -> (i, xs))


instance Arbitrary a => Arbitrary (NonOverlappingRange Vector a) where
  arbitrary = do base <- arbitrary
                 return $ NonOverlappingRange (deNonNegative (nonOverlapping V.length base))

instance Arbitrary a => Arbitrary (NonOverlappingRange [] a) where
 arbitrary = do base <- arbitrary
                return $ NonOverlappingRange (deNonNegative (nonOverlapping P.length base))
-}