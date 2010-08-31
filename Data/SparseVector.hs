{-# LANGUAGE MultiParamTypeClasses #-}
module Data.SparseVector where

-- The interval map stuff is borrowed and expanded from Data.IntervalMap.Fingertree in the fingertree package

import Control.Applicative

import Data.Monoid
import Data.Foldable
import Data.Traversable

import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.FingerTree as F
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), (<|), (><))

type Index = Int

data Interval = Interval { low :: !Index, high :: !Index }
  deriving (Eq, Ord, Show)

point :: Index -> Interval
point v = Interval v v

data Node a = Node Interval a
  deriving (Eq)

instance Functor Node where
	fmap f (Node i x) = Node i (f x)

instance Foldable Node where
	foldMap f (Node _ x) = f x

instance Traversable Node where
	traverse f (Node i x) = Node i <$> f x

data IntInterval = NoInterval | IntInterval Interval Index

instance Monoid IntInterval where
	mempty = NoInterval
	
	NoInterval `mappend` i	= i
	i `mappend` NoInterval	= i
	IntInterval _ hi1 `mappend` IntInterval int2 hi2 = IntInterval int2 (max hi1 hi2)

instance Measured IntInterval (Node a) where
	measure (Node i _) = IntInterval i (high i)


newtype SparseVector a = SparseVector (FingerTree IntInterval (Node (Vector a)))
  deriving (Eq)

empty :: SparseVector a
empty = SparseVector F.empty

singleton :: Index -> a -> SparseVector a
singleton i x = SparseVector (F.singleton (Node (point i) (V.singleton x)))
