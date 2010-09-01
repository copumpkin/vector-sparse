{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}
module Data.SparseVector where

-- The interval map stuff is borrowed and expanded from Data.IntervalMap.Fingertree in the fingertree package.
-- I can't just link to it because their constructors aren't exposed and I need more operations.

import Prelude hiding (length, (++))

import Control.Applicative ((<$>))
import Control.Arrow (second)

import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Text.Show

import qualified Data.Vector as V
import Data.Vector (Vector)

import qualified Data.FingerTree as F
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), ViewR(..), (<|), (><), viewl, viewr, split)

type Index = Int

data Interval = Interval { low :: !Index, high :: !Index }
  deriving (Eq, Ord, Show)

-- Now I'm kind of glad I'm using ints
shiftInterval :: Index -> Interval -> Interval
shiftInterval i (Interval l h) = Interval (l + i) (h + i)

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

data IntInterval = NoInterval | IntInterval !Interval !Index

atleast :: Index -> IntInterval -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ NoInterval = error "remind me again why I can't use a semigroup for fingertrees?"

greater :: Index -> IntInterval -> Bool
greater k (IntInterval i _) = low i > k
greater _ NoInterval = error "remind me again why I can't use a semigroup for fingertrees?"

instance Monoid IntInterval where
	mempty = NoInterval
	
	NoInterval `mappend` i	= i
	i `mappend` NoInterval	= i
	IntInterval _ hi1 `mappend` IntInterval int2 hi2 = IntInterval int2 (max hi1 hi2)

instance Measured IntInterval (Node a) where
	measure (Node i _) = IntInterval i (high i)


newtype SparseVector a = SparseVector (FingerTree IntInterval (Node (Vector a)))
  deriving (Eq)
  
instance Show a => Show (SparseVector a) where
  showsPrec d sv = showParen (d > appPrec) $ showString "fromLists " . showsPrec (appPrec + 1) (toLists sv)
    where appPrec = 10

empty :: SparseVector a
empty = SparseVector F.empty

singleton :: Index -> a -> SparseVector a
singleton i x = SparseVector (F.singleton (Node (point i) (V.singleton x)))

(!) :: SparseVector a -> Index -> Maybe a
sv ! i = (\(b, v) -> v V.! (i - b)) <$> getSegment i sv

length :: SparseVector a -> Index
length (SparseVector (viewr -> EmptyR)) = 0
length (SparseVector (viewr -> _ :> Node (Interval _ h) _)) = h

getSegment :: Index -> SparseVector a -> Maybe (Index, Vector a)
getSegment i = listToMaybe . getSegments i i

getSegments :: Index -> Index -> SparseVector a -> [(Index, Vector a)]
getSegments lo hi (SparseVector t) = matches (F.dropUntil (atleast lo) (F.takeUntil (greater hi) t))
  where matches (viewl -> EmptyL) = []
        matches (viewl -> Node i x :< xs) = (low i, x) : matches xs


fromVector :: Index -> Vector a -> SparseVector a
fromVector i v = SparseVector (F.singleton (Node (Interval i (i + V.length v)) v))

fromVectors :: [(Index, Vector a)] -> SparseVector a
fromVectors = foldl' union empty . map (uncurry fromVector)

fromList :: Index -> [a] -> SparseVector a
fromList i = fromVector i . V.fromList

fromLists :: [(Index, [a])] -> SparseVector a
fromLists = fromVectors . map (second V.fromList)

toVectors :: SparseVector a -> [(Index, Vector a)]
toVectors (SparseVector t) = map (\(Node i v) -> (low i, v)) . toList $ t

toLists :: SparseVector a -> [(Index, [a])]
toLists = map (second V.toList) . toVectors

-- Does this make sense to expose? we'd need a more meaningful behavior to do for overlapping segments, because we don't want overlapping segments
union  :: SparseVector a -> SparseVector a -> SparseVector a
union (SparseVector xs) (SparseVector ys) = SparseVector (merge1 xs ys)
  where merge1 (viewl -> EmptyL) bs = bs
        merge1 (viewl -> a@(Node i _) :< as) bs = l >< a <| merge2 as r
          where (l, r) = split larger bs
                larger (IntInterval k _) = k >= i
			
        merge2 as (viewl -> EmptyL) = as
        merge2 as (viewl -> b@(Node i _) :< bs)	= l >< b <| merge1 r bs
          where (l, r) = split larger as
                larger (IntInterval k _) = k > i

instance Functor SparseVector where
  fmap f (SparseVector t) = SparseVector (F.unsafeFmap (fmap (V.map f)) t)


shift :: Index -> SparseVector a -> SparseVector a
shift d (SparseVector t) = SparseVector (F.unsafeFmap (\(Node i v) -> Node (shiftInterval d i) v) t)

(++) :: SparseVector a -> SparseVector a -> SparseVector a
t ++ v = union t (shift (length t) v)
