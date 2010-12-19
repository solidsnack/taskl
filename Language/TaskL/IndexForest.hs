{-# LANGUAGE StandaloneDeriving
  #-}

module Language.TaskL.IndexForest where

import qualified Data.List as List
import Data.Tree
import Data.Monoid
import Data.Ord

import Data.Number.Natural


newtype Index                =  Index (Forest Natural)
deriving instance Eq Index
deriving instance Show Index
instance Ord Index where
  Index a `compare` Index b  =  a `compareF` b
instance Monoid Index where
  mempty                     =  Index []
  Index a `mappend` Index b  =  Index (mergeF a b)


-- | Merge a 'Tree' into a 'Forest'.
mergeT                      ::  (Eq t) => Forest t -> Tree t -> Forest t
mergeT forest (Node x forest') = case break ((== x) . rootLabel) forest of
  (a, [                 ])  ->  a
  (a, (Node _ forest''):b)  ->  a ++ [Node x (mergeF forest' forest'')] ++ b


-- | Merge all the 'Tree' in the right 'Forest' into the left 'Forest'.
mergeF                      ::  (Eq t) => Forest t -> Forest t -> Forest t
mergeF forest                =  concatMap (mergeT forest)


compareT                    ::  (Ord t) => Tree t -> Tree t -> Ordering
Node a a' `compareT` Node b b' = case compare a b of EQ     ->  compareF a' b'
                                                     x      ->  x


compareF                    ::  (Ord t) => Forest t -> Forest t -> Ordering
compareF a b                 =  case (a', b') of
  ([   ], [   ])            ->  EQ
  ([   ], _    )            ->  LT
  (_    , [   ])            ->  GT
  (t0:f0, t1:f1)            ->  case compareT t0 t1 of EQ   ->  compareF f0 f1
                                                       x    ->  x
 where
  a'                         =  List.sortBy compareT a
  b'                         =  List.sortBy compareT b


