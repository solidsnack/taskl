
module System.TaskL.IndexForest where

import Data.Tree
import Data.Monoid

import Data.Number.Natural


newtype Index                =  Index (Forest Natural)
deriving instance Eq Index
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

