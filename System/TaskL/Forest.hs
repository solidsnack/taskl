
module System.TaskL.Forest where

import Data.Tree


-- | Merge a 'Tree' into a 'Forest'.
mergeT                      ::  (Eq t) => Forest t -> Tree t -> Forest t
mergeT forest tree@(Node x forest') = case break ((== x) . rootLabel) forest of
  (a, [                 ])  ->  a
  (a, (Node _ forest''):b)  ->  a ++ [Node x (mergeF forest' forest'')] ++ b


-- | Merge all the 'Tree' in the right 'Forest' into the left 'Forest'.
mergeF                      ::  (Eq t) => Forest t -> Forest t -> Forest t
mergeF forest                =  concatMap (mergeT forest)

