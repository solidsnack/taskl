{-# LANGUAGE StandaloneDeriving
           , ParallelListComp
  #-}

{-| The input to the Task\\L system is a list of 'Tree' of 'Task' items,
    specifying operations and their dependencies. Multiple declarations of the
    same task are merged and contradictions checked for before the whole
    structure is flattened into a linear schedule in which each attribute -- a
    file's permissions, a group's membership list -- is touched but one time.

 -}

module System.TaskL.Schedule where

import qualified Data.List as List
import Data.Tree
import Control.Arrow

import Data.Number.Natural
import Data.Monoid

import System.TaskL.Combination
import System.TaskL.IndexForest
import System.TaskL.Task
import System.TaskL.Op


{-| The script backend consumes a schedule, a list of basic operations, which
    is constructed from a forest of 'Task' nodes. The forest encodes
    dependencies among tasks.

    The forest is first translated to a forest in which every task is labelled
    with its position in the forest, giving a notion of the desired ordering.
    The forest is then collapsed to a list in which like tasks are merged and
    paired with their collective dependents. At this stage, contradictions are
    detected; unmergeable operations result in an error.

    The commands are then scheduled according to these guidelines:

   *  Dependencies must come before their dependents.

   *  It would be nice if tasks that were earlier in the input came before
      commands that came later in the input.

   *  Commands with multiple dependents should be grouped so that each
      dependent is being run for as short a time as is practical.

 -}
schedule                    ::  [Tree Task] -> ([Op], [Error], [Warn])
schedule                     =  operations . merge . trees . indexed


{-| Labels every node with a sequence of indices into the tree, pairing the
    sequence with the node's subtree.
 -}
indexed                     ::  Forest Task -> Forest (Index, Task)
indexed                      =  fmap (fmap index) . indexed'' []
 where
  index                      =  first (Index . (:[]) . unfold . reverse)
  unfold = unfoldTree (head &&& (filter (/=[]) . (:[]) . tail))

indexed'' :: [Natural] -> Forest Task -> Forest ([Natural], Task)
indexed'' path forest =
 [ Node (path', x) (indexed'' path' forest') | Node x forest' <- forest
                                             | n <- [0..], let path' = n:path ]


trees                       ::  Forest (Index, Task) -> [Tree (Index, Task)]
trees forest = concat (forest : fmap (trees . subForest) forest)


instance Combine (Tree (Index, Task)) where
  combine a@(Node (i0, t0) d0) b@(Node (i1, t1) d1) = case combine t0 t1 of
    Combined c              ->  Combined (Node (mappend i0 i1, c) (d0 ++ d1))
    Separate _ _            ->  Separate a b
    Contradictory _ _       ->  Contradictory a b


merge :: [Tree (Index, Task)] -> ([Tree (Index, Task)], [Error], [Warn])
merge                        =  foldr mergeOne ([], [], [])


data Error = Conflict (Tree (Index, Task)) (Tree (Index, Task))
deriving instance Show Error

data Warn = Overlap (Tree (Index, Task)) (Tree (Index, Task))
deriving instance Show Warn


mergeOne                    ::  Tree (Index, Task)
                            ->  ([Tree (Index, Task)], [Error], [Warn])
                            ->  ([Tree (Index, Task)], [Error], [Warn])
mergeOne node ([], e, w)     =  ([node], e, w)
mergeOne node x@(nodes, _, _) = (foldr place x . map (combine node)) nodes
 where
  place result (tasks, errors, warns) = case result of
    Contradictory a b       ->  (tasks, (Conflict a b):errors, warns)
    Separate a _            ->  (a:tasks, errors, warns)
    Combined c              ->  (c:tasks, errors, warns)


operations                  ::  ([Tree (Index, Task)], [Error], [Warn])
                            ->  ([Op], [Error], [Warn])
operations (tasks, e, w)     =  ((List.sort . concatMap ops) tasks, e, w)

