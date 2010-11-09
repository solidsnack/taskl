
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , ParallelListComp
 #-}

{-| The Task\\L system provides a language for configuring Linux machines (and
    it probably works on other UNIX-alikes, provided the GNU core-utils are
    installed).

    Tasks in Task\\L are idempotent by design; one may build up packages of
    idempotent actions from a library of primitive commands that each resemble
    a classic UNIX command (see the @IdemShell@ docs to find out more about
    these commands).

    The input to the Task\\L system is a list of 'Tree' of 'Task' items,
    specifying operations and their dependencies. Multiple declarations of the
    same task are merged and contradictions checked for before the whole
    structure is flattened into a linear schedule in which each attribute -- a
    file's permissions, a group's membership list -- is touched but one time.

    The schedule is compiled to a Bash script which may then be executed to
    instantiate the configuration. As it runs, the script gives notes which
    package (or packages) it is operating on as well any subtasks; the script
    may be run in an interactive mode that allows delaying, skipping or
    aborting at any step.

 -}

module System.TaskL where

import qualified Data.List as List
import Data.Tree
import Data.String
import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad.Identity
import Control.Monad.State

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Number.Natural
import Data.Monoid

import qualified System.TaskL.IdemShell as IdemShell
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
schedule                    ::  Forest Task -> Either Error (Warn, [Op])
schedule                     =  undefined


{-| Labels every node with a sequence of indices into the tree, pairing the
    sequence with the node's subtree.
 -}
dPath                       ::  Forest Task -> Forest (Index, Task)
dPath                        =  fmap (fmap index) . dPath'' []
 where
  index                      =  first (Index . reverse)

dPath'' :: Forest Natural -> Forest Task -> Forest (Forest Natural, Task)
dPath'' path forest =
 [ Node (path', x) (dPath'' path' forest') | Node x forest' <- forest
                                           | n <- [0..],
                                             let path' = [Node n path] ]


trees                       ::  Forest (Index, Task) -> [Tree (Index, Task)]
trees forest = concat (forest : fmap (trees . subForest) forest)


instance Combine (Tree (Index, Task)) where
  combine a@(Node (i0, t0) d0) b@(Node (i1, t1) d1) = case combine t0 t1 of
    Combined c              ->  Combined (Node (mappend i0 i1, c) (d0 ++ d1))
    Separate _ _            ->  Separate a b
    Contradictory _ _       ->  Contradictory a b


merge :: [Tree (Index, Task)] -> ([Tree (Index, Task)], [Error], [Warn])
merge                        =  foldr mergeOne ([], [], [])


mergeOne                    ::  Tree (Index, Task)
                            ->  ([Tree (Index, Task)], [Error], [Warn])
                            ->  ([Tree (Index, Task)], [Error], [Warn])
mergeOne node x@(nodes, _, _) = (foldr place x . map (combine node)) nodes
 where
  place result (tasks, errors, warns) = case result of
    Contradictory a b       ->  (tasks, (Conflict a b):errors, warns)
    Separate a b            ->  (b:tasks, errors, warns)
    Combined c              ->  (c:tasks, errors, warns)


data Error = Conflict (Tree (Index, Task)) (Tree (Index, Task))

data Warn = Overlap (Tree (Index, Task)) (Tree (Index, Task))




