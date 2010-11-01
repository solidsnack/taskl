
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

import Data.List (groupBy)
import Data.Tree
import Data.String
import Control.Applicative
import Control.Arrow (first, second)
import Control.Monad.Identity

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Number.Natural

import qualified System.TaskL.IdemShell as IdemShell


{-| A task may be either a command or a named package. Commands provide their
    own labels. Every 'Task' may attach tests to restrict the circumstances
    under which a command is run.
 -}
data Task                    =  Command IdemShell.Command [IdemShell.Test]
                             |  Package ByteString [IdemShell.Test]

label                       ::  Task -> ByteString
label (Command c _)          =  IdemShell.label c
label (Package b _)          =  "task:" `ByteString.append` b


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
dPath                       ::  Forest Task -> Forest ([Natural], Task)
dPath                        =  fmap (fmap (first reverse)) . dPath'' []

dPath'' path forest =
 [ Node (path', x) (dPath'' path' forest') | Node x forest' <- forest
                                           | n <- [0..], let path' = n:path ]


trees :: Forest ([Natural], Task) -> [Tree ([Natural], Task)]
trees forest = concat (forest : fmap (trees . subForest) forest)


group :: [Tree ([Natural], Task)] -> [[Tree ([Natural], Task)]]
group                        =  groupBy comparingLabel
 where
  labelNode                  =  label . snd . rootLabel
  comparingLabel a b         =  labelNode a == labelNode b


{-| A backend supports these operations.
 -}
data Op
  = Enter ByteString                  -- ^ Notify that task is starting.
  | Leave ByteString                  -- ^ Notify that task is complete.
  | Check ByteString [IdemShell.Test] -- ^ Perform check for labelled task.
  | Perform IdemShell.Command         -- ^ Execute command if necessary. 


data Error
data Warn
