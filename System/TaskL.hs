
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
import System.TaskL.Schedule

