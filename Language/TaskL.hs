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

module Language.TaskL
  ( bash
  , Language.TaskL.Task.Task(..)
  , Language.TaskL.IdemShell.Command(..)
  , Language.TaskL.IdemShell.Test(..)
  , Language.TaskL.Schedule.schedule
  , Language.TaskL.Schedule.Error(..)
  , Language.TaskL.Schedule.Warn(..)
  ) where

import Data.Tree
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import Data.ByteString.Lazy.Char8 (ByteString, append)
import qualified Data.ByteString.Lazy.Char8

import Data.Digest.Pure.SHA

import Language.TaskL.Task
import Language.TaskL.IdemShell
import Language.TaskL.Schedule (schedule, Error(..), Warn(..))
import Language.TaskL.Bash (builder, colPPState)
import Language.TaskL.Codegen (stateArrays, code, codeForOp)
import Language.TaskL.BashTemplate
  (preamble, runtime, postamble, generatedCodeHeading, splitTemplate)


bash :: [Tree Task] -> Either [Error] (ByteString, [Warn])
bash tasks                   =  case errors of [ ] -> Right (bytes, warns)
                                               _:_ -> Left errors
 where
  (ops, errors, warns)       =  schedule tasks
  topLevel                   =  map rootLabel tasks
  (arrays, install)          =  code topLevel ops
  text ppS t                 =  Builder.toLazyByteString (builder ppS t)
  bytes                      =  Data.ByteString.Lazy.Char8.intercalate "\n"
                                  [ toLazy preamble
                                  , toLazy runtime
                                  , toLazy generatedCodeHeading
                                  , ""
                                  , "taskl_script_key=" `append` digest
                                  , ""
                                  , generatedCode
                                  , ""
                                  , ""
                                  , ""
                                  , ""
                                  , toLazy postamble ]
  digest = (Data.ByteString.Lazy.Char8.pack . showDigest . sha1) generatedCode
  generatedCode              =  Data.ByteString.Lazy.Char8.intercalate "\n"
    [ "# State arrays."
    , text (colPPState 0) arrays
    , ""
    , "# Installation routine."
    , "function taskl_apply {"
    , "  local T=\"$1\""
    , text (colPPState 2) install
    , "}" ]
  toLazy                     =  Data.ByteString.Lazy.Char8.fromChunks . (:[])

