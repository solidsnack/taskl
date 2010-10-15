
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
 #-}

module System.TaskL where

import Data.String
import Control.Applicative
import Control.Monad.Identity

import Data.Text

import System.TaskL.IdemShell


data Task label              =  Atomic label [Test] Command
                             |  Aggregate label [Test] [Task label]



data Tasks where
  Tasks         ::  forall t. (SystemObject t) => [Task t] -> Tasks


class SystemObject t where
  type Label t              ::  *
  type Spec t               ::  (* -> *) -> *
  check         ::  (Monad m) => Label t -> Spec t m -> ShellScript
  rm                        ::  Label t -> ShellScript
  make                      ::  Label t -> Spec t Identity -> ShellScript


data FilesystemNode
instance SystemObject FilesystemNode where
  type Label FilesystemNode  =  Path
  type Spec FilesystemNode   =  NodeAttrs

data UserEntry
instance SystemObject UserEntry where
  type Label UserEntry       =  User
  type Spec UserEntry        =  UserAttrs

data GroupEntry
instance SystemObject GroupEntry where
  type Label GroupEntry      =  Group
  type Spec GroupEntry       =  GroupAttrs




data Path
data Own
data Mode
data NodeAttrs              ::  (* -> *) -> *

data User
data UserAttrs              ::  (* -> *) -> *
data Group
data GroupAttrs             ::  (* -> *) -> *

newtype ShellScript          =  ShellScript Text
deriving instance IsString ShellScript

