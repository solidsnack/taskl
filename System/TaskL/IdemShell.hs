
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
 #-}


module System.TaskL.IdemShell where

import Control.Monad.Identity

import System.TaskL.IdemShell.PasswdDB
import System.TaskL.IdemShell.Path

data CMD where
  CHOWN                     ::  Ownership -> CMD
  CHMOD                     ::  Permissions -> CMD
  MKNOD                     ::  Layout Identity -> CMD
  RM                        ::  (Monad m) => Layout m -> CMD
  USERADD                   ::  User -> CMD
  USERDEL                   ::  User -> CMD
  GROUPADD                  ::  Group -> CMD
  GROUPDEL                  ::  Group -> CMD


data Layout m where
  File                      ::  Path -> Layout Identity
  Directory                 ::  Path -> Layout Identity
  Symlink                   ::  (Monad m) => Path -> m Path -> Layout m


data Ownership

data Permissions

