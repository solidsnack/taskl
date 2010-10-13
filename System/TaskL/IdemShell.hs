
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
 #-}


module System.TaskL.IdemShell where

import Control.Monad.Identity

import System.TaskL.IdemShell.PasswdDB
import System.TaskL.IdemShell.Path


data Command                 =  CHOWN Path Ownership
                             |  CHMOD Path Mode
                             |  RM Path
                             |  CP Path Path
                             |  LN_S Path Path
                             |  TOUCH Path
                             |  MKDIR Path
                             |  USERADD User UserAttrs
                             |  USERDEL User
                             |  GROUPADD Group GroupAttrs
                             |  GROUPDEL Group


data Check                   =  CHKMOD Path Mode
                             |  CHKOWN Path Ownership
                             |  DASHe Path
                             |  DASH_ Path NodeType


data NodeType                =  File
                             |  Directory
                             |  Symlink


data Ownership               =  Both User Group
                             |  OnlyUser User
                             |  OnlyGroup Group


data Mode

data UserAttrs

data GroupAttrs


