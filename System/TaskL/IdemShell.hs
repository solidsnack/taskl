
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


data Test                    =  CHKOWN Path Ownership
                             |  CHKMOD Path Mode
                             |  DASHe Path
                             |  DASH_ NodeType Path
                             |  DIFF Path Path
                             |  CHKLN_S Path Path
                             |  Not Test


data NodeType                =  File
                             |  Directory
                             |  Symlink


data Ownership               =  Both User Group
                             |  OnlyUser User
                             |  OnlyGroup Group


data Mode

data UserAttrs

data GroupAttrs



essentialTests              ::  Command -> [Test]
essentialTests CHOWN p o     =  [CHKOWN p o]
essentialTests CHMOD p m     =  [CHKMOD p o]
essentialTests RM p          =  [Not (DASHe p)]
essentialTests CP p p'       =  [Not (DIFF p p')]
essentialTests TOUCH p       =  [DASH_ File p]
essentialTests MKDIR p       =  [DASH_ Directory p]
essentialTests LN_S p p'     =  [DASH_ Symlink p, CHKLN_S p p']

