
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
  #-}


module System.TaskL.IdemShell where

import Control.Applicative
import Control.Monad.Identity

import Data.ByteString

import System.TaskL.IdemShell.PasswdDB
import System.TaskL.IdemShell.Path
import Data.ByteString.EncDec


data Command                 =  CHOWN Path Ownership
                             |  CHMOD Path Mode
                             |  RM Path
                             |  CP Path Path
                             |  LN_S Path Path
                             |  TOUCH Path
                             |  MKDIR Path
                             |  USERADD UNick UserAttrs
                             |  USERDEL UNick
                             |  GROUPADD GNick GroupAttrs
                             |  GROUPDEL GNick
                             |  GPASSWDa GNick UNick
                             |  GPASSWDd GNick UNick



data Test                    =  CHKOWN Path Ownership
                             |  CHKMOD Path Mode
                             |  DASHe Path
                             |  DASH_ NodeType Path
                             |  DIFFq Path Path
                             |  CHKLN_S Path Path
                             |  GETENT GettableEnt
                             |  Not Test


data NodeType                =  File
                             |  Directory
                             |  Symlink

data GettableEnt             =  User UNick
                             |  Group GNick

data Ownership               =  Both User Group
                             |  OnlyUser User
                             |  OnlyGroup Group


data Mode

data UserAttrs

data GroupAttrs


essentialTests              ::  Command -> [Test]
essentialTests thing         =  case thing of
   CHOWN p o                ->  [CHKOWN p o]
   CHMOD p m                ->  [CHKMOD p m]
   RM p                     ->  [Not (DASHe p)]
   CP p p'                  ->  [Not (DIFFq p p')]
   LN_S p p'                ->  [DASH_ Symlink p, CHKLN_S p p']
   TOUCH p                  ->  [DASH_ File p]
   MKDIR p                  ->  [DASH_ Directory p]
   USERADD nick _           ->  [(GETENT . User) nick]
   USERDEL nick             ->  [(Not . GETENT . User) nick]
   GROUPADD nick _          ->  [(GETENT . Group) nick]
   GROUPDEL nick            ->  [(Not . GETENT . Group) nick]
   GPASSWDa gNick uNick     ->  []
   GPASSWDd gNick uNick     ->  []


label                       ::  Command -> ByteString
label thing                  =  case thing of
   CHOWN p _                ->  "fs/o:" `append` enc p
   CHMOD p _                ->  "fs/m:" `append` enc p
   RM p                     ->  "fs/s:" `append` enc p
   CP _ p                   ->  "fs/s:" `append` enc p
   LN_S _ p                 ->  "fs/s:" `append` enc p
   TOUCH p                  ->  "fs/s:" `append` enc p
   MKDIR p                  ->  "fs/s:" `append` enc p
   USERADD nick _           ->  "pw/u:" `append` enc nick
   USERDEL nick             ->  "pw/u:" `append` enc nick
   GROUPADD nick _          ->  "pw/g:" `append` enc nick
   GROUPDEL nick            ->  "pw/g:" `append` enc nick
   GPASSWDa nick _          ->  "pw/g:" `append` enc nick
   GPASSWDd nick _          ->  "pw/g:" `append` enc nick

