
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
  #-}


module System.TaskL.IdemShell where

import qualified Data.List as List
import Control.Applicative

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
                             |  GPASSWDa GNick [UNick]
                             |  GPASSWDd GNick [UNick]
deriving instance Eq Command


data Test                    =  LSo Path Ownership
                             |  LSm Path Mode
                             |  DASHe Path
                             |  DASH_ NodeType Path
                             |  DIFFq Path Path
                             |  CHKLN_S Path Path
                             |  GETENT GettableEnt
                             |  GROUPS UNick GNick
                             |  Not Test


data NodeType                =  File
                             |  Directory
                             |  Symlink

data GettableEnt             =  User UNick
                             |  Group GNick

data Ownership               =  Both User Group
                             |  OnlyUser User
                             |  OnlyGroup Group
deriving instance Eq Ownership
deriving instance Show Ownership


data Mode                    =  Mode TriState -- ^ User read bit.
                                     TriState -- ^ User write bit.
                                     TriState -- ^ User execute bit.
                                     TriState -- ^ Set UID bit.
                                     TriState -- ^ Group read bit.
                                     TriState -- ^ Group write bit.
                                     TriState -- ^ Group execute bit.
                                     TriState -- ^ Set GID bit.
                                     TriState -- ^ Other read bit.
                                     TriState -- ^ Other write bit.
                                     TriState -- ^ Other execute bit.
                                     TriState -- ^ Sticky bit.
deriving instance Eq Mode
deriving instance Show Mode

data TriState                =  On | Indifferent | Off
deriving instance Eq TriState
deriving instance Show TriState


data UserAttrs               =  UserAttrs
deriving instance Eq UserAttrs
deriving instance Show UserAttrs

data GroupAttrs              =  GroupAttrs
deriving instance Eq GroupAttrs
deriving instance Show GroupAttrs


{-| Test commands that will assuredly exit 0 after the command is run.
 -}
essentialTests              ::  Command -> [Test]
essentialTests thing         =  case thing of
   CHOWN p o                ->  [LSo p o]
   CHMOD p m                ->  [LSm p m]
   RM p                     ->  [Not (DASHe p)]
   CP p' p                  ->  [Not (DIFFq p' p)]
   LN_S p' p                ->  [DASH_ Symlink p, CHKLN_S p' p]
   TOUCH p                  ->  [DASH_ File p]
   MKDIR p                  ->  [DASH_ Directory p]
   USERADD nick _           ->  [(GETENT . User) nick]
   USERDEL nick             ->  [(Not . GETENT . User) nick]
   GROUPADD nick _          ->  [(GETENT . Group) nick]
   GROUPDEL nick            ->  [(Not . GETENT . Group) nick]
   GPASSWDa gNick uNicks    ->  flip GROUPS gNick <$> uNicks
   GPASSWDd gNick uNicks    ->  Not . flip GROUPS gNick <$> uNicks


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


data Combination             =  Contradictory Command Command
                             |  Combined Command
                             |  Separate Command Command
deriving instance Eq Combination


merge                       ::  Command -> Command -> Combination
merge a b                    =  if a == b then Combined a
                                          else merge'' a b

--  Use GADTs for this later.
merge''                     ::  Command -> Command -> Combination
merge'' a@(CHOWN p0 _) b     =  case b of
  CHOWN p1 _                ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge'' a@(CHMOD p0 _) b     =  case b of
  CHMOD p1 _                ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  LN_S _ p1                 ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge'' a@(RM p0) b          =  case b of
  CHOWN p1 _                ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  CHMOD p1 _                ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  CP _ p1                   ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  LN_S p' p1                ->  if p0 == p1 || p0 == p' then Contradictory a b
                                                        else Separate a b
  TOUCH p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  MKDIR p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge'' a@(CP _ p0) b        =  case b of
  CP _ p1                   ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge'' a@(LN_S _ p0) b      =  case b of
  LN_S _ p1                 ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge'' a@(TOUCH p0) b       =  case b of
  TOUCH p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  MKDIR p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge'' a@(MKDIR p0) b       =  case b of
  TOUCH p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  MKDIR p1                  ->  if p0 == p1 then Contradictory a b
                                            else Separate a b
  RM _                      ->  merge b a
  _                         ->  Separate a b
merge'' a@(USERADD u0 _) b   =  case b of
  USERADD u1 _              ->  if u0 == u1 then Contradictory a b
                                            else Separate a b
  USERDEL u1                ->  if u0 == u1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge'' a@(USERDEL u0) b     =  case b of
  USERADD u1 _              ->  if u0 == u1 then Contradictory a b
                                         else Separate a b
  USERDEL u1                ->  if u0 == u1 then Contradictory a b
                                         else Separate a b
  _                         ->  Separate a b
merge'' a@(GROUPADD g0 _) b  =  case b of
  GROUPADD g1 _             ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GROUPDEL g1               ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge'' a@(GROUPDEL g0) b    =  case b of
  GROUPADD g1 _             ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GROUPDEL g1               ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  GPASSWDa g1 _             ->  if g0 == g1 then Contradictory a b
                                            else  Separate a b
  GPASSWDd g1 _             ->  if g0 == g1 then Contradictory a b
                                            else Separate a b
  _                         ->  Separate a b
merge'' a@(GPASSWDa g0 u0) b =  case b of
  GPASSWDa g1 u1            ->  if g0 == g1
                                  then  Combined (GPASSWDa g0 all)
                                  else  Separate a b
                                 where
                                  all = List.union u0 u1
  GPASSWDd g1 u1            ->  if g0 == g1 && (not . List.null) overlap
                                  then  Contradictory a b
                                  else  Separate a b
                                 where
                                  overlap = List.intersect u0 u1
  _                         ->  Separate a b
merge'' a@(GPASSWDd g0 u0) b =  case b of
  GPASSWDa g1 u1            ->  if g0 == g1 && (not . List.null) overlap
                                  then  Contradictory a b
                                  else  Separate a b
                                 where
                                  overlap = List.intersect u0 u1
  GPASSWDd g1 u1            ->  if g0 == g1
                                  then  Combined (GPASSWDd g0 all)
                                  else  Separate a b
                                 where
                                  all = List.union u0 u1
  _                         ->  Separate a b

