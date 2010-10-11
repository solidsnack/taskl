
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.PasswdDB where

import Control.Monad.Error
import Data.String
import Data.Int

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Text.EncDec
import System.TaskL.IdemShell.Nick
import System.TaskL.IdemShell.ID
import System.TaskL.IdemShell.Path


data User                    =  Username UNick | UserID UID

data Group                   =  Groupname GNick | GroupID GID


data Password                =  Hashed Text | Literal Text
 deriving (Eq, Ord, Show)


data UserEntry               =  UserEntry (Maybe UNick) --  Nick.
                                          (Maybe Password) --  Password.
                                          (Maybe UID) --  Numeric ID.
                                          (Maybe Group) --  Primary group.
                                          (Maybe Text) --  Comment.
                                          --(Maybe Path) --  Home.
                                          --(Maybe Path) --  Shell.



deriving instance Eq User
deriving instance Ord User
deriving instance Show User
instance EncDec User where
  enc (Username nick)        =  enc nick
  enc (UserID id)            =  '+' `Text.cons` enc id
  dec t                      =  case Text.stripPrefix "+" t of
                                  Just t'     ->  UserID `fmap` dec t'
                                  _           ->  Username `fmap` dec t

deriving instance Eq Group
deriving instance Ord Group
deriving instance Show Group
instance EncDec Group where
  enc (Groupname nick)       =  enc nick
  enc (GroupID id)           =  '+' `Text.cons` enc id
  dec t                      =  case Text.stripPrefix "+" t of
                                  Just t'     ->  GroupID `fmap` dec t'
                                  _           ->  Groupname `fmap` dec t

