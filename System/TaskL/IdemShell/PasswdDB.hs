
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.PasswdDB
  ( module System.TaskL.IdemShell.Nick
  , module System.TaskL.IdemShell.ID
  , User
  , Group
  , Password
  ) where

import Prelude hiding (tail)
import Control.Monad.Error
import Data.String
import Data.Int

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Char8

import Data.ByteString.EncDec
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
  enc (UserID id)            =  '+' `cons` enc id
  dec                        =  chownStyle UserID Username

deriving instance Eq Group
deriving instance Ord Group
deriving instance Show Group
instance EncDec Group where
  enc (Groupname nick)       =  enc nick
  enc (GroupID id)           =  '+' `cons` enc id
  dec                        =  chownStyle GroupID Groupname

chownStyle consID consNick b
  | "+" `isPrefixOf` b       =  consID `fmap` dec (tail b)
  | otherwise                =  consNick `fmap` dec b

