
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


data User                    =  Username UNick | UserID UID

data Group                   =  Groupname GNick | GroupID GID


newtype UNick                =  UNick Nick

newtype UID                  =  UID ID

newtype GNick                =  GNick Nick

newtype GID                  =  GID ID


newtype ID                   =  ID Int32
fromIntegralMaybe i
  | i < 0 || i > max         =  Nothing
  | otherwise                =  Just (ID (fromIntegral i))
 where
  max                        =  fromIntegral (maxBound :: Int32)


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

deriving instance Eq UNick
deriving instance Ord UNick
deriving instance Show UNick
deriving instance EncDec UNick

deriving instance Eq UID
deriving instance Ord UID
deriving instance Show UID
deriving instance EncDec UID

deriving instance Eq GNick
deriving instance Ord GNick
deriving instance Show GNick
deriving instance EncDec GNick

deriving instance Eq GID
deriving instance Ord GID
deriving instance Show GID
deriving instance EncDec GID

deriving instance Eq ID
deriving instance Ord ID
deriving instance Show ID
instance EncDec ID where
  enc (ID n)                 =  Text.pack (show n)
  dec t                      =  do
    (i :: Int32)            <-  dec t
    maybe (throwError "ID not in range.") return (fromIntegralMaybe i)

