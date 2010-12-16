{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , OverlappingInstances
           , GeneralizedNewtypeDeriving
  #-}

module System.TaskL.IdemShell.Nick
  ( Nick()
  , UNick
  , GNick
  , check
  , message
  , Check
  ) where

import Prelude hiding (any, null)
import Control.Monad.Error
import Data.String

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.ByteString (ByteString)
import Data.ByteString.Char8

import Data.ByteString.EncDec


newtype UNick                =  UNick Nick

newtype GNick                =  GNick Nick

{-| Validated UNIX nick. Neither usernames nor groupnames may begin with @+@,
    @-@ or the at-sign, due to interference with NIS; they may not contain the
    @:@ since that is the separator in the @passwd@ DB files. Nicks must be
    non-empty.
 -}
newtype Nick                 =  Nick ByteString


deriving instance Eq UNick
deriving instance Ord UNick
deriving instance Show UNick
deriving instance EncDec UNick
deriving instance IsString UNick

deriving instance Eq GNick
deriving instance Ord GNick
deriving instance Show GNick
deriving instance EncDec GNick
deriving instance IsString GNick

deriving instance Eq Nick
deriving instance Ord Nick
deriving instance Show Nick
instance IsString Nick where
  fromString s               =  case dec (Text.encodeUtf8 $ Text.pack s) of
    Right nick              ->  nick
    Left msg                ->  error (Text.unpack $ Text.decodeUtf8 msg)
instance EncDec Nick where
  enc (Nick b)               =  b
  dec b                      =  case check b of
                                  Ok          ->  return (Nick b)
                                  err         ->  throwError (message err)


{-| Check if text is an acceptable UNIX username.
 -}
check                       ::  ByteString -> Check
check b
  | null b                   =  Empty
  | "@" `isPrefixOf` b       =  BadLeadingNIS '@'
  | "+" `isPrefixOf` b       =  BadLeadingNIS '+'
  | "-" `isPrefixOf` b       =  BadLeadingNIS '-'
  | (== '\0') `any` b        =  NullsAreBad
  | (== ':') `any` b         =  ColonsAreBad
  | (== ',') `any` b         =  CommasConfuseSystem
  | (== ' ') `any` b         =  RejectedByTools ' '
  | (== '\v') `any` b        =  RejectedByTools '\v'
  | (== '\t') `any` b        =  RejectedByTools '\t'
  | (== '\n') `any` b        =  RejectedByTools '\n'
  | (== '\r') `any` b        =  RejectedByTools '\r'
  | (== '\f') `any` b        =  RejectedByTools '\f'
  | otherwise                =  Ok
--  All bytes except ':' and the ones mark as rejected above are accepted by
--  `useradd'. This is probably bad.
--  Characters that are allowed but that should not be: \a \b
--  Perhaps a patch to `useradd' is in order...


{-| Characterizes success or failure of username check. 
 -}
data Check                   =  Ok
                             |  Empty
                             |  BadLeadingNIS Char
                             |  NullsAreBad
                             |  ColonsAreBad
                             |  CommasConfuseSystem
                             |  RejectedByTools Char
deriving instance Eq Check
deriving instance Show Check

message                     ::  Check -> ByteString
message Ok                   =  "Okay."
message Empty                =  "Empty usernames are not allowed."
message (BadLeadingNIS c)
  = "Leading `" `snoc` c `append` "' interferes with NIS naming conventions."
message NullsAreBad
  = "Nulls will doubtless confuse all C programs managing the passwd DB."
message ColonsAreBad
  = "Colons are used to separate fields in the passwd DB."
message CommasConfuseSystem
  = "Commas are used to separate group members in `/etc/groups'."
message (RejectedByTools c)
  = "Char `" `snoc` c `append` "' is rejected by utilities."

