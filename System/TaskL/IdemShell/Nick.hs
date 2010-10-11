
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.Nick
  ( Nick()
  , check
  , message
  , Check
  ) where

import Control.Monad.Error
import Data.String

import qualified Data.Text as Text
import Data.Text (Text)

import Data.Text.EncDec


{-| Validated UNIX nick. Neither usernames nor groupnames may begin with @+@,
    @-@ or the at-sign, due to interference with NIS; they may not contain the
    @:@ since that is the separator in the @passwd@ DB files. Nicks must be
    non-empty.
 -}
newtype Nick                 =  Nick Text
deriving instance Eq Nick
deriving instance Ord Nick
deriving instance Show Nick
instance IsString Nick where
  fromString s               =  case dec (Text.pack s) of
                                  Right nick  ->  nick
                                  Left msg    ->  error (Text.unpack msg)
instance EncDec Nick where
  enc (Nick t)               =  t
  dec t                      =  case check t of
                                  Ok          ->  return (Nick t)
                                  err         ->  throwError (message err)


{-| Check if text is an acceptable UNIX username.
 -}
check                       ::  Text -> Check
check t
  | Text.null t              =  Empty
  | "@" `Text.isPrefixOf` t  =  BadLeadingNIS '@'
  | "+" `Text.isPrefixOf` t  =  BadLeadingNIS '+'
  | "-" `Text.isPrefixOf` t  =  BadLeadingNIS '-'
  | (== ':') `Text.any` t    =  Bad ':'
  | otherwise                =  Ok

{-| Characterizes success or failure of username check. 
 -}
data Check                   =  Ok | Empty | BadLeadingNIS Char | Bad Char
deriving instance Eq Check
deriving instance Show Check

message                     ::  Check -> Text
message Ok                   =  "Okay."
message Empty                =  "Empty usernames are not allowed."
message (BadLeadingNIS c)
  = "Leading `" `Text.snoc` c
                `Text.append` "' interferes with NIS naming conventions."
message (Bad c)
  = "Char `" `Text.snoc` c `Text.append` "' is not allowed in usernames."

