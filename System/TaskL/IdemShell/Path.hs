
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.Path where

import Control.Monad.Error
import Data.String

import qualified Data.Text as Text
import Data.Text (Text)

import Data.Text.EncDec


newtype Path                 =  Path Text
deriving instance Eq Path
deriving instance Ord Path
deriving instance Show Path
instance IsString Path where
  fromString s               =  case dec (Text.pack s) of
                                  Right nick  ->  nick
                                  Left msg    ->  error (Text.unpack msg)
instance EncDec Path where
  enc (Path t)               =  t
  dec t                      =  case check t of
                                  Ok          ->  return (Path t)
                                  err         ->  throwError (message err)


{-| Check if text is an acceptable UNIX path.
 -}
check t
  | Text.null t              =  Empty
  | Text.any (== '\0') t     =  NoNull
  | otherwise                =  Ok

{-| Characterizes success or failure of path check.
 -}
data Check                   =  Ok | Empty | NoNull

message                     ::  Check -> Text
message Ok                   =  "Okay."
message Empty                =  "Empty paths are not allowed."
message NoNull               =  "UNIX paths may no contain null."

