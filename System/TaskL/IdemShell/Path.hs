
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.Path
  ( Path()
  , (</>)
  , message
  , check
  , Check
  ) where

import Control.Monad.Error
import Data.String

import qualified Data.Text as Text
import Data.Text (Text)

import Data.Text.EncDec


{-| Validated UNIX path. The text of the path does not contain ASCII NULL. All
    paths are of non-zero length and begin with @./@ or @/@.
 -}
newtype Path                 =  Path Text

{-| Join two UNIX paths. An addition @/@ is introduced between the paths if 
    necessary. Multiple @/@ are not collapsed.
 -}
(</>)                       ::  Path -> Path -> Path
Path a </> Path b            =  (Path . Text.concat) [a, shim, b']
 where
  b'                         =  snd (Text.break "/" b)
  shim | Text.last a == '/'  =  ""
       | otherwise           =  "/"


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
  | (not . under) prefixes   =  NoPrefix
  | otherwise                =  Ok
 where
  prefixes                   =  ["/", "./", "../"]
  under                      =  or . fmap (`Text.isPrefixOf` t)

{-| Characterizes success or failure of path check.
 -}
data Check                   =  Ok | Empty | NoNull | NoPrefix
deriving instance Eq Check
deriving instance Show Check

message                     ::  Check -> Text
message Ok                   =  "Okay."
message Empty                =  "Empty paths are not allowed."
message NoNull               =  "UNIX paths may not contain null."
message NoPrefix             =  "Path should begin with `/', `./' or `../'."

