
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

import Prelude hiding (break, concat, null, last, any, init)
import Control.Monad.Error
import Data.String

import Data.ByteString.Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)

import Data.ByteString.EncDec


{-| Validated UNIX path. The text of the path does not contain ASCII NULL. All
    paths are of non-zero length and begin with @./@ or @/@.
 -}
newtype Path                 =  Path ByteString

{-| Join two UNIX paths. An additional @/@ is introduced between the paths if
    necessary. Multiple @/@ are not collapsed.
 -}
(</>)                       ::  Path -> Path -> Path
Path a </> Path b            =  (Path . concat) [a, shim, b']
 where
  b'                         =  snd (break (== '/') b)
  shim | last a == '/'       =  ""
       | otherwise           =  "/"

{-| Is the second path syntactically below or equal to the first one?
 -}
(</?)                       ::  Path -> Path -> Bool
Path a </? Path b            =  a' `isPrefixOf` b
 where
  a' | last a == '/'         =  init a
     | otherwise             =  a


deriving instance Eq Path
deriving instance Ord Path
deriving instance Show Path
instance IsString Path where
  fromString s               =  case dec (Text.encodeUtf8 $ Text.pack s) of
    Right nick              ->  nick
    Left msg                ->  error (Text.unpack $ Text.decodeUtf8 msg)
instance EncDec Path where
  enc (Path b)               =  b
  dec b                      =  case check b of
                                  Ok          ->  return (Path b)
                                  err         ->  throwError (message err)


{-| Check if text is an acceptable UNIX path.
 -}
check b
  | null b                   =  Empty
  | any (== '\0') b          =  NoNull
  | (not . under) prefixes   =  NoPrefix
  | otherwise                =  Ok
 where
  prefixes                   =  ["/", "./", "../"]
  under                      =  or . fmap (`isPrefixOf` b)

{-| Characterizes success or failure of path check.
 -}
data Check                   =  Ok | Empty | NoNull | NoPrefix
deriving instance Eq Check
deriving instance Show Check

message                     ::  Check -> ByteString
message Ok                   =  "Okay."
message Empty                =  "Empty paths are not allowed."
message NoNull               =  "UNIX paths may not contain null."
message NoPrefix             =  "Path should begin with `/', `./' or `../'."

