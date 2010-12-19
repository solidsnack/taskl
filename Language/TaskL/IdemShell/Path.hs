
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , PostfixOperators
  #-}

module System.TaskL.IdemShell.Path
  ( Path()
  , (</>)
  , (</?)
  , (-/)
  , (/-)
  , (/?)
  , message
  , check
  , Check
  ) where

import Prelude hiding ( init, null, concat, break,
                        drop, head, length, last, any )
import Control.Monad.Error
import Data.String

import Data.ByteString.Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import System.TaskL.EncDec


{-| Validated UNIX path. The text of the path does not contain ASCII NULL. All
    paths are of non-zero length and begin with @./@ or @/@.
 -}
newtype Path                 =  Path ByteString

{-| Join two UNIX paths. An additional @/@ is introduced between the paths.
 -}
(</>)                       ::  Path -> Path -> Path
Path a </> Path b            =  Path (a `snoc` '/' `append` b')
 where
  b'                         =  snd (break (== '/') b)

{-| Is the second path syntactically below or equal to the first one?
 -}
(</?)                       ::  Path -> Path -> Bool
Path a </? Path b            =  a `isPrefixOf` b
                            &&  head (drop (length a) b) == '/'

{-| Obtain dirname.
 -}
(-/)                        ::  Path -> Path
(-/) (Path a)                =  case breakEnd (== '/') a of
                                  ("/", _)    ->  Path "/"
                                  (d,   _)    ->  Path (init d)

{-| Obtain basename.
 -}
(/-)                        ::  Path -> Path
(/-) (Path a)                =  (Path . snd . breakEnd (== '/')) a

{-| Is the path absolute?
 -}
(/?)                        ::  Path -> Bool
(/?) (Path a)                =  '/' == head a


deriving instance Eq Path
deriving instance Ord Path
deriving instance Show Path
instance IsString Path where
  fromString s               =  case dec (Text.encodeUtf8 $ Text.pack s) of
    Right nick              ->  nick
    Left msg                ->  error (Text.unpack $ Text.decodeUtf8 msg)
instance EncDec Path where
  enc (Path b)               =  b
  dec b                      =  case check b' of
                                  Ok          ->  return (Path b')
                                  err         ->  throwError (message err)
                                 where
                                  b'           =  norm b

{-| Normalize a path, removing trailing slashes and collapsing slash runs.
 -}
norm                        ::  ByteString -> ByteString
norm "/"                     =  "/"
norm "./"                    =  "."
norm "../"                   =  ".."
norm b                       =  (snd . foldl' (<<) (False, "")) b
 where
  (_    , bytes) << '/'      =  (True , bytes)
  (True , bytes) << c        =  (False, bytes `snoc` '/' `snoc` c)
  (False, bytes) << c        =  (False, bytes `snoc` c)


{-| Check if text is an acceptable UNIX path.
 -}
check                       ::  ByteString -> Check
check b
  | null b                   =  Empty
  | any (== '\0') b          =  NoNull
  | b == "/"                 =  Ok
  | last b == '/'            =  NoFinalSlash
  | "//" `isInfixOf` b       =  NoSlashRuns
  | (not . under) prefixes   =  MustHavePrefix
  | otherwise                =  Ok
 where
  prefixes                   =  ["/", "./", "../"]
  under                      =  or . fmap (`isPrefixOf` b)

{-| Characterizes success or failure of path check.
 -}
data Check = Ok | Empty | NoNull | NoFinalSlash | NoSlashRuns | MustHavePrefix
deriving instance Eq Check
deriving instance Show Check

message                     ::  Check -> ByteString
message Ok                   =  "Okay."
message Empty                =  "Empty paths are not allowed."
message NoNull               =  "UNIX paths may not contain null."
message NoFinalSlash         =  "Only the root path may have a final slash."
message NoSlashRuns          =  "Don't create paths with runs of slashes."
message MustHavePrefix       =  "Path should begin with `/', `./' or `../'."

