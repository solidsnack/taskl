
{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
  #-}

module System.TaskL.IdemShell.ID
  ( ID()
  , UID
  , GID
  , fromIntegralMaybe
  ) where

import Control.Monad.Error
import Data.String
import Data.Int

import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

import Data.ByteString.EncDec


{-| IDs that are specifically User IDs. 
 -}
newtype UID                  =  UID ID

{-| IDs that are specifically Group IDs. 
 -}
newtype GID                  =  GID ID


{-| Validated UNIX ID. On most modern UNIXes, IDs must be in the range
    [0, 2^31 - 1], inclusive.
 -}
newtype ID                   =  ID Int32

{-| Create an ID from an integral type if it is in the correct range. 
 -}
fromIntegralMaybe           ::  (Integral i) => i -> Maybe ID
fromIntegralMaybe i
  | i < 0 || i > polyMax     =  Nothing
  | otherwise                =  Just (ID (fromIntegral i))


deriving instance Eq UID
deriving instance Ord UID
deriving instance Show UID
deriving instance Bounded UID
deriving instance Num UID
deriving instance EncDec UID

deriving instance Eq GID
deriving instance Ord GID
deriving instance Show GID
deriving instance Bounded GID
deriving instance Num GID
deriving instance EncDec GID

deriving instance Eq ID
deriving instance Ord ID
deriving instance Show ID
instance Bounded ID where
  minBound                   =  0
  maxBound                   =  polyMax
instance Num ID where
  (+)                        =  undefined -- Definitely fail if called.
  (*)                        =  undefined -- Definitely fail if called.
  (-)                        =  undefined -- Definitely fail if called.
  negate                     =  undefined -- Definitely fail if called.
  abs                        =  undefined -- Definitely fail if called.
  signum                     =  undefined -- Definitely fail if called.
  fromInteger i              =  case fromIntegralMaybe i of
                                  Just id     ->  id
                                  Nothing     ->  error (ByteString.unpack msg)
instance EncDec ID where
  enc (ID n)                 =  ByteString.pack (show n)
  dec b                      =  do
    (i :: Int32)            <-  dec b
    maybe (throwError msg) return (fromIntegralMaybe i)


polyMax                      =  fromIntegral (maxBound :: Int32)

msg = ByteString.concat [ "UIDs and GIDs must be in the range 0.."
                        , enc (maxBound :: ID)
                        , ", inclusive." ]

