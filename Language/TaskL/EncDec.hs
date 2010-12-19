
{-# LANGUAGE StandaloneDeriving
           , UndecidableInstances
           , OverlappingInstances
           , FlexibleInstances
           , OverloadedStrings
  #-}

module Language.TaskL.EncDec where

import Data.String
import Control.Monad.Error

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString


class EncDec t where
  enc                       ::  t -> ByteString
  dec                       ::  ByteString -> Either ByteString t
instance EncDec ByteString where
  enc                        =  id
  dec                        =  return
instance (Read n, Num n) => EncDec n where
  enc                        =  ByteString.pack . show
  dec t                      =  case (reads . ByteString.unpack) t of
                                  [     ]     ->  throwError "Not a number."
                                  (n,_):_     ->  return n

instance Error ByteString where
  strMsg                     =  ByteString.pack

