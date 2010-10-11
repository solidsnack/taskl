
{-# LANGUAGE StandaloneDeriving
           , UndecidableInstances
           , OverloadedStrings
  #-}

module Data.Text.EncDec where

import Data.String
import Control.Monad.Error

import Data.Text (Text)
import qualified Data.Text as Text


class EncDec t where
  enc                       ::  t -> Text
  dec                       ::  Text -> Either Text t
instance EncDec Text where
  enc                        =  id
  dec                        =  return
instance (Read n, Num n) => EncDec n where
  enc                        =  Text.pack . show
  dec t                      =  case (reads . Text.unpack) t of
                                  [     ]     ->  throwError "Not a number."
                                  (n,_):_     ->  return n

instance Error Text where
  strMsg                     =  Text.pack

