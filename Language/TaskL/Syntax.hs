{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , TypeFamilies
           , GADTs
  #-}
module Language.TaskL.Syntax where

import Data.Monoid
import Prelude hiding (lines)
import Data.ByteString.Char8
import Data.Map (Map)
import Data.Set (Set)

import Language.TaskL.Name
--import Language.TaskL.StringL
data StringL                 =  StringL


data Namespace               =  Namespace (Map Name TaskBody)

data TaskBody                =  TaskBody Test Task (Set Call)

data Call                    =  Call Name [StringL] (Set Call)

data Task                    =  forall t . Task [Code t]

data Test                    =  forall t . Test [Code t]

data Code t where
  Bash                      ::  ByteString -> Code ByteString
  Exec                      ::  StringL -> [StringL] -> Code [StringL]
  Hask                      ::  IO Bool -> Code (IO Bool)
instance Show (Code t) where
  show code                  =  case code of
    Bash b                  ->  "Bash " ++ fill "     " b
    Exec _ _                ->  "Exec _ [...]"
    Hask _                  ->  "Hask <IO Bool>"


fill s                       =  unpack . intercalate s . lines

