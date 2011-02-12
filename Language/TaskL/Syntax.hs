
module Language.TaskL.Syntax where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)


data Module                  =  Module (Map Name TaskBody)

data TaskBody                =  TaskBody (Maybe Test) (Maybe Task) (Set Call)

data Call                    =  Call Name [StringL] TaskBody

newtype Name                 =  Name ByteString

newtype StringL              =  StringL [StringLToken]

data StringLToken            =  Literal ByteString
                             |  Substitution ByteString


