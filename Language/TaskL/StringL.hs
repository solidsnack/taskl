
module Language.TaskL.StringL where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Word (Word16)
import Data.Set (Set)


newtype StringL              =  StringL [StringLToken]

data StringLToken            =  Literal ByteString | Variable Word16


