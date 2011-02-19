
module Language.TaskL.Name where

import Data.Monoid
import Data.ByteString.Char8
import Data.Map (Map)
import Data.Set (Set)
import Data.String

import Text.Regex.XMLSchema.String


newtype Name                 =  Name ByteString


names                       ::  ByteString -> Maybe (Name, [Name])
names bytes                  =  undefined
 where
  s                          =  unpack bytes

popName                      =  splitSubex namesRE

namesRE = "({name}" ++ nameRE ++ ")" ++ "({tail}(:" ++ nameRE ++ ")*)"
nameRE                       =  "[a-z_][a-zA-Z0-9_]*"


