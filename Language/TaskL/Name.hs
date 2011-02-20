{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}


module Language.TaskL.Name where

import Data.Monoid
import Data.ByteString.Char8 hiding (reverse, map)
import Data.Map (Map)
import Data.Set (Set)
import Data.String

import Text.Regex.XMLSchema.String


newtype Name                 =  Name { bytes :: ByteString }
deriving instance Eq Name
deriving instance Ord Name
deriving instance Show Name


names :: ByteString -> Either (ByteString, ByteString) (Name, [Name])
names b                      =  case popName (unpack b) of
  Right (name, s)           ->  names' (name, []) s
  Left err                  ->  Left err
 where
  names' (name, names) ""    =  Right (name, reverse names)
  names' (name, names) s     =  case popName s of
    Right (name', s')       ->  names' (name, name':names) s'
    Left (before, after)    ->  Left (append repacked (cons ':' before), after)
   where
    repacked = (intercalate ":" . map bytes) (name : reverse names)

popName :: String -> Either (ByteString, ByteString) (Name, String)
popName s = maybe (Left (pack s, "")) rewrite (splitSubexRE namesRE s)
 where
  repack a b                 =  append (pack a) (':' `cons` pack b)
  rewrite res                =  case res of
    ([("name",""),("tail","")],"")  ->  Left  (pack s, "")
    ([("name",n),("tail",t)]  ,"")  ->  Right (Name (pack n), t)
    ([("name",n),("tail",t)],trash) ->  Left  (repack n t, pack trash)
    ([("name","")]          ,  "")  ->  Left  (pack s, "")
    ([("name",n)]           ,  "")  ->  Right (Name (pack n), "")
    ([("name",n)]           ,trash) ->  Left  (pack n, pack trash)
    (_                      ,   _)  ->  Left  (pack s, "")

namesRE                      =  mkBr "name" nameRE
                             /  mkOpt $ colon
                             /  mkBr "tail" $ nameRE
                             /  mkStar (colon / nameRE)
 where
  infixr 0 /
  (/)                        =  mkSeq
  colon                      =  mkSym1 ':'

nameRE                       =  parseRegex "[a-z_][a-zA-Z0-9_]*"

