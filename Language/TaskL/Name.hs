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


{-| A Task\\L name is one or more name components, separated by colons (@:@).
 -}
data Name                    =  Name { base :: Component,
                                       qualifier :: [Component] }
deriving instance Eq Name
deriving instance Ord Name
deriving instance Show Name
instance IsString Name where
  fromString s               =  case name (pack s) of
    Right x                 ->  x
    Left _                  ->  error ("Bad name: " ++ s)


{-| A Task\\L name component conforms to the regex @[a-z_][a-zA-Z0-9_]*@,
    resembling the syntax of identifiers found in many programming languages.
 -}
newtype Component            =  Component { bytes :: ByteString }
deriving instance Eq Component
deriving instance Ord Component
deriving instance Show Component


{-| Parses a 'Name' from a 'ByteString'.
 -}
name :: ByteString -> Either (ByteString, ByteString) Name
name b                       =  uncurry Name `fmap` components b


{-| Parses a 'ByteString' potentially containing a qualified name. The name in
    the first slot of the tuple is the rightmost name in the hierarchy, the
    \"basename\".
 -}
components
 :: ByteString -> Either (ByteString, ByteString) (Component, [Component])
components b                 =  case popName (unpack b) of
  Right (name, s)           ->  names' (name, []) s
  Left err                  ->  Left err
 where
  names' (name, names) ""    =  Right (name, reverse names)
  names' (name, names) s     =  case popName s of
    Right (name', s')       ->  names' (name', name:names) s'
    Left (before, after)    ->  Left (append repacked (cons ':' before), after)
   where
    repacked = (intercalate ":" . map bytes) (name : reverse names)

popName :: String -> Either (ByteString, ByteString) (Component, String)
popName s = maybe (Left (pack s, "")) rewrite (splitSubexRE namesRE s)
 where
  repack a b                 =  append (pack a) (':' `cons` pack b)
  rewrite res                =  case res of
    ([("name",""),("tail","")],"")  ->  Left  (pack s, "")
    ([("name",n),("tail",t)]  ,"")  ->  Right (Component (pack n), t)
    ([("name",n),("tail",t)],trash) ->  Left  (repack n t, pack trash)
    ([("name","")]          ,  "")  ->  Left  (pack s, "")
    ([("name",n)]           ,  "")  ->  Right (Component (pack n), "")
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

