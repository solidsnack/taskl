{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           , GADTs
  #-}

module Language.TaskL.Code where

import Prelude hiding (lines)
import Data.ByteString.Char8


data Code t where
  Bash                      ::  ByteString -> Code ByteString
  Exec                      ::  ByteString -> [ByteString] -> Code [ByteString]
--Exec                      ::  StringL -> [StringL] -> Code [StringL]
  Hask                      ::  IO Bool -> Code (IO Bool)


instance Show (Code t) where
  show code                  =  case code of
    Bash b                  ->  "Bash " ++
                                (unpack . intercalate "     " . lines) b
    Exec _ _                ->  "Exec _ [...]"
    Hask _                  ->  "Hask <IO Bool>"


{-| Adds code to the test body.
 -}
(-?)                        ::  (t -> Code t) -> t -> TaskL
(-?)                         =  undefined

{-| Adds code to the task body. 
 -}
(-!)                        ::  (t -> Code t) -> t -> TaskL
(-!)                         =  undefined

data TaskL                   =  TaskL
