{-# LANGUAGE TypeFamilies
  #-}

module Language.TaskL.Code where

import Data.ByteString.Char8


--data Code t where            =  Bash ByteString
--                             |  Exec StringL [StringL]
--                             |  Hask (IO Bool)


data TaskL                   =  forall t. (Code t) => TaskL (T t)
instance Show TaskL where
  show t = case t of TaskL code -> "TaskL (" ++ (unpack . name) code ++ ")"

class Code t where
  data T t                  ::  *
  name                      ::  (T t) -> ByteString
instance Code ByteString where
  data T ByteString          =  Bash ByteString
  name _                     =  "bash"
instance Code [ByteString] where
  data T [ByteString]        =  Exec ByteString [ByteString]
  name _                     =  "exec"
instance Code (IO Bool) where
  data T (IO Bool)           =  Hask (IO Bool)
  name _                     =  "hask"

(-*)                        ::  (Code t) => (t -> T t) -> t -> TaskL
f -* t                       =  TaskL (f t)

{-| Adds code to the test body.
 -}
(-?)                         =  undefined

{-| Adds code to the task body. 
 -}
(-!)                         =  undefined

