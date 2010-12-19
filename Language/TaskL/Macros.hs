module Language.TaskL.Macros where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.ByteString.Char8


{-| Just a macro to pull in a file.
 -}
pullFile f = lift =<< runIO (Data.ByteString.Char8.readFile f)


instance Lift ByteString where
  lift = lift . Data.ByteString.Char8.unpack

