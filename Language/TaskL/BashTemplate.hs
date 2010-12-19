{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
  #-}

module Language.TaskL.BashTemplate
  ( template
  , preamble
  , runtime
  , postamble
  , generatedCodeHeading
  , runtimeCodeHeading
  , goCodeHeading
  , splitTemplate ) where

import Data.ByteString.Char8

import Language.TaskL.Macros


template                    ::  ByteString
template                     =  $(pullFile "./example/example.bash")


preamble, runtime, postamble :: ByteString
(preamble, runtime, postamble) = splitTemplate template


splitTemplate :: ByteString -> (ByteString, ByteString, ByteString)
splitTemplate bytes          =  (preamble, runtime, postamble)
 where
  (preamble, bytes')         =  breakSubstring runtimeCodeHeading bytes
  (runtime, bytes'')         =  breakSubstring generatedCodeHeading bytes'
  (_, postamble)             =  breakSubstring goCodeHeading bytes''

generatedCodeHeading, runtimeCodeHeading, goCodeHeading :: ByteString
generatedCodeHeading         =  append bar "\n# Generated code."
runtimeCodeHeading           =  append bar "\n# Runtime code:"
goCodeHeading                =  append bar "\n# Go."

bar = "################################################################"

