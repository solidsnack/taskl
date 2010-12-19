{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
  #-}

module System.TaskL.Bash.Template ( template
                                  , preamble
                                  , postamble
                                  , splitOnGeneratedCode ) where

import Data.ByteString.Char8

import System.TaskL.Macros


template                    ::  ByteString
template                     =  $(pullFile "./example/example.bash")


preamble                    ::  ByteString
preamble                     =  fst templateSplitted

postamble                   ::  ByteString
postamble                    =  snd templateSplitted


splitOnGeneratedCode        ::  ByteString -> (ByteString, ByteString)
splitOnGeneratedCode bytes   =  (a `append` generatedCodeDelimiter, z)
 where
  (a, crud)                  =  breakSubstring generatedCodeDelimiter bytes
  (_, z)                     =  breakSubstring (bar `append` "\n# Go.") crud
  generatedCodeDelimiter     =  append bar "\n# Generated code."
  bar = "################################################################"


templateSplitted             =  splitOnGeneratedCode template

