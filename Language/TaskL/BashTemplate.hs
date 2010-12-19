{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
  #-}

module Language.TaskL.BashTemplate
  ( template
  , preamble
  , postamble
  , splitOnGeneratedCode ) where

import Data.ByteString.Char8

import Language.TaskL.Macros


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

