{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
  #-}

module System.TaskL.Bash.Template where

import Data.ByteString.Char8

import System.TaskL.Macros


template                    ::  ByteString
template                     =  $(pullFile "./example/example.bash")

