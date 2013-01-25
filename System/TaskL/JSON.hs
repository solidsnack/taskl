{-# LANGUAGE OverloadedStrings
           , FlexibleInstances #-}
module System.TaskL.JSON where

import           Control.Monad

import           Data.Aeson

import System.TaskL.Phases


instance FromJSON (Module Templated) where
  parseJSON (Object _) = undefined
  parseJSON _          = mzero

instance ToJSON (Module Templated) where
  toJSON _ = undefined

