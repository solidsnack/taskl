#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.UTF8 as ByteString (fromString)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Tree (Forest)
import           System.IO
import           System.Environment
import           System.Exit

import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import           Data.Yaml

import           System.TaskL
import           System.TaskL.JSONTree


data WhatDo = List | Compile deriving (Eq, Ord, Show)

main :: IO ()
main = do
  todo   <- whatdo
  loaded <- load <$> ByteString.getContents
  case loaded of ([  ], [  ]) -> err "No tasks loaded."
                 (ss:_, [  ]) -> err ("Failed to load tasks. " <>> ss)
                 ([  ], code) -> gogogo todo (forestMap code)
                 (errs, code) -> do msg "Some definitions were not loadable:"
                                    mapM_ (msg . ByteString.pack) errs
                                    gogogo todo (forestMap code)
 where
  msg = ByteString.hPutStrLn stderr
  out = ByteString.hPutStrLn stdout
  err = (>> exitFailure) . msg . ("o.O " <>)
  tryCompile mod = case schedule (graph mod) of
    Right traversal -> out . script $ command <$> traversal
    Left cycles     -> do msg "Scheduling failure due to cycles:"
                          mapM_ (mapM_ msg . prettyPrintCycle) cycles
                          exitFailure
  whatdo = do arg <- (ByteString.pack <$>) . listToMaybe <$> getArgs
              case arg of
                Nothing     -> return (Compile, Nothing)
                Just "list" -> return (List, Nothing)
                Just b      -> case Attoparsec.parseOnly name b of
                                 Right name -> return (Compile, wrap name)
                                 Left _     -> err "Invalid task name."
   where wrap name = Just $ Task (Abstract name) []
  gogogo (what, selection) map = do
    map' <- case selection of
              Nothing   -> return map
              Just name -> maybe (err "Failed to find requested task.") return
                                 (cull name map)
    case what of List    -> ByteString.putStr . draw $ dependencies map'
                 Compile -> tryCompile map'


 ---------------------------- Pretty printing tools ---------------------------

prettyPrintCycle :: [Task] -> [ByteString]
prettyPrintCycle = withPrefixes False . (prettyPrintTask <$>)
 where withPrefixes _     [     ] = [         ]
       withPrefixes False [  h  ] = ["╳ " <<> h]
       withPrefixes False (h:s:t) = ("╔ " <<> h) : withPrefixes True (s:t)
       withPrefixes True  (h:s:t) = ("║ " <<> h) : withPrefixes True (s:t)
       withPrefixes True  [  h  ] = ["╚ " <<> h]

prettyPrintTask :: Task -> ByteString
prettyPrintTask (Task call args) = ByteString.unwords
                                 $ prettyPrintCall call : args

prettyPrintCall :: Call -> ByteString
prettyPrintCall (Cmd (ShHTTP b)) = b
prettyPrintCall (Cmd (Path b))   = b
prettyPrintCall (Abstract b)     = b

(<<>) :: String -> ByteString -> ByteString
s <<> b = ByteString.fromString s <> b

(<>>) :: ByteString -> String -> ByteString
b <>> s = b <> ByteString.fromString s

