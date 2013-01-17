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
                 ([  ], defs) -> gogogo todo defs
                 (errs, defs) -> do msg "Some definitions were not loadable:"
                                    mapM_ (msg . ByteString.pack) errs
                                    gogogo todo defs
 where
  msg = ByteString.hPutStrLn stderr
  out = ByteString.hPutStrLn stdout
  err = (>> exitFailure) . msg . ("o.O " <>)
  tryCompile mod = case schedule (graph mod) of
    Right traversal -> out . script $ command <$> traversal
    Left cycles     -> do msg "Scheduling failure due to cycles:"
                          mapM_ (mapM_ msg . prettyPrintCycle) cycles
                          exitFailure
  whatdo = do args <- (ByteString.pack <$>) <$> getArgs
              case args of
                [      ] -> return (Compile, [])
                ["list"] -> return (List, [])
                _:_      -> do let parsed  = Attoparsec.parseOnly name <$> args
                                   (e, ok) = partitionEithers parsed
                               if e /= [] then err "Invalid task name."
                                          else return (Compile, wrap <$> ok)
   where wrap name = Task (Abstract name) []
  gogogo (what, selections) forest = do
    code <- forestMap <$> case selections of
              [ ] -> return forest
              _:_ -> case trim selections forest of
                       [ ] -> err "Failed to find requested task."
                       frs -> return frs
    case what of List    -> ByteString.putStr . draw $ dependencies code
                 Compile -> tryCompile code


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
prettyPrintCall (Cmd (HTTPx b)) = b
prettyPrintCall (Cmd (Path b))  = b
prettyPrintCall (Abstract b)    = b

(<<>) :: String -> ByteString -> ByteString
s <<> b = ByteString.fromString s <> b

(<>>) :: ByteString -> String -> ByteString
b <>> s = b <> ByteString.fromString s

