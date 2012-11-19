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


main :: IO ()
main = do
  taskPattern <- getTask
  Just trees  <- getTrees
  let (failed, compiled) = partitionEithers (tasks <$> trees)
      graphMap           = forestMap compiled
  (failed /= []) `when` do msg "Some definitions were not loadable:"
                           mapM_ (msg . ByteString.pack . show) failed
  case taskPattern of
    Nothing  -> tryCompile graphMap
    Just pat -> case cull pat graphMap of
                  Nothing  -> err "Failed to find requested task."
                  Just sub -> tryCompile sub
 where
  getTask = do arg <- (ByteString.pack <$>) . listToMaybe <$> getArgs
               case arg of
                  Nothing -> return Nothing
                  Just b  -> case Attoparsec.parseOnly name b of
                              Right name -> return . Just $ wrap name
                              Left _     -> err "Invalid task name."
   where wrap name = Task (Abstract name) []
  getTrees :: IO (Maybe (Forest [ByteString]))
  getTrees  = do forest <- decode <$> ByteString.hGetContents stdin
                 return ((tree2tree <$>) <$> forest)
  msg = ByteString.hPutStrLn stderr
  out = ByteString.hPutStrLn stdout
  err = (>> exitFailure) . msg . ("o.O" <>)
  tryCompile mod = case schedule (graph mod) of
    Right traversal -> out . script $ command <$> traversal
    Left cycles     -> do msg "Scheduling failure due to cycles:"
                          mapM_ (mapM_ msg . prettyPrintCycle) cycles
                          exitFailure


 ---------------------------- Pretty printing tools ---------------------------

prettyPrintCycle :: [Task] -> [ByteString]
prettyPrintCycle = withPrefixes False . (prettyPrintTask <$>)
 where withPrefixes _     [     ] = [         ]
       withPrefixes False [  h  ] = ["╳ " <<> h]
       withPrefixes False (h:s:t) = ("╔ " <<> h) : withPrefixes True (s:t)
       withPrefixes True  (h:s:t) = ("║ " <<> h) : withPrefixes True (s:t)
       withPrefixes True  [  h  ] = ["╚ " <<> h]
       s <<> b = ByteString.fromString s <> b

prettyPrintTask :: Task -> ByteString
prettyPrintTask (Task call args) = ByteString.unwords
                                 $ prettyPrintCall call : args

prettyPrintCall :: Call -> ByteString
prettyPrintCall (Cmd (ShHTTP b)) = b
prettyPrintCall (Cmd (Path b))   = b
prettyPrintCall (Abstract b)     = b

