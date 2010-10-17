
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
 #-}

module System.TaskL where

import Data.String
import Control.Applicative
import Control.Monad.Identity

import Data.ByteString

import qualified System.TaskL.IdemShell as IdemShell


{-| How this is supposed to work:

* Client generates Task DAG.

* Scheduler in TaskL creates schedule. Backends compile schedule.

* The tasks are defined in terms of IdemShell, the idempotent shell, which
  is also exposed to clients. What does the idempotent have to say to clients?
  A command has a test. If the test succeeds, the command is not executed.
  If the command is executed, however, the test definitely will succeed the
  next time. You can add more tests in a way that causes the command not to
  run in more scenarios (hence we have @[IdemShell.Test]@ terms in all the
  'TaskL' terms); this does not wreck the idempotence.

* Task\L does not understand character encodings at this level though a future
  front-end might. Usernames, file paths and similar labels must be converted
  to bytes at some point; and the ability to set raw bytes seems important (a
  character interface would not allow this).

 -}


data Task = Task ByteString [IdemShell.Test] (Maybe IdemShell.Command) [Task]


data Schedule ByteString     =  Enter ByteString
                             |  Leave ByteString
                             |  Check ByteString [IdemShell.Test]
                             |  Perform ByteString [IdemShell.Command]





