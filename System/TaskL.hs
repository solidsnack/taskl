
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
 #-}

module System.TaskL where

import Data.String
import Control.Applicative
import Control.Monad.Identity

import Data.Text

import qualified System.TaskL.IdemShell as IdemShell


{-| How this is supposed to work:

* Client generates TaskDAG.

* Scheduler in TaskL creates schedule. Backends compile schedule.

* The tasks are defined in terms of IdemShell, the idempotent shell, which
  is also exposed to clients. What does the idempotent have to say to clients?
  A command has a test. If the test succeeds, the command is not executed.
  If the command is executed, however, the test definitely will succeed the
  next time. You can add more tests in a way that causes the command not to
  run in more scenarios (hence we have @[IdemShell.Test]@ terms in all the
  'TaskL' terms); this does not wreck the idempotence.

* This adding more tests business should perhaps be shoved down in to
  IdemShell; but then how do we add tests to aggregates? It puzzles me.

 -}


data TaskL label             =  Atomic label [IdemShell.Test] IdemShell.Command
                             |  Aggregate label [IdemShell.Test] [Task label]


data Schedule label          =  Enter label
                             |  Leave label
                             |  Check label [IdemShell.Test]
                             |  Perform label [IdemShell.Command]





