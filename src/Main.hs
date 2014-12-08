-- A helper tool for conducting dojos. Inspired by dose.py
-- Copyright (C) 2014 Pedro Tacla Yamada
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
module Main
  where

import Prelude hiding (FilePath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad
import Filesystem (getWorkingDirectory)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)
import qualified GHC.IO (FilePath)
import System.Exit (ExitCode(..))
import System.FilePath (makeRelative)
import System.FilePath.Glob (Pattern, match)
import System.FSNotify (withManager, watchTree, WatchManager, Event, Event(..))
import System.Process (runCommand, terminateProcess, waitForProcess,
                       ProcessHandle)

import Hdose.Logging
import Hdose.Options

-- |
-- The main entry point
main :: IO ()
main = do
    opts <- getOptions
    tDir <- getWorkingDirectory

    let to  = optionsTimeout opts
        cmd = optionsCommand opts
        ign = optionsIgnore opts

    printHeader tDir cmd to
    withManager $ watchAndRun ign tDir cmd to

data DojoState = Failling
               | Running ProcessHandle
               | Passing

watchAndRun :: [Pattern] -> FilePath -> String -> Int -> WatchManager -> IO ()
watchAndRun ignore tDir cmd to man = do
    tvar <- atomically $ newTVar Passing

    -- Watch the directory for changes, running the command on them
    let action = actionForCommand (encodeString tDir) ignore tvar cmd
    _ <- watchTree man tDir (const True) action

    -- Loop `to` minutes and warn the user the timeout has been
    -- reached
    timeoutA <- async $ forever $ do
        threadDelay to'
        st <- atomically (readTVar tvar)
        printTimeout st

    wait timeoutA
  where
    to' = 60 * 1000 * 1000 * to

actionForCommand :: GHC.IO.FilePath -> [Pattern] -> TVar DojoState -> String -> Event
                 -> IO ()
actionForCommand cwd ignore _ _ event | any (`match` fp) ignore = do
    printWarn $ "Ignoring event on " ++ fp
    return ()
  where fp = eventFilePath cwd event
actionForCommand _ _ tvar cmd event = do
    state <- atomically $ readTVar tvar
    printEvent event
    case state of
        Running pHandle ->
            terminateProcess pHandle >>
            printError "Terminated hanging process..." >>
            execute
        _ -> execute
  where execute = do
            pHandle <- runCommand cmd
            atomically $ writeTVar tvar (Running pHandle)
            exitCode <- waitForProcess pHandle
            case exitCode of
                ExitSuccess -> do
                  printSuccess "Command exited with 0. Test suite is green!"
                  atomically $ writeTVar tvar Passing
                ExitFailure code -> do
                  printError $ "Command exited with " ++ show code ++ ". " ++
                               "Test suite is red!"
                  atomically $ writeTVar tvar Failling

eventFilePath :: GHC.IO.FilePath -> Event -> GHC.IO.FilePath
eventFilePath cwd (Added fp _)    = makeRelative cwd $ encodeString fp
eventFilePath cwd (Removed fp _)  = makeRelative cwd $ encodeString fp
eventFilePath cwd (Modified fp _) = makeRelative cwd $ encodeString fp

-- More logging functions
-------------------------------------------------------------------------------

printTimeout :: DojoState -> IO ()
printTimeout (Running pHandle) =
    waitForProcess pHandle >>
    printTimeout'
printTimeout _ = printTimeout'

printTimeout' :: IO ()
printTimeout' =
  printWarn "The dojo session has ended; another pilot should enter!"

printHeader :: FilePath -> String -> Int -> IO ()
printHeader tDir cmd to = do
    printInfo $ "Starting to watch " ++ show tDir ++ " to run " ++ cmd
    printInfo $ "Sessions will timeout after " ++ show to ++
                " minutes"
