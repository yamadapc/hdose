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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Filesystem (getWorkingDirectory)
import Filesystem.Path (FilePath)
import System.Exit (ExitCode(..))
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

    printHeader tDir cmd to
    withManager $ watchAndRun tDir cmd to

data DojoState = Failling
               | Running ProcessHandle
               | Passing

printTimeout :: DojoState -> IO ()
printTimeout (Running pHandle) =
    waitForProcess pHandle >>
    printTimeout'
printTimeout _ = printTimeout'

printTimeout' :: IO ()
printTimeout' =
  printWarn "The dojo session has ended; another pilot should enter!"

printEvent :: String -> Event -> IO ()
printEvent cmd evt = case evt of
    (Added fp _)    -> printEvent' fp "added"
    (Removed fp _)  -> printEvent' fp "removed"
    (Modified fp _) -> printEvent' fp "modified"
  where printEvent' fp verb =
            printInfo $ "File " ++ show fp ++ " was " ++ verb ++
                        ". Running " ++ cmd ++ "...."

printHeader :: FilePath -> String -> Int -> IO ()
printHeader tDir cmd to = do
    printInfo $ "Starting to watch " ++ show tDir ++ " to run " ++ cmd
    printInfo $ "Sessions will timeout after " ++ show to ++
                " minutes"

watchAndRun :: FilePath -> String -> Int -> WatchManager -> IO ()
watchAndRun tDir cmd to man = do
    tvar <- atomically $ newTVar Passing

    -- Watch the directory for changes, running the command on them
    let action = actionForCommand tvar cmd
    _ <- watchTree man tDir (const True) action

    -- Loop `to` minutes and warn the user the timeout has been
    -- reached
    _ <- forkIO $ do
        threadDelay to'
        st <- atomically (readTVar tvar)
        printTimeout st

    forever $ threadDelay maxBound
  where
    to' = 60 * 1000 * 1000 * to

actionForCommand :: TVar DojoState -> String -> Event -> IO ()
actionForCommand tvar cmd event = do
    state <- atomically $ readTVar tvar
    printEvent cmd event
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

