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

module Main (main) where

-- Imports
--------------------------------------------------
import Prelude hiding (FilePath)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Filesystem (getWorkingDirectory)
import Filesystem.Path (FilePath)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                              usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FSNotify (withManager, watchTree, WatchManager, Event, Event(..))
import System.IO (hPutStrLn, stderr)
import System.Process (runCommand, terminateProcess, waitForProcess,
                       ProcessHandle)

import PrettyPrint

-- Data Types
--------------------------------------------------
data Options = Options { help    :: Bool   -- -h
                       , timeout :: Int    -- -t integer
                       , command :: String -- ...command
                       }

data DojoState = Failling
               | Running ProcessHandle
               | Passing

-- Data
--------------------------------------------------
mcsPerMinute :: Int
mcsPerMinute = 60 * 1000 * 1000

options :: [OptDescr (Options -> Options)]
options = [ Option "t" ["timeout"]
            (ReqArg (\t o -> o { timeout = mcsPerMinute * read t }) "timeout")
            "Sets the timeout in minutes for the alarm (default = 10)"
          , Option "h" ["help"]
            (NoArg (\o -> o { help = True }))
            "Print this help message."
          ]

-- Functions
--------------------------------------------------
printUsage :: IO ()
printUsage = hPutStrLn stderr $ usageInfo header options
  where header = "Usage: hdose [options] test-command"

printTimeout :: DojoState -> IO ()
printTimeout (Running pHandle) =
    waitForProcess pHandle >>
    printTimeout'
printTimeout _ = printTimeout'

printTimeout' :: IO ()
printTimeout' =
  printWarn "The dojo session has ended; another pilot should enter!"

printEvent :: String -> Event -> IO ()
printEvent cmd evt =
    case evt of
        (Added fp _) -> printEvent' fp "added"
        (Removed fp _) -> printEvent' fp "removed"
        (Modified fp _) -> printEvent' fp "modified"
  where printEvent' fp verb =
            printInfo $ "File " ++ show fp ++ " was " ++ verb ++
                        ". Running " ++ cmd ++ "...."

printHeader :: FilePath -> String -> Int -> IO ()
printHeader tDir cmd to = do
    printInfo $ "Starting to watch " ++ show tDir ++ " to run " ++ cmd
    printInfo $ "Sessions will timeout after " ++ show to ++
                " microseconds"

watchAndRun :: FilePath -> String -> Int -> WatchManager -> IO ()
watchAndRun tDir cmd to man = do
    tvar <- atomically $ newTVar Passing

    -- Watch the directory for changes, running the command on them
    let action = actionForCommand tvar cmd
    _ <- watchTree man tDir (const True) action

    -- Loop `to` microseconds and warn the user the timeout has been
    -- reached
    _ <- forkIO $ threadDelay to >> atomically (readTVar tvar) >>= printTimeout
    forever $ threadDelay maxBound

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

setDefaults :: [Options -> Options] -> String -> Options
setDefaults opts cmd = foldl (flip ($)) defaultOpts opts
  where defaultOpts = Options { help    = False
                              , timeout = 10 * mcsPerMinute
                              , command = cmd
                              }

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (opts, rest, []) ->
            if help opts' || null rest
                then printUsage
                else startWithOptions opts'
              where opts' = setDefaults opts (unwords rest)
        _ -> printUsage

startWithOptions :: Options -> IO ()
startWithOptions opts = do
    tDir <- getWorkingDirectory -- for prototyping only
    printHeader tDir cmd to
    withManager $ watchAndRun tDir cmd to
  where to  = timeout opts
        cmd = command opts
