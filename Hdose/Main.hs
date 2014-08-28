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
import Control.Monad (forever)
import Data.List (null)
import Filesystem (getWorkingDirectory)
import Filesystem.Path (FilePath)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                              usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess)
import System.FSNotify (withManager, watchTree, WatchManager, Action, Event,
                        Event(..))
import System.IO (hPutStrLn, stderr)
import System.Process (system)

-- Data Types
--------------------------------------------------
data Options = Options { help    :: Bool   -- -h
                       , timeout :: Int    -- -t integer
                       , command :: String -- ...command
                       }

-- Data
--------------------------------------------------
mcsPerMinute = 60 * 1000 * 1000

options :: [OptDescr (Options -> Options)]
options = [ Option ['t'] ["timeout"]
            (ReqArg (\t o -> o { timeout = mcsPerMinute * (read t) }) "timeout")
            "Sets the timeout in minutes for the alarm (default = 10)"
          , Option ['h'] ["help"]
            (NoArg (\o -> o { help = True }))
            "Print this help message."
          ]

-- Functions
--------------------------------------------------
printUsage :: IO ()
printUsage = hPutStrLn stderr $ usageInfo header options
  where header = "Usage: hdose [options] test-command"

printTimeout :: IO ()
printTimeout =
    putStrLn "The dojo session has ended; another pilot should enter!"

printHeader :: FilePath -> String -> Int -> IO ()
printHeader tDir command timeout = do
    putStrLn $ "Starting to watch " ++ (show tDir) ++ " to run " ++ command
    putStrLn $ "Sessions will timeout after " ++ (show timeout) ++ " microseconds"

-- Thanks to http://stackoverflow.com/questions/16580941
watchAndRun :: FilePath -> String -> WatchManager -> IO ()
watchAndRun tDir command man = do
    watchTree man tDir (const True) action
    forever $ threadDelay maxBound
  where action = actionForCommand command

actionForCommand :: String -> Action
actionForCommand command (Modified _ _) = system command >>= handleExitCode
  where handleExitCode ExitSuccess =
            putStrLn "Command exited successfully. Test suite is green!"
        handleExitCode (ExitFailure code) =
            putStrLn "Command exited with a non-zero exit code. :("
actionForCommand _ _ = return ()

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
            if (help opts') || (null $ rest)
                then printUsage
                else startWithOptions opts'
          where opts' = setDefaults opts (unwords rest)
        _ -> printUsage

startWithOptions :: Options -> IO ()
startWithOptions opts = do
    tDir <- getWorkingDirectory -- for prototyping only
    printHeader tDir cmd to
    forkIO $ threadDelay to >> printTimeout
    withManager $ watchAndRun tDir cmd
  where to  = timeout opts
        cmd = command opts
