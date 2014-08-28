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
import Data.List (find)
import Filesystem (getWorkingDirectory)
import Filesystem.Path (FilePath)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                              usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess)
import System.FSNotify (withManager, watchTree)
import System.IO (hPutStrLn, stderr)
import System.Process (system)

-- Data Types
--------------------------------------------------
data Flag = Help        -- -h
          | Timeout Int -- -t integer
          deriving(Eq, Show)

-- Data
--------------------------------------------------
msPerMinute = 60 * 1000

flags :: [OptDescr Flag]
flags = [ Option ['t'] ["timeout"] (OptArg readTimeout "timeout")
          "Sets the timeout in minutes for the alarm (default = 10)"
        , Option ['h'] ["help"] (NoArg Help)
          "Print this help message."
        ]
  where readTimeout (Just n) = Timeout $ read n
        readTimeout Nothing  = Timeout 10 -- default to 10 minutes

-- Functions
--------------------------------------------------
parseArgs :: [String] -> Maybe ([Flag], String)
parseArgs args = case getOpt RequireOrder flags args of
                   (options, command, []) ->
                     if Help `elem` options
                       then Nothing
                       else Just (options, unwords command)
                   _ -> Nothing

printUsage :: IO ()
printUsage = hPutStrLn stderr $ usageInfo header flags
  where header = "Usage: hdose [options] test-command"

onTimeout :: IO ()
onTimeout = putStrLn "The dojo session has ended; another pilot should enter!"

getTimeout :: [Flag] -> Int
getTimeout options = case find pred options of
                       Just (Timeout x) -> x * msPerMinute
                       -- any other case can't happen because timeout has
                       -- a default
  where pred (Timeout _) = True
        pred _ = False

-- Thanks to http://stackoverflow.com/questions/16580941
watchAndRun tDir command man = do
  watchTree man tDir (const True) $ \fp -> do
    exitCode <- system command
    case exitCode of
      ExitSuccess ->
        putStrLn "Command exited successfully. Test suite is green!"
      ExitFailure code ->
        putStrLn "Command exited with a non-zero exit code. :("
    return ()
  return ()

main = case parseArgs ["-t", "mocha", "-w"] of
         Nothing -> printUsage
         Just (options, command) -> do
           forkIO $ threadDelay timeout >> onTimeout
           tDir <- getWorkingDirectory -- for prototyping only
           withManager $ watchAndRun tDir command
           return ()
          where timeout = getTimeout options
