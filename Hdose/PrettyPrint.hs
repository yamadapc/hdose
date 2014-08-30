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

module PrettyPrint (printLog, printInfo, printWarn, printSuccess, printError)
  where

import Data.Time (getCurrentTime, formatTime)
import System.Console.ANSI
import System.Locale (defaultTimeLocale)

reset :: IO ()
reset = setSGR []

timestamp :: IO String
timestamp = do
    currentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "[%X] " currentTime

setForeground :: Color -> IO ()
setForeground fg = setSGR [ SetColor Foreground Vivid fg
                          , SetConsoleIntensity BoldIntensity ]

printLog :: Color -> String -> IO ()
printLog fg x = do
    setForeground fg
    ts <- timestamp
    putStrLn $ ts ++ x
    reset

printInfo :: String -> IO ()
printInfo = printLog Blue

printSuccess :: String -> IO ()
printSuccess = printLog Green

printWarn :: String -> IO ()
printWarn = printLog Yellow

printError :: String -> IO ()
printError = printLog Red
