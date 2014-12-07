{-# LANGUAGE LambdaCase #-}
module Hdose.Options
    (
      Options(..)
    , defaultOptions
    , getOptions
    , printUsage
    )
  where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                              usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

-- |
-- Runtime options data type
data Options = Options { optionsHelp :: Bool
                       -- ^ Display a help message and exit
                       , optionsTimeout :: Int
                       -- ^ The timeout between sessions in minutes
                       , optionsCommand :: String
                       -- ^ The command to run when files change
                       }

defaultOptions :: Options
defaultOptions = Options { optionsHelp = False
                         , optionsTimeout = 10
                         , optionsCommand = error "No command specified"
                         }

-- |
-- Prints usage information for `hdose` to stderr
printUsage :: IO ()
printUsage = hPutStrLn stderr $ usageInfo header options
  where header = "Usage: hdose [options] test-command"

options :: [OptDescr (Options -> Options)]
options = [ Option "t" ["timeout"]
            (ReqArg (\t o -> o { optionsTimeout = read t }) "timeout")
            "Sets the timeout in minutes for the alarm (default = 10)"
          , Option "h" ["help"]
            (NoArg (\o -> o { optionsHelp = True }))
            "Print this help message."
          ]

getOptions :: IO Options
getOptions = getOpt RequireOrder options <$> getArgs >>= \case
    (optMods, rest, []) -> do
        when (optionsHelp opts) $ do
            printUsage
            exitSuccess

        when (null rest) $ do
            printUsage
            exitWith (ExitFailure 1)

        return opts { optionsCommand = unwords rest
                    }
      where opts = foldr ($) defaultOptions optMods
    (_, _, errs) -> do
        mapM_ (hPutStrLn stderr) errs
        printUsage >> exitWith (ExitFailure 1)
