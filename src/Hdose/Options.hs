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
import Control.Exception (try, SomeException)
import Control.Monad (when)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                              usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath.Glob (Pattern, compile)
import System.IO (hPutStrLn, stderr)

import Hdose.Logging

-- |
-- Runtime options data type
data Options = Options { optionsHelp :: Bool
                       -- ^ Display a help message and exit
                       , optionsTimeout :: Int
                       -- ^ The timeout between sessions in minutes
                       , optionsCommand :: String
                       -- ^ The command to run when files change
                       , optionsIgnore :: [Pattern]
                       -- ^ A list of file globs to ignore
                       }

defaultOptions :: Options
defaultOptions = Options { optionsHelp = False
                         , optionsTimeout = 10
                         , optionsCommand = error "No command specified"
                         , optionsIgnore = map compile [ ".*"
                                                       , ".**/*"
                                                       , "*.tags"
                                                       , "*.o"
                                                       ]
                         }

-- |
-- Prints usage information for `hdose` to stderr
printUsage :: IO ()
printUsage = hPutStrLn stderr $ usageInfo header options
  where header = "Usage: hdose [options] test-command"

options :: [OptDescr (Options -> Options)]
options = [ Option "t" ["timeout"]
                (ReqArg (\t o -> o { optionsTimeout = read t }) "n")
                "Sets the timeout in minutes for the alarm (default = 10)"
          , Option "I" ["ignore"]
                (ReqArg
                    (\t o -> o { optionsIgnore = compile t:optionsIgnore o })
                    "file")
                "A file glob to ignore"
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

        extraGlobs <- getGlobsFromGitignore

        return opts { optionsCommand = unwords rest
                    , optionsIgnore = optionsIgnore opts ++ extraGlobs
                    }
      where opts = foldr ($) defaultOptions optMods
    (_, _, errs) -> do
        mapM_ (hPutStrLn stderr) errs
        printUsage >> exitWith (ExitFailure 1)

getGlobsFromGitignore :: IO [Pattern]
getGlobsFromGitignore = do
    ef <- try $ readFile ".gitignore" :: IO (Either SomeException String)
    case ef of
        Left _ -> return []
        Right str -> do
            printWarn "Loading ignore patterns from `.gitignore`"
            return $ map compile (lines str)
