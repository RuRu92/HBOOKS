{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module HBooks.Cli (
    versionString,
    main,
    mainMode,
    argsToCliOpts,
    module HBooks.Cli.CliOptions,
    module HBooks.Cli.Utils,
    module HBooks.Cli.DocFiles
)
where

import Control.Monad (when)
import Control.Monad.Reader
import Data.List
import Safe
import qualified System.Console.CmdArgs.Explicit as C
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf

import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Data.Text (Text)

import HBooks.Cli.DocFiles
import HBooks.Cli.CliOptions
import HBooks.Cli.Utils
import HBooks.Domain
import HBooks.Config (loadConfig, AppSettings)

versionString ::  String
versionString = "1.0.0"

mainMode :: String
mainMode = "MAIN"

argsToCliOpts :: [String]
argsToCliOpts = []

newtype AppT a = App {
    runAppT :: ReaderT AppSettings IO a
} deriving (Functor, Applicative, Monad)

main :: IO ()
main = do
    startTime <- getCurrentTime
    appSettings <- loadConfig
    print startTime
    -- putStrLn $ formatTime defualtTimeLocale "%FT%T" $ posixSecondsToUTCTime startTime 



--  "RUB", "CHF", "AUD", "CAD", "CNH"