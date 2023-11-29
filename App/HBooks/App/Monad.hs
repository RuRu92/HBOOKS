module HBooks.App.Monad
       ( -- * Application monad
         App (..)
       , AppEnv
       , runAppAsIO
       ) where

import HBooks.Prelude
import HBooks.App.Types
import HBooks.App.Env

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Relude.Extra.Bifunctor (firstF)

-- import HBooks.App.Error (AppError, AppException (..))

-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

newtype ApplicationRunner a = ApplicationRunner { runApp :: ReaderT Env IO a }

-- instance MonadError
