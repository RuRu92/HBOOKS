{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HBooks.App.Env
  ( Env(..)
  , Has(..)
  , grab
  , -- * Type aliases for 'Env' fields
    DbPool
  ) where

import           HBooks.Prelude

import           Colog                      (HasLog (..), LogAction, Message)
import           Data.HashMap.Strict
import           Data.IORef
import           Data.Kind                  (Type)
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)

import           HBooks.App.Session         (SessionExpiry, Sessions)
import           HBooks.App.Types       (OrderBook)

type DbPool = Pool Connection

type Timings = IORef (HashMap Text Distribution)

data Env (m :: Type -> Type) = Env
  { envDbPool        :: !DbPool
  , envOrderBook     :: !OrderBook
  , envSessions      :: !Sessions
  , envSessionExpiry :: !SessionExpiry
  , envLogAction     :: !(LogAction m Message)
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}
  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newAction env = env {envLogAction = newAction}
  {-# INLINE setLogAction #-}

{- | General type class representing which @field@ is in @env@.
Instead of plain usage like this:
@
foo = do
    secret <- asks jwtSecret
@
you should use 'Has' type class like this:
@
foo = do
    secret <- grab @JwtSecret
 -- secret <- asks $ obtain @JwtSecret
@
-}
class Has field env where
  obtain :: env -> field

instance Has DbPool (Env m)
 where
  obtain = envDbPool

instance Has Sessions (Env m)
 where
  obtain = envSessions

instance Has OrderBook (Env m)
 where
  obtain = envOrderBook

instance Has SessionExpiry (Env m)
 where
  obtain = envSessionExpiry

instance Has (LogAction m Message) (Env m)
 where
  obtain = envLogAction

grab ::
     forall field env m. (MonadReader env m, Has field env)
  => m field
grab = asks $ obtain @field

{-# INLINE grab #-}
