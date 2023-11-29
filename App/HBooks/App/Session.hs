module HBooks.App.Session
  ( Session(..)
  , SessionExpiry(..)
  , Sessions
  , sessionExpired
  , mkNewSession
  ) where

import           HBooks.Prelude

import           Control.Concurrent.MVar (MVar)
import           Control.Monad.IO.Class  (MonadIO (..))

import           Data.Text
import           Data.Time.Clock         (NominalDiffTime, UTCTime, addUTCTime,
                                          getCurrentTime)

import           Data.HashMap.Strict     (HashMap)

import           HBooks.App.Id           (AnyId)
import           HBooks.Core.OrderBook   (Price, Quantity)
import           HBooks.Core.User        (User)

type Sessions = MVar (HashMap AnyId Session)

newtype Session = Session
  { sLoginTime :: UTCTime
  } deriving stock (Eq, Show)

newtype SessionExpiry = SessionExpiry
  { unSessionExpiry :: NominalDiffTime
  } deriving newtype (Num)


-- | Checks whether session expired within given interval relative to current time
sessionExpired :: SessionExpiry -> UTCTime -> Session -> Bool
sessionExpired (SessionExpiry expiry) currentTime session =
  let sessionEnd = addUTCTime expiry $ sLoginTime session
   in sessionEnd <= currentTime


-- | Created a new 'Session'.
mkNewSession :: MonadIO m => m Session
mkNewSession = liftIO $ Session <$> getCurrentTime
