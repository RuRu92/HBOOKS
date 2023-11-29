{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE PatternSynonyms #-}

module HBooks.Prelude
       ( module Relude
       , module Colog
       , module Json
       , module Sql
       , module Web
       , module UUID
       , toTextUUID
       , WithLog
       ) where

-- Reexport
import Relude

-- import Control.Lens ((.~), (^.))

import Colog (pattern D, pattern E, pattern I, LogAction (..), Severity (..), pattern W, log)

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))

import Data.UUID (UUID)
import qualified Data.UUID as UUID

-- import Data.ProtoLens.Message as Proto (defMessage)

import Database.PostgreSQL.Simple.FromField as Sql (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow as Sql (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ as Sql (sql)
import Database.PostgreSQL.Simple.ToField as Sql (ToField (toField))
import Database.PostgreSQL.Simple.ToRow as Sql (ToRow (toRow))
import Database.PostgreSQL.Simple.Types as Sql (Only (..))
import PgNamed as Sql ((=?))

import Servant.API as Web ((:>), Capture, Get, Header, Header', JSON, NoContent (NoContent), Post,
                           QueryParam, QueryParam', ReqBody)

-- import Servant.API.ContentTypes.Proto as Web (Proto)
import Servant.API.Generic as Web ((:-), toServant)
import Web.HttpApiData as Web (FromHttpApiData (..), ToHttpApiData (..))

-- Internal
import qualified Colog (Message, WithLog)


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m


toTextUUID :: UUID -> Text
toTextUUID = UUID.toText