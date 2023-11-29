{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module HBooks.App.Id (
    Id(..),
    AnyId,
    castId,
) where

import HBooks.Prelude

import Data.Type.Equality (type (==))
-- import GHC.Generics (Generic)
-- import Data.Hashable (Hashable)
-- import Data.Aeson (FromJSON, ToJSON)
-- import Data.Csv (FromField, ToField)
-- import Data.Text (Text)
-- import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import Data.Proxy (Proxy(..))
-- import Servant (FromHttpApiData(..), ToHttpApiData(..))

-- | Wrapper for textual id. Contains phantom type parameter for increased
-- type-safety.
newtype Id a = Id { unId :: a }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData, FromJSON, ToJSON)


-- | When we don't care about type of 'Id' but don't want to deal with type variables
type AnyId = Id ()

-- | Unsafe cast of 'Id'. Implementation uses smart trick to enforce usage
-- always with @TypeApplications@.
castId :: forall to from . (from ~ to) => Id from -> Id to
castId (Id a) = Id a
