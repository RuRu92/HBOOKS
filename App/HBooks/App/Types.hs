module HBooks.App.Types (
    Price,
    Quantity,
    AssetType,
    UserId,
    Identifiable(..),
    Currency(..),
    UserId(..),
    User(..),
    Role(..),
    Order(..),
    module HBooks.App.Id
) where

import HBooks.Prelude
import HBooks.App.Id

import qualified Data.UUID.V4 as UUIDv4


type Price = Double
type Quantity = Int

type UserId = Id Text

class Identifiable a where
  getId :: a -> Id Text

data AssetType = FX | Stock | Crypto
  deriving (Eq, Show)

newtype Currency =
  Currency Text
  deriving (Eq, Show)

data Order a = Order
  { orderId :: Id Int
  , asset    :: a
  , orderPrice    :: Price
  , quantity :: Quantity
  , user     :: UserId
  } deriving (Show, Identifiable)

data Role
  = Admin
  | OPS
  | Customer
  deriving (Show)

data User = User
  { userId   :: UserId
  , name :: String
  , role :: Role
  } deriving (Show, Identifiable)


getUUID :: IO Text
getUUID = UUIDv4.nextRandom >>= return . toTextUUID

createUser :: String -> Role -> IO User
createUser name role = getUUID >>= 
                          \id -> return $ User (Id id) name role
  