module HBooks.Core.OrderBook (
    Price,
    Quantity,
    Currency(..),
    Asset(..),
    FxCurrency(..),
    Order(..),
    LevelMap(..),
    Table(..),
    OrderMap(..),
    OrderBook(..),
) where

import           HBooks.Prelude
import           HBooks.App.Types   (Identifiable, Currency (..), Price, Quantity, Order, Id, AssetType,
                                      quantity)

import           Data.Text           (Text)

import qualified Data.HashMap.Strict as HashMap


class AssetLike a where
  getAssetPrice :: a -> Price


data FxCurrency = FxCurrency
  { fxAssetId  :: Id Currency
  , fxPrice    :: Price
  , midPrice :: Price
  , pip      :: Int
  } deriving (Show, Identifiable)

data Asset = Asset
  { assetId    :: Id ByteString
  , price      :: Price
  , tickMarker :: Text
  } deriving (Show, Identifiable)

instance AssetLike Asset
 where
  getAssetPrice    = price

instance AssetLike FxCurrency
 where
  getAssetPrice    = fxPrice

--

data PriceMap a where
  PriceMap :: (AssetLike a, Identifiable i) => HashMap.HashMap i (HashMap.HashMap Price [Order a]) -> PriceMap a


instance (AssetLike a, Identifiable i) => Show (PriceMap a) where
  show (PriceMap m) = show m


-- newtype PriceBook a = PriceBook
--   { priceLevels :: HashMap.HashMap Price [Order a]
--   } deriving (Show)


-- Cannot use AssetLike here because of the type signature. Requires a kind of * -> * -> *
-- type Table (a :: AssetLike) = HashMap a (HashMap.HashMap Price [Order a])
-- class (AssetLike a) => HasPriceBook a where
--     type PriceBook a :: *
--     emptyTable :: PriceBook a
  -- insertOrder :: Order a -> Table a -> Table a
  -- deleteOrder :: Order a -> Table a -> Table a
  -- updateOrder :: Order a -> Table a -> Table a


-- instance AssetLike a => PriceBook a where
--   type PriceBook a = HashMap a (HashMap Price [Order a])
--   emptyTable = HashMap.empty
--   insertOrder o@(Order id, price) table = 


newtype OrderMap a = OrderMap
  { orders :: PriceMap a
  } deriving (Show)

data OrderBook a = OB
  { buyBook  :: PriceMap a
  , sellBook :: PriceMap a
  , orderMap :: OrderMap a
  } deriving (Show)
