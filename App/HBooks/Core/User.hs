module HBooks.Core.User (
    Role(..),
    User(..),
    UserOrderBook(..),
    PriceOrderAggregate(..),
    calculateTotalOrdersPrice
) where

--- How to handle cyclic dependencies in Haskell?

import         HBooks.Prelude
import         HBooks.App.Types


data OrdersSnapshot a = OrdersSnapshot
  { buyOrders  :: [Order a]
  , sellOrders :: [Order a]
  } deriving (Show)

data UserOrderBook a = UserOrderBook
  { user                  :: User
  , userOrderBookSnapshot :: OrdersSnapshot a
  } deriving (Show)

data PriceOrderAggregate = POA
  { totalBuyOrdersPrice  :: Price
  , totalSellOrdersPrice :: Price
  } deriving (Show)

calculateTotalOrdersPrice :: UserOrderBook a -> PriceOrderAggregate
calculateTotalOrdersPrice (UserOrderBook user, book) =
  POA
    { totalBuyOrdersPrice = sum $ map price $ buyOrders book
    , totalSellOrdersPrice = sum $ map price $ sellOrders book
    }

instance Show PriceOrderAggregate where
  show poa =
    "--------------------------\n" ++
    "[Order Aggregate: \n" ++
    "|total bid orders sum = " ++ show (totalBuyOrdersPrice poa) ++ "\n" ++
    "|totalSellOrdersPrice = " ++ show (totalSellOrdersPrice poa) ++ "\n" ++
    "|total = " ++ show (totalBuyOrdersPrice poa + totalSellOrdersPrice poa) ++ " ]" ++ 
    "\n" "--------------------------\n"
