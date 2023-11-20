module HBooks.Domain (User, Role, Currency) where

import Data.Text (Text)


data Role = Admin | OPS | Customer deriving (Show)

data User = User {
    name :: String
,   role :: Role
} deriving (Show)

newtype Currency = Currency Text
    deriving (Eq, Show)
