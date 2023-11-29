{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HBooks.Config
  ( loadConfig
  , AppSettings
  ) where

import HBooks.Prelude

import           Control.Monad.IO.Class
import           Data.ByteString
import           Data.Text
import           Toml                   (TomlBiMap, TomlCodec, (.=))

import qualified Toml

import           HBooks.Core.User          (Role, User)
import           HBooks.Core.OrderBook     (Currency (..))

data AppSettings = AppSettings
  { appName           :: Text
  , apiKey            :: Text
  , supportedCurrency :: [Currency]
  -- , activeUser        :: Maybe Domain.User
  } deriving (Show)


-- TextBy :: forall a. => (a -> Text) -> (Text -> a) -> TomlBiMap a AnyValue 
currencyCodec :: TomlBiMap Currency Toml.AnyValue
currencyCodec = Toml._TextBy showCurrency parseCurrency
  where
    showCurrency (Currency t) = t
    parseCurrency t           = Right $ Currency t

-- | The codec for the @AppSettings@ type.
-- | Toml.arrayOf :: TomlBiMap a -> Text -> TomlCodec [a]
appSettingsCodec :: TomlCodec AppSettings
appSettingsCodec =
  AppSettings <$> Toml.text "appName" .= appName <*>
  Toml.text "apiKey" .= apiKey <*>
  Toml.arrayOf currencyCodec "supportedCurrency" .= supportedCurrency

-- | Loads the @config.toml@ file.
-- loadConfig :: MonadIO m => m AppSettings
loadConfig :: MonadIO m => m AppSettings
loadConfig = do
  tomlResult <-
    liftIO $ Toml.decodeFileEither appSettingsCodec "app-settings.toml"
  case tomlResult of
    Left errors ->
      liftIO $ do
        putStrLn "Failed to parse application settings"
        print errors -- Print the actual errors for better debugging
        error "Failed to parse application settings"
    Right settings -> do
      let currencies = supportedCurrency settings
      liftIO $ print currencies
      let pairs = buildSupportedCurrencyPairs currencies
      let updatedSettings = settings {supportedCurrency = pairs}
      liftIO $ print updatedSettings
      return settings

buildSupportedCurrencyPairs :: [Currency] -> [Currency]
buildSupportedCurrencyPairs currencies =
  [ Currency (currencyToText c1 <> "/" <> currencyToText c2)
  | c1 <- currencies
  , c2 <- currencies
  , c1 /= c2
  ]
  where
    currencyToText (Currency t) = t
