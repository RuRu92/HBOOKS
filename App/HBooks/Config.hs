{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HBooks.Config
  ( loadConfig
  , AppSettings
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString
import           Data.Text
import           Prelude                as P
import           Toml                   (TomlBiMap, TomlCodec, (.=))

import qualified Toml

import           HBooks.Domain          (Role, User)

import qualified HBooks.Domain          as Domain

newtype Currency =
  Currency Text
  deriving (Eq, Show)

data AppSettings = AppSettings
  { appName           :: Text
  , apiKey            :: Text
  , supportedCurrency :: ![Text]
  -- , activeUser        :: Maybe Domain.User
  } deriving (Show)


currencyCodec :: TomlBiMap Currency Toml.AnyValue
currencyCodec = Toml._TextBy showCurrency parseCurrency
  where
    showCurrency (Currency t) = t
    parseCurrency t           = Right $ Currency t

currencyTomlCodec :: TomlCodec Currency
currencyTomlCodec = Toml.diwrap (Toml.text "supportedCurrency")

appSettingsCodec :: TomlCodec AppSettings
appSettingsCodec =
  AppSettings <$> Toml.text "appName" .= appName <*>
  Toml.text "apiKey" .= apiKey <*>
  -- Toml.list currencyTomlCodec "supportedCurrency" .= supportedCurrency
  -- Toml.list currencyTomlCodec "supportedCurrency" .= supportedCurrency
  Toml.arrayOf currencyTomlCodec "supportedCurrency" .= supportedCurrency

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
      -- let pairs = buildSupportedCurrencyPairs currencies
      -- let updatedSettings = settings {supportedCurrency = pairs}
      -- liftIO $ print updatedSettings
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
