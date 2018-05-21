{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import MqttLog
import qualified Data.Text as T

options :: Parser Options
options =
    Options
      <$> switch (long "debug" <> help "Enable debug output")
      <*> option str (long "hostname" <> short 'H' <> help "Server hostname" <> value "localhost")
      <*> optional (option text (long "user" <> short 'u' <> help "User to connect as"))
      <*> optional (option text (long "password" <> short 'p' <> help "Password"))
  where
    text = T.pack <$> str

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options) mempty
    MqttLog.run opts
