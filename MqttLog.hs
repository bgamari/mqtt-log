{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MqttLog where

import Data.String
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Time
import qualified Network.MQTT as MQTT
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.FilePath

data MessageT f
    = Message { _msgId :: Columnar f Int
              , _msgTime :: Columnar f LocalTime
              , _msgTopic :: Columnar f T.Text
              , _msgPayload :: Columnar f BS.ByteString
              }
    deriving (Generic)

instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (Columnar f Int) deriving Generic
    primaryKey = MessageId . _msgId
instance Beamable MessageT
instance Beamable (PrimaryKey MessageT)
type MessageId = PrimaryKey MessageT Identity
type Message = MessageT Identity
deriving instance Show Message

data LogDb f = LogDb { _logdbMessages :: f (TableEntity MessageT) }
             deriving (Generic)
instance Database be LogDb

logDb :: DatabaseSettings be LogDb
logDb = defaultDbSettings

data Options = Options { optDebug    :: Bool
                       , optHostname :: String
                       , optUsername :: Maybe T.Text
                       , optPassword :: Maybe T.Text
                       , optDatabase :: FilePath
                       , optTopics   :: [String]
                       }

run :: Options -> IO ()
run Options{..} = do
    chan <- newTChanIO
    cmds <- MQTT.mkCommands
    let config = (MQTT.defaultConfig cmds chan) { MQTT.cHost = "ben-server.local"
                                                , MQTT.cLogDebug = if optDebug then putStrLn else const $ return ()
                                                , MQTT.cClientID = "mqtt-log"
                                                , MQTT.cUsername = optUsername
                                                , MQTT.cPassword = optPassword
                                                }

    mqtt <- async $ void $ MQTT.run config >>= print
    MQTT.subscribe config [ (fromString topic, MQTT.Handshake) | topic <- optTopics ]

    conn <- open optDatabase
    forever $ do
        MQTT.Message _ msgBody <- atomically $ readTChan chan
        MQTT.Publish topic msgId payload <- pure msgBody
        t <- getZonedTime
        runBeamSqliteDebug putStrLn conn $ runInsert
            $ insert (_logdbMessages logDb)
            $ insertExpressions [ Message { _msgId = default_
                                          , _msgTime = val_ (zonedTimeToLocalTime t)
                                          , _msgTopic = val_ (MQTT.text $ MQTT.fromTopic topic)
                                          , _msgPayload = val_ payload
                                          } ]
