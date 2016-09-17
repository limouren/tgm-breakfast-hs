{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad.Trans
import           Data.Char                   (isSpace)
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           GHC.Generics
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           System.Envy
import           Web.Spock
import           Web.Spock.Config
import qualified Web.Telegram.API.Bot        as TGM

data BKConfig = BKConfig {
    port           :: Int -- "PORT"
  , tgmbkToken     :: T.Text -- "TGMBK_TOKEN"
  , tgmbkLocations :: [T.Text] -- "TGMBK_LOCATIONS"
  , tgmbkLocation404Msg :: T.Text -- "TGMBK_LOCATION_404_MSG"
  } deriving (Generic, Show)

instance DefConfig BKConfig where
  defConfig = BKConfig 8080 "" ["Mon", "Tue", "Wed", "Thu", "Fri"] "No breakfast for you (≧∇≦)b"

instance FromEnv BKConfig

instance System.Envy.Var [T.Text] where
  toVar =  T.unpack . T.intercalate ", "
  fromVar = Just . (map T.strip) . (T.splitOn ",") . T.pack

data MySession = EmptySession
data MyAppState = MyAppState BKConfig

main :: IO ()
main =
    do (Right cfg) <- decodeEnv :: IO (Either String BKConfig)
       runApp cfg

runApp :: BKConfig -> IO ()
runApp cfg =
    do let path = T.unpack . tgmbkToken $ cfg
       let port' = port cfg
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (MyAppState cfg)
       runSpock port' (spock spockCfg (app path))

app :: String -> SpockM () MySession MyAppState ()
app pathToListen =
    do get root $
           text ""
       post (static pathToListen) $
            do (MyAppState (BKConfig {
                    tgmbkToken=token
                  , tgmbkLocations=locations
                  , tgmbkLocation404Msg=location404Msg
                  })) <- getState
               TGM.Update { TGM.message=Just m } <- jsonBody'
               dayOfWeek <- liftIO $ localDayOfWeek <$> getCurrentTime
               let messageText = case locations `safeIndex` (dayOfWeek - 1) of
                                   Just x -> x
                                   Nothing -> location404Msg

               let chatID = TGM.chat_id . TGM.chat $ m
               let messageID = TGM.message_id m

               let request = TGM.SendMessageRequest (showT chatID) messageText Nothing Nothing Nothing (Just messageID) Nothing
               resp <- liftIO $ doSendMessage (TGM.Token token) request
               case resp of
                   Left e -> do
                     liftIO $ putStrLn "Request failed"
                   Right (TGM.Response m) -> do
                     liftIO $ putStrLn "Request succeded"

               text ""

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < length xs = Just $ xs !! i
    | otherwise     = Nothing

hkt :: TimeZone
hkt = TimeZone 480 False "HKT"

localDayOfWeek :: UTCTime -> Int
localDayOfWeek = (\(_, _, dayOfWeek) -> dayOfWeek)
               . toWeekDate
               . localDay
               . utcToLocalTime hkt

doSendMessage token request = do
    manager <- newManager tlsManagerSettings
    TGM.sendMessage token request manager

showT :: Show a => a -> T.Text
showT = T.pack . show

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
