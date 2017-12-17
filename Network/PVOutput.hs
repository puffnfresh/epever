{-# LANGUAGE OverloadedStrings #-}

module Network.PVOutput where

import           Control.Lens
import           Control.Monad
import qualified Data.ByteString             as BS
import           Data.ByteString.Strict.Lens
import           Data.Time
import           Energy.Types
import           Network.Wreq                hiding (Status)
import           System.Environment

data Status
  = Status { _statusGenerated  :: WattHours
           , _statusConsumed   :: WattHours
           , _statusVoltage    :: Volts
           , _statusInputPower :: Watts
           , _statusLoadPower  :: Watts
           }
  deriving (Eq, Ord, Show)

statusGenerated :: Lens' Status WattHours
statusGenerated =
  lens _statusGenerated (\(Status _ b c d e) a -> Status a b c d e)

statusConsumed :: Lens' Status WattHours
statusConsumed =
  lens _statusConsumed (\(Status a _ c d e) b -> Status a b c d e)

statusVoltage :: Lens' Status Volts
statusVoltage =
  lens _statusVoltage (\(Status a b _ d e) c -> Status a b c d e)

statusInputPower :: Lens' Status Watts
statusInputPower =
  lens _statusInputPower (\(Status a b c _ e) d -> Status a b c d e)

statusLoadPower :: Lens' Status Watts
statusLoadPower =
  lens _statusLoadPower (\(Status a b c d _) e -> Status a b c d e)

systemId :: Int
systemId =
  55808

csvDateTime :: IO (String, String)
csvDateTime =
  (\s -> (date s, time s)) <$> getZonedTime
  where
    f =
      formatTime defaultTimeLocale
    date =
      f "%Y%m%d"
    time =
      f "%H:%M"

sendStatus :: Status -> IO ()
sendStatus o = do
  apiKey <- getEnv "PVOUTPUT_API_KEY"
  (d, t) <- csvDateTime
  let body = [ "d" := (d ^. packedChars)
             , "t" := (t ^. packedChars)
             , "v1" := (o ^. statusGenerated . _WattHours . to show)
             , "v2" := (o ^. statusInputPower . _Watts . to show)
             , "v3" := (o ^. statusConsumed . _WattHours . to show)
             , "v4" := (o ^. statusLoadPower . _Watts . to show)
             , "v6" := (o ^. statusVoltage . _Volts . to show)
             ]
      options = defaults
        & headers .~ [ ("X-Pvoutput-Apikey", apiKey ^. packedChars)
                     , ("X-Pvoutput-SystemId", show systemId ^. packedChars)
                     ]
  void $ postWith options "https://pvoutput.org/service/r2/addstatus.jsp" body
