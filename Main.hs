module Main where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Network.Epever
import           Network.PVOutput
import           System.Hardware.Serialport

getStatus :: SerialPort -> MaybeT IO Status
getStatus h = do
  g <- MaybeT $ readGeneratedEnergyToday h
  c <- MaybeT $ readConsumedEnergyToday h
  v <- MaybeT $ readChargingEquipmentInputVoltage h
  i <- MaybeT $ readChargingEquipmentInputPower h
  l <- MaybeT $ readDischargingEquipmentOutputPower h
  pure $ Status g c v i l

run :: IO ()
run = do
  h <- openSerial port (defaultSerialSettings { commSpeed = CS115200, timeout = 100 })
  -- result <- encapsulatedInterfaceTransport h
  -- for_ result $ \(RtuResponse _ (EncapsulatedInterfaceTransportResponse bs)) -> traverse_ BS.putStrLn bs

  -- putStrLn ""

  putStrLn "Input volts:"
  readChargingEquipmentInputVoltage h >>= traverse_ print

  putStrLn "Input power:"
  readChargingEquipmentInputPower h >>= traverse_ print

  putStrLn "Load:"
  readDischargingEquipmentOutputPower h >>= traverse_ print

  putStrLn "Consumed:"
  readConsumedEnergyToday h >>= traverse_ print

  putStrLn "Generated:"
  readGeneratedEnergyToday h >>= traverse_ print

  -- putStrLn "Battery:"
  -- readBatterySOC h >>= traverse_ print

  runMaybeT (getStatus h) >>= traverse_ sendStatus

  closeSerial h

main :: IO ()
main =
  forever $ do
    run
    threadDelay $ 1000 * 1000 * 60 * 5
