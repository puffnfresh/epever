module Main where

import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.ByteString.Lazy       hiding (putStrLn)
import qualified Data.ByteString.Lazy       as BS hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.ByteString.Lens
import           Data.Digest.CRC16
import           Data.Foldable
import           Data.Halves
import           Data.Semigroup
import           Data.Tickle
import           Data.Word
import           System.Hardware.Serialport

newtype UnitId
  = UnitId Word8
  deriving Show

data FunctionCode
   = ReadCoils
   | ReadDiscreteInputs
   | ReadHoldingRegisters
   | ReadInputRegisters
   | WriteSingleCoil
   | WriteSingleRegister
   | ReadExceptionStatus
   | Diagnostics
   | GetCommEventCounter
   | GetCommEventLog
   | WriteMultipleCoils
   | WriteMultipleRegisters
   | ReportSlaveID
   | ReadFileRecord
   | WriteFileRecord
   | MaskWriteRegister
   | ReadWriteMultipleRegisters
   | ReadFIFOQueue
   | EncapsulatedInterfaceTransport
   | UserDefinedCode Word8
   | ReservedCode Word8
   | OtherCode Word8
   | ExceptionCode FunctionCode
     deriving (Eq, Show)

data ModbusRequest
  = ReadInputRegistersRequest Register
  | EncapsulatedInterfaceTransportRequest
  deriving Show

data ModbusResponse
  -- = ReadInputRegistersResponse [Word16]
  = ReadInputRegistersResponse ByteString
  | EncapsulatedInterfaceTransportResponse [ByteString]
  deriving Show

readInputRegistersResponse :: Prism' ModbusResponse ByteString
readInputRegistersResponse =
  prism' ReadInputRegistersResponse f
  where
    f (ReadInputRegistersResponse a) = Just a
    f _ = Nothing

getFunctionCode :: Get () FunctionCode
getFunctionCode =
  word8 >>= f
  where
    f 0x4  = pure ReadInputRegisters
    f 0x2B = pure EncapsulatedInterfaceTransport
    f _    = failGet ()

data RtuRequest
  = RtuRequest UnitId ModbusRequest
  deriving Show

data RtuResponse
  = RtuResponse UnitId ModbusResponse
  deriving Show

rtuResponseModbusResponse :: Traversal' RtuResponse ModbusResponse
rtuResponseModbusResponse f (RtuResponse a b) =
  RtuResponse a <$> f b

putRtuRequest :: RtuRequest -> ByteString
putRtuRequest request =
  let x = putRtuRequest' request
  in x <> putWord16le (crc16 (unpack x))

putRtuRequest' :: RtuRequest -> ByteString
putRtuRequest' (RtuRequest unitId request) =
  putUnitId unitId <> putRequest request

putWord16le :: Word16 -> ByteString
putWord16le w =
  singleton (fromIntegral w) <> singleton (fromIntegral (shiftR w 8))

putWord16be :: Word16 -> ByteString
putWord16be w =
  BS.reverse $ putWord16le w

putUnitId :: UnitId -> ByteString
putUnitId (UnitId i) =
  singleton i

putRequest :: ModbusRequest -> ByteString
putRequest (ReadInputRegistersRequest r) =
  pack [0x4] <> putRegister r
putRequest EncapsulatedInterfaceTransportRequest =
  pack [0x2B, 0xe, 0x1, 0x0]

putRegister (Register (Address a) (RegisterCount rc)) =
  putWord16be a <> putWord16be rc

getUnitId :: Get () UnitId
getUnitId =
  UnitId <$> word8

getRtuResponse :: Get () RtuResponse
getRtuResponse =
  RtuResponse <$> getUnitId <*> do
    code <- getFunctionCode
    getResponse code

word16 :: Get () Word16
word16 = do
  a <- word8
  b <- word8
  pure $ shiftL (fromIntegral a) 8 .|. fromIntegral b

getResponse :: FunctionCode -> Get () ModbusResponse
getResponse ReadInputRegisters = do
  i <- word8
  -- ReadInputRegistersResponse <$> replicateM (fromIntegral i `div` 2) word16
  ReadInputRegistersResponse <$> lazyByteString (fromIntegral i)
getResponse EncapsulatedInterfaceTransport = do
  skip 5
  byteCount <- word8
  EncapsulatedInterfaceTransportResponse <$> (replicateM (fromIntegral byteCount) $ do
    skip 1
    i <- word8
    lazyByteString $ fromIntegral i)
getResponse _ =
  failGet ()

run
  :: SerialPort -> ModbusRequest -> IO (RunGetResult () RtuResponse)
run h rq = do
  send h . toStrict . putRtuRequest $ RtuRequest (UnitId 1) rq
  r <- recv h 1024
  pure . runGet getRtuResponse $ fromStrict r

encapsulatedInterfaceTransport
  :: SerialPort -> IO (RunGetResult () RtuResponse)
encapsulatedInterfaceTransport =
  flip run EncapsulatedInterfaceTransportRequest

newtype Address
  = Address Word16
  deriving Show

newtype RegisterCount
  = RegisterCount Word16
  deriving Show

data Register
  = Register Address RegisterCount
  deriving Show

chargingEquipmentRatedInputVoltage :: Address
chargingEquipmentRatedInputVoltage =
  Address 0x3000

chargingEquipmentInputVoltage :: Address
chargingEquipmentInputVoltage =
  Address 0x3100

chargingEquipmentInputPower :: Address
chargingEquipmentInputPower =
  Address 0x3102

dischargingEquipmentOutputPower :: Address
dischargingEquipmentOutputPower =
  Address 0x310E

totalConsumedEnergy :: Address
totalConsumedEnergy =
  Address 0x330A

totalGeneratedEnergy :: Address
totalGeneratedEnergy =
  Address 0x3312

generatedEnergyToday :: Address
generatedEnergyToday =
  Address 0x330C

consumedEnergyToday :: Address
consumedEnergyToday =
  Address 0x3304

batterySOC :: Address
batterySOC =
  Address 0x311A

batteryCapacity :: Address
batteryCapacity =
  Address 0x9001

port :: [Char]
port =
  "/dev/ttyXRUSB0"

onlyOne :: Traversal' ([a], [b]) a
onlyOne f ([a], []) =
  (\a' -> ([a'], [])) <$> f a
onlyOne _ xs =
  pure xs

onlyWord32 :: Traversal' [Word8] Word32
onlyWord32 =
  chunkQuarters . onlyOne

onlyWord16 :: Traversal' [Word8] Word16
onlyWord16 =
  chunkHalves . onlyOne

rtuReadInputRegistersBytes :: Traversal' (RunGetResult a RtuResponse) [Word8]
rtuReadInputRegistersBytes =
  _RunGet . rtuResponseModbusResponse . readInputRegistersResponse . unpackedBytes

rtuReadInputRegistersWord32 :: Traversal' (RunGetResult a RtuResponse) Word32
rtuReadInputRegistersWord32 =
  rtuReadInputRegistersBytes . onlyWord32

rtuReadInputRegistersWord16 :: Traversal' (RunGetResult a RtuResponse) Word16
rtuReadInputRegistersWord16 =
  rtuReadInputRegistersBytes . onlyWord16

rtuReadInputRegistersWatts :: Fold (RunGetResult a RtuResponse) Watts
rtuReadInputRegistersWatts =
  rtuReadInputRegistersWord32 . to watts

rtuReadInputRegistersWattHours :: Fold (RunGetResult a RtuResponse) WattHours
rtuReadInputRegistersWattHours =
  rtuReadInputRegistersWord32 . to wattHours

rtuReadInputRegistersPercent :: Fold (RunGetResult a RtuResponse) Percent
rtuReadInputRegistersPercent =
  rtuReadInputRegistersWord16 . re _Percent

data Watts
  = Watts Double
  deriving (Eq, Ord)

instance Show Watts where
  show (Watts a) =
    show a ++ "w"

watts :: Word32 -> Watts
watts a =
  Watts $ (fromIntegral (a ^. swappedHalves) :: Double) / 100

data WattHours
  = WattHours Word32
  deriving (Eq, Ord)

instance Show WattHours where
  show (WattHours a) =
    show a ++ "wh"

wattHours :: Word32 -> WattHours
wattHours a =
  WattHours $ (a ^. swappedHalves) * 10

data Percent
  = Percent Word16
  deriving (Eq, Ord)

instance Show Percent where
  show (Percent a) =
    show a ++ "%"

_Percent :: Iso' Percent Word16
_Percent =
  iso (\(Percent x1_0) -> x1_0) (\x1_1 -> Percent x1_1)

readAddressWatts :: Address -> SerialPort -> IO (Maybe Watts)
readAddressWatts r h =
  let request = ReadInputRegistersRequest (Register r (RegisterCount 2))
  in (^? rtuReadInputRegistersWatts) <$> run h request

readAddressWattHours :: Address -> SerialPort -> IO (Maybe WattHours)
readAddressWattHours r h =
  let request = ReadInputRegistersRequest (Register r (RegisterCount 2))
  in (^? rtuReadInputRegistersWattHours) <$> run h request

readAddressPercent :: Address -> SerialPort -> IO (Maybe Percent)
readAddressPercent r h =
  let request = ReadInputRegistersRequest (Register r (RegisterCount 1))
  in (^? rtuReadInputRegistersPercent) <$> run h request

readChargingEquipmentInputPower :: SerialPort -> IO (Maybe Watts)
readChargingEquipmentInputPower =
  readAddressWatts chargingEquipmentInputPower

readDischargingEquipmentOutputPower :: SerialPort -> IO (Maybe Watts)
readDischargingEquipmentOutputPower =
  readAddressWatts dischargingEquipmentOutputPower

readConsumedEnergyToday :: SerialPort -> IO (Maybe WattHours)
readConsumedEnergyToday =
  readAddressWattHours consumedEnergyToday

readGeneratedEnergyToday :: SerialPort -> IO (Maybe WattHours)
readGeneratedEnergyToday =
  readAddressWattHours generatedEnergyToday

readBatterySOC :: SerialPort -> IO (Maybe Percent)
readBatterySOC =
  readAddressPercent batterySOC

main :: IO ()
main = do
  h <- openSerial port (defaultSerialSettings { commSpeed = CS115200, timeout = 100 })
  -- result <- encapsulatedInterfaceTransport h
  -- for_ result $ \(RtuResponse _ (EncapsulatedInterfaceTransportResponse bs)) -> traverse_ BS.putStrLn bs

  -- putStrLn ""

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

  closeSerial h
