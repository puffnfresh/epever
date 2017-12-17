module Energy.Types where

import           Control.Lens
import           Data.Word

data Volts
  = Volts Double
  deriving (Eq, Ord)

instance Show Volts where
  show (Volts a) =
    show a ++ "v"

_Volts :: Iso' Volts Double
_Volts =
  iso (\(Volts w) -> w) Volts

data Watts
  = Watts Double
  deriving (Eq, Ord)

instance Show Watts where
  show (Watts a) =
    show a ++ "w"

_Watts :: Iso' Watts Double
_Watts =
  iso (\(Watts w) -> w) Watts

data WattHours
  = WattHours Word32
  deriving (Eq, Ord)

instance Show WattHours where
  show (WattHours a) =
    show a ++ "wh"

_WattHours :: Iso' WattHours Word32
_WattHours =
  iso (\(WattHours w) -> w) WattHours
