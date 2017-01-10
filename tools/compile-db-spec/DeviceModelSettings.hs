module DeviceModelSettings where

import Data.Word (Word64)

data DMSettings = DMSettings
  { vbds :: [VBD]
  , dmVifs   :: [VIF]
  , dmDeviceModel :: Maybe FilePath
  , dmHvm    :: Bool
  , dmSdl    :: Bool
  , dmTpm    :: Bool
  , dmVtpm   :: Bool
  , dmTpmMgr :: Bool
  , dmOpengl :: Bool
  , dmVnc    :: Maybe VNCSettings
  , dmStdvga :: Bool
  , dmSerial :: Maybe String
  , dmName   :: Maybe String
  , dmBuilder :: Maybe String
  , dmBoot   :: String
  , dmVramMb :: Word64
  }
  deriving (Show)

data VNCSettings = VNCSettings
  { vncUnused  :: Bool
  , vncListen  :: Maybe String
  , vncDisplay :: Maybe String
  , vncPasswd  :: Maybe String
  , vncConsole :: Bool
  }
  deriving (Show)

data VIF = VIF
  { vifType    :: Maybe String
  , vifModel   :: Maybe String
  , vifName    :: Maybe String
  , vifMac     :: Maybe String
  , vifBridge  :: Maybe String
  , vifIP      :: Maybe String
  , vifScript  :: Maybe String
  , vifBackend :: Maybe Int
  }
  deriving (Show)

emptyVif = VIF Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data VBD = VBD
  { vbdDeviceType :: DevicePhysType
  , vbdDev        :: String
  , vbdMode       :: DeviceMode
  , vbdType       :: DeviceType
  }
  deriving (Show)

data DeviceType
  = Disk
  | Cdrom
  deriving (Show)

data DeviceMode
  = ReadOnlyDeviceMode
  | ReadWriteDeviceMode
  deriving (Show)

data DevicePhysType
  = FileDevice FilePath
  deriving (Show)

vbdToEntries :: VBD -> [String]
vbdToEntries vbd =
  [ showDeviceType (vbdType vbd)
  , vbdDev vbd
  , "1" -- bootable?
  , showDeviceMode (vbdMode vbd)
  , showPhysType   (vbdDeviceType vbd)
  , showPhysParams (vbdDeviceType vbd)
  ]

showDeviceType Cdrom = "cdrom"
showDeviceType Disk  = "disk"

showPhysType (FileDevice _) = "file"

showPhysParams (FileDevice fp) = fp

showDeviceMode ReadOnlyDeviceMode = "r"
showDeviceMode ReadWriteDeviceMode = "w"

{-
DOMID=$1
XS_0=/local/domain/0/backend/vbd/$DOMID
XS_PATH=/local/domain/$DOMID
DEV=1
XS_FRONT=$XS_PATH/device/vbd/$DEV
XS_BACK=$XS_0/$DEV

sudo xenstore-write $XS_FRONT/backend-id      "0"
sudo xenstore-write $XS_FRONT/virtual-device  "$DEV"
sudo xenstore-write $XS_FRONT/device-type     "cdrom"
sudo xenstore-write $XS_FRONT/backend         "$XS_BACK"

sudo xenstore-write $XS_BACK/frontend-id      "$DOMID"
sudo xenstore-write $XS_BACK/frontend         "$XS_FRONT"
sudo xenstore-write $XS_BACK/dev              "hdb"
sudo xenstore-write $XS_BACK/bootable         "1"
sudo xenstore-write $XS_BACK/mode             "r"
sudo xenstore-write $XS_BACK/type             "file"
sudo xenstore-write $XS_BACK/params           "/home/emertens/windows-xp-sp2.iso"
-}
