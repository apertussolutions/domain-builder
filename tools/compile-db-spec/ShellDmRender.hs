module ShellDmRender where

import DeviceModelSettings
import Data.List
import Data.Monoid
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import DeviceNumber
import MonadLib

type Gather = WriterT (Endo [(String,String)]) Id

(=:) :: String -> String -> Gather ()
key =: val = put (Endo ((key,val):))

(=?:) :: String -> Maybe String -> Gather ()
key =?: val = traverse_ (key =:) val

infix 0 =:, =?:

gather :: Gather () -> [(String,String)]
gather m = let ((), f) = runId (runWriterT m) in appEndo f []

renderForShell :: DMSettings -> String
renderForShell settings = unlines $ map (uncurry assign) $ gather $ do
  "VRAM_MB" =: show (dmVramMb settings)
  "VBD_NUM" =: show (length (vbds settings))
  "VIF_NUM" =: show (length (dmVifs settings))
  "BOOT_DR" =: dmBoot settings

  zipWithM_ vbdEntry [1..] (vbds settings)
  zipWithM_ vifEntry [1..] (dmVifs settings)
  traverse_ vncForShell (dmVnc settings)

  "HVM"    =?: if dmHvm    settings then Just "1" else Nothing

  "TPM"    =?: if dmTpm    settings then Just "1" else Nothing
  "VTPM"   =?: if dmVtpm   settings then Just "1" else Nothing
  "TPMMGR" =?: if dmTpmMgr settings then Just "1" else Nothing

option False _    = return ()
option True  name = name =: "1"

vncForShell vnc = do
  "VNC_FLAG"   =: "-vnc "++fromMaybe "localhost" (vncListen vnc)++":"++fromMaybe "0" (vncDisplay vnc)
  "VNC_UNUSED" =: if vncUnused vnc then "-vncunused" else ""
  "VNC_PASSWD" =: fromMaybe "" (vncPasswd vnc)
  
vbdEntry i vbd = do
  let aux name = concat ["VBD_", name, "[", show i, "]"]
  aux "VIRTUAL_DEVICE" =: maybe (error "Bad device name") show (computeDeviceNumber =<< computeDeviceName (vbdDev vbd))
  aux "DEVICE_TYPE"    =: showDeviceType (vbdType vbd)
  aux "DEV"            =: vbdDev vbd
  aux "BOOTABLE"       =: "1" -- bootable?
  aux "MODE"           =: showDeviceMode (vbdMode vbd)
  aux "TYPE"           =: showPhysType (vbdDeviceType vbd)
  aux "PARAMS"         =: showPhysParams (vbdDeviceType vbd)

vifEntry i vif = do
  let aux name = concat ["VIF_", name, "[", show i, "]"]
  aux "TYPE"    =?: vifType    vif
  aux "MODEL"   =?: vifModel   vif
  aux "NAME"    =?: vifName    vif
  aux "MAC"     =?: vifMac     vif
  aux "BRIDGE"  =?: vifBridge  vif
  aux "IP"      =?: vifIP      vif
  aux "SCRIPT"  =?: vifScript  vif
  aux "BACKEND" =?: fmap show (vifBackend vif)

assign key val = key ++ "=" ++ shellShow val

shellShow xs = '"' : foldr escape "\"" xs
  where
  escape '"'  xs = '\\':'"':xs
  escape '\n' xs = '\\':'n':xs
  escape '\\' xs = '\\':'\\':xs
  escape c    xs = c       :xs
