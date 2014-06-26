import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.StackSet as W (focusUp, focusDown, sink)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified Data.Map as M

myManageHook = composeAll $
  [ className =? "Squeak" --> doFloat
  , className =? "Plugin-container" --> doFloat
  , className =? "Xfce4-notifyd" --> doF W.focusDown
  , className =? "KeePass2" --> doFloat
  , className =? "keepass2" --> doFloat
  , title =? "Pick Characters - KeePass" --> doFloat
  , title =? "KeePassHttp: Confirm Access" --> doFloat
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  ]

myLogHook xmproc = do
  -- dynamicLogWithPP xmobarPP {
  --   ppOutput = hPutStrLn xmproc,
  --   ppTitle = xmobarColor "green" ""
  --   }
  updatePointer (TowardsCentre 0.025 0.025)
  return ()

main = do
  xmproc <- return () -- spawnPipe "xmobar"
  xmonad $ ewmh defaultConfig {
    manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks,
    layoutHook = smartBorders $ layoutHook defaultConfig,
    logHook = myLogHook xmproc,
    borderWidth        = 2,
    normalBorderColor  = "#383838",
    focusedBorderColor = "#6F6F6F",
    keys = myKeys <+> keys defaultConfig,
    modMask = mod4Mask,
    startupHook = do
        setDefaultCursor xC_left_ptr
        setWMName "LG3D"
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList [
  ((modm, xK_F12   ), xmonadPrompt defaultXPConfig),
  ((modm, xK_r     ), shellPrompt  defaultXPConfig),
  ((modm, xK_v     ), spawn "keepass --auto-type"),
  ((modm, xK_Escape), toggleWS),
  ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+ unmute"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%- unmute")
  ]
