import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
--import MyEwmhDesktops
import XMonad.Hooks.EwmhDesktops
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified Data.Map as M
import Control.Monad
import Data.Maybe

myManageHook = composeAll 
  [ className =? "Squeak" --> doFloat
  , className =? "Plugin-container" --> doFloat
  -- , title =? "TIS-100" --> doIgnore
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  ]

layout = tiled ||| three ||| noBorders Full
  where
    tiled   = Tall nmaster delta (1/2)
    three   = ThreeCol nmaster delta (1/3)
    nmaster = 1
    delta   = 3/100

main = do
  --xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh defaultConfig {
    terminal = "urxvt",
    manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks,
    -- <+> insertPosition Master Newer 
    layoutHook = smartBorders $ layout,
    --logHook = dynamicLogWithPP xmobarPP {
    --  ppOutput = hPutStrLn xmproc,
    --  ppTitle = xmobarColor "green" ""
    --  } <+> updatePointer (TowardsCentre 0.025 0.025),
    --logHook = updatePointer (TowardsCentre 0.025 0.025),
    borderWidth        = 4,
    normalBorderColor  = "#383838",
    focusedBorderColor = "#6F6F6F",
    keys = myKeys <+> keys defaultConfig,
    modMask = mod4Mask,
    handleEventHook = fullscreenEventHook <+> docksEventHook,
    startupHook = do
        setDefaultCursor xC_left_ptr
        addEWMHFullscreen
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList [
  ((modm, xK_F12   ), xmonadPrompt defaultXPConfig),
  ((modm, xK_r     ), shellPrompt  defaultXPConfig),
  ((modm, xK_o     ), spawn "sleep 0.1; $HOME/bin/cycle-res"),
  ((modm, xK_Escape), toggleWS),
  ((modm .|. controlMask .|. shiftMask, xK_l), spawn "xset s activate"),
  ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+ unmute"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%- unmute"),
  ((0, xF86XK_AudioMicMute), spawn "amixer -q set Capture toggle"),
  ((0, xF86XK_MonBrightnessUp), spawn "$HOME/bin/backlight-adjust 2"),
  ((0, xF86XK_MonBrightnessDown), spawn "$HOME/bin/backlight-adjust -2"),
  ((modm, xK_F11), spawn "amixer -q set Master toggle"),
  ((modm, xK_F10), spawn "amixer -q set Master 5%+ unmute"),
  ((modm, xK_F9), spawn "amixer -q set Master 5%- unmute")
  ]


addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]
