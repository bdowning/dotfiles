import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified Data.Map as M

myManageHook = composeAll 
  [ className =? "Squeak" --> doFloat
  , className =? "Plugin-container" --> doFloat
  , isDialog --> doCenterFloat
  , isFullscreen --> doFullFloat
  ]

layout = avoidStruts (tiled ||| three ||| Mirror tiled ||| Full)
  where
     tiled   = Tall nmaster delta (1/2)
     three   = ThreeCol nmaster delta (1/3)
     nmaster = 1
     delta   = 3/100

myEwmh x = x

main = do
  -- dzenproc <- spawnPipe "dzen2 -fn 'DejaVu Sans Mono:pixelsize=12' -ta l -e 'onstart=lower'"
  xmonad $ myEwmh defaultConfig {
    terminal = "xterm",
    manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
    -- <+> insertPosition Master Newer 
    layoutHook = smartBorders $ layout,
    --logHook = dynamicLogWithPP xmobarPP {
    --  ppOutput = hPutStrLn xmproc,
    --  ppTitle = xmobarColor "green" ""
    --  } <+> updatePointer (TowardsCentre 0.025 0.025),
    logHook = -- dynamicLogWithPP dzenPP {
    --   ppOutput = hPutStrLn dzenproc
    -- } <+>
              updatePointer (TowardsCentre 0.025 0.025),
    borderWidth        = 2,
    normalBorderColor  = "#383838",
    focusedBorderColor = "#6F6F6F",
    keys = myKeys <+> keys defaultConfig,
    modMask = mod4Mask,
    handleEventHook = docksEventHook,
    startupHook = setDefaultCursor xC_left_ptr
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList [
  ((modm, xK_F12   ), xmonadPrompt defaultXPConfig),
  ((modm, xK_r     ), shellPrompt  defaultXPConfig),
  ((modm, xK_z     ), sendMessage ToggleStruts),
  ((modm, xK_Escape), toggleWS),
  ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+ unmute"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%- unmute")
  ]
