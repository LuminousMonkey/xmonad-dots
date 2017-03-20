import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Config.Desktop
import System.IO



-- Defaults on which we build
myBaseConfig = defaultConfig

-- Display
myBorderWidth = 3
myNormalBorderColour = "#404040"
myFocusedBorderColour = "#A54242"

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["[1:Emacs]", "[2:Web]", "[3:Term]", "[4:Reference]", "[5:VMWare]", "[6:Game]", "[7:Music]", "[8:Other]"]

-- Layouts
basicLayout = Tall nmaster delta ratio where
  nmaster = 1
  delta = 3/100
  ratio = 1/2
tallLayout = named "tall" $ avoidStruts $ smartBorders $ basicLayout
webLayout = named "web" $ avoidStruts $ smartBorders $ Tall 1 (3/100) (62/100)
wideLayout = named "wide" $ avoidStruts $ smartBorders $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ smartBorders $ noBorders Full

-- Make Java apps work better
myStartupHook = startupHook myBaseConfig >> setWMName "LG3D" >> takeTopFocus

myLayoutHook = web $ normal where
  normal = tallLayout ||| wideLayout ||| singleLayout
  web = onWorkspace "[2:Web]" (webLayout ||| wideLayout ||| singleLayout)

-- Special treatment for specific windows:
-- Put browsers in the web workspace
-- Put initial terminals in the terms workspace
myManageHook = webManageHooks <+> termManageHooks <+> musicManageHooks <+> manageHook myBaseConfig

webManageHooks = composeAll [isWeb --> moveToWeb] where
    isWeb     = foldr1 (<||>) [isBrowser]
    isBrowser = className =? "Google-chrome"
    moveToWeb = doF $ S.shift "[2:Web]"

termManageHooks = isInitTerm --> moveToTerms where
    isInitTerm  = stringProperty "WM_WINDOW_ROLE" =? "initTerm"
    moveToTerms = doF $ S.shift "[3:Term]"

musicManageHooks = isMusic --> moveToMusic where
    isMusic     = stringProperty "WM_WINDOW_ROLE" =? "ncmpcpp" <||> className =? "Spotify"
    moveToMusic = doF $ S.shift "[7:Music]"

myEventHook = do
  ewmhDesktopsEventHook
  docksEventHook

-- Main
main :: IO ()
main = do
  workspaceBar <- spawnPipe myWorkspaceBar
  topStatusBar <- spawnPipe myTopStatusBar
  xmonad $ desktopConfig {
    manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig,
    layoutHook = myLayoutHook
    , handleEventHook = myEventHook
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    , focusFollowsMouse = True
    , terminal = "terminator"
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColour
    , focusedBorderColor = myFocusedBorderColour
    , logHook = myLogHook workspaceBar >> fadeInactiveLogHook 0xdddddddd
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    } `additionalKeys`
    [ ((mod4Mask, xK_p), spawn "exec=`dmenu_run -h 20 -fn \"Noto Sans\"` && eval \"exec $exec\"") ]
--------------------------------------------------------------------------------------------
-- APPEARANCE CONFIG                                                                      --
--------------------------------------------------------------------------------------------

-- Colors and fonts
myFont               = "Noto Sans:size=12"
dzenFont             = "Noto Sans:size=12"
colorBlack           = "#1a1a1a" --Background (Dzen_BG)
colorBlackAlt        = "#404040" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#808080" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#87afd7"
colorYellow          = "#FFE863"
colorRed             = "#d75f5f"
colorGreen           = "#BDE077"
myArrow              = "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"

-- StatusBars
myWorkspaceBar, myTopStatusBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '0' -h '20' -w '1500' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e ''"
myTopStatusBar    = "conky -c /home/aldredmr/.conkydzentop | dzen2 -x '920' -y '0' -h '20' -w '1540' -fg '" ++colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ dzenFont ++ "' -ta r -e ''"

myBitmapsDir = "/home/aldredmr/.xmonad/dzen"

-- Layout Hook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  {
    ppCurrent = dzenColor "#ebac54" "#1b1d1e" . pad
    , ppVisible = dzenColor "white" "#1b1d1e" . pad
    , ppHidden = dzenColor "white" "#1b1d1e" . pad
    , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
    , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
    , ppWsSep             =   " "
    , ppSep               =   "  |  "
    , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                              (\x -> case x of
                                "ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                "Mirror ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                "Full" -> "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                "Simple Float" -> "~"
                                _ -> x
                              )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
  }
