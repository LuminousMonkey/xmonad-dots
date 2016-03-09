import XMonad
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO

-- Main
main :: IO ()
main = do
  workspaceBar <- spawnPipe myWorkspaceBar
  topStatusBar <- spawnPipe myTopStatusBar
  xmonad $ defaultConfig {
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ layoutHook defaultConfig
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    , focusFollowsMouse = True
    , terminal = "urxvt"
    , borderWidth = 2
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook = myLogHook workspaceBar >> fadeInactiveLogHook 0xdddddddd
    , workspaces = myWorkspaces
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
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#A54242"

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["[1:Emacs]", "[2:Web]", "[3:Term]", "[4:Reference]", "[5:VMWare]", "[6:Game]", "[7:Music]", "[8:Other]"]

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
