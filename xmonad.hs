import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Grid
import XMonad.Layout.Gaps
import Data.Ratio ((%))
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.GridSelect
import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Define workspaces
myWorkspaces = ["w1","w2","w3","web","mail","mus"]
-- Define layouts
layoutHook'  =  onWorkspaces ["w1","w2","w3"] stdLayout $
                onWorkspaces ["web","mail","mus"] webLayout $ customLayout2
                --onWorkspaces ["im"] chatLayout $ customLayout2

--chatLayout = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full 
--  where
--    tiled = ResizableTall 1 (2/100) (1/3)[]
--chatLayout =  avoidStruts $ withIM (1%6) (ClassName "Skype") Full 
--chatLayout =  avoidStruts $ withIM (1%6) rooster  Full 
  --where
    --rooster = And (ClassName "Skype") (Not (Role "ConversationsWindow"))
--(Not (Title "Options")))

--smartBorders removes the border from full screen apps.
--avoidStruts doesn't cover other layout elements such as status bar.
webLayout = gaps[(U, 25)] $ smartBorders $ Mirror tiled ||| tiled ||| Full
  where
    tiled = ResizableTall 1 (3/100) (3/4)[]

stdLayout = gaps[(U, 25)] $  tiled ||| Mirror tiled ||| Full
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

customLayout2 = avoidStruts $ Full ||| tiled ||| Mirror tiled
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

--define manageHook
--manageHood configures the behaviour of WM with respect to applications
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r --> doIgnore          | r <- myIgnores] -- ignore desktop
    , [className    =? c --> doShift  "web"    | c <- myWebs   ] -- move webs to main
    , [className    =? c --> doShift  "mail"   | c <- myMail   ] -- move webs to main
    , [className    =? c --> doShift  "mus"    | c <- myMusic  ] -- move music to music
    , [className    =? c --> doCenterFloat     | c <- myFloats ]
    ])

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        -- Plugin-container is used to avoid tiling Full-screend YouTube videos.
        myFloats  = ["MPlayer","Plugin-container","Vlc","vlc"]
        myWebs    = ["Firefox", "qutebrowser"]
        myChat    = ["Skype"]
        myMail    = ["Thunderbird"]
        myMusic   = ["Easytag"]

        -- resources
        myIgnores = ["trayer"]

-- Define logHook
myLogHook :: Handle -> X ()
--myLogHook = fadeInactiveLogHook fadeAmount where fadeAmount = 0.8  
--This is mainy dzen config. Please note that I use special icons for layouts.
myLogHook h = dynamicLogWithPP $ def 
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   " | "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

myXmonadBar = "dzen2 -x '0' -y '0' -h '25' -w '1100' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/robert/.xmonad/conky.conf | dzen2 -x '1100' -y '0' -w '820' -h '25' -ta 'right' -bg '#1B1D1E' -fg '#FFFFFF'"
myBitmapsDir = "/home/robert/.xmonad/icons"
main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ ewmh def
        {
        borderWidth = 1
        , terminal = "urxvt"
        , workspaces = myWorkspaces 
        --, keys = keys'
        --remaps the mod key to Meta key
        , modMask = mod4Mask
        , startupHook = setWMName "LG3D"
        , focusFollowsMouse = False
        , layoutHook = layoutHook'
        , manageHook = manageDocks <+> manageHook'
        , handleEventHook = docksEventHook <+> handleEventHook def
        , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0.8
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
--Rofi shortcuts modes: run, window switcher, ssh
        , ((mod4Mask, xK_p), spawn "rofi -show run")
        , ((mod4Mask, xK_o), spawn "rofi -show window")
        , ((mod4Mask, xK_s), spawn "rofi -show ssh")
        --, ((mod4Mask, xK_p), spawn "dmenu_run -fn xfs:inconsolata -nb black -nf grey -sb midnightblue -sf white")
        , ((mod4Mask, xK_Tab), goToSelected def)
--XF86AudioNext
        , ((0, 0x1008ff17), spawn "cmus-remote --next")
--XF86AudioPrev
        , ((0, 0x1008ff16), spawn "cmus-remote --prev")
--Alternative next
        , ((mod4Mask, xK_n), spawn "cmus-remote --next")
--Alternative prev
        , ((mod4Mask, xK_b), spawn "cmus-remote --prev")
--XF86AudioPlay
        , ((0, 0x1008ff14), spawn "cmus-remote --pause")
--AlternativePlay
        , ((mod4Mask, xK_c), spawn "cmus-remote --pause")
--XF86AudioRaiseVolume
        , ((0, 0x1008ff13), spawn "amixer set Master playback 2%+")
--XF86AudioLowerVolume
        , ((0, 0x1008ff11), spawn "amixer set Master playback 2%-")
--XF86AudioLowerVolume
        , ((mod4Mask, 0x2d), spawn "amixer set Master playback 2%-")
--XF86AudioRaiseVolume
        , ((mod4Mask, 0x3d), spawn "amixer set Master playback 2%+")
--ranger shortcut
        , ((mod4Mask, xK_r), spawn "urxvt -e zsh -ic ranger")
--mutt shortcut
        , ((mod4Mask, xK_m), spawn "urxvt -e zsh -ic mutt")
        ]
