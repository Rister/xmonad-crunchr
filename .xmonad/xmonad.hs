-- import XMonad.Layout.Grid
-- import XMonad.Layout.ResizableTile
-- import qualified Data.Text as DT

import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.Circle
import XMonad.Layout.HintedGrid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myModMask = mod4Mask -- set mod key to the windows (super) key

myFocusedBorderColor = "#FF00FF"

myNormalBorderColor = "#00FF00"

myBorderWidth = 2

myGap = 2

myBorder = Border myGap myGap myGap myGap

myTerminal = "uxterm"

myWorkspaces =
  [ "web",
    "zk",
    "coms",
    "term",
    "5",
    "6",
    "7",
    "8",
    "9:DUMP"
  ]

mySpacing = spacingRaw True myBorder False myBorder True

myLayoutHook =
  onWorkspace "web" myDefaultLayouts $
    onWorkspace "zk" myZKLayouts $
      onWorkspace "coms" myComsLayouts $
        onWorkspace "term" myTermLayouts $
          onWorkspaces (map show [5 .. 8]) myDefaultLayouts $
            onWorkspace "9:DUMP" myDefaultLayouts $
              smartBorders (layoutHook def)
  where
    myDefaultLayouts =
      avoidStruts $
        mySpacing (Grid False)
          ||| mySpacing (Grid True)
          ||| mySpacing (smartBorders (ThreeColMid 1 (3 / 100) (3 / 4)))
          ||| noBorders (Circle)
          ||| noBorders (Full)
    myComsLayouts =
      avoidStruts $
        mySpacing (Grid False)
          ||| mySpacing (Grid True)
          ||| mySpacing (Full)
    myZKLayouts =
      avoidStruts $
        noBorders Full
          ||| Circle
    myTermLayouts =
      noBorders Full
        ||| avoidStruts (mySpacing $ Grid False)
        ||| avoidStruts Circle

--      ||| ResizableTall 1 (3 / 100) (1 / 2) []
--      ||| Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])

-- Settings for my utility menu
myGSUtils =
  [ ("XRANDR - Single Screen", (spawn "bash ~/.xprofile.onescreen")),
    ("XRANDR - Dual Screens", (spawn "bash ~/.xprofile")),
    ("Kill Touchpad", (spawn "synclient TouchpadOff=1")),
    ("Revive Touchpad", (spawn "synclient TouchpadOff=0"))
  ]

-- custom keybindings
myKeys =
  [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"), -- Lock screen with Mod-Shift-z`
  --((mod4Mask, xK_g), goToSelected def),
    ((mod4Mask, xK_u), runSelectedAction def myGSUtils),
    ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"), -- screenshot Window with Ctrl-PrintScreen
    ((0, xK_Print), spawn "scrot") -- screenshot entire screen with PrintScreen
  ]

myManageHook =
  composeAll
    [ className =? "Thunderbird" --> doShift "coms",
      className =? "Discord" --> doShift "coms",
      className =? "yakyak" --> doShift "coms",
      className =? "Brave" --> doShift "web",
      className =? "MyZettelkasten" --> doShift "zk",
      manageDocks
    ]

myStartupHook = do
  -- spawnOnce "nitrogen --restore &" -- set the desktop background
  spawnOnce "trayer --edge top --align right --widthtype percent --width 11 --margin 0 --padding 6 --SetDockType true --SetPartialStrut true --expand false --monitor primary --transparent true --alpha 1 --tint 0x000000 --height 24 &" -- system tray
  spawnOnce "nm-applet &" -- network manager icon
  spawnOnce "volumeicon &" -- Volume tray icon
  spawnOnce "xscreensaver -no-splash &" -- Start the screensaver daemon
  spawnOnce "syncthing-gtk &" -- Start my Sync Client

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    docks $
      ewmh
        def
          { layoutHook = myLayoutHook,
            startupHook = myStartupHook,
            manageHook = myManageHook <+> manageHook def,
            normalBorderColor = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            borderWidth = myBorderWidth,
            terminal = myTerminal,
            modMask = myModMask,
            logHook = do
              updatePointer (1, 1) (0, 0)
              wallpaperSetter
                defWallpaperConf
                  { wallpapers =
                      defWPNames myWorkspaces
                        <> WallpaperList
                          [ ("9:DUMP", WallpaperFix "9.jpg")
                          ]
                  }
              dynamicLogWithPP xmobarPP {ppOutput = hPutStrLn xmproc, ppTitle = xmobarColor "#00FF30" "" . shorten 150},
            -- focusFollowsMouse = False,
            -- clickJustFocuses = False,
            workspaces = myWorkspaces
          }
        `additionalKeys` myKeys
