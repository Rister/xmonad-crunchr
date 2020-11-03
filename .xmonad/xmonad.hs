import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myModMask = mod4Mask -- set mod key to the windows (super) key

myFocusedBorderColor = "#FF00FF"

myNormalBorderColor = "#00FF00"

myBorderWidth = 2

myTerminal = "gnome-terminal"

myWorkspaces = ["web", "zk", "coms", "term", "5", "6", "7", "8", "9:DUMP"]

mySpacing = spacingRaw True (Border 5 5 5 5) False (Border 5 5 5 5) True

myLayoutHook =
  onWorkspace "web" defLayout $
    onWorkspace "zk" myZKLayouts $
      onWorkspace "coms" defLayout $
        onWorkspace "term" myTermLayouts $
          onWorkspaces (map show [5 .. 8]) defLayout $
            onWorkspace "9:DUMP" defLayout $
              smartBorders (layoutHook def)
  where
    defLayout =
      avoidStruts $
        mySpacing (Grid)
          ||| mySpacing (smartBorders (ThreeColMid 1 (3 / 100) (3 / 4)))
          ||| noBorders (Circle)
          ||| noBorders (Full)
    myTermLayouts =
      noBorders Full
        ||| avoidStruts (mySpacing Grid)
        ||| avoidStruts Circle
    myZKLayouts =
      avoidStruts $
        noBorders Full
          ||| Circle

--      ||| ResizableTall 1 (3 / 100) (1 / 2) []
--      ||| Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])

-- custom keybindings
myKeys =
  [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"), -- Lock screen with Mod-Shift-z
    ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"), -- screenshot Window with Ctrl-PrintScreen
    ((0, xK_Print), spawn "scrot") -- screenshot entire screen with PrintScreen
  ]

myStartupHook = do
  spawnOnce "nitrogen --restore &" -- set the desktop background
  spawnOnce "nm-applet &" -- network manager icon
  spawnOnce "volumeicon &" -- Volume tray icon
  spawnOnce "xscreensaver -no-splash &" -- Start the screensaver daemon
  spawnOnce "syncthing-gtk &" -- Start my Sync Client
  spawnOnce "trayer --edge top --align right --widthtype percent --width 21 --margin 0 --padding 6 --SetDockType true --SetPartialStrut true --expand false --monitor primary --transparent true --alpha 1 --tint 0x000000 --height 32 &" -- system tray

-- I need to look into this more TODO
myWallpaperSetter =
  defWallpaperConf
    { wallpapers = defWPNames myWorkspaces
    }

main = do
  xmproc <- spawnPipe "xmobar ~/.xmobar/xmobarrc"
  xmonad $
    docks
      def
        { layoutHook = myLayoutHook,
          startupHook = myStartupHook,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          borderWidth = myBorderWidth,
          terminal = myTerminal,
          modMask = myModMask,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppTitle = xmobarColor "#00FF30" "" . shorten 150
                },
          -- focusFollowsMouse = False,
          workspaces = myWorkspaces
        }
      `additionalKeys` myKeys
