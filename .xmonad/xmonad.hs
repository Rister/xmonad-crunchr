import qualified Data.Map as M
import System.IO
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Roledex
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myModMask = mod4Mask -- set mod key to the windows (super) key

myFocusedBorderColor = "#FF00FF"

myNormalBorderColor = "#00FF00"

myBorderWidth = 2

myGap = 5

myBorder = Border myGap myGap myGap myGap

myTerminal = "alacritty"

myBrowser = "brave --new-window"

myWorkspaces =
  [ "web",
    "zettelkasten",
    "email",
    "terminal",
    "hangouts",
    "discord",
    "web1",
    "coding",
    "dump"
  ]

myProjects =
  [ Project
      { projectName = "web",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawnHere myBrowser
      },
    Project
      { projectName = "zettelkasten",
        projectDirectory = "~/Documents/MyZettelkasten/",
        projectStartHook =
          Just $ do
            spawnHere "code ~/Documents/MyZettelkasten/MyZettelkasten.code-workspace"
            spawnHere $ myTerminal ++ " -e topydo prompt"
      },
    Project
      { projectName = "email",
        projectDirectory = "~/Downloads/",
        projectStartHook = Just $ do spawnHere "thunderbird"
      },
    Project
      { projectName = "terminal",
        projectDirectory = "~/",
        projectStartHook =
          Just $ do
            spawnHere myTerminal
            spawnHere myTerminal
            spawnHere myTerminal
      },
    Project
      { projectName = "hangouts",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawnHere "yakyak"
      },
    Project
      { projectName = "discord",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawnHere "discord"
      },
    Project
      { projectName = "web1",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawnHere myBrowser
      },
    Project
      { projectName = "coding",
        projectDirectory = "~/github/",
        projectStartHook =
          Just $ do
            spawnHere myTerminal
            spawnHere "code"
      },
    Project
      { projectName = "files",
        projectDirectory = "~/",
        projectStartHook =
          Just $ do
            spawnHere myTerminal
            spawnHere "nautilus"
      },
    Project
      { projectName = "xmonad",
        projectDirectory = "~/github/xmonad-crunchr",
        projectStartHook =
          Just $ do
            spawnHere myTerminal
            spawnHere "code ~/github/xmonad-crunchr/xmonad-crunchr.code-workspace"
      }
  ]

mySpacing = spacingRaw True myBorder False myBorder True

myLayoutHook =
  avoidStruts $
    onWorkspace "zettelkasten" myZKLayouts $
      onWorkspaces ["email", "discord", "hangouts"] myComsLayouts $
        onWorkspace
          "terminal"
          myTermLayouts
          myDefaultLayouts
  where
    myDefaultLayouts =
      noBorders Full
        ||| Roledex
        ||| MosaicAlt M.empty
        ||| Accordion
        ||| simpleTabbed
    myComsLayouts =
      mySpacing $
        MosaicAlt M.empty
          ||| noBorders Full
    myZKLayouts =
      MosaicAlt M.empty
        ||| Accordion
        ||| noBorders Full
        ||| simpleTabbed
    myTermLayouts =
      MosaicAlt M.empty
        ||| Accordion
        ||| Roledex
        ||| simpleTabbed

--      ||| ResizableTall 1 (3 / 100) (1 / 2) []
--      ||| Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])

-- Settings for my utility menu
myGSUtils =
  [ ("XRANDR - Single Screen", spawn "bash ~/.xprofile.onescreen"),
    ("XRANDR - Dual Screens", spawn "bash ~/.xprofile"),
    ("Kill Touchpad", spawn "synclient TouchpadOff=1"),
    ("Revive Touchpad", spawn "synclient TouchpadOff=0")
  ]

-- custom keybindings
myKeys =
  [ -- Lock screen with Mod-Shift-z`
    ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
    -- screenshot Window with Ctrl-PrintScreen
    ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
    -- screenshot entire screen with PrintScreen
    ((0, xK_Print), spawn "scrot"),
    -- Grid Select the workspace
    ((myModMask, xK_g), gridselectWorkspace def (\ws -> W.view ws)),
    ((myModMask .|. shiftMask, xK_g), gridselectWorkspace def (\ws -> W.view ws . W.shift ws)),
    -- GridSelect some utilities
    ((myModMask, xK_u), runSelectedAction def myGSUtils),
    -- Dynamic Project Prompt
    ((myModMask, xK_backslash), switchProjectPrompt def),
    ((myModMask .|. shiftMask, xK_backslash), shiftToProjectPrompt def)
  ]

myManageHook =
  composeAll
    [ manageDocks
    ]

myStartupHook = do
  -- set the desktop background
  spawnOnce "nitrogen --restore &"
  -- system tray
  spawnOnce "trayer --edge top --align right --widthtype percent --width 11 --margin 0 --padding 6 --SetDockType true --SetPartialStrut true --expand false --monitor primary --transparent true --alpha 1 --tint 0x000000 --height 24 &"
  spawnOnce "nm-applet &" -- network manager icon
  spawnOnce "volumeicon &" -- Volume tray icon
  spawnOnce "xscreensaver -no-splash &" -- Start the screensaver daemon
  spawnOnce "syncthing-gtk &" -- Start my Sync Client

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    docks $
      ewmh $
        dynamicProjects
          myProjects
          def
            { normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              terminal = myTerminal,
              layoutHook = myLayoutHook,
              manageHook = myManageHook <+> manageHook def,
              workspaces = myWorkspaces,
              modMask = myModMask,
              borderWidth = myBorderWidth,
              logHook = do
                updatePointer (1, 1) (0, 0)
                dynamicLogWithPP
                  xmobarPP
                    { ppHidden = (\foo -> ""),
                      ppUrgent = xmobarColor "#FF0000" "",
                      ppTitle = xmobarColor "#00FF30" "" . shorten 150,
                      ppOutput = hPutStrLn xmproc
                    },
              startupHook = myStartupHook,
              focusFollowsMouse = False,
              clickJustFocuses = False
            }
          `additionalKeys` myKeys
