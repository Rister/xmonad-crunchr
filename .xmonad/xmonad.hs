import System.IO (hPutStrLn)
import XMonad
  ( Default (def),
    Full (Full),
    XConfig
      ( borderWidth,
        clickJustFocuses,
        focusFollowsMouse,
        focusedBorderColor,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    composeAll,
    controlMask,
    mod4Mask,
    shiftMask,
    spawn,
    xK_Print,
    xK_backslash,
    xK_g,
    xK_u,
    xK_z,
    xmonad,
    (.|.),
    (<+>),
    (|||),
  )
import XMonad.Actions.DynamicProjects
  ( Project (Project, projectDirectory, projectName, projectStartHook),
    dynamicProjects,
    shiftToProjectPrompt,
    switchProjectPrompt,
  )
import XMonad.Actions.GridSelect
  ( gridselectWorkspace,
    runSelectedAction,
  )
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
  ( PP (ppHidden, ppOutput, ppTitle, ppUrgent),
    dynamicLogWithPP,
    shorten,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.Mosaic (mosaic)
import XMonad.Layout.MosaicAlt ()
import XMonad.Layout.NoBorders ()
import XMonad.Layout.OneBig (OneBig (OneBig))
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Roledex ()
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.Square ()
import XMonad.Layout.Tabbed (simpleTabbed)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

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
    "coms",
    "terminal",
    "yellow",
    "magenta",
    "cyan",
    "emacs",
    "coding"
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
            spawnHere $ myTerminal ++ " -e task shell"
            spawnHere $ myTerminal ++ " -e watch -tc -n 35 timew"
            spawnHere $ myTerminal
      },
    Project
      { projectName = "coms",
        projectDirectory = "~/Downloads/",
        projectStartHook =
          Just $ do
            spawnHere "thunderbird"
            spawnHere "discord"
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
      },
    Project
      { projectName = "emacs",
        projectDirectory = "~/",
        projectStartHook =
          Just $ do
            spawnHere myTerminal
            spawnHere "emacs"
      },
    Project
      { projectName = "obsidian",
        projectDirectory = "~/Documents/MyZettelkasten/",
        projectStartHook =
          Just $ do
            spawnHere "obsidian"
      }
  ]

mySpacing = spacingRaw True myBorder False myBorder True

myLayoutHook =
  avoidStruts $
    onWorkspace "zettelkasten" myZKLayouts $
      onWorkspaces ["coms"] myComsLayouts $
        onWorkspace "terminal" myTermLayouts $
          myDefaultLayouts
  where
    myDefaultLayouts =
      Full
        ||| mosaic 3 [5, 3]
        ||| Accordion
        ||| simpleTabbed
        ||| OneBig (3 / 4) (3 / 4)
    myComsLayouts =
      simpleTabbed
        ||| mosaic 3 [5, 3]
        ||| OneBig (3 / 4) (3 / 4)
    myZKLayouts =
      OneBig (4 / 5) (4 / 5)
        ||| mosaic 3 [5, 2]
        ||| simpleTabbed
    myTermLayouts =
      mosaic 3 [5, 3]
        ||| Accordion
        ||| simpleTabbed
        ||| OneBig (3 / 5) (3 / 5)

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
