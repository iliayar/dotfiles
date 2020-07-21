import Data.List
import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.DynamicLog
import Data.Monoid
import System.Exit

import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import Control.Arrow (first)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.EZConfig


import XMonad.Layout.NoBorders


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

myPlayer        = "spotify"
myBrowser       = "brave"


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#1d2021"
myFocusedBorderColor = "#83a598"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys = \conf -> mkKeymap conf $

    -- launch a terminal
    [ ("M-<Return>", spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- , ((modm,               xK_d     ), spawn "dmenu_run")
    , ("M-d", shellPrompt myXPConfig)

    -- launch gmrun
    , ("M-S-d", spawn "gmrun")

    -- close focused window
    , ("M-S-q", kill)

     -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M-S-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp    )

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-C-e", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-S-c", spawn "notify-send 'restarting Xmonad'; xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [("M" ++ m ++ "-" ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ map (:[]) ['1' .. '9']
        , (f, m) <- [(W.greedyView, ""), (W.shift, "-S")]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [(("M" ++ m ++ "-" ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "-S")]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = composeAll
--     [ className =? "MPlayer"        --> doFloat
--     , className =? "Gimp"           --> doFloat
--     , resource  =? "desktop_window" --> doIgnore
--     , resource  =? "kdesktop"       --> doIgnore ]
myManageHook = composeAll
  [ className =? "Nitrogen"            --> doFloat ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
-- Autostart
myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends -b"
          spawnOnce "xbindkeys --poll-rc"
          spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
          spawnOnce "xmobar"
          spawn "xrdb ~/.Xresources"
          setWMName "LG3D"
          --spawnOnce "emacs --daemon &"

------------------------------------------------------------------------
-- Prompt general config

myXPConfig :: XPConfig
myXPConfig = def
      { font                = "xft:Hack Nerd Font Mono:size=9"
      , bgColor             = "#1d2021"
      , fgColor             = "#d5c4a1"
      , bgHLight            = "#665c54"
      , fgHLight            = "#fb4934"
      , borderColor         = "#83a598"
      , promptBorderWidth   = 0
      , promptKeymap        = defaultXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
        xmproc <- spawnPipe "xmobar -x 0 /home/iliayar/.config/xmobar/xmobarrc"
        
        xmonad $ docks $ ewmh def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ smartBorders $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP xmobarPP
                { ppOutput = \x -> hPutStrLn xmproc x
                , ppExtras  = []-- [windowCount]                           -- # of windows current workspace
                , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                },
        startupHook        = myStartupHook
    }
