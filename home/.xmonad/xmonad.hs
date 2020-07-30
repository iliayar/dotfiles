-- Imports
import Data.List
import Data.Monoid
import Data.Tree
import Data.Maybe
import qualified Data.Map as M

import System.IO (hPutStrLn, hClose, hPutStr, Handle)
import System.Exit
import System.Directory
import System.FilePath ((</>))

import Control.Arrow (first)
import Control.Monad

import XMonad

import XMonad.Hooks.DynamicLog (xmobarPP, dynamicLogWithPP, xmobarColor, PP(..), wrap, shorten)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName

import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

import XMonad.Layout.NoBorders
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL, NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Actions.Submap
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS

import qualified XMonad.StackSet as W


--------------------------------------------------------
-- Functions

dropRdTuple :: (a, b, c) -> (a, b)
dropRdTuple (a, b, _) = (a, b)
  
getDescription' :: (String, a, String) -> String
getDescription' (k, _, d) = k ++ ": " ++ (takeWhile (/='\n') d)

getDescription :: String -> [(String, b, String)] -> String
getDescription sep keys = intercalate sep $ map getDescription' keys

alignHelp :: String -> String
alignHelp s
  | (length $ lines s) == 1 = s
  | otherwise = intercalate "\n  " $ map alignHelp $ lines s

getHelp' :: (String, a, String) -> String
getHelp' (k, _, d) = "[" ++ k ++ "]: " ++ (alignHelp d)

getHelp :: [(String, a, String)] -> String
getHelp keys = unlines $ map getHelp' keys

escapeSymbols :: String -> String -> String
escapeSymbols esc = concatMap doubleLts
  where
        doubleLts x
          | x `elem` esc = ['\\', x]
          | otherwise = [x]

dzenEscape = escapeSymbols "|"

dzen' :: String -> String
dzen' m = "(echo " ++
  dzenEscape m ++
  " | column; cat) | " ++ unwords [ "dzen2"
                         , "-fn 'Hack Nerd Font Mono 9'"
                         , "-fg '#d5c4a1'"
                         , "-bg '#1d2021'"
                         ]

termShowKeybindings :: String -> X () 
termShowKeybindings m = spawn $ "termite --hold -e \"echo '" ++ m ++  "'\" -t 'termite-keybindings'"

termSpawn :: String -> [String] -> X ()
termSpawn a p = spawn $ "termite " ++ (unwords p) ++ " -e '" ++ a ++ "'"  

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

addExitMap :: [(String, X (), String)] -> [(String, X (), String)]
addExitMap m = ("<Escape>", io $ return (), "Cancel") : m

dzenKeymapsPipe :: String -> [(String, X (), String)] -> X Handle
dzenKeymapsPipe d m = do
   h <- spawnPipe $ "~/bin/dzenShowKeymap.sh \"" ++ (dzenEscape d) ++ "\""
   io $ hPutStrLn h $ dzenEscape $ getDescription "\n" $ addExitMap m
   io $ hPutStrLn h "END"
   io $ return h

withDzenKeymapsPipe :: String -> [(String, X (), String)] -> X () -> X ()
withDzenKeymapsPipe d m f = do
  h <- dzenKeymapsPipe d m
  f
  io $ hClose h

--------------------------------------------------------
-- Variables

myTerminal      = "termite"

myFont = "xft:Hack Nerd Font Mono:size=9"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces = map show [1..9] ++ ["Music"]
myWorkspacesClickable    = clickable . (map xmobarEscape) $ (map show [1..9]) ++ ["Music"]
    where
        clickable l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip (map show [1..9] ++ ["m"]) l,
                      let n = i ]

myNormalBorderColor  = "#928374"
myFocusedBorderColor = "#cc241d"

--------------------------------------------------------
-- GridSelect

mygridConfig :: (a -> Bool -> X (String, String)) -> GSConfig a
mygridConfig colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

orange :: a -> Bool -> X (String, String)
orange = myColor "#ff4301"

myColor :: String -> a -> Bool -> X (String, String)
myColor color _ isFg = do
  return $ if isFg
           then (color, "#1d2021")
           else ("#1d2021" ,"#d5c4a1")

spawnSelected' :: [(String, String)] -> X()
spawnSelected' = runSelectedAction (mygridConfig orange) . map (\(a, b) -> (a, spawn b))

myAppGrid = [ ("Emacs", "emacsclient -c -a emacs")
            , ("Brave", "brave")
            , ("Spotify", "spotify")
            , ("Telegram", "telegram-desktop")
            , ("File Manager", "pcmanfm")
            ]

--------------------------------------------------------
-- ScratchPads

myScratchPads :: [NamedScratchpad]
myScratchPads = [ termApp "terminal" "" manageQuake
                , NS "notes" spawnNotes findNotes manageNotes
                , termApp "weather" "--hold -e 'bash -c \"curl wttr.in; cat\"'" manageWeather
                , termApp "ipython" "-e 'ipython'" manageQuake
                , termApp "ghci" "-e 'ghci'" manageQuake
                ]
  where
    termApp name cmd manage = NS name ("termite -r '" ++ name ++ "-scratchpad' " ++ cmd) (role =? (name ++ "-scratchpad")) manage

    spawnNotes = "emacsclient -c -a emacs -F '(quote (name . \"emacs-notes\"))' ~/.org/Notes.org"
    findNotes  = title =? "emacs-notes"

    manageNotes = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
    manageWeather = customFloating $ W.RationalRect 0.1 0.1 0.53 0.64
    manageQuake = customFloating $ W.RationalRect 0 0 1 0.5

    role = stringProperty "WM_WINDOW_ROLE"

--------------------------------------------------------
-- TreeSelect

tsAll =
   [ Node (TS.TSNode "+ General" "General purpose applications" (return ()))
       [ Node (TS.TSNode "Pcmanfm" "File Manager" (spawn "pcmanfm")) []
       , Node (TS.TSNode "Brave" "Browser" (spawn "brave")) []
       ]
   , Node (TS.TSNode "+ Programming" "programming" (return ()))
       [ Node (TS.TSNode "IPython" "IPython interactive shell" (spawn "termite -e 'ipython'")) []
       , Node (TS.TSNode "Emacs" "IDE/Text editor" (spawn "emacsclient -c -a emacs")) []
       , Node (TS.TSNode "Termite" "Terminal" (spawn "termite")) []
       ]
   , Node (TS.TSNode "+ Tools" "Various tools" (return ()))
       tsTools
   , Node (TS.TSNode "+ Management" "Any management commands" (return ()))
       tsManagement
   , Node (TS.TSNode "Test" "Test command for debug" (appPrompt myXPConfig)) []
   ]

tsManagement =
   [ Node (TS.TSNode "Picom" "Picom switch on/off" (return ()))
       [ Node (TS.TSNode "off" "Switch off compositor" (spawn "killall picom")) []
       , Node (TS.TSNode "on" "Switch on compositor" (spawn "/usr/local/bin/picom --experimental-backends -b")) []
       ]
   , Node (TS.TSNode "Brightness" "Set Brightness" (return ()))
       [ Node (TS.TSNode "Max Brightness" "Set Brightness to 100" (spawn "light -S 100")) []
       , Node (TS.TSNode "Norm Brightness" "Set Brightness to 50" (spawn "light -S 50")) []
       , Node (TS.TSNode "Min Brightness" "Set Brightness to 5" (spawn "light -S 5")) []
       ]
   , Node (TS.TSNode "Close all dzen" "Kill broken dzen" (spawn "killall dzen2")) []
   , Node (TS.TSNode "Pacman update" "Get updates from pacman" (termSpawn "sudo pacman -Syyu" [])) []
   , Node (TS.TSNode "AUR update" "Get updates from AUR" (termSpawn "yay -Syyu" [])) []
   , Node (TS.TSNode "Layout" "Manipulate layout" (return ()))
     tsLayout
   ]
tsLayout =
   [ Node (TS.TSNode "Tile" "Make Window Tiled" (withFocused $ windows . W.sink)) []
   , Node (TS.TSNode "Tile All" "Make All Windows Tiled" sinkAll) []
   , Node (TS.TSNode "Toggle Struts"   "Show/Hide bar" (sendMessage ToggleStruts)) [ ]
   , Node (TS.TSNode "Toggle Fullscreen"   "" makeFullscreenNoDock) [ ]
   , Node (TS.TSNode "Toggle NoBorders"   "" (sendMessage (MT.Toggle NOBORDERS))) [ ]
   , Node (TS.TSNode "Toggle Mirrored"   "" (sendMessage (MT.Toggle MIRROR))) [ ]
   , Node (TS.TSNode "Toggle Floats"   "" (sendMessage (T.Toggle "floats"))) [ ]
   , Node (TS.TSNode "Arrange"   "" (sendMessage Arrange)) [ ]
   , Node (TS.TSNode "DeArrange"   "" (sendMessage DeArrange)) [ ]
   ]
tsTools =
  [ Node (TS.TSNode "NetworkManager TUI" "Terminal Interface for NetworkManager" (termSpawn "nmtui" [])) []
  , Node (TS.TSNode "PulseMixer" "Pulse Audio Mixer" (termSpawn "pulsemixer" [])) []
  ]


makeFullscreenNoDock :: X ()
makeFullscreenNoDock = sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
  
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0x00000000
                              , TS.ts_font         = "xft:Hack Nerd Font Mono:size=9"
                              , TS.ts_node         = (0xffd5c4a1, 0xff1d2021)
                              , TS.ts_nodealt      = (0xffd5c4a1, 0xff282828)
                              , TS.ts_highlight    = (0xffffffff, 0xffff4301)
                              , TS.ts_extra        = 0xffd5c4a1
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 25
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = TS.defaultNavigation
                              }
--------------------------------------------------------
-- Keybindings

myKeys = \conf -> let
  createSubmap m = submap $ mkKeymap conf $ map dropRdTuple $ addExitMap m
  prefix p m d = (p, withDzenKeymapsPipe d m $ createSubmap m,
                  d ++ " (submap)\n" ++ getHelp m)
  dzenAllBindings = withDzenKeymapsPipe "Keybindings" keymap $ createSubmap []
  restartRecompile = intercalate " && "
    [ "cd ~/.xmonad"
    , "stack ghc -- --make ~/.config/xmobar/xmobar.hs"
    , "xmonad --recompile"
    , "xmonad --restart"
    , "notify-send 'restarting Xmonad'"
    ]
  keymap =
    [ ("M-."         , sendMessage (IncMasterN (-1))                          , "Decrease Master N"        )
    , ("M-,"         , sendMessage (IncMasterN 1)                             , "Increase Master N"        )
    , ("M-h"         , sendMessage Shrink                                     , "Shrink window"            )
    , ("M-j"         , windows W.focusDown                                    , "Prev window"              )
    , ("M-k"         , windows W.focusUp                                      , "Next window"              )
    , ("M-l"         , sendMessage Expand                                     , "Expand window"            )
    , ("M-f"         , makeFullscreenNoDock                                   , "Fullscreen Toggle"        )
    , ("M-<Space>"   , sendMessage NextLayout                                 , "Cicle layouts"            )
    , ("M-<Return>"  , spawn $ XMonad.terminal conf                           , "Launch terminal"          )
    , ("M-S-/"       , termShowKeybindings $ getHelp keymap                   , "Show this help"           )
    , ("M-S-c"       , spawn restartRecompile                                 , "Recompile, restart XMonad")
    , ("M-S-d"       , spawn "notify-send 'DUNST_COMMAND_TOGGLE'"             , "Toggle notifications"     )
    , ("M-S-j"       , windows W.swapDown                                     , "Swap window with prev"    )
    , ("M-S-k"       , windows W.swapUp                                       , "Swap window with next"    )
    , ("M-S-q"       , kill                                                   , "Close window"             )
    , ("M-S-<Space>" , windows W.focusMaster                                  , "Focus master window"      )
    , ("M-S-<Return>", windows W.swapMaster                                   , "Make window master"       )
    , ("M-C-/"       , dzenAllBindings                                        , "Show this help"           )
    , ("M-C-x"       , spawn "xkill"                                          , "Launch xkill"             )
    , prefix "M-s"
      [ ("t", namedScratchpadAction myScratchPads "terminal", "Terminal scratchpad" )
      , ("n", namedScratchpadAction myScratchPads "notes"   , "Notes.org scratchpad")
      , ("w", namedScratchpadAction myScratchPads "weather" , "Weather scratchpad")
      , ("p", namedScratchpadAction myScratchPads "ipython" , "IPython intercative shell")
      , ("g", namedScratchpadAction myScratchPads "ghci"    , "Haskell intercative shell")
      ] "Sratchpads"
    , prefix "M-S-e"
       [ ("r"  , spawn "reboot"           , "Reboot"     )
       , ("s"  , spawn "systemctl suspend", "Suspend"    )
       , ("e"  , io (exitWith ExitSuccess), "Exit XMonad")
       , ("S-s", spawn "shutdown 0"       , "Shutdown"   )
       ] "Power management"
    , prefix "M-g"
      [ ("g", spawnSelected' myAppGrid                , "Grid select favorite apps")
      , ("t", goToSelected $ mygridConfig orange , "Goto selected window"     )
      , ("b", bringSelected $ mygridConfig orange, "Bring selected window"    )
      ] "GridSelect"
    , prefix "M-t"
      [ ("a", TS.treeselectAction tsDefaultConfig tsAll, "TreeSelect All")
      , ("m", TS.treeselectAction tsDefaultConfig tsManagement, "TreeSelect Management")
      , ("l", TS.treeselectAction tsDefaultConfig tsLayout, "TreeSelect Layout")
      , ("t", TS.treeselectAction tsDefaultConfig tsTools, "TreeSelect Tools")
      ] "TreesSlect"
    , prefix "M-d"
      [ ("d", shellPrompt myXPConfig, "Shell prompt")
      , ("a", appPrompt myXPConfig  , "Applications prompt")
      ] "Prompts"
    , prefix "M-C-f" ( createSearchPrompt searchEngines) "Search Engines Prompt"
    , prefix "M-S-f" ( createSearchSelect searchEngines) "Selech Engines Select"
    ]
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i, d ++ " to workspace " ++ n)
        | (n, i, k) <- zip3 myWorkspaces (XMonad.workspaces conf) $ map show [1..9] ++ ["m"]
        , (f, m, d) <- [ (W.greedyView                   , ""  , "Switch")
                    , (liftM2 (.) W.greedyView W.shift, "-S", "Move window and switch")
                    , (W.shift                        , "-C", "Move window")]]
    ++
    [("M" ++ m ++ "-" ++ k, screenWorkspace sc >>= flip whenJust (windows . f)
    , d ++ " to screen " ++ (show sc))
        | (k, sc) <- zip ["p", "[", "]"] [0..]
        , (f, m, d) <- [ (W.view, ""   , "Switch")
                       , (W.shift, "-S", "Move window")]]
    ++
    [ ("<XF86MonBrightnessUp>"  , spawn "light -A 5"                  , "Brightness up"  )
    , ("<XF86MonBrightnessDown>", spawn "light -U 5"                  , "Brightness down")
    , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume '@DEFAULT_SINK@' +5%" , "Audio up"       )
    , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume '@DEFAULT_SINK@' -5%" , "Audio down"     )
    , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute '@DEFAULT_SINK@' toggle", "Mute/Unmute"    )
    ]
    ++
    [ (    "<Print>", scrot "" 0, "Screenshot"                      )
    , (  "M-<Print>", scrot "-u" 0, "Screenshot of active window"   )
    , ("M-S-<Print>", scrot "-s" 0.1, "Screenshot interactive"      )
    , ("<XF86AudioNext>", playerctl "next", "Music next"            )
    , ("<XF86AudioPrev>", playerctl "previous", "Music prev"        )
    , ("<XF86AudioPlay>", playerctl "play-pause", "Music play/pause")
    ]
  in mkKeymap conf $ map dropRdTuple keymap
    where
      scrot p t = spawn $ "sleep " ++ (show t) ++ ";\
                \scrot " ++ p ++ " -e '\
                                \xclip -selection clipboard -t image/png -i $f;\
                                \mv $f ~/Pictures/screenshots/;\
                                \notify-send \"Screenshot saved: $f\";'"
      playerctl a = spawn $ "playerctl " ++ a ++ " -p spotify"
      createSearchPrompt = map (\ (a, b, c) -> (a, S.promptSearch myXPConfig b, c))
      createSearchSelect = map (\ (a, b, c) -> (a, S.selectSearch b, c))
      archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
      searchEngines = [ ("g", S.google, "Google")
                      , ("h", S.hoogle, "Hoogle")
                      , ("i", S.images, "Images")
                      , ("d", S.duckduckgo, "DuckDuckGo")
                      , ("a", archwiki, "Arch Wiki")
                      ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--------------------------------------------------------
-- Layouts

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts
         $ windowArrange
         $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
         $ T.toggleLayouts floats
         -- $ smartBorders
         $   tall
         ||| floats
         ||| noBorders tabs
         ||| magnify
         ||| noBorders monocle
         ||| spirals
         ||| grid
  where
    tall    = renamed [Replace "tall"]
            $ limitWindows 12
            $ mySpacing' 4
            $ mkToggle (single MIRROR)
            $ ResizableTall 1 (3/100) (1/2) []
    floats  = renamed [Replace "floats"]
            $ limitWindows 20 simplestFloat
    magnify = renamed [Replace "magnify"]
            $ magnifier
            $ limitWindows 12
            $ mySpacing 6
            $ ResizableTall 1 (3/100) (1/2) []
    monocle = renamed [Replace "monocle"]
            $ limitWindows 20 Full
    spirals = renamed [Replace "spirals"]
            $ mySpacing' 4
            $ spiral (6/7)
    grid    = renamed [Replace "grid"]
            $ limitWindows 12
            $ mySpacing 4
            $ mkToggle (single MIRROR)
            $ Grid (16/10)
    tabs    = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
            $ tabbed shrinkText $ def
                   { fontName            = "xft:Hack Nerd Font Mono:size=9"
                   , activeColor         = "#282828"
                   , inactiveColor       = "#1d2021"
                   , activeBorderColor   = "#292d3e"
                   , inactiveBorderColor = "#292d3e"
                   , activeTextColor     = "#fb4934"
                   , inactiveTextColor   = "#ebdbb2"
                   }

--------------------------------------------------------
-- Hooks
myManageHook = composeAll
  [ isFullscreen                       --> doFullFloat
  , className =? "Nitrogen"            --> doFloat
  , className =? "feh"                 --> doFloat
  , resource  =? "stalonetray"         --> doIgnore
  , title     =? "termite-keybindings" --> (customFloating $ W.RationalRect 0 0 0.5 1)
  , manageDocks
  , namedScratchpadManageHook myScratchPads
  ]

myEventHook = docksEventHook

myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends -b"
          spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
          spawnOnce "stalonetray"
          spawnOnce "xsetroot -cursor_name arrow"
          spawn "xrdb ~/.Xresources"
          setWMName "LG3D"
          spawnOnce "emacs --daemon &"

--------------------------------------------------------
-- Prompt
myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "#1d2021"
      , fgColor             = "#ebdbb2"
      , bgHLight            = "#282828"
      , fgHLight            = "#fb4934"
      , borderColor         = "#fabd2f"
      , promptBorderWidth   = 2
      , promptKeymap        = defaultXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 25
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

data App = App

instance XPrompt App where
  showXPrompt App = "Application: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

appPrompt :: XPConfig -> X ()
appPrompt c = do
  userHome <- io $ getHomeDirectory
  li <- fmap catMaybes $  io $ foldM (\acc -> fmap (acc++) . getApplications) []
         [ "/usr/share/applications"
         , userHome ++ "/.local/share/applications"
         ]
  let compl = \s -> fst <$> filter (fuzzyMatch s . fst) li
  mkXPrompt App c (return . compl) (spawn . (M.fromList li M.!))


getApplications :: FilePath -> IO [Maybe (String, String)]
getApplications dir = do
  l <- listDirectory dir
  foldM (\acc -> fmap (acc++) . getApplicationData dir) [] l

getApplicationData :: FilePath -> FilePath -> IO [Maybe (String, String)]
getApplicationData dir file = do
  t <- readFile $ dir </> file
  let names = filter (isPrefixOf "Name=") $ lines t
      cmds = filter (isPrefixOf "Exec=") $ lines t
      values = zip names cmds
  return (if names == [] then [Nothing] else map Just $ addPrefix $ map (\ (a, b) -> (getValue a, getValue b)) values) 
  where
    getValue = dropWhile (==' ') . drop 1 . dropWhile (/='=')
    addPrefix a = (head a) : (map (\ (b, c) -> ((fst $ head a) ++ " (" ++ b ++ ")", c)) (tail a))
  
--------------------------------------------------------
-- Main
main = do
        xmproc <- spawnPipe "xmobar -x 0 /home/iliayar/.config/xmobar/xmobar.hs"
        xmonad $ ewmh def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspacesClickable,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP xmobarPP
                { ppOutput  = \x -> hPutStrLn xmproc x
                , ppCurrent = xmobarColor "#b8bb26" "" . wrap "[" "]"
                , ppVisible = xmobarColor "#b8bb26" ""
                , ppTitle   = xmobarColor "#fb4934" "" . shorten 30
                , ppExtras  = []-- [windowCount]                           -- # of windows current workspace
                , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t] -- workspaces : layout : extras : title
                },
        startupHook        = myStartupHook
    }
