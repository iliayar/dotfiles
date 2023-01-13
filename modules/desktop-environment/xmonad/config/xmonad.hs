-------------------------------------------------
------------------ Imports ----------------------
-------------------------------------------------
import qualified Theme

import           Data.List
import           Data.Monoid
import           Data.Tree
import           Data.Maybe
import           Data.Bifunctor (first, bimap)
import qualified Data.Map as M
import           Data.List.Split (splitOn)

import           System.IO
import           System.Exit
import           System.Posix.IO
import           System.Directory
import           System.FilePath ((</>))
import           System.Environment

import           Codec.Binary.UTF8.String (decodeString)

-- import qualified System.IO.UTF8 as UTF8

import           Control.Monad

import           XMonad

import           XMonad.Hooks.DynamicLog (xmobarPP, dynamicLogWithPP, xmobarColor, PP(..), wrap, shorten)
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, ToggleStruts(..))
import           XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, (^?), (~?))
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.ServerMode

import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Types

import           XMonad.Prompt
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Input
import           XMonad.Prompt.Unicode
import           XMonad.Prompt.Shell (shellPrompt)
import           XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import           XMonad.Prompt.AppLauncher as AL

import           XMonad.Layout.Simplest
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.NoBorders
import           XMonad.Layout.GridVariants
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL, NBFULL, MIRROR, NOBORDERS))
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Renamed (renamed, Rename(Replace))
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import           XMonad.Actions.Submap
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WithAll (sinkAll, killAll)
import           XMonad.Actions.Commands (defaultCommands)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS

import qualified XMonad.StackSet as W

-------------------------------------------------
----------------- Functions ---------------------
-------------------------------------------------

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
getHelp' (k, _, d) = "[\x1b[33m" ++ k ++ "\x1b[m]: " ++ (alignHelp d)

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
                         , "-fn 'Fira Code'"
                         , "-fg '" ++ Theme.color7 ++ "'"
                         , "-bg '" ++ Theme.color0 ++ "'"
                         ]

termShowKeybindings :: String -> X () 
termShowKeybindings m = termSpawn tempAlacrittyHold $ "echo '" ++ m ++ "'"

data Terminal = Termite
                { termiteTitle :: Maybe String
                , termiteRole  :: Maybe String
                , termiteHold  :: Bool
                , termiteArgs  :: [String]
                , termiteCmd   :: Maybe String
                }
              | URxvt
                { urxvtTitle :: Maybe String
                , urxvtHold  :: Bool
                , urxvtArgs  :: [String]
                , urxvtCmd   :: Maybe String
                }
              | Alacritty
                { alacrittyTitle :: Maybe String
                , alacrittyClass :: Maybe String
                , alacrittyHold  :: Bool
                , alacrittyArgs  :: [String]
                , alacrittyCmd   :: Maybe String
                }

termSpawnCmd :: Terminal -> String
termSpawnCmd (Termite t r h a cmd) = unwords
                                   [ "termite"
                                   , getTermRole r
                                   , getTermTitle t
                                   , getTermHold h
                                   , unwords a
                                   , getTermCmd cmd
                                   ]
  where
    -- Sure there is a function that do this FIXME
    getTermRole (Just s) = "-r " ++ s
    getTermRole Nothing = ""
    getTermTitle (Just s) = "-t " ++ s
    getTermTitle Nothing = ""
    getTermHold True = "--hold"
    getTermHold False = ""
    getTermCmd Nothing = ""
    getTermCmd (Just cmd) = "-e \"" ++ cmd ++ "\"" 
  
termSpawnCmd (URxvt t h a cmd) = unwords
                               [ "urxvt"
                               , getTermTitle t
                               , getTermHold h
                               , unwords a
                               , getTermCmd cmd
                               ]
  where
    getTermTitle (Just s) = "-title " ++ s
    getTermTitle Nothing = ""
    getTermHold True = "-hold"
    getTermHold False = ""
    getTermCmd Nothing = ""
    getTermCmd (Just cmd) = "-e " ++ cmd

termSpawnCmd (Alacritty t c h a cmd) = unwords
                                   [ "alacritty"
                                   , getTermTitle t
                                   , getTermClass c
                                   , getTermHold h
                                   , unwords a
                                   , getTermCmd cmd
                                   ]
  where
    -- Sure there is a function that do this FIXME
    getTermClass (Just s) = "--class " ++ s
    getTermClass Nothing = ""
    getTermTitle (Just s) = "-t " ++ s
    getTermTitle Nothing = ""
    getTermHold True = "--hold"
    getTermHold False = ""
    getTermCmd Nothing = ""
    getTermCmd (Just cmd) = "-e " ++ cmd

termSpawn :: (Maybe String -> Terminal) -> String -> X()
termSpawn t c = spawn $ termSpawnCmd $ t $ Just c
  
termSpawn' :: Terminal -> X ()
termSpawn' t = spawn $ termSpawnCmd t

tempTermiteQuake = Termite (Just "temp-term-quake") Nothing False []
tempTermiteHold = Termite (Just "temp-term") Nothing True []
termite = Termite Nothing Nothing False []
tempTermite = Termite (Just "temp-term") Nothing False []

tempAlacrittyQuake = Alacritty (Just "temp-term-quake") Nothing False []
tempAlacrittyHold = Alacritty (Just "temp-term") Nothing True []
alacritty = Alacritty Nothing Nothing False []
tempAlacritty = Alacritty (Just "temp-term") Nothing False []

tempURxvt   = URxvt (Just "temp-term") False []

wrapZsh c = "zsh -c 'source ~/.zshrc; " ++ c ++ "'"



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
   curscreen <-
      (fromIntegral . W.screen . W.current) `fmap` gets windowset :: X Int
   h <- spawnPipe $ "dzenShowKeymap \"" ++ (dzenEscape d) ++ "\" " ++ (show $ curscreen + 1)
   io $ hPutStrLn h $ dzenEscape $ getDescription "\n" $ addExitMap m
   io $ hPutStrLn h "END"
   io $ return h

withDzenKeymapsPipe :: String -> [(String, X (), String)] -> (Handle -> X ()) -> X ()
withDzenKeymapsPipe d m f = do
  h <- dzenKeymapsPipe d m
  f h

afIcon s = "<fn=1>" ++ s ++ "</fn>"

data ScreenLayout = SingleScreen
                  | SamsungRight
                  | DefaultScreenLayout


screenLayoutFile :: ScreenLayout -> String
screenLayoutFile SingleScreen = "single"
screenLayoutFile SamsungRight = "samsungRight"
screenLayoutFile DefaultScreenLayout = "default"

screenLayoutDescription :: ScreenLayout -> String
screenLayoutDescription SingleScreen = "Sinle Screen"
screenLayoutDescription SamsungRight = "Samsung monitor in the right"

screenLayoutCmd :: ScreenLayout -> String
screenLayoutCmd l = "bash ~/.screenlayout/" ++ (screenLayoutFile l) ++ ".sh"

changeScreenLayout :: ScreenLayout -> X ()
changeScreenLayout l = do
  let file = screenLayoutFile l
  spawn $ "cp ~/.screenlayout/" ++ file ++ ".sh ~/.screenlayout/default.sh"
  return ()

applyScreenLayout :: ScreenLayout -> X ()
applyScreenLayout l = do
  let cmd = screenLayoutCmd l
  spawn cmd
  return ()

defaultScreenLayout :: ScreenLayout
defaultScreenLayout = DefaultScreenLayout

-------------------------------------------------
----------------- Variables ---------------------
-------------------------------------------------

myTerminal      = "alacritty"

myFont = "xft:Fira Code:size=9"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces = [afIcon "\xf001", afIcon "\xf120", afIcon "\xf1c9", afIcon "\xf03d", afIcon "\xf3f6", afIcon "\xf305", "6", afIcon "\xf468", afIcon "\xf268", afIcon "\xf3fe"]
myWorkspacesClickable    = clickable myWorkspaces
    where
        clickable l = [ "<action=~/.xmonad/xmonadctl " ++ i ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip (map show [2..11]) l]

myNormalBorderColor  = Theme.color8
myFocusedBorderColor = Theme.foreground

-------------------------------------------------
---------------- GridSelect ---------------------
-------------------------------------------------

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
orange = myColor Theme.color4

myColor :: String -> a -> Bool -> X (String, String)
myColor color _ isFg = do
  return $ if isFg
           then (color, Theme.background)
           else (Theme.color0 ,Theme.foreground)

spawnSelected' :: [(String, String)] -> X()
spawnSelected' = runSelectedAction (mygridConfig orange) . map (\(a, b) -> (a, spawn b))

myAppGrid = [ ("Emacs", "emacsclient -c -a emacs")
            , ("Brave", "brave")
            , ("Telegram", "telegram-desktop")
            , ("Qutebrowser", "qutebrowser")
            , ("Cutter", "Cutter")
            , ("File Manager", "pcmanfm")
            , ("Urxvt", "urxvt")
            , ("Alacritty", "alacritty")
            , ("Spotify", "spotify")
            , ("Discord", "discord")
            , ("Steam", "flatpak run com.valvesoftware.Steam")
            , ("Emacs Instance", "emacs")
            ]

-------------------------------------------------
--------------- Scratchpads ---------------------
-------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ termAppClass alacrittyScratchpad "terminal" Nothing manageQuake
                , NS "notes" spawnNotes findNotes manageNotes
                , termAppClass alacrittyScratchpad "weather" (Just $ wrapZsh "curl wttr.in; cat") manageWeather
                , termAppClass alacrittyScratchpad "ipython" (Just "ipython") manageQuake
                , termAppClass alacrittyScratchpad "julia" (Just "julia") manageQuake
                , termAppClass alacrittyScratchpad "ghci" (Just "ghci") manageQuake
                , termAppClass alacrittyScratchpad "spotify" (Just "spt") manageNotes
                , termAppClass alacrittyScratchpad "htop" (Just "htop") manageNotes
                , NS "drawing" spawnDrawing findDrawing manageNotes
                , NS "qutebrowser" spawnQutebrowser findQutebrowser manageNotes
                ]
  where
    termAppProp prop term name cmd manage = NS name (termSpawnCmd $ term (name ++ "-scratchpad") cmd) (prop =? (name ++ "-scratchpad")) manage
    termApp = termAppProp role
    termAppClass = termAppProp resource


    termiteScratchpad r = Termite Nothing (Just r) False [] 
    termiteScratchpadHold r = Termite Nothing (Just r) True [] 

    alacrittyScratchpad c = Alacritty Nothing (Just c) False [] 
    alacrittyScratchpadHold c = Alacritty Nothing (Just c) True [] 

    spawnQutebrowser = "qutebrowser --qt-arg name qutebrowser-scratchpad"
    spawnNotes = "emacsclient -c -a emacs -F '(quote (name . \"emacs-notes\"))' -e '(find-file \"~/org/Notes.org\")'"
    spawnDrawing = "drawing --name=drawing-scratchpad"
    findQutebrowser = appName =? "qutebrowser-scratchpad"
    findNotes  = title =? "emacs-notes"
    findDrawing = className =? "Drawing-scratchpad"

    manageNotes = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9
    manageWeather = customFloating $ W.RationalRect 0.05 0.05 0.53 0.64
    manageQuake = customFloating $ W.RationalRect 0 0 1 0.5

    role = stringProperty "WM_WINDOW_ROLE"
    name = stringProperty "WM_NAME"

-------------------------------------------------
---------------- TreeSelect ---------------------
-------------------------------------------------

tsAll =
   [ Node (TS.TSNode "+ General" "General purpose applications" (return ()))
       [ Node (TS.TSNode "Caja" "File Manager" (spawn "caja")) []
       , Node (TS.TSNode "Brave" "Browser" (spawn "brave")) []
       , Node (TS.TSNode "Neofetch" "Show off" (termSpawn' $ URxvt (Just "neofetch-term") True [] $ Just "neofetch --w3m --source ~/Wallpapers/Neofetch.png --image_size 360")) []
       ]
   , Node (TS.TSNode "+ Programming" "programming" (return ()))
       [ Node (TS.TSNode "IPython" "IPython interactive shell" (termSpawn termite "ipython")) []
       , Node (TS.TSNode "Emacs" "IDE/Text editor" (spawn "emacsclient -c -a emacs")) []
       , Node (TS.TSNode "Termite" "Terminal" (spawn "termite")) []
       , Node (TS.TSNode "Alacritty" "Alacritty" (spawn "alacritty")) []
       , Node (TS.TSNode "Urxvt" "Rxvt Unicode" (spawn "urxvt")) []
       -- , Node (TS.TSNode "Restart Emacs" "Restart Emacs daemon" (spawn $ wrapBash "killall -9 emacs; touch ~/.emacs.d/config.org; emacs --daemon")) []
       ]
   , Node (TS.TSNode "+ Tools" "Various tools" (return ()))
       tsTools
   , Node (TS.TSNode "+ System" "Any system commands" (return ()))
       tsSystem
   , Node (TS.TSNode "+ Commands" "Any usefull commands" (return ()))
       tsCommands
   , Node (TS.TSNode "Test" "Test command for debug" (appPrompt myXPConfig)) []
   ]

tsSystem =
   [ Node (TS.TSNode "Lock" "Lock screen" (spawn "locker")) []
   , Node (TS.TSNode "+ Picom" "Picom switch on/off" (return ()))
       [ Node (TS.TSNode "off" "Switch off compositor" (spawn "systemctl --user stop picom")) []
       , Node (TS.TSNode "on" "Switch on compositor" (spawn "systemctl --user start picom")) []
       ]
   , Node (TS.TSNode "+ Auto lock" "xautolock switch on/off" (return ()))
       [ Node (TS.TSNode "off" "Switch off xautolock" (spawn "rust-blocks-client lock disable")) []
       , Node (TS.TSNode "on" "Switch on xautolock" (spawn "rust-blocks-client lock enable")) []
       ]
   , Node (TS.TSNode "+ Wireguard" "wireguard switch on/off" (return ()))
       [ Node (TS.TSNode "off" "Switch off wireguard" (spawn "rust-blocks-client vpn down")) []
       , Node (TS.TSNode "on" "Switch on wireguard" (spawn "rust-blocks-client vpn up")) []
       ]
   , Node (TS.TSNode "+ Brightness" "Set Brightness" (return ()))
       [ Node (TS.TSNode "Max Brightness" "Set Brightness to 100" (spawn "light -S 100")) []
       , Node (TS.TSNode "Norm Brightness" "Set Brightness to 50" (spawn "light -S 50")) []
       , Node (TS.TSNode "Min Brightness" "Set Brightness to 1" (spawn "light -S 1")) []
       ]
   , Node (TS.TSNode "+ Tray" "Show/Hide stalonetray" (return ()))
       [ Node (TS.TSNode "Show" "Show stalonetray" (spawn "stalonetray &")) []
       , Node (TS.TSNode "Hide" "HIde stalonetray" (spawn "killall stalonetray")) []
       ]
   , Node (TS.TSNode "+ Layout" "Manipulate layout" (return ()))
     tsLayout
   ]
tsCommands =
   [ Node (TS.TSNode "Nix flake config" "Open dotfiles flake.nix in Emacs" (spawn "emacsclient -c -a emacs -e '(find-file \"~/Repos/dotfiles/flake.nix\")'")) []
   , Node (TS.TSNode "+ Pass" "Pass commands" (return ()))
     [ Node (TS.TSNode "Push" "Push to remote" $ gitCmd "push" ["origin", "github", "gitlab"]) []
     , Node (TS.TSNode "Pull" "Pull from remote" $ gitCmd "pull" ["origin"]) []
     ]
   , Node (TS.TSNode "Restart XMonad" "" (spawn "xmonad --restart")) []
   , Node (TS.TSNode "+ Screen Layouts" "" (return ())) $ map screenLayoutNode
     [ SingleScreen
     , SamsungRight
     ]
   ]
  where
    gitCmd s h =
      (termSpawn tempAlacrittyQuake $ wrapZsh $ (++ "; sleep 1")
        $ intercalate "; " $ map (\ x -> "echo " ++ x ++ "; pass git " ++ s ++  " " ++ x ++ " master") h)
    screenLayoutNode l = Node (TS.TSNode (screenLayoutDescription l) "" (changeScreenLayout l >> spawn "xmonad --restart")) []
  
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
  [ Node (TS.TSNode "NetworkManager TUI" "Terminal Interface for NetworkManager" (termSpawn termite "nmtui")) []
  , Node (TS.TSNode "Blueman" "Bluetooth manager" (spawn "blueman-manager")) []
  , Node (TS.TSNode "PulseMixer" "Pulse Audio Mixer" (termSpawn termite "pulsemixer")) []
  , Node (TS.TSNode "Syncthing GUI" "Open Syncthing GUI in browser" (spawn "xdg-open 'https://localhost:8384'")) []
  , Node (TS.TSNode "Main Workflow" "Run Brace, Telegram on appropriate workspaces"
          (do
              spawnOn (myWorkspacesClickable !! 9) "telegram-desktop"
              spawnOn (myWorkspacesClickable !! 8) "brave"
              spawnOn (myWorkspacesClickable !! 1) "termite"
              spawnOn (myWorkspacesClickable !! 0) "spotify")) []
  ]


makeFullscreenNoDock :: X ()
makeFullscreenNoDock = sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts
  
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = Theme.ts_background
                              , TS.ts_font         = myFont
                              , TS.ts_node         = Theme.ts_node
                              , TS.ts_nodealt      = Theme.ts_nodealt
                              , TS.ts_highlight    = Theme.ts_highlight
                              , TS.ts_extra        = Theme.ts_extra
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 25
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = TS.defaultNavigation
                              }

-------------------------------------------------
---------------- Keybindings --------------------
-------------------------------------------------

myKeys = \conf -> let
  createSubmap m h = submap $ mkKeymap conf $ map ((\ (k, a) -> (k, do
                                                                    io $ hClose h
                                                                    a
                                                                )) . dropRdTuple) $ addExitMap m
  prefix p m d = (p, withDzenKeymapsPipe d m $ createSubmap m,
                  "+ " ++ d ++ "\n" ++ getHelp m)
  dzenAllBindings = withDzenKeymapsPipe "Keybindings" keymap $ createSubmap []
  restartRecompile = wrapZsh $ (++" || (echo \"\x1b[31mFailed\x1b[m\"; echo Press Enter; read)") $ wrap "(" ")" $ intercalate " && "
    [ "home-manager switch -j 8"
    , "xmonad --restart"
    , "echo \"\x1b[32mSucceed\x1b[m\""
    , "sleep 1"
    ]
  keymap =
    [ ("M-."         , sendMessage (IncMasterN (-1))             , "Decrease Master N")
    , ("M-\\"        , spawn "xkb-switch -n"                     , "Switch keyboard layout")
    , ("M-,"         , sendMessage (IncMasterN 1)                , "Increase Master N")
    , ("M-a"         , sendMessage MirrorShrink                  , "Mirror shrink")
    , ("M-e"         , spawn "emacsclient -F '(quote (name . \"emacs-everywhere\"))' --eval '(emacs-everywhere)'", "Emacs everywhere")
    , ("M-z"         , sendMessage MirrorExpand                  , "Mirror expand")
    , ("M-h"         , sendMessage Shrink                        , "Shrink window")
    , ("M-j"         , windows W.focusDown                       , "Prev window")
    , ("M-k"         , windows W.focusUp                         , "Next window")
    , ("M-l"         , sendMessage Expand                        , "Expand window")
    , ("M-f"         , makeFullscreenNoDock                      , "Fullscreen Toggle")
    , ("M-<Space>"   , sendMessage NextLayout                    , "Cicle layouts")
    , ("M-<Return>"  , spawn $ XMonad.terminal conf              , "Launch terminal")
    , ("M-S-/"       , termShowKeybindings $ getHelp keymap      , "Show this help")
    , ("M-S-c"       , termSpawn tempAlacritty restartRecompile  , "Recompile, restart XMonad")
    , ("M-C-S-m"     , withFocused (sendMessage . MergeAll)      , "Merge all windows to one groups")
    , ("M-C-S-u"     , withFocused (sendMessage . UnMergeAll)    , "Unmerge all window in group")
    , ("M-C-u"       , withFocused (sendMessage . UnMerge)       , "Pull window from group")
    , ("M-S-j"       , windows W.swapDown                        , "Swap window with prev")
    , ("M-S-k"       , windows W.swapUp                          , "Swap window with next")
    , ("M-S-q"       , kill                                      , "Close window")
    , ("M-S-<Space>" , windows W.focusMaster                     , "Focus master window")
    , ("M-S-<Return>", windows W.swapMaster                      , "Make window master")
    , ("M-C-/"       , dzenAllBindings                           , "Show this help")
    , ("M-C-x"       , spawn "xkill"                             , "Launch xkill")
    , prefix "M-s"
      [ ("t", namedScratchpadAction myScratchPads "terminal", "Terminal scratchpad" )
      , ("n", namedScratchpadAction myScratchPads "notes"   , "Notes.org scratchpad")
      , ("w", namedScratchpadAction myScratchPads "weather" , "Weather scratchpad")
      , ("p", namedScratchpadAction myScratchPads "ipython" , "IPython intercative shell")
      , ("j", namedScratchpadAction myScratchPads "julia"   , "Julia intercative shell")
      , ("g", namedScratchpadAction myScratchPads "ghci"    , "Haskell intercative shell")
      , ("m", namedScratchpadAction myScratchPads "spotify" , "Spotify TUI client")
      , ("h", namedScratchpadAction myScratchPads "htop"    , "System monitoring scratchpad")
      , ("d", namedScratchpadAction myScratchPads "drawing" , "Drawing")
      , ("q", namedScratchpadAction myScratchPads "qutebrowser" , "Qutebrowser")
      ] "Sratchpads"
    , prefix "M-S-e"
       [ ("r"  , spawn "reboot"                                     , "Reboot")
       , ("s"  , spawn "systemctl suspend"                          , "Suspend")
       , ("e"  , io (exitWith ExitSuccess)                          , "Exit XMonad")
       , ("S-s", spawn "shutdown 0"                                 , "Shutdown")
       ] "Power management"
    , prefix "M-g"
      [ ("g", spawnSelected' myAppGrid                , "Grid select favorite apps")
      , ("t", goToSelected $ mygridConfig orange , "Goto selected window"     )
      , ("b", bringSelected $ mygridConfig orange, "Bring selected window"    )
      ] "GridSelect"
    , prefix "M-t"
      [ ("a", TS.treeselectAction tsDefaultConfig tsAll, "TreeSelect All")
      , ("s", TS.treeselectAction tsDefaultConfig tsSystem, "TreeSelect System")
      , ("c", TS.treeselectAction tsDefaultConfig tsCommands, "TreeSelect Commands")
      , ("l", TS.treeselectAction tsDefaultConfig tsLayout, "TreeSelect Layout")
      , ("t", TS.treeselectAction tsDefaultConfig tsTools, "TreeSelect Tools")
      ] "TreesSelect"
    , prefix "M-d"
      [ ("d", shellPrompt myXPConfig, "Shell prompt")
      , ("a", appPrompt myXPConfig  , "Applications prompt")
      , ("h", hooglePrompt myXPConfig  , "Hoogle search prompt")
      , ("g", anonGooglePrompt myXPConfig  , "Anonymous Google search prompt")
      , ("e", withFixingClipboard $ unicodePrompt "/home/iliayar/.xmonad/unicode-prompt/unicode" emojiXPConfig, "Unicode prompt")
      , prefix "p"
        [ ("p", passPrompt myXPConfig, "Get password")
        , ("g", passGeneratePrompt myXPConfig, "Generate password")
        , ("r", passRemovePrompt myXPConfig, "Remove password")
        , ("i", passInsertPrompt myXPConfig, "Add password")
        ] "Pass prompts"
      ] "Prompts"
    , prefix "M-n"
      [ ("a", spawn "dunstctl close-all", "Close all")
      , ("n", spawn "dunstctl close", "Close last")
      , ("h", spawn "dunstctl history-pop", "Show history")
      , ("c", spawn "dunstctl context", "Context action")
      , ("t", spawn "dunstctl set-paused toggle", "Toggle notifications")
      ] "Notifications"
    , prefix "M-C-f" ( createSearchPrompt searchEngines) "Search Engines Prompt"
    , prefix "M-S-f" ( createSearchSelect searchEngines) "Selech Engines Select"
    ]
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i, d ++ " to workspace " ++ n)
        | (n, i, k) <- zip3 myWorkspaces (XMonad.workspaces conf) $ map show [0..9]
        , (f, m, d) <- [ (W.greedyView                   , ""  , "Switch")
                    , (liftM2 (.) W.view W.shift, "-S", "Move window and switch")
                    , (W.shift                        , "-C", "Move window")]]
    ++
    [("M" ++ m ++ "-" ++ k, screenWorkspace sc >>= flip whenJust (windows . f)
    , d ++ " to screen " ++ (show sc))
        | (k, sc) <- zip ["'", "[", "]"] [2, 1, 0]
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
    [ (    "<Print>", scrot ""                              0, "Screenshot"                 )
    , (  "M-<Print>", scrot "-u"                            0, "Screenshot of active window")
    , ("M-S-<Print>", scrot "-a $(slop -f '%x,%y,%w,%h')" 0.1, "Screenshot interactive"     )
    , ("<XF86AudioNext>", musicBlock "next", "Music next"            )
    , ("<XF86AudioPrev>", musicBlock "prev", "Music prev"            )
    , ("<XF86AudioPlay>", musicBlock "play-pause", "Music play/pause")
    ]
  in mkKeymap conf $ map dropRdTuple keymap
    where
      scrot p t = spawn $ "sleep " ++ (show t) ++ ";\
                \scrot " ++ p ++ " -e '\
                                \xclip -selection clipboard -t image/png -i $f;\
                                \mv $f ~/Pictures/screenshots/;\
                                \notify-send -a 'XMonad' 'Scrot' \"Screenshot copied to clipboard\" -i \"~/Pictures/screenshots/$f\"'"
      playerctl a = spawn $ "playerctl " ++ a ++ " -p spotify"
      rustBlocks block cmd = spawn $ "rust-blocks-client " ++ block ++ " " ++ cmd
      musicBlock = rustBlocks "music"
      createSearchPrompt = map (\ (a, b, c) -> (a, S.promptSearch myXPConfig b, c))
      createSearchSelect = map (\ (a, b, c) -> (a, S.selectSearch b, c))
      archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
      googleTranslate = S.searchEngine "google-translate" "https://translate.google.com/?hl=ru&sl=en&tl=ru&op=translate&text="
      searchEngines = [ ("g", S.google, "Google")
                      , ("h", S.hoogle, "Hoogle")
                      , ("i", S.images, "Images")
                      , ("d", S.duckduckgo, "DuckDuckGo")
                      , ("a", archwiki, "Arch Wiki")
                      , ("t", googleTranslate, "Google Translate")
                      ]
      withFixingClipboard f = do
        f
        spawn $ wrapZsh "xclip -o -selection primary | xclip -selection clipboard"
        return()

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-------------------------------------------------
----------------- Layouts -----------------------
-------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts
         $ onWorkspace (myWorkspacesClickable !! 9) tabs
         $ windowArrange
         $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
         $ T.toggleLayouts floats
         -- $ smartBorders
         $   tall
         ||| floats
         ||| noBorders tabs
         ||| magnify
         ||| tallTabbed
         -- ||| noBorders monocle
         -- ||| spirals
         ||| grid
         -- ||| showoff
         -- ||| tile
  where
    tall    = renamed [Replace "tall"]
            $ limitWindows 12
            $ mySpacing' 4
            $ mkToggle (single MIRROR)
            $ ResizableTall 1 (3/100) (1/2) []
    tallTabbed =
            renamed [Replace "tall tabbed"]
            $ addTabs shrinkText tabbedConf $ subLayout [] Simplest tall
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
            -- $ mySpacing 4
            $ mkToggle (single MIRROR)
            $ mkToggle (single NOBORDERS)
            $ Grid (16/10)
    showoff = renamed [Replace "showoff"]
            $ limitWindows 12
            $ mySpacing 8
            $ mkToggle (single MIRROR)
            $ mkToggle (single NOBORDERS)
            $ Grid (16/10)
    tile    = renamed [Replace "tile"]
            $ limitWindows 12
            $ ResizableTall 1 (5/100) (1/3) []
    tabs    = renamed [Replace "tabs"]
            $ tabbed shrinkText tabbedConf
    tabbedConf = def
                   { fontName            = myFont ++ ",Noto Color Emoji"
                   , activeColor         = Theme.color0
                   , inactiveColor       = Theme.background
                   , activeBorderColor   = Theme.color4
                   , inactiveBorderColor = Theme.color0
                   , activeTextColor     = Theme.color4
                   , inactiveTextColor   = Theme.foreground
                   }

-------------------------------------------------
------------------ Hooks ------------------------
-------------------------------------------------

myManageHook = composeAll
  [ isFullscreen                       --> doFullFloat
  , className =? "Nitrogen"            --> doFloat
  , className =? "feh"                 --> doFloat
  , className =? "Peek"                --> doFloat
  , className =? "Gifview"             --> doFloat
  , className =? "Conky"               --> doFloat
  , className =? "yakuake"             --> doFloat
  , resource  =? "stalonetray"         --> doIgnore
  , title     =? "xmessage"            --> doFloat
  , title     =? "Bluetooth Devices"   --> doFloat
  , title     =? "emacs-everywhere"    --> doFloat
  , title     =? "temp-term"           --> (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)
  , title     =? "temp-term-quake"     --> (customFloating $ W.RationalRect 0 0 1 0.5)
  , title     =? "neofetch-term"       --> (customFloating $ W.RationalRect 0.5 0.05 0.45 0.45)
  , manageDocks
  , namedScratchpadManageHook myScratchPads
  ] <+> manageSpawn

myCommands = [ ("Nix flake config", (spawn "emacsclient -c -a emacs -e '(find-file \"~/Repos/dotfiles/flake.nix\")'")) -- 1 Action on bar icon
             ]
             ++
             [("Switch to workspace " ++ n, windows $ W.greedyView i) | (n, i) <- zip myWorkspaces myWorkspacesClickable]
             ++
             [ ("Switch layout", sendMessage NextLayout)
             , ("Music scratchpad", namedScratchpadAction myScratchPads "spotify")
             ]


myEventHook = serverModeEventHook' (return myCommands)
          <+> serverModeEventHookCmd
          <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
          <+> docksEventHook

myStartupHook = do
          applyScreenLayout defaultScreenLayout
          spawnOnce "killall -9 conky; run_conky"
          spawn     "killall rust-blocks; rust-blocks &"
          spawn     "nitrogen --restore"
          setWMName "LG3D"

-------------------------------------------------
------------------ Prompts ----------------------
-------------------------------------------------

emojiXPConfig :: XPConfig
emojiXPConfig = def
      { font                = myFont ++ ",Noto Color Emoji:size=12:antialise=true,Symbola:regular:size=12:antialise=true"
      , bgColor             = Theme.background
      , fgColor             = Theme.foreground
      , bgHLight            = Theme.color0
      , fgHLight            = Theme.color1
      , borderColor         = Theme.color8
      , promptBorderWidth   = 2
      , promptKeymap        = defaultXPKeymap
      , position            = Top
      , height              = 25
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , sorter              = fuzzySort
      , maxComplRows        = Nothing
      }


myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = Theme.background
      , fgColor             = Theme.foreground
      , bgHLight            = Theme.color0
      , fgHLight            = Theme.color1
      , borderColor         = Theme.color8
      , promptBorderWidth   = 2
      , promptKeymap        = defaultXPKeymap
      , position            = Top
      , height              = 25
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , sorter              = fuzzySort
      , alwaysHighlight     = True
      , maxComplRows        = Nothing
      }

data App = App

instance XPrompt App where
  showXPrompt App = "Application: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion


newtype PairFirst a b = PairFirst {getPairFirst :: (a, b)}
instance Eq a => Eq (PairFirst a b) where
  PairFirst (x, _) == PairFirst (y, _) = x == y

appPrompt :: XPConfig -> X ()
appPrompt c = do
  xdgDirs' <- fmap ((++[ "/home/iliayar/.local/share" ]) . splitOn ":") $ io $ getEnv "XDG_DATA_DIRS"
  xdgDirs <- fmap catMaybes $ io $ mapM
    (\d -> do
        let nd = d ++ "/applications"
        exists <- doesDirectoryExist nd
        return $ if exists then Just nd else Nothing) xdgDirs'
  li <- fmap (nubPairs . filterWithArgs . catMaybes) $ io $ getApplications xdgDirs
  let compl = \s -> fst <$> filter (fuzzyMatch s . fst) li
  mkXPrompt App c (return . compl) (spawn . (M.fromList li M.!))
  where
    filterWithArgs = filter (\ (_, c) -> not $ '%' `elem` c)
    nubPairs = map getPairFirst . nub . map PairFirst

getApplications :: [FilePath] -> IO [Maybe (String, String)]
getApplications dirs = do
  entries <- foldM (\acc dir -> do
                       ents <- listDirectory dir
                       return $ acc ++ (map (dir </>) ents)) [] dirs
  fileEntries <- filterM doesFileExist entries
  dirEntries <- filterM doesDirectoryExist entries
  filesData <- foldM (\acc -> fmap (acc++) . getApplicationData) [] fileEntries
  if dirEntries == []
    then return filesData
    else fmap (filesData ++) $ getApplications dirEntries

getApplicationData :: FilePath -> IO [Maybe (String, String)]
getApplicationData file = do
  f <- doesFileExist file
  if f
    then do
      t <- readFile file
      let
        names = filter (isPrefixOf "Name=") $ lines t
        cmds = filter (isPrefixOf "Exec=") $ lines t
        values = zip names cmds
      return $ map Just $ addPrefix $ map (bimap getValue getValue) values
  else
    return []
  where
    getValue = dropWhile (==' ') . drop 1 . dropWhile (/='=')
    addPrefix [] = []
    addPrefix (x@(xName, _):xs) = x : (map (first (\ x -> xName ++ " (" ++ x ++ ")")) xs)
  
hooglePrompt c =
    inputPrompt c "Hoogle" ?+ \query ->
        termSpawn tempAlacrittyHold ("hoogle " ++ query)

anonGooglePrompt c =
    inputPrompt c "Anonymous google" ?+ \query ->
        spawn $ "brave 'https://google.com/search?q=" ++ query ++ "' --incognito"

passInsertPrompt c =
    inputPrompt c "Add password" ?+ \query ->
         termSpawn tempAlacritty $ "pass insert " ++ query

-------------------------------------------------
------------------ Main -------------------------
-------------------------------------------------

main = do
        homeDir <- getHomeDirectory
        xmproc0 <- spawnPipe $ "xmobar"
        -- xmproc1 <- spawnPipe $ homeDir ++ "/.config/xmobar/xmobar_mon2"
        xmproc2 <- spawnPipe $ "killall -9 xmobar_top; xmobar_top"
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
                { ppOutput  = \x ->
                    hPutStrLn xmproc0 x
                  -- >> hPutStrLn xmproc1 encX
                  -- >> appendFile "/tmp/.xmonad_data" x
                , ppCurrent = xmobarColor Theme.color2 "" -- . wrap "[" "]"
                , ppVisible = xmobarColor Theme.color3 ""
                , ppTitle   = xmobarColor Theme.color9 "" . shorten 25
                , ppLayout  = (\x -> "<action=~/.xmonad/xmonadctl 12>" ++ x ++ "</action>")
                , ppExtras  = []
                , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                },
        startupHook        = myStartupHook
    }
