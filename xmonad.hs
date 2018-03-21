import System.IO

import XMonad

import XMonad.Config

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Grid

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import qualified Data.Map as DM


-- set my own terminal
createTerminal = "terminator"

-- set my Mod key
createModKey = mod4Mask

-- create my own workspaces
createWorkspaces = [ "1: init", "2: dev", "3: term", "4: file", "5: text", "6: www", "7: doc", "8: media", "logs" ]

-- set normal border color
createNormalBorderColor = "#8bacaf"

-- set focused border color
createFocusedBorderColor = "#01a1bf"

-- create my own key map
createMyKeys x = DM.union (keys defaultConfig x) (keysToAdd x) 
  where
    keysToAdd = \c -> mkKeymap c $ [
        ("M1-b", sendMessage ToggleStruts),
        ("M1-r", spawn "xmonad --recompile"),
        ("M1-w", spawn "firefox"),
        ("M1-f", spawn "pantheon-files"),
        ("M1-t", spawn "terminator")
     ]

-- general layouts definition
standartLayouts = standartTile ||| Mirror standartTile ||| Grid ||| noBorders Full
  where
        standartTile = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

-- create own layouts
createLayouts = avoidStruts $ smartBorders $ standartLayouts

-- general reaction on hook
createHooks = composeAll . concat $
        [ 
                  [className =? c --> doF (W.shift "1: init") | c <- initialApps]
                , [className =? c --> doF (W.shift "2: dev")  | c <- developmentApps]
                , [className =? c --> doF (W.shift "3: term")  | c <- terminalApps]
                , [className =? c --> doF (W.shift "4: file")  | c <- fileApps]
                , [className =? c --> doF (W.shift "5: text")  | c <- textApps]
                , [className =? c --> doF (W.shift "6: www")  | c <- wwwApps]
                , [className =? c --> doF (W.shift "7: doc")  | c <- docsApps]
                , [className =? c --> doF (W.shift "8: www")  | c <- mediaApps]
                , [manageDocks]
        ]
        where
                initialApps  = ["test"]
                developmentApps = ["Sublime_text"]
                terminalApps = ["Terminator"]
                fileApps = ["Pantheon-files"]
                textApps = ["sublimetext"]
                wwwApps = ["Firefox"]
                docsApps = ["libreoffice"]
                mediaApps = ["vlc"]

-- setup xmobar
createXmobar h = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn h,
                          ppTitle = xmobarColor "#8bacaf" "" . shorten 50,
                          ppCurrent = xmobarColor "#01a19f" "" . wrap "[" "]"
                        }

-- startup actions
startupActions = do
        spawn "feh --bg-scale /home/impfunctionalist/.xmonad/dandelion-flower.jpg"
        spawn "xsetroot -cursor_name left_ptr"
        spawn "terminator"
        setWMName "LG3D"

-- set own parameters
generalParameters xmproc = def {
        terminal           = createTerminal,
        borderWidth        = 3,
        modMask            = createModKey,
        workspaces         = createWorkspaces,
        normalBorderColor  = createNormalBorderColor,
        focusedBorderColor = createFocusedBorderColor,
        keys               = createMyKeys,
        layoutHook         = createLayouts,
        manageHook         = createHooks <+> manageDocks,
        startupHook        = startupActions,
        logHook            = createXmobar xmproc 
}


main = do
        xmproc <- spawnPipe "/usr/bin/xmobar /home/impfunctionalist/.xmonad/xmobarrc" 
        xmonad $ generalParameters xmproc