{-# LANGUAGE TypeOperators #-}

import System.IO (hPutStrLn)

import XMonad
import XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks         (AvoidStruts, avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier     (ModifiedLayout)
import XMonad.Layout.Maximize           (Maximize, maximize, maximizeRestore)
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Util.Cursor
import XMonad.Util.Run                  (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig             (additionalKeysP, removeKeysP)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
   xmproc <- spawnPipe "~/.xmonad/bin/xmobar ~/.xmonad/.xmobarrc"
   xmonad $ def
      { terminal           = "urxvt"
      , workspaces         = ["α", "β", "γ", "δ", "ε", "ζ"]
      , borderWidth        = 2
      , focusFollowsMouse  = False
      , modMask            = mod4Mask -- Rebind Mod to the Windows key
      , manageHook         = manageDocks <+> manageHook def
      , layoutHook         = layout_hook
      , handleEventHook    = docksEventHook <+> handleEventHook def
      , logHook            = dynamicLogWithPP xmobarPP { ppCurrent = xmobarColor "black" "gray"
                                                       , ppHidden  = xmobarColor "orange" ""
                                                       , ppHiddenNoWindows = id
                                                       , ppOutput  = hPutStrLn xmproc
                                                       , ppSep     = xmobarColor "orange" "" " | "
                                                       , ppTitle   = xmobarColor "lightblue" "" . shorten 120
                                                       -- Don't log layout name
                                                       , ppOrder   = \[a,_,b] -> [a,b]
                                                       }
      , startupHook = startup_hook
      }
      `removeKeysP` removeKeys'
      `additionalKeysP` additionalKeys'

type (:+) f g = Choose f g
infixr 5 :+

layout_hook
  :: ModifiedLayout AvoidStruts
       (ModifiedLayout Maximize
         (ModifiedLayout SmartSpacing (Tall :+ Mirror Tall :+ Full)))
     Window
layout_hook = modify (tall ||| Mirror tall ||| Full)
 where
  modify = avoidStruts . maximize . smartSpacing 5
  tall = Tall 1 (3/100) (1/2)

startup_hook :: X ()
startup_hook = do
  setWMName "LG3D"
  setDefaultCursor xC_top_left_arrow

removeKeys' :: [String]
removeKeys' = [ "M-S-<Return>" -- terminal
              , "M-S-c"        -- kill
              , "M-<Tab>"      -- focus down
              , "M-S-<Tab>"    -- focus up
              , "M-h"          -- shrink
              , "M-l"          -- expand
              , "M-<Return>"   -- swap master
              , "M-m"          -- focus master
              ]

xpconfig :: XPConfig
xpconfig = greenXPConfig
  { font = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
  , height = 26
  , historySize = 0
  , promptBorderWidth = 1
  }

additionalKeys' :: [(String, X ())]
additionalKeys' =   -- Window/workspace management
                  [ ("M-c",          kill)
                  -- , ("M-S-<Return>", myAddWorkspacePrompt xpconfig)
                  , ("M-S-c",        removeEmptyWorkspace)
                  , ("M-h",          prevWS)
                  , ("M-l",          nextWS)
                  , ("M-S-h",        shiftToPrev >> prevWS)
                  , ("M-S-l",        shiftToNext >> nextWS)
                  , ("M-M1-h",       sendMessage Shrink)
                  , ("M-M1-l",       sendMessage Expand)
                  -- Shell/browser
                  , ("M-<Return>",   spawn =<< asks (terminal . config))
                  , ("M-i",          spawn "google-chrome-stable")
                  -- Search
                  --, ("M-o",          promptSearchBrowser xpconfig "google-chrome-unstable" google)
                  , ("M-o g", promptSearch xpconfig google)
                  , ("M-o h", promptSearch xpconfig hackage)
                  -- Shutdown
                  , ("M-S-<Delete>", spawn "sudo shutdown")

                  , ("M-m", withFocused (sendMessage . maximizeRestore))
                  -- , ("M-M1-j", sendMessage (ModifySpacing (+1)))
                  ]

-- Like promptSearchBrowser, but open it up so I have access to the flags to
-- pass to the browser. This lets me pass "--new-window" to chrome, so my
-- searches don't appear in new tabs on some random existing browser window.
promptSearchBrowser' :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearchBrowser' config browser (SearchEngine name site) =
    mkXPrompt (Search' name) config (historyCompletionP ("Search [" `isPrefixOf`))
      (\query -> safeSpawn browser ["--new-window", site query])

data Search' = Search' Name
instance XPrompt Search' where
    showXPrompt (Search' name)= "Search [" ++ name ++ "]: "
    nextCompletion _ = getNextCompletion
    commandToComplete _ c = c
