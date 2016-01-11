import System.IO (hPutStrLn)

import XMonad
import XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.Search            (google, promptSearchBrowser)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks         (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Layout.Accordion          (Accordion(..))
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
      , workspaces         = ["α", "β", "γ", "δ"]
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

layout_hook = avoidStruts $
  layoutHook def ||| Accordion

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
              ]

xpconfig :: XPConfig
xpconfig = amberXPConfig

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
                  , ("M-<Return>",   spawn "urxvt") -- TODO: don't hard-code
                  , ("M-i",          spawn "google-chrome-stable")
                  -- Search
                  , ("M-o",          promptSearchBrowser xpconfig "google-chrome-unstable" google)
                  -- Shutdown
                  , ("M-S-<Delete>", spawn "sudo shutdown")
                  ]
