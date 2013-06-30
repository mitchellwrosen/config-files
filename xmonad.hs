import System.IO (hPutStrLn)

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import qualified XMonad.StackSet as W

main :: IO ()
main = do
   xmproc <- spawnPipe "/usr/bin/xmobar /home/mitchell/.xmobarrc"
   xmonad $ defaultConfig
      { terminal           = "urxvt"
      , workspaces         = ["web", "code", "scrap", "chat", "music", "misc"]
      , borderWidth        = 2
      {-, focusedBorderColor = "red"-}
      {-, normalBorderColor  = "#cccccc"-}
      , focusFollowsMouse  = False
      , modMask            = mod4Mask -- Rebind Mod to the Windows key
      , manageHook         = manageDocks <+> manageHook defaultConfig
      , layoutHook         = avoidStruts $ layoutHook defaultConfig
      , logHook            = dynamicLogWithPP xmobarPP { ppCurrent = xmobarColor "black" "gray"
                                                       , ppHidden  = xmobarColor "orange" ""
                                                       , ppHiddenNoWindows = id
                                                       , ppOutput  = hPutStrLn xmproc
                                                       , ppSep     = xmobarColor "orange" "" " | "
                                                       , ppTitle   = xmobarColor "lightblue" "" . shorten 120
                                                       }
      , startupHook = setWMName "LG3D"
      }
      `removeKeysP` removeKeys'
      `additionalKeysP` additionalKeys'

removeKeys' :: [String]
removeKeys' = [ "M-S-<Return>" -- terminal
              , "M-S-c"      -- kill
              , "M-<Tab>"    -- focus down
              , "M-S-<Tab>"  -- focus up
              , "M-h"        -- shrink
              , "M-l"        -- expand
              , "M-<Return>" -- swap master
              ]

additionalKeys' :: [(String, X ())]
additionalKeys' = [ ("M-<Return>",   spawn "urxvt") -- TODO: don't hard-code
                  , ("M-S-<Return>", windows W.swapMaster)
                  , ("M-c",          kill)
                  , ("M-i",          spawn "chromium")
                  , ("M-S-i",        spawn "chromium --incognito")
                  , ("M-M1-h",       sendMessage Shrink)
                  , ("M-M1-l",       sendMessage Expand)
                  , ("M-h",          prevWS)
                  , ("M-l",          nextWS)
                  , ("M-S-h",        shiftToPrev >> prevWS)
                  , ("M-S-l",        shiftToNext >> nextWS)
                  , ("M-S-<Delete>", spawn "sudo shutdown")
                  ]
