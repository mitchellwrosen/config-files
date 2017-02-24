{-# LANGUAGE TypeOperators #-}

import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO

import XMonad
  (ChangeLayout(NextLayout), Choose, Full(Full), IncMasterN(IncMasterN),
    Mirror(Mirror), Resize(Expand, Shrink), Tall(Tall), Window, X,
    XConfig(XConfig), (|||), def, io, kill, launch, mod4Mask, sendMessage,
    spawn, windows, withFocused, xC_top_left_arrow)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToPrev, shiftToNext)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks
  (AvoidStruts, avoidStruts, docksEventHook, docksStartupHook, manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Spacing (SmartSpacing, smartSpacing)
import XMonad.StackSet (focusDown, focusUp, sink, swapDown, swapUp)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (mkKeymap)

import qualified XMonad as X (XConfig(..))
import qualified XMonad.Hooks.DynamicLog as X (PP(..))

type (:+) f g = Choose f g
infixr 5 :+

type L =
  ModifiedLayout AvoidStruts
    (ModifiedLayout SmartSpacing
      (Tall :+ Mirror Tall :+ Full))

main :: IO ()
main = do
  xmobar <- spawnPipe "/home/mitchell/.xmonad/xmobar /home/mitchell/.xmonad/xmobarrc"
  launch (myConfig xmobar)

myConfig :: Handle -> XConfig L
myConfig xmobar = XConfig
  { X.borderWidth = 2
  , X.terminal = "urxvt"
  , X.workspaces = ["1", "2", "3", "4", "5", "6"]
  , X.layoutHook = layout_hook
  , X.normalBorderColor = "gray"
  , X.focusedBorderColor = "red"
  , X.modMask = mod4Mask -- Rebind Mod to the Windows key
  , X.keys = \cfg -> mkKeymap cfg (keymap cfg)
  , X.manageHook = manageDocks
  , X.handleEventHook = docksEventHook
  , X.mouseBindings = X.mouseBindings def
  , X.logHook = dynamicLogWithPP $ xmobarPP
      { X.ppCurrent = xmobarColor "black" "gray"
      , X.ppHidden  = xmobarColor "orange" ""
      , X.ppHiddenNoWindows = id
      , X.ppOutput  = hPutStrLn xmobar
      , X.ppSep     = xmobarColor "orange" "" " | "
      , X.ppTitle   = xmobarColor "lightblue" "" . shorten 120
      -- Don't log layout name
      , X.ppOrder   = \[a,_,b] -> [a,b]
      }

  , X.startupHook = do
      -- TODO: check keymap
      setWMName "LG3D"
      setDefaultCursor xC_top_left_arrow
      docksStartupHook

  , X.focusFollowsMouse = False
  , X.clickJustFocuses = True
  , X.clientMask = X.clientMask def
  , X.rootMask = X.rootMask def
  , X.handleExtraArgs = X.handleExtraArgs def
  }

keymap :: XConfig l -> [(String, X ())]
keymap cfg =
  [ ("M-,",        sendMessage (IncMasterN 1))
  , ("M-.",        sendMessage (IncMasterN (-1)))
  , ("M-<Return>", spawn (X.terminal cfg))
  , ("M-<Space>",  sendMessage NextLayout)
  , ("M-c",        kill)
  , ("M-h",        prevWS)
  , ("M-i",        spawn "google-chrome-stable")
  , ("M-j",        windows focusDown)
  , ("M-k",        windows focusUp)
  , ("M-l",        nextWS)
  , ("M-p",        spawn "dmenu_run")
  , ("M-t",        withFocused (\w -> windows (sink w)))
  , ("M-S-h",      shiftToPrev >> prevWS)
  , ("M-S-j",      windows swapDown)
  , ("M-S-k",      windows swapUp)
  , ("M-S-l",      shiftToNext >> nextWS)
  , ("M-S-q",      io (exitWith ExitSuccess))
  , ("M-M1-h",     sendMessage Shrink)
  , ("M-M1-l",     sendMessage Expand)
  ]

layout_hook :: L Window
layout_hook = f (tall ||| Mirror tall ||| Full)
 where
  f = avoidStruts . smartSpacing 5
  tall = Tall 1 (3/100) (1/2)
