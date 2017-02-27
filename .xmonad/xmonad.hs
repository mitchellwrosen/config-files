{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Monoid
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO
import System.Process
import XMonad
  (ChangeLayout(NextLayout), Choose, Dimension, Full(Full), IncMasterN(IncMasterN),
    ManageHook, Mirror(Mirror), Resize(Expand, Shrink), Tall(Tall), Window, X,
    XConfig(..), (|||), def, io, kill, launch, mod4Mask, restart,
    sendMessage, windows, withFocused, xC_top_left_arrow)
import XMonad.Actions.CycleWS
  (Direction1D(Next, Prev), WSType(WSIs), moveTo, shiftTo)
import XMonad.Actions.Submap (submap)
import XMonad.Core (trace, windowset)
import XMonad.Hooks.DynamicLog
  (PP(..), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks
  (AvoidStruts, avoidStruts, docksEventHook, docksStartupHook, manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders
import XMonad.StackSet
  (focusDown, focusUp, sink, swapDown, swapMaster, swapUp, tag)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import XMonad.Actions.Search
import XMonad.Prompt

import XMonad.Hooks.InsertPosition

import XMonad.Layout.FixedColumn
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Circle
import XMonad.Layout.Magnifier

type (:+) f g = Choose f g
infixr 5 :+

type (:&) f g = ModifiedLayout f g
infixr 5 :&

-- type L =
--   AvoidStruts
--     :& ToggleLayouts L1 L2
--
-- type L1 =
--   WithBorder
--     :& Full
--
-- type L2 =
--   SmartBorder
--     :& ((Decoration TabbedDecoration DefaultShrinker :& Simplest)
--           :+ Tall
--           :+ Mirror Tall)

main :: IO ()
main = do
  xmobar <- spawnPipe "/home/mitchell/.xmonad/xmobar /home/mitchell/.xmonad/xmobarrc"
  launch (myConfig xmobar)

-- myConfig :: Handle -> XConfig L
myConfig xmobar = XConfig
  { borderWidth = 1
  , terminal = "urxvt"
  , workspaces = ["1", "2", "3", "4", "5", "6"]
  , layoutHook = myLayoutHook
  , normalBorderColor = "gray"
  , focusedBorderColor = "white"
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  , keys = \cfg -> mkKeymap cfg (myKeymap cfg)
  , manageHook = myManageHook
  , handleEventHook = docksEventHook
  , mouseBindings = mouseBindings def
  , logHook = myLogHook xmobar
  , startupHook = myStartupHook
  , focusFollowsMouse = False
  , clickJustFocuses = True
  , clientMask = clientMask def
  , rootMask = rootMask def
  , handleExtraArgs = handleExtraArgs def
  }

myManageHook :: ManageHook
myManageHook = mconcat
  [ -- Draw (but don't manage) dock programs.
    manageDocks

    -- Create new windows *below* the current window, to avoid annoyingly
    -- replacing the master window.
  , insertPosition Below Newer

    -- Scratchpad windows are managed specially: floating, with the default
    -- dimensions.
  , scratchpadManageHookDefault
  ]

myLogHook :: Handle -> X ()
myLogHook xmobar = dynamicLogWithPP $ xmobarPP
  { ppCurrent = xmobarColor "black" "gray"
  , ppHidden = xmobarColor "orange" ""
  , ppHiddenNoWindows = id
  , ppOutput = hPutStrLn xmobar
  , ppSep = xmobarColor "orange" "" " | "
  , ppSort = (. scratchpadFilterOutWorkspace) <$> getSortByIndex
  , ppTitle = xmobarColor "lightblue" "" . shorten 120
  , ppOrder = id
  }

myStartupHook :: X ()
myStartupHook = do
  -- TODO: check keymap

  -- Some Java GUI compatibility nonsense
  setWMName "LG3D"

  -- Normal looking cursor, not ugly X
  setDefaultCursor xC_top_left_arrow

  -- Shrug, undocumented function in XMonad.Hooks.ManageDocks
  docksStartupHook

-- layoutHook :: L Window
myLayoutHook = avoidStruts (toggleLayouts l1 l2)
 where
  -- l1 :: L1 Window
  l1 = noBorders Full

  -- l2 :: L2 Window
  l2 = smartBorders $
        tabbed shrinkText theme
    ||| Tall 1 (3/100) (6/10)
    ||| magnifiercz' 1.4 (FixedColumn 1 1 80 10)
    ||| Mirror (Tall 1 (3/100) (80/100))
   where
    theme :: Theme
    theme = def
      { fontName = "xft:DejaVu Sans Mono:pixelsize=12" }

myKeymap :: XConfig l -> [(String, X ())]
myKeymap cfg =
  [ ("M--",        sendMessage (IncMasterN (-1)))
  , ("M-/",        submap (mkKeymap cfg mySearchKeymap))
  , ("M-<Return>", safeSpawn (terminal cfg) [])
  , ("M-<Space>",  sendMessage NextLayout)
  , ("M-c",        kill)
  , ("M-f",        sendMessage ToggleLayout)
  , ("M-h",        moveTo Prev notScratchpad)
  , ("M-i",        safeSpawn "google-chrome-stable" [])
  , ("M-j",        windows focusDown)
  , ("M-k",        windows focusUp)
  , ("M-l",        moveTo Next notScratchpad)
  , ("M-m",        windows swapMaster)
  , ("M-p",        safeSpawn "dmenu_run" [])
  , ("M-s",        scratchpadSpawnAction cfg)
  , ("M-t",        withFocused (\w -> windows (sink w)))
  , ("M-S-=",      sendMessage (IncMasterN 1))
  , ("M-S-h",      shiftTo Prev notScratchpad >> moveTo Prev notScratchpad)
  , ("M-S-j",      windows swapDown)
  , ("M-S-k",      windows swapUp)
  , ("M-S-l",      shiftTo Next notScratchpad >> moveTo Next notScratchpad)
  , ("M-S-r",      restart "/home/mitchell/.xmonad/xmonad" True)
  , ("M-S-q",      io (exitWith ExitSuccess))
  , ("M-M1-h",     sendMessage Shrink)
  , ("M-M1-l",     sendMessage Expand)
  ]

mySearchKeymap :: [(String, X ())]
mySearchKeymap =
  [ ("g", promptSearchBrowser' greenXPConfig "google-chrome-stable" google)
  , ("h", promptSearchBrowser' (greenXPConfig { showCompletionOnTab = False }) "google-chrome-stable" hackage)
  ]

notScratchpad :: WSType
notScratchpad = WSIs (pure (\ws -> tag ws /= "NSP"))

-- Like promptSearchBrowser, but open it up so I have access to the flag to pass
-- to the browser. This lets me pass "--new-window" to chrome, so my searches
-- don't appear in new tabs on some random existing browser window.
promptSearchBrowser' :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearchBrowser' config browser (SearchEngine name site) =
    mkXPrompt (Search' name) config
      (historyCompletionP ("Search [" `isPrefixOf`))
        (\query -> safeSpawn browser ["--new-window", site query])

data Search' = Search' Name

instance XPrompt Search' where
    showXPrompt (Search' name)= name ++ ": "
    nextCompletion _ = getNextCompletion
    commandToComplete _ c = c
