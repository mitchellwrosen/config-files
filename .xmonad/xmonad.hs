{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

import Data.Map (Map)
import Data.Monoid (All)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO (Handle, hPutStrLn)
import XMonad
  (Button, ButtonMask, ChangeLayout(NextLayout), Choose, Dimension, Event,
    EventMask, Full(Full), IncMasterN(IncMasterN), KeyMask, KeySym, Layout,
    ManageHook, Mirror(Mirror), Resize(Expand, Shrink), Tall(Tall), Window,
    WindowSpace, WorkspaceId, X, XConfig(..), (|||), def, io, kill, launch,
    mod4Mask, restart, sendMessage, windows, withFocused, xC_top_left_arrow)
import XMonad.Actions.CycleWS
  (Direction1D(Next, Prev), WSType(WSIs), moveTo, shiftTo)
import XMonad.Actions.Search
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog
  (PP(..), dynamicLogWithPP, dzenEscape, shorten, wrap, xmobarColor,
    xmobarStrip)
import XMonad.Hooks.InsertPosition
  (Focus(Newer), Position(Below), insertPosition)
import XMonad.Hooks.ManageDocks
  (AvoidStruts, avoidStruts, docksEventHook, docksStartupHook, manageDocks)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.FixedColumn (FixedColumn(FixedColumn))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Magnifier (Magnifier, magnifiercz')
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.StackSet
  (focusDown, focusUp, sink, swapDown, swapMaster, swapUp, tag)
import XMonad.Prompt
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified XMonad as X

type (:+) f g = Choose f g
infixr 5 :+

type (:&) f g = ModifiedLayout f g
infixr 5 :&

-- Overall layout: toggle between two layouts (L1 and L2) with Mod+f. In both
-- layouts, avoid painting over xmobar.
type L
  = AvoidStruts
    :& ToggleLayouts L1 L2

type L1
  = WithBorder
    :& Full

type L2
  = SmartBorder
    :& ((Decoration TabbedDecoration DefaultShrinker :& Simplest)
          :+ Tall
          :+ (Magnifier :& FixedColumn)
          :+ Mirror Tall
          :+ (Magnifier :& Mirror Tall))

main :: IO ()
main = do
  xmobar <- spawnPipe "/home/mitchell/.xmonad/xmobar /home/mitchell/.xmonad/xmobarrc"
  launch (myConfig xmobar)

myConfig :: Handle -> XConfig L
myConfig xmobar = XConfig{..}
 where
  borderWidth :: Dimension
  borderWidth = 1

  terminal :: String
  terminal = "urxvt"

  workspaces :: [String]
  workspaces = ["1", "2", "3", "4", "5", "6"]

  layoutHook :: L Window
  layoutHook = avoidStruts (toggleLayouts l1 l2)
   where
    l1 :: L1 Window
    l1 = noBorders Full

    l2 :: L2 Window
    l2 = smartBorders $
          tabbed shrinkText theme
      ||| Tall 1 (3/100) (1/2)
      ||| magnifiercz' 1.6 (FixedColumn 1 1 80 10)
      ||| Mirror (Tall 1 (3/100) (6/10))
      ||| magnifiercz' 3 (Mirror (Tall 1 (3/100) (6/10)))
     where
      theme :: Theme
      theme = def
        { fontName = "xft:DejaVu Sans Mono:pixelsize=12" }

  normalBorderColor :: String
  normalBorderColor = "gray"

  focusedBorderColor :: String
  focusedBorderColor = "red"

  -- Rebind Mod to the Windows key
  modMask :: KeyMask
  modMask = mod4Mask

  keys :: XConfig l -> Map (KeyMask, KeySym) (X ())
  keys cfg = mkKeymap cfg (myKeymap cfg)

  manageHook :: ManageHook
  manageHook = mconcat
    [ -- Draw (but don't manage) dock programs.
      manageDocks

      -- Create new windows *below* the current window, to avoid annoyingly
      -- replacing the master window.
    , insertPosition Below Newer

      -- Scratchpad windows are managed specially: floating, with the default
      -- dimensions.
    , scratchpadManageHookDefault
    ]

  handleEventHook :: Event -> X All
  handleEventHook = docksEventHook

  mouseBindings :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
  mouseBindings = X.mouseBindings def

  logHook :: X ()
  logHook = dynamicLogWithPP PP{..}
   where
    ppCurrent :: String -> String
    ppCurrent = xmobarColor "black" "gray"

    ppExtras :: [X (Maybe String)]
    ppExtras = []

    ppHidden :: String -> String
    ppHidden = xmobarColor "orange" ""

    ppHiddenNoWindows :: WorkspaceId -> String
    ppHiddenNoWindows = id

    ppLayout :: String -> String
    ppLayout = id

    ppOutput :: String -> IO ()
    ppOutput = hPutStrLn xmobar

    ppOrder :: [String] -> [String]
    ppOrder = id

    ppSep :: String
    ppSep = xmobarColor "orange" "" " | "

    ppSort :: X ([WindowSpace] -> [WindowSpace])
    ppSort = (. scratchpadFilterOutWorkspace) <$> getSortByIndex

    ppTitle :: String -> String
    ppTitle = xmobarColor "lightblue" "" . shorten 120

    ppTitleSanitize :: String -> String
    ppTitleSanitize = xmobarStrip . dzenEscape

    ppUrgent :: String -> String
    ppUrgent = xmobarColor "red" "yellow"

    ppVisible :: String -> String
    ppVisible = wrap "(" ")"

    ppWsSep :: String
    ppWsSep = " "

  startupHook :: X ()
  startupHook = do
    -- TODO: check keymap

    -- Some Java GUI compatibility nonsense
    setWMName "LG3D"

    -- Normal looking cursor, not ugly X
    setDefaultCursor xC_top_left_arrow

    -- Shrug, undocumented function in XMonad.Hooks.ManageDocks
    docksStartupHook

  focusFollowsMouse :: Bool
  focusFollowsMouse = False

  clickJustFocuses :: Bool
  clickJustFocuses = True

  clientMask :: EventMask
  clientMask = X.clientMask def

  rootMask :: EventMask
  rootMask = X.rootMask def

  handleExtraArgs :: [String] -> XConfig Layout -> IO (XConfig Layout)
  handleExtraArgs = X.handleExtraArgs def

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
