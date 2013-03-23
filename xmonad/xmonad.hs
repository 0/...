import qualified Data.Map as M

import qualified XMonad as X

import qualified XMonad.Actions.CopyWindow as CopyW
import qualified XMonad.Actions.CycleWS as Cycle
import qualified XMonad.Actions.DynamicWorkspaces as DynaW
import qualified XMonad.Actions.FloatSnap as Snap
import qualified XMonad.Actions.GridSelect as Grid
import qualified XMonad.Actions.Warp as Warp

import qualified XMonad.Hooks.DynamicLog as DLog
import qualified XMonad.Hooks.ManageDocks as Docks
import qualified XMonad.Hooks.UrgencyHook as Urg

import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.LayoutScreens as Screens
import qualified XMonad.Layout.Magnifier as Mag
import qualified XMonad.Layout.Minimize as Min
import qualified XMonad.Layout.MultiToggle as Multi
import qualified XMonad.Layout.MultiToggle.Instances as MultiI
import qualified XMonad.Layout.Reflect as Refl
import qualified XMonad.Layout.ResizableTile as Resiz
import qualified XMonad.Layout.TwoPane as TwoP

import qualified XMonad.Prompt as Prompt

import qualified XMonad.StackSet as W

import qualified XMonad.Util.Cursor as Cur
import qualified XMonad.Util.EZConfig as EZ
import qualified XMonad.Util.Loggers as Log
import qualified XMonad.Util.Run as Run

main = do
  -- Get the handle of the status bar pipe.
  dzen <- Run.spawnPipe myStatusBar
  -- NoUrgencyHook is necessary so that XMonad pays attention to urgent
  -- windows.
  X.xmonad $ Urg.withUrgencyHook Urg.NoUrgencyHook
           $ myXConfig
                 { X.logHook = do
                       -- Allow window copies to be highlighted in the status
                       -- bar.
                       copies <- CopyW.wsContainingCopies
                       DLog.dynamicLogWithPP $ myDzenPP dzen copies
                 }
           `EZ.additionalKeysP` (myKeyBindings myXConfig)

{----------------
-  Colors, &c.  -
----------------}

myFont = "-*-fixed-medium-r-*-*-12-*-*-*-*-*-*-*"

myNormalFG    = "#ffffff"
myNormalBG    = "#000000"
myFocusedFG   = "#000000"
myFocusedBG   = "#cccccc"
mySpecialFG   = "#aaffaa"
mySpecialBG   = myNormalBG
myAltFG       = myNormalFG
myAltBG       = "#777777"
myUrgentFG    = "#ffffff"
myUrgentBG    = "#ff6600"
mySeparatorFG = myNormalFG
mySeparatorBG = "#000077"
myCopyFG      = "#ff0000"

myExternalMonitor = "VGA1"

{------------------------------
-  Keyboard & mouse bindings  -
------------------------------}

myKeyBindings conf =
    [ ("M-z", X.spawn $ X.terminal conf)
    -- Close only focused instance of focused window.
    , ("M-S-c", CopyW.kill1)
    -- Close all other instances of focused window.
    , ("M-C-S-c", CopyW.killAllOtherCopies)
    -- Focus and arrange windows.
    , ("M-<Tab>", Boring.focusDown)
    , ("M-j", Boring.focusDown)
    , ("M-S-<Tab>", Boring.focusUp)
    , ("M-k", Boring.focusUp)
    , ("M-m", Boring.focusMaster)
    , ("M-S-m", X.windows W.swapMaster)
    , ("M-S-j", X.windows W.swapDown)
    , ("M-S-k", X.windows W.swapUp)
    -- Resize the master and slave areas.
    , ("M-h", X.sendMessage X.Shrink)
    , ("M-l", X.sendMessage X.Expand)
    , ("M-S-h", X.sendMessage Resiz.MirrorShrink)
    , ("M-S-l", X.sendMessage Resiz.MirrorExpand)
    -- Push the window back into tiling.
    , ("M-t", X.withFocused $ X.windows . W.sink)
    -- Change the number of windows in the master area.
    , ("M-,", X.sendMessage (X.IncMasterN 1))
    , ("M-.", X.sendMessage (X.IncMasterN (-1)))
    -- Layout toggles.
    , ("M-C-S-m", X.sendMessage $ Mag.Toggle)
    , ("M-C-f", X.sendMessage $ Multi.Toggle MultiI.FULL)
    , ("M-C-m", X.sendMessage $ Multi.Toggle MultiI.MIRROR)
    , ("M-C-S-f", X.sendMessage $ Multi.Toggle MultiI.NBFULL)
    , ("M-C-b", X.sendMessage $ Multi.Toggle MultiI.NOBORDERS)
    , ("M-C-x", X.sendMessage $ Multi.Toggle Refl.REFLECTX)
    , ("M-C-y", X.sendMessage $ Multi.Toggle Refl.REFLECTY)
    -- Change workspaces.
    , ("M-a", Urg.focusUrgent)
    , ("M-S-a", Urg.clearUrgents)
    , ("M-<R>", Cycle.moveTo Cycle.Next Cycle.HiddenNonEmptyWS)
    , ("M-<L>", Cycle.moveTo Cycle.Prev Cycle.HiddenNonEmptyWS)
    , ("M-s", Cycle.toggleWS)
    , ("M-d", workspaceLeaveWrapper $ Grid.gridselectWorkspace myGSConfig W.greedyView)
    -- Dynamic workspaces.
    , ("M-v", workspaceLeaveWrapper $ DynaW.selectWorkspace myXPConfig)
    , ("M-S-v m", DynaW.withWorkspace myXPConfig (X.windows . W.shift))
    , ("M-S-v c", DynaW.withWorkspace myXPConfig (X.windows . CopyW.copy))
    , ("M-S-v r", DynaW.renameWorkspace myXPConfig)
    -- Physical screens.
    , ("<XF86Display>", X.spawn ("~/bin/toggle-monitor " ++ myExternalMonitor))
    -- Virtual screens.
    , ("M-g 1", Screens.layoutSplitScreen 2 (TwoP.TwoPane 0.5 0.5))
    , ("M-g 2", Screens.layoutSplitScreen 2 (X.Mirror $ TwoP.TwoPane 0.5 0.5))
    , ("M-g 3", X.rescreen)
    -- Boring windows.
    , ("M-b", X.withFocused Min.minimizeWindow)
    , ("M-S-b", X.sendMessage Min.RestoreNextMinimizedWin)
    -- Fling the cursor.
    , ("M-'", Warp.banishScreen Warp.LowerRight)
    -- Lock and suspend.
    , ("M-x l", X.spawn "~/bin/xlock")
    , ("M-x s", X.spawn "~/bin/suspend")
    -- Media, &c. keys.
    , ("<XF86MonBrightnessUp>", X.spawn "xbacklight -steps 1 -time 0 +20")
    , ("<XF86MonBrightnessDown>", X.spawn "xbacklight -steps 1 -time 0 -20")
    , ("<XF86AudioRaiseVolume>", X.spawn "amixer set Master 10%+")
    , ("<XF86AudioLowerVolume>", X.spawn "amixer set Master 10%-")
    , ("<XF86AudioMute>", X.spawn "amixer set Master toggle")
    -- Miscellaneous utilities.
    , ("M-n", X.refresh)
    , ("<Print>", X.spawn "import /tmp/screenshot.png")
    , ("M1-<Space>", X.spawn "urxvt -e alsamixer")
    , ("M-x q", X.spawn "xmonad --recompile && xmonad --restart")
    ]
    ++
    -- Convenient shortcuts for simple workspaces.
    [ ("M-" ++ m ++ k, f k)
        | k <- simpleWorkspaces
        , (f, m) <- [ (workspaceLeaveWrapper . DynaW.addWorkspace, "")
                    , (X.windows . W.shift, "S-")
                    ]
    ]

snapTolerance = Just 50

myMouseBindings (X.XConfig {X.modMask = modm}) = M.fromList $
    -- Set the window to floating mode and move by dragging.
    [ ((modm, X.button1), \w -> X.focus w >>
                                X.mouseMoveWindow w >>
                                Snap.snapMagicMove snapTolerance snapTolerance w >>
                                X.windows W.shiftMaster)
    -- Set the window to floating mode and resize by dragging.
    , ((modm, X.button3), \w -> X.focus w >>
                                X.mouseResizeWindow w >>
                                Snap.snapMagicResize [Snap.R, Snap.D] snapTolerance snapTolerance w >>
                                X.windows W.shiftMaster)
    -- Resize the master and slave areas by scrolling.
    , ((modm, X.button4), \w -> X.sendMessage X.Expand)
    , ((modm, X.button5), \w -> X.sendMessage X.Shrink)
    , ((modm X..|. X.shiftMask, X.button4), \w -> X.sendMessage Resiz.MirrorExpand)
    , ((modm X..|. X.shiftMask, X.button5), \w -> X.sendMessage Resiz.MirrorShrink)
    ]

{---------------
-  Status bar  -
---------------}

-- Right-aligned at the top of the screen.
myStatusBar = "dzen2 -x '0' -y '0' -ta r -fg '" ++ myNormalFG ++ "' -bg '" ++ myNormalBG ++ "' -fn '" ++ myFont ++ "'"

myDzenPP h copies = DLog.defaultPP
    { DLog.ppCurrent = DLog.dzenColor myFocusedFG myFocusedBG . DLog.pad
    , DLog.ppVisible = DLog.dzenColor myAltFG myAltBG . DLog.pad
    , DLog.ppHidden  = checkCopies myNormalFG myNormalBG
    , DLog.ppUrgent  = DLog.dzenColor myUrgentFG myUrgentBG . DLog.wrap ">" "<" . DLog.dzenStrip
    , DLog.ppExtras  = [ Log.dzenColorL mySpecialFG mySpecialBG $ Log.date "%d %a %H:%M:%S"
                       ]
    , DLog.ppTitle   = DLog.dzenColor mySpecialFG mySpecialBG . DLog.shorten 75
    , DLog.ppSep     = DLog.pad $ DLog.dzenColor myNormalFG mySeparatorBG "|"
    -- Don't show the layout.
    , DLog.ppOrder   = \(ws:l:t:exs) -> [t, ws] ++ exs
    , DLog.ppOutput  = Run.hPutStrLn h
    }
    where
      checkCopies usualFG usualBG ws
          | ws `elem` copies = DLog.dzenColor myCopyFG usualBG ws
          | otherwise = DLog.dzenColor usualFG usualBG ws

{-------------------------
-  Workspaces & layouts  -
-------------------------}

-- Workspaces with single-character names that can be keyed in with no
-- modifiers.
simpleWorkspaces = [[x] | x <- "`1234567890-="]

-- Make sure workspaces exist only when they're supposed to.
workspaceLeaveWrapper = DynaW.removeEmptyWorkspaceAfterExcept $ X.workspaces myXConfig

-- A single layout with many toggles.
myLayout = Docks.avoidStruts $
           Multi.mkToggle1 MultiI.FULL $
           Multi.mkToggle1 MultiI.MIRROR $
           Multi.mkToggle1 MultiI.NBFULL $
           Multi.mkToggle1 MultiI.NOBORDERS $
           Multi.mkToggle1 Refl.REFLECTX $
           Multi.mkToggle1 Refl.REFLECTY $
           Mag.magnifierOff $
           Boring.boringWindows $
           Min.minimize $
           -- 1 window in the master pane, taking up half the screen, resizing
           -- by 3% at a time.
           Resiz.ResizableTall 1 0.03 0.5 []

{-----------------------
-  Customized configs  -
-----------------------}

myXPConfig = Prompt.defaultXPConfig
    { Prompt.fgColor     = myNormalFG
    , Prompt.bgColor     = myNormalBG
    , Prompt.font        = myFont
    -- Don't bother keeping track.
    , Prompt.historySize = 0
    }

myXConfig = X.defaultConfig
    { X.terminal           = "urxvt"
    , X.modMask            = X.mod4Mask
    , X.normalBorderColor  = "#000099"
    , X.focusedBorderColor = "#990000"
    -- Disable the default bindings.
    , X.keys               = const $ M.fromList []
    , X.workspaces         = simpleWorkspaces
    , X.mouseBindings      = myMouseBindings
    , X.manageHook         = X.composeAll
                                 [ X.className X.=? "Xmessage" X.--> X.doFloat
                                 , Docks.manageDocks
                                 ]
    , X.layoutHook         = myLayout
    , X.startupHook        = return () >>
                             EZ.checkKeymap myXConfig (myKeyBindings myXConfig) >>
                             Cur.setDefaultCursor Cur.xC_crosshair
    }

myGSConfig = Grid.defaultGSConfig
    { Grid.gs_navigate = Grid.navNSearch
    }
