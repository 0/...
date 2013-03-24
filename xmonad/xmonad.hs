import Data.Map (Map(), fromList)

import System.IO (Handle())

import XMonad ((-->), (.|.), (=?))
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
import qualified XMonad.Util.Run as Run


import qualified VLC

import qualified XMonad.Layout.CompactName as Compact


-- This configuration requires xmonad >=0.11.


main = do
  -- Get the handle of the status bar pipe.
  xmobar <- Run.spawnPipe "xmobar"
  -- NoUrgencyHook is necessary so that XMonad pays attention to urgent
  -- windows.
  X.xmonad $ Urg.withUrgencyHook Urg.NoUrgencyHook
           $ myXConfig
                 { X.logHook = do
                       -- Allow window copies to be highlighted in the status
                       -- bar.
                       copies <- CopyW.wsContainingCopies
                       DLog.dynamicLogWithPP $ myLogPP xmobar copies
                 }
           `EZ.additionalKeysP` myKeyBindings

{----------------
-  Colors, &c.  -
----------------}

myNormalFG    = "#ffffff"
myNormalBG    = "#000000"
myCurrentFG   = myNormalFG
myCurrentBG   = "#888888"
myVisibleFG   = myNormalFG
myVisibleBG   = "#444444"
myUrgentFG    = myNormalFG
myUrgentBG    = "#ff6600"
mySpecial1FG  = "#aaffaa"
mySpecial1BG  = myNormalBG
mySpecial2FG  = "#ffaaff"
mySpecial2BG  = myNormalBG
mySeparatorFG = "#000066"
mySeparatorBG = "#000033"
myCopyFG      = "#ff0000"

myFont  = "DejaVu Sans Mono-8"
myFont' = "xft:" ++ myFont

myTerminal        = "urxvt"
myModMask         = X.mod4Mask
myExternalMonitor = "VGA1"
myVLCSock         = "/tmp/vlc.sock"

{------------------------------
-  Keyboard & mouse bindings  -
------------------------------}

myKeyBindings :: [(String, X.X ())]
myKeyBindings =
    [ ("M-z", X.spawn myTerminal)
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
    , ("M-C-S-m", X.sendMessage Mag.Toggle)
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
    , ("M-M1-<Space>", X.catchIO $ VLC.pause myVLCSock)
    , ("M-M1-S-<L>", X.catchIO $ VLC.prev myVLCSock)
    , ("M-M1-S-<R>", X.catchIO $ VLC.next myVLCSock)
    , ("M-M1-<L>", X.catchIO $ VLC.left myVLCSock)
    , ("M-M1-<R>", X.catchIO $ VLC.right myVLCSock)
    , ("M-M1-<D>", X.catchIO $ VLC.voldn myVLCSock)
    , ("M-M1-<U>", X.catchIO $ VLC.volup myVLCSock)
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

myMouseBindings :: Map (X.KeyMask, X.Button) (X.Window -> X.X ())
myMouseBindings = fromList
    -- Set the window to floating mode and move by dragging.
    [ ((myModMask, X.button1), \w -> X.focus w >>
                                X.mouseMoveWindow w >>
                                Snap.snapMagicMove snapTolerance snapTolerance w >>
                                X.windows W.shiftMaster)
    -- Set the window to floating mode and resize by dragging.
    , ((myModMask, X.button3), \w -> X.focus w >>
                                X.mouseResizeWindow w >>
                                Snap.snapMagicResize [Snap.R, Snap.D] snapTolerance snapTolerance w >>
                                X.windows W.shiftMaster)
    -- Resize the master and slave areas by scrolling.
    , ((myModMask, X.button4), \w -> X.sendMessage X.Expand)
    , ((myModMask, X.button5), \w -> X.sendMessage X.Shrink)
    , ((myModMask .|. X.shiftMask, X.button4), \w -> X.sendMessage Resiz.MirrorExpand)
    , ((myModMask .|. X.shiftMask, X.button5), \w -> X.sendMessage Resiz.MirrorShrink)
    ]
    where snapTolerance = Just 50

{---------------
-  Status bar  -
---------------}

myLogPP :: Handle -> [X.WorkspaceId] -> DLog.PP
myLogPP h copies = DLog.defaultPP
    { DLog.ppCurrent = DLog.xmobarColor myCurrentFG myCurrentBG . DLog.pad
    , DLog.ppVisible = DLog.xmobarColor myVisibleFG myVisibleBG . DLog.pad
    , DLog.ppHidden  = checkCopies myNormalFG myNormalBG
    , DLog.ppUrgent  = DLog.xmobarColor myUrgentFG myUrgentBG . DLog.wrap ">" "<" . DLog.xmobarStrip
    , DLog.ppTitle   = DLog.xmobarColor mySpecial1FG mySpecial1BG . DLog.shorten 75
    , DLog.ppLayout  = DLog.xmobarColor mySpecial2FG mySpecial2BG
    , DLog.ppSep     = DLog.pad $ DLog.xmobarColor mySeparatorFG mySeparatorBG "|"
    , DLog.ppOutput  = Run.hPutStrLn h
    }
    where
      checkCopies usualFG usualBG ws
          | ws `elem` copies = DLog.xmobarColor myCopyFG usualBG ws
          | otherwise = DLog.xmobarColor usualFG usualBG ws

{-------------------------
-  Workspaces & layouts  -
-------------------------}

-- Workspaces with single-character names that can be keyed in with no
-- modifiers.
simpleWorkspaces :: [X.WorkspaceId]
simpleWorkspaces = [[w] | w <- "`1234567890-="]

-- Make sure dynamic workspaces exist only when they're supposed to.
workspaceLeaveWrapper :: X.X () -> X.X ()
workspaceLeaveWrapper = DynaW.removeEmptyWorkspaceAfterExcept simpleWorkspaces

-- A single layout with many toggles.
myLayout = Compact.compactName $
           Docks.avoidStruts $
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

myXPConfig :: Prompt.XPConfig
myXPConfig = Prompt.defaultXPConfig
    { Prompt.fgColor     = myNormalFG
    , Prompt.bgColor     = myNormalBG
    , Prompt.font        = myFont'
    -- Don't bother keeping track.
    , Prompt.historySize = 0
    }

myXConfig = X.defaultConfig
    { X.terminal           = myTerminal
    , X.modMask            = myModMask
    , X.normalBorderColor  = "#000099"
    , X.focusedBorderColor = "#990000"
    -- Disable the default bindings.
    , X.keys               = const $ fromList []
    , X.workspaces         = simpleWorkspaces
    , X.mouseBindings      = const myMouseBindings
    , X.manageHook         = X.composeAll
                                 [ X.className =? "Xmessage" --> X.doFloat
                                 , Docks.manageDocks
                                 ]
    , X.layoutHook         = myLayout
    , X.startupHook        = return () >>
                             EZ.checkKeymap myXConfig myKeyBindings >>
                             Cur.setDefaultCursor Cur.xC_crosshair
    }

myGSConfig :: Grid.GSConfig X.WorkspaceId
myGSConfig = Grid.defaultGSConfig
    { Grid.gs_font     = myFont'
    , Grid.gs_navigate = Grid.navNSearch
    }
