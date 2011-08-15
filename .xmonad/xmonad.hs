{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

-------------------------------------------------------------------------- {{{
-- |
-- Module      :  xmonad
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Modular xmonad config.
-- 
-- Highlights:
--   * pager with icons for DynamicLog
--   * per application configuration
--   * minimize windows
-- 
-- Requires xmonad 0.9. Note that this work is not finished.
-- There are still lot of things I want to behave differently,
-- and I need to do some cleanup here and there.
-- 
-- Still, I hope you can get inspired by some of my ideas. Enjoy :-)
-- 
-------------------------------------------------------------------------- }}}

-- IMPORTS {{{

-- Haskell modules
import Control.Monad (when, liftM)
import Data.IORef (IORef)
import Data.List
import Data.Maybe (isJust)
import qualified Data.Map as M
import System.IO (Handle)

-- XMonad modules
import XMonad hiding ( (|||) )
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.Place
import XMonad.Hooks.RestoreMinimized
import XMonad.Hooks.ServerMode
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Util.Run (hPutStrLn)
import XMonad.Util.WorkspaceCompare (getSortByTag)

-- Custom modules
import App
import BorderColors
import Commands
import DMenu
import Panel
import Config
import IM
import Layout
import MyApps
import Pager
import Utils
import Workspace
-- }}}

-- MAIN {{{
main ::  IO ()
main = do 
    host <- getHost
    pipes  <- spawnPanels
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ myXConfig host pipes
-- }}}

-- SETTINGS {{{

-- | Layout to show initially, and when issuing the according keybinding.  My
--   desktop is widescreen, but not my laptop.
defaultLayout Laptop  = "Tall"
defaultLayout Netbook = "Wide"

cycledLayouts Laptop  = ["Mirror",            defaultLayout Laptop]
cycledLayouts Netbook = ["Accordion", "Tall", defaultLayout Netbook]

myWorkspaces = map show [1..8] ++ [hiddenWorkspaceTag, summonWorkspaceTag]

-- Colors
myNormalBorderColor  = defaultBG
myFocusedBorderColor = "#3939ff"
masterBorderColor    = "#ff1010"
floatBorderColor     = "#10c010"

myPlacement = withGaps (22, 0, 0, 0) $ smart (0.5,0.5)

myXConfig host pipes = XConfig
    { terminal            = "xterm" -- unused
    , focusFollowsMouse   = True
    , borderWidth         = 3
    , modMask             = mod5Mask -- unused
    , numlockMask         = mod2Mask
    , workspaces          = myWorkspaces
    , normalBorderColor   = myNormalBorderColor
    , focusedBorderColor  = myFocusedBorderColor
    , keys                = myKeys        host
    , mouseBindings       = myMouseBindings
    , handleEventHook     = myHandleEventHook
    , layoutHook          = myLayoutHook
    , manageHook          = myManageHook  host
    , logHook             = myLogHook     host pipes
    , startupHook         = myStartupHook host
    }
-- }}}

-- KEYS/MOUSE {{{

-- | The keybindings are optimized for the Colemak (<http://colemak.com>)
--   keyboard layout.  The keys are placed in the right side of the keyboard,
--   using right alt as the modifier.
myKeys host _ = M.fromList $

    makeKeys apps
    ++

    [ ((i , xK_comma), runCommand)
    , ((i , xK_slash), dmenuRun)
    , ((u , xK_h),     hideSummonWindows apps)

    -- See https://addons.mozilla.org/en-US/firefox/addon/61262.
    , ((is, xK_f),     spawn "firefox -unfocus")

    -- Enhance clipboard functionality in xterm (otherwise, xterm easily
    -- "forgets" the selection). Also, xclip will remember the selection
    -- even if the host app exits.
    , ((i , xK_z), spawn "xclip -selection primary -o | xclip -selection clipboard -i")


    -- FLOATING WINDOWS
    , ((u , xK_p), placeFocused $ myPlacement)
    , ((u , xK_b), withFocused $ windows . W.sink)


    -- WINDOW HANDLING
    , ((i , xK_j), windows W.focusDown >> warpToWindow')
    , ((i , xK_k), windows W.focusUp   >> warpToWindow')
    , ((is, xK_j), windows W.swapMaster)
    , ((i , xK_h), swapOrRaise)
    , ((is, xK_h), swapOrLower)

    , ((i , xK_s), windows $ hideFocused)
    , ((i , xK_r), windows $ restoreLast)

    , ((is, xK_n), kill)
    , ((mod1Mask,  xK_F4), kill)

    -- LAYOUT MESSAGES
    , ((i , xK_space), cycleThroughLayouts $ cycledLayouts host)
    , ((is, xK_space), sendMessage $ JumpToLayout $ defaultLayout host)

    , ((u , xK_n),     sendMessage $ JumpToLayout "NoBorders")
    , ((u , xK_u),     sendMessage $ ToggleStruts)

    , ((im, xK_Right), sendMessage Shrink)
    , ((im, xK_Left),  sendMessage Expand)
    , ((im, xK_Down),  sendMessage MirrorShrink)
    , ((im, xK_Up),    sendMessage MirrorExpand)

    , ((i , xK_Left),  withFocused $ keysMoveWindow (-300,    0))
    , ((i , xK_Right), withFocused $ keysMoveWindow ( 300,    0))
    , ((i , xK_Up),    withFocused $ keysMoveWindow (   0, -200))
    , ((i , xK_Down),  withFocused $ keysMoveWindow (   0,  200))
    , ((is, xK_Left),  withFocused $ snapMove L Nothing)
    , ((is, xK_Right), withFocused $ snapMove R Nothing)
    , ((is, xK_Up),    withFocused $ snapMove U Nothing)
    , ((is, xK_Down),  withFocused $ snapMove D Nothing)


    -- SESSION
    , ((i , xK_Delete),    spawn "gnome-session-save --shutdown-dialog")
    , ((is, xK_BackSpace), spawn "gnome-session-save --logout")
    , ((i , xK_BackSpace), killPanels >> restart "xmonad" True)

    -- WORKSPACES
    -- Note that I have swapped Y and J in my modified Colemak keyboard layout.
    , ((i , xK_y), doWithWS W.greedyView    Prev EmptyWS)
    , ((is, xK_y), doWithWS shiftView       Prev EmptyWS)
    , ((im, xK_y), doWithWS swapWithCurrent Prev EmptyWS)

    , ((i , xK_u), doWithWS W.greedyView    Prev NonEmptyWS)
    , ((is, xK_u), doWithWS shiftView       Prev NonEmptyWS)
    , ((im, xK_u), doWithWS swapWithCurrent Prev NonEmptyWS)

    , ((i , xK_i), doWithWS W.greedyView    Next NonEmptyWS)
    , ((is, xK_i), doWithWS shiftView       Next NonEmptyWS)
    , ((im, xK_I), doWithWS swapWithCurrent Next NonEmptyWS)

    , ((i , xK_o), doWithWS W.greedyView    Next EmptyWS)
    , ((is, xK_o), doWithWS shiftView       Next EmptyWS)
    , ((im, xK_o), doWithWS swapWithCurrent Next EmptyWS)

    , ((i , xK_l), doWithWS shiftView       Next EmptyWS)
    , ((is, xK_l), doWithWS W.shift         Next EmptyWS)

    , ((i , xK_7), swapNextScreen')
    , ((i , xK_8), toggleWS)
    , ((i , xK_9), screenWorkspace 0 >>= flip whenJust (windows . W.view)            >> warpToWindow')
    , ((is, xK_9), screenWorkspace 0 >>= flip whenJust (windows . shiftViewUngreedy) >> warpToWindow')
    , ((i , xK_0), screenWorkspace 1 >>= flip whenJust (windows . W.view)            >> warpToWindow')
    , ((is, xK_0), screenWorkspace 1 >>= flip whenJust (windows . shiftViewUngreedy) >> warpToWindow')
    ]

-- MOUSE
myMouseBindings _ = M.fromList $
    [ ((mod5Mask,               button1), focusAnd   mouseMoveWindow   $ snapMagicMove (Just 50) (Just 50))
    , ((mod5Mask .|. shiftMask, button1), focusAnd   mouseMoveWindow   $ snapMagicResize [L,R,U,D] (Just 50) (Just 50))
    , ((mod5Mask,               button3), focusAnd   mouseResizeWindow $ snapMagicResize [R,D] (Just 50) (Just 50))

    ]
  where

    -- | Focus and raise the window before performing a mouse operation.
    focusAnd job1 job2 w = focus w >> windows W.swapMaster >> job1 w >> job2 w
-- }}}

-- LAYOUTHOOK {{{

myLayoutHook  
    = avoidStruts
    $ smartBorders
    $ withIM (1/5) (Role "gimp-toolbox")
    (   (named "Wide"   $ Mirror       $ ResizableTall 1 (3/40) (2/3) [])
    ||| (named "Tall"   $ reflectHoriz $ ResizableTall 1 (3/40) (4/7) [])
    ||| (named "Mirror"                $ ResizableTall 1 (3/40) (4/7) [])
    ||| (twoAccordion)
    ||| (named "NoBorders" $ noBorders Full)
    )

-- }}}

-- MANAGEHOOK {{{

myManageHook xs = composeAll
    [ floats                 --> doCenterFloat
    , className =? "MPlayer" --> doFloat
    , ignores                --> doIgnore
    , appManageHook apps
    , manageDocks
    ]
  where
    floats = foldr1 (<||>)
        [ checkDialog
        , title     =? "." <&&> ( className =? "" <||> appName =? "." ) 
        , title     =? "VLC media player"
        , className =? "Nautilus" <&&> fmap (not . isSuffixOf " - File Browser") title
        , className =? "Firefox" <&&> fmap (/="Navigator") appName 
        , flip fmap className $ flip elem
            [ "Gnome_swallow"
            , "Gdmsetup"
            , "Xmessage"
            , "Zenity"
            ]
        ]

    ignores = foldr1 (<||>)
        [ className =? "Gnome-typing-monitor"
        ]
-- }}}

-- HANDLEEVENTHOOK {{{
myHandleEventHook = do
    restoreMinimizedEventHook
    serverModeEventHook' smCommands
-- }}}

-- STARTUP HOOK {{{
myStartupHook :: Host -> X ()
myStartupHook host = do
    broadcastMessage $ JumpToLayout $ defaultLayout $ host
    refresh
-- }}}

-- LOGHOOK {{{
myLogHook :: Host -> [Handle] -> X ()
myLogHook host pipes = do
    -- I found it least confusing when coloring the master window only.  This
    -- makes it easy to tell which window has focus, without moving your eyes
    -- to the border of the screen, as the coloring is based on the window
    -- position.
    colorWhen isMaster masterBorderColor
    -- Make it easy to distinguish between floating and non-floating windows.
    -- Sometimes I accidently makes a window floating without moving it out of
    -- its position.
    colorWhen isFloat floatBorderColor

    mapM_ (\pipe -> dynamicLogString (myPP host) >>= io . hPutStrLn pipe) pipes

-- TODO: refactor
myPP host = defaultPP 
    { ppCurrent         = highlight
    , ppVisible         = pad 2
    -- ppHidden overwrites colors of ppUrgent
    , ppHidden          = pad 6
    , ppHiddenNoWindows = pad 2
    , ppUrgent          = pad 6 . ((dzenColor "#01ce02" "#fcfb03") (adjust " ! ")++) -- temporary solution
    , ppTitle           = pad 2
    , ppLayout          = ifNonDefault host (highlight . adjust)
    , ppWsSep           = ""
    , ppSep             = " "
    , ppSort            = getSortByTag
    , ppOrder           = order
    , ppExtras          = [ labeledPager $ myPP host
                          ]
    } 
  where

    -- Ignore the original workspace list and use labeledPager instead.
    order (_:l:t:ws:[]) = (" " ++ ws):l:adjust t:[]
    order xs            = ["Error in order list: " ++ show xs]

    -- Hide the layout label when default layout is used.
    ifNonDefault host f s 
        | s == defaultLayout host = ""
        | otherwise               = f s

    highlight x = leftIcon ++ dzenColor hilightFG hilightBG x ++ rightIcon

    -- Called every time a text string is shown, making the font appear vertically 
    -- aligned with the icons.
    adjust x = "^p(;+2)" ++ x ++ "^p()"

    pad w x  = concat ["^p(", show w, ")", x, "^p(", show w, ")"]
-- }}}

-- vim: set ft=haskell fdm=marker fdl=1 fdc=4:
