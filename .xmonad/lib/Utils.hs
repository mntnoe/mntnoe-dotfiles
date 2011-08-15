------------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Utility functions for XMonad.
-- 
------------------------------------------------------------------------------

module Utils where

-- Haskell modules
import Control.Concurrent.MVar
import Control.Monad (unless, when, liftM)
import Control.Monad.Trans (lift)
import Data.List
import Data.Monoid (Endo(Endo))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Unistd(getSystemID, nodeName)
import qualified Data.Map as M

-- XMonad modules
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicHooks (oneShotHook)
import XMonad.Hooks.FloatNext
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W

-- Other moduls
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Extras


-- GENERAL

-- | Perform k x if x return a 'Just' value.
(?+) :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
x ?+ k = x >>= maybe (return ()) k
infixl 1 ?+

-- | Helper function for use with monads.
if_ :: t -> t -> Bool -> t
if_ t f c = if c 
                  then t
                  else f

-- | Change type to "m ()"
do_ :: (Monad m) => m a -> m ()
do_ x = x >> return ()

quote :: String -> String
quote x = "'" ++ x ++ "'"


-- WINDOW ACTIONS

-- | Swap the focused window with the last window in the stack.
swapBottom :: W.StackSet i l a s sd -> W.StackSet i l a s sd
swapBottom = W.modify' $ \c -> case c of
    W.Stack _ _ [] -> c    -- already bottom.
    W.Stack t ls rs -> W.Stack t (xs ++ x : ls) [] where (x:xs) = reverse rs

-- | Swap the focused window with the following window, or if the window is
--   floating, lower it to the bottom.
swapOrLower :: X ()
swapOrLower = withFocused $ \w ->
    runQuery isFloat w >>= if_ (windows swapBottom) (windows W.swapDown)

-- | Swap the focused window with the preceding window, or if the window is
--   floating, raise it to the top.
swapOrRaise :: X ()
swapOrRaise = withFocused $ \w ->
    runQuery isFloat w >>= if_ (windows W.swapMaster) (windows W.swapUp)

-- spawnOnThisWS :: GHC.IOBase.IORef XMonad.Hooks.DynamicHooks.DynamicHooks-> Query Bool-> String-> X ()
spawnOnThisWS dhr q cmd = withWindowSet $ \ws -> do
    oneShotHook dhr q $ doF $ W.shift $ W.currentTag ws
    spawn cmd

-- | Warp the mouse pointer to the focused window only if the workspace has
--   no floating windows to steal the focus.
warpToWindow' = withWindowSet $ \ws -> do
    let floats  = M.keys $ W.floating ws
        visible = W.integrate' $ W.stack $ W.workspace $ W.current ws
        vf      = floats `intersect` visible
    when (null vf) $ warpToWindow (1/2) (1/2)


-- QUERIES ETC

-- | Is the focused window the \"master window\" of the current workspace?
isMaster :: Query Bool
isMaster = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ Just w == master ws)
  where
    master :: WindowSet -> Maybe Window
    master ws = 
        case W.integrate' $ W.stack $ W.workspace $ W.current ws of
             [] -> Nothing
             (x:xs) -> Just x

-- | Is the focused window a floating window?
isFloat :: Query Bool
isFloat = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ M.member w $ W.floating ws)

-- | Helper to read a property
-- getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- | Check if window is DIALOG window
checkDialog :: Query Bool
checkDialog = ask >>= \w -> liftX $ do
                a <- getAtom "_NET_WM_WINDOW_TYPE"
                dialog <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
                mbr <- getProp a w
                case mbr of
                  Just [r] -> return $ elem (fromIntegral r) [dialog]
                  _ -> return False

-- | Determine the number of physical screens.
countScreens :: (MonadIO m, Integral i) => m i
countScreens = liftM genericLength . liftIO $ openDisplay "" >>= getScreenInfo


-- HOST

-- | For use in cross host configutions.
data Host = Laptop | Netbook deriving Eq

-- | Determine the host.
getHost = do 
    host <- getSystemID
    case nodeName host of
         "mntnoe-laptop"  -> return Laptop
         "mntnoe-netbook" -> return Netbook
         _                -> return Laptop


-- MISC

-- | Return a string that launches xterm with the given 'title', 'appName' and
--   command to execute.
xterm :: String -> String -> String
xterm a e = concat ["xterm -wf -title '", e,  "' -name '", a, "' -e '", e, "'"]

