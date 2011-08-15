
-------------------------------------------------------------------------- {{{
-- |
-- Module      :  App
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Per application configuration. See MyApps for use.
-- 
-------------------------------------------------------------------------- }}}

module App
     ( App (..)
     , AppType (..)
     , nullApp
     , raiseApp
     , jumpToOrRestore
     , hideSummonWindows
     , summonWindow
     , hideFocused
     , restoreLast
     , appManageHook
     , makeKeys
     ) where

-- Haskell modules
import Control.Monad (filterM)
import Data.Maybe 
import Data.List

-- XMonad modules
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Core
import XMonad.ManageHook (composeAll)
import qualified XMonad.StackSet as W

-- Custom modules
import Config
import Utils


-- | Holds WM related configuration for a given application.
data App = App
    { cmd     :: X ()                 -- ^ Command used to launch the application.
    , appType :: AppType              -- ^ See AppType.
    , query   :: Query Bool           -- ^ Used to identify the windows owned by the application.
    , key     :: (ButtonMask, KeySym) -- ^ Key binding to launch the application. (0,0) if no key
                                      --   binding is associated.
    , icon    :: String               -- ^ Relative path to the XPM icon used by the Pager module.
    , hook    :: Maybe ManageHook     -- ^ Application ManageHook.
    }


-- | Used when toggling between applications of type Summon.
--   As they are floating, it makes sense to only show one
--   at a time.
instance Eq App where
    (==) App { appType = Summon a _ } 
         App { appType = Summon b _ } = a == b
    _ == _ = False


data AppType = OpenNew -- ^ Open a new instance of the application each time.
             | JumpTo  -- ^ Jump to the workspace containing the application.
             | Summon  -- ^ Summon the application to the current workspace.
                       --   They are typically floating, and used for 
                       --   "transient" tasks.
                     String -- Identifier.
                     [App]  -- Applications to replace when toggling.


-- | Default to these settings when entries are omitted.
nullApp = App
    { cmd     = return ()
    , appType = OpenNew
    , query   = return False
    , key     = (0, 0)
    , icon    = defaultIcon
    , hook    = Nothing
    }


-- Focus an application. How this happens is specified by the application's AppType.
raiseApp App 
    { appType = OpenNew
    , cmd     = c
    }         = c
raiseApp App 
    { appType = JumpTo
    , query   = q
    , cmd     = c
    }         = jumpToOrRestore c q
raiseApp app@App 
    { appType = Summon _ apps
    , query   = q
    }         = summonWindow (filterSummonedApps apps) app


-- | Raise a window as follows. 
--   If there exists a matching window
--     * that is hidden, shift it to the current workspace.
--     * on the current workspace, hide it.
--     * on another workspace, jump to it.
--   Otherwise launch the application.
--   TODO: This behavior made it impossible to cycle between two windows,
--         as we now hide the current window instead of jumping to the next.
--         I'll have to rethink this one eventually, but as I seldomly need 
--         to cycle between windows of the same app, it is not a big
--         problem at the moment.
jumpToOrRestore c q = flip (ifWindows q) c $ \ws -> withWindowSet $ \s -> dispatch ws s
  where

    dispatch ws s = 
        case hidden of
             [] -> jumpToOrHide
             hws -> shiftToCurrent hws
      where

        hidden = filter (\w -> fromMaybe "" (W.findTag w s) == hiddenWorkspaceTag) ws

        shiftToCurrent hws = mapM_ (windows . W.shiftWin (W.currentTag s)) hws

        cws = maybe [] W.integrate $ W.stack $ W.workspace $ W.current s

        jumpToOrHide = 
            case cws `intersect` ws of
                 []  -> jumpTo $ W.peek s
                 iws -> mapM_ (windows . W.shiftWin hiddenWorkspaceTag) iws

        jumpTo (Just w) | w `elem` ws =
            let (_:y:_) = dropWhile (/=w) $ cycle ws -- cannot fail to match
            in  windows $ W.focusWindow y
        jumpTo _ = windows . W.focusWindow . head $ ws


-- | Hide all windows on the current workspace of the AppType Summon.
hideSummonWindows :: [App] ->  X ()
hideSummonWindows apps = withWindowSet $ \s -> do
    let ws = (maybe [] W.integrate . W.stack . W.workspace . W.current) s
        sWinsQuery = foldr1 (<||>) $ map query $ filterSummonedApps apps
    sWins <- filterM (runQuery sWinsQuery) ws
    mapM_ (windows . W.shiftWin summonWorkspaceTag) sWins


-- | Shift the specified app to the current workspace or hide it. 
summonWindow :: [App] -- ^ Apps of type Summon to replace.
             -> App   -- ^ App to summon.
             -> X ()
summonWindow apps app = withWindowSet $ \s -> do
    let ws = (maybe [] W.integrate . W.stack . W.workspace . W.current) s
        q = query app
        o = foldr1 (<||>) $ map query $ filter (app/=) apps

    matchingWins <- filterM (runQuery q) ws
    otherWins    <- filterM (runQuery o) ws

    case matchingWins of
        (x:_) -> do
            hideSummonWindows apps
        [] -> do
            mapM_ (windows . W.shiftWin summonWorkspaceTag) otherWins

            filterAll <- filterM (runQuery (query app)) (W.allWindows s)
            case filterAll of
                (x:_) -> windows $ W.shiftWin (W.currentTag s) x
                []    -> cmd app


-- | Hide the focused window. A hidden window is placed on a workspace that is
--   treated specially by all other workspace handling commands used.
hideFocused :: WindowSet -> WindowSet
hideFocused = W.shift hiddenWorkspaceTag


-- | Restore the window that was hidden most recently, like pushing and pulling
--   from a stack.
restoreLast :: WindowSet -> WindowSet
restoreLast s = maybe s (flip (W.shiftWin $ W.currentTag s) s) $ getHidden s
  where
    getHidden s 
        = listToMaybe
        $ maybe [] (W.integrate' . W.stack) 
        $ listToMaybe 
        $ filter (\wsp -> W.tag wsp == hiddenWorkspaceTag) 
        $ W.workspaces s


-- | Run all the hooks associated with the applications.
appManageHook :: [App] -> ManageHook
appManageHook = composeAll . fmap makeQueriedHook . filter hasHook
  where
    hasHook app = isJust $ hook app
    makeQueriedHook app@App 
        { query = q
        , hook  = Just h
        }       = q --> h
    makeQueriedHook _ = idHook -- never reached


-- | Generate the keybinding list from a list of Apps.
makeKeys :: [App] -> [((ButtonMask, KeySym), X ())]
makeKeys apps = map makeKey $ filter hasKey apps 
  where
    makeKey app = (key app, raiseApp app)
    hasKey app = key app /= (0, 0)


filterSummonedApps = filter (isSummonedApp . appType)
  where
    isSummonedApp (Summon _ _) = True
    isSummonedApp _            = False
