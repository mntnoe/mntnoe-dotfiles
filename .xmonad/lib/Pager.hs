------------------------------------------------------------------------------
-- |
-- Module      :  Pager 
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- A pager for DynamicLog showing an for each window on each workspace.
-- TODO: Gets slow when there are many windows. Optimize! Not a problem
--       for casual use however.
-- 
------------------------------------------------------------------------------

module Pager (
    labeledPager
  ) where

-- XMonad modules
import XMonad
import Data.Char (toLower)
import Data.Maybe ( isJust, fromMaybe )
import qualified Data.Map as M
import Data.Map ( (!) )
import Data.List
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

-- Custom modules
import App
import Config
import MyApps
import Utils

-- | The 'DynamicLog' logger to add to 'ppExtras' using the given pretty
--   printer and window label map.
labeledPager :: PP -> X (Maybe String)
labeledPager pp = do
    s       <- gets windowset
    urgents <- readUrgents
    sort'   <- ppSort pp
    wl      <- queryWindows s windowLabelMap
    return $ Just $ pprWindowSet' sort' urgents wl pp s

-- | like 'pprWindowSet', but append to each workspace the outcome of
--   'printWindows'.
pprWindowSet' :: ([W.Workspace String l Window] -> [W.Workspace String l Window])
                                                   -- ^ sorting function
              -> [Window]                          -- ^ urgent windows
              -> M.Map Window String               -- ^ window to symbol map
              -> PP                                -- ^ pretty-Printer
              -> W.StackSet String l Window sid sd -- ^ stack set
              -> String
pprWindowSet' sort' urgents wl pp s 
    = sepBy (ppWsSep pp) . map fmt . sort' $
            map W.workspace (W.current s : W.visible s) ++ W.hidden s
  where 
    this     = W.tag (W.workspace (W.current s))
    visibles = map (W.tag . W.workspace) (W.visible s)

    fmt ws   = (printer ws) pp $ print path ws
      where
        path
            | W.tag ws == this               = hilightIconPath
            | W.tag ws == summonWorkspaceTag = grayIconPath
            | W.tag ws == hiddenWorkspaceTag = grayIconPath
            | otherwise                      = iconPath

    printer ws
        | W.tag ws == this               = ppCurrent
        | W.tag ws `elem` visibles = ppVisible
        | any (\x -> maybe False (== W.tag ws) (W.findTag x s)) urgents  
                                    = \ppC -> ppUrgent ppC . ppHidden ppC
        | isJust (W.stack ws)      = ppHidden
        | otherwise                = ppHiddenNoWindows

    print path ws = printWindows path wl (W.integrate' $ W.stack ws)

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)

-- | Print a concatenated string of symbols for a list of windows.
printWindows :: String              -- ^ icon path
             -> M.Map Window String -- ^ window to symbol map
             -> [Window]            -- ^ windows on the workspace
             -> String
printWindows path wl ws = handleEmpty $ intercalate (icon path "sep.xpm") $ map (\w -> icon path $ fromMaybe defaultIcon (M.lookup w wl)) ws
  where

    icon path i = "^i(" ++ path ++ i ++ ")"

    handleEmpty "" = "^ro(6x6)"
    handleEmpty xs = xs

-- | Query each window in the 'WindowSet' and assign a symbol to it in a map.
queryWindows :: WindowSet -> [(String, Query Bool)] -> X (M.Map Window String)
queryWindows ws lm = do
    mapM (qw lm) (W.allWindows ws) >>= return . M.fromList
  where
    qw :: [(String, Query Bool)] -> Window -> X (Window, String)
    qw [] w           = return (w, defaultIcon)
    qw ((l, q):lqs) w = runQuery q w >>= if_ (return (w, l)) (qw lqs w)


-- | Map windows to symbols for the pager.  Symbols for floating windows are in
--   lower case.
windowLabelMap :: [(String, Query Bool)]
windowLabelMap =
    map whenFloat windows ++ windows
  where

    whenFloat (l, q) = (map toLower l, isFloat <&&> q)

    windows = zip (map icon apps) (map query apps)

