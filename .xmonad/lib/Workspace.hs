------------------------------------------------------------------------------
-- |
-- Module      :  Workspace 
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Workspace actions.
-- 
------------------------------------------------------------------------------

module Workspace where

-- Haskell modules
import Data.Maybe ( isNothing, isJust )

-- XMonad modules
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Util.WorkspaceCompare (getSortByTag)
import qualified XMonad.StackSet as W

-- Custom modules
import App
import Config
import Utils

-- | Shift a window to a workspace and switch to that workspace in one
--   operation.
shiftView :: WorkspaceId -> WindowSet -> WindowSet
shiftView id ws = shiftView' id ws
  where
    shiftView' id ws = W.greedyView id $ W.shift id ws

shiftViewUngreedy id ws = shiftView' id ws
  where
    shiftView' id ws = W.view id $ W.shift id ws

-- | Perform a workspace transformation on the next workspace in 'WSDirection'
--   of type 'WSType'.
doWithWS :: (String -> (WindowSet -> WindowSet)) -> Direction1D -> WSType -> X ()
doWithWS f dir wstype = do
    i <- findWorkspace getSortByTag dir (WSIs pred) 1
    windows $ f i
  where
    pred = do
        hidden <- isHidden
        return $ (\ws -> notSummon ws && notHidden ws && isWsType ws && hidden ws)

    notSummon ws = W.tag ws /= (summonWorkspaceTag)
    notHidden ws = W.tag ws /= (hiddenWorkspaceTag)

    isWsType ws = wsTypeToPred wstype ws

    wsTypeToPred EmptyWS    = isNothing . W.stack
    wsTypeToPred NonEmptyWS = isJust . W.stack
    wsTypeToPred _          = const False

    isHidden = do
        hs <- gets (map W.tag . W.hidden . windowset)
        return (\ws -> W.tag ws `elem` hs)

-- | Swap workspace contents with next screen and focus it. Useful when you work on
--   a laptop with an external screen and keyboard, and want to switch between them.
swapNextScreen' :: X ()
swapNextScreen' = do 
    ws <- gets windowset
    screenWorkspace (nextScreen ws) ?+ windows . swap (W.currentTag ws)

  where

    nextScreen ws = (W.screen (W.current ws) + 1) 
                    `mod` 
                    fromIntegral (length (W.screens ws))

    swap f t = W.view f . W.greedyView t

