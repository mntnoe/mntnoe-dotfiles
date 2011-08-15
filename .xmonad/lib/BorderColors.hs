{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------- {{{
-- |
-- Module      :  BorderColors 
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Application specific border colors for XMonad.  You can color any kind of
-- windows, but I found it least confusing when coloring the master window
-- only.  This makes it easy to tell which window has focus, without moving
-- your eyes to the border of the screen, breaking your work flow.
-- 
-------------------------------------------------------------------------- }}}

module BorderColors (colorWhen) where

-- Haskell modules
import Control.Monad (when)

-- XMonad modules
import XMonad

-- | Set the border color when the query is satisfied.  Should be added to the
--   ManageHook.
colorWhen :: Query Bool -> String -> X ()
colorWhen q cl = withFocused $ \w -> runQuery q w >>= flip when (setWindowBorder' cl w)

-- | Give set the border color of a window to the given HTML color code.
setWindowBorder' ::(MonadReader XConf m, MonadIO m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

