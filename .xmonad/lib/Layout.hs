{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Layout 
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Custom layout algorithms. 
-- 
------------------------------------------------------------------------------

module Layout (
    twoAccordion
    ) where

-- XMonad modules
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LimitWindows


-- Hacked Accordion layout.  Useful for LaTeX editing, where you switch between
-- an editor window and a preview window.  Accordion originally by
-- <glasser (@) mit.edu>.  
twoAccordion = limitSlice 2 TwoAccordion


data TwoAccordion a = TwoAccordion deriving ( Read, Show )

instance LayoutClass TwoAccordion Window where
    pureLayout _ sc ws = zip ups tops ++ [(W.focus ws, mainPane)] ++ zip dns bottoms
     where
       ups    = W.up ws
       dns    = W.down ws
       (top,  allButTop) = splitVerticallyBy (1/3) sc
       (center,  bottom) = splitVerticallyBy (1/2) allButTop
       (allButBottom, _) = splitVerticallyBy (2/3) sc
       mainPane | ups /= [] && dns /= [] = center
                | ups /= []              = allButTop
                | dns /= []              = allButBottom
                | otherwise              = sc
       tops    = if ups /= [] then splitVertically (length ups) top    else []
       bottoms = if dns /= [] then splitVertically (length dns) bottom else []
    description _ = "Accordion"
