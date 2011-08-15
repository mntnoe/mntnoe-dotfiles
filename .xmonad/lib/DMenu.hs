-------------------------------------------------------------------------- {{{
-- |
-- Module      :  DMenu 
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- DMenu helper functions.
-- 
-------------------------------------------------------------------------- }}}

module DMenu (dmenu, dmenuRun) where

-- Haskell modules
import Data.List (intercalate)

-- XMonad modules
import XMonad
import XMonad.Util.Run

-- Custom modules
import Config
import Utils

dmenu :: [String] -> X (String)
dmenu opts = run "dmenu" (dmenuArgs "Select:") opts


-- | Run command in path.
dmenuRun :: X ()
dmenuRun = do_ $ safeSpawn "dmenu_run" $ dmenuArgs "Run:"

dmenuArgs :: String -> [String]
dmenuArgs prompt =
    [ "-b"
    , "-fn" , font
    , "-nb" , defaultBG
    , "-nf" , defaultFG
    , "-sb" , hilightBG
    , "-sf" , hilightFG
    , "-p"  , prompt
    ]

run :: String -> [String] -> [String] -> X String
run cmd args opts = io $ runProcessWithInput cmd args (unlines opts)

