
-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Config
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Static module configuration which I am too lazy to pass around.
--
-------------------------------------------------------------------------- }}}

module Config where

-- XMonad modules
import XMonad


-- GUI

font         = "Consolas-9:rgba=rgb"
defaultBG    = "#dbdbdb"
defaultFG    = "#000000"
hilightBG    = "#5e8eba"
hilightFG    = "#ffffff"


-- PANEL

wTrayer = 100
wConky  = 140
wHbar   = 280 -- width of piped dzen
height  = "18"

hbar    =  "hbar -cmbdt | "
conkyrc = "/home/mntnoe/.conkyrc-dzen"

-- KEYS

i  = mod5Mask -- (I)SO_LEVEL5_SHIFT
u  = mod4Mask -- S(U)PER
s  = shiftMask
m  = mod1Mask
c  = controlMask
is = i .|. s
im = i .|. m
ic = i .|. c
us = u .|. s

-- APP

-- | Workspace containing "hidden" windows. Treated specially by workspace handling commands.
hiddenWorkspaceTag :: String
hiddenWorkspaceTag = "H"

-- | Workspace containing "summoned" windows. Treated specially by workspace handling commands.
summonWorkspaceTag :: String
summonWorkspaceTag = "S"


-- ICONS

-- | The icons located here are simply 16x16 XPM icons from hicolor, gnome and gnome-colors.
--   TODO: refactor

iconPath          = "/home/mntnoe/.xmonad/icons/default/"
hilightIconPath   = "/home/mntnoe/.xmonad/icons/hilight/"
grayIconPath      = "/home/mntnoe/.xmonad/icons/gray/"

defaultIcon       = "apps/application-default-icon.xpm"

defaultSepIcon    = "^i(/home/mntnoe/.xmonad/icons/default-sep.xpm)"
hilightSepIcon    = "^i(/home/mntnoe/.xmonad/icons/hilight-sep.xpm)"
leftIcon          = "^i(/home/mntnoe/.xmonad/icons/left.xpm)"
rightIcon         = "^i(/home/mntnoe/.xmonad/icons/right.xpm)"

