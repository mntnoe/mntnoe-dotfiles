-------------------------------------------------------------------------- {{{
-- |
-- Module      :  MyApps
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Per application configuration. See App.
-- 
-------------------------------------------------------------------------- }}}

module MyApps (apps) where

-- Haskell modules
import Data.List

-- XMonad modules
import XMonad
import XMonad.Hooks.ManageHelpers (doRectFloat, doCenterFloat)
import XMonad.StackSet (RationalRect (RationalRect))

-- Custom modules
import App
import Config
import Utils


apps =

    -- Firefox 
    [ nullApp
      { cmd     = spawn "firefox"
      , appType = JumpTo
      , key     = (i, xK_f)
      , query   = className =? "Firefox"
      , icon    = "apps/firefox.xpm"
      }

    -- XTerm (new)
    , nullApp
      { cmd     = spawn "xterm"
      , appType = OpenNew
      , key     = (i, xK_x)
      }

    -- XTerm (jump)
    , nullApp
      { cmd     = spawn "xterm"
      , appType = JumpTo
      , key     = (i, xK_c)
      , query   = fmap (/="xterm-scratchpad") appName
                  <&&>
                  terminalWithTitle (\t -> not (isPrefixOf "root:" t)
                                        && not (isInfixOf  "emerge" t)
                                        && not (isPrefixOf "vim:" t))
      , icon    = "apps/utilities-terminal.xpm"
      }

    -- XTerm (superuser)
    , nullApp
      { query   = terminalWithTitle (\t -> isPrefixOf "root:" t 
                                        || isInfixOf "emerge" t)
      , icon    = "apps/gksu-root-terminal.xpm"
      }

    -- Vim
    , nullApp
      { cmd     = spawn "xvim"
      , appType = JumpTo
      , key     = (i, xK_v)
      , query   = ( className =? "XTerm" <&&> fmap (isPrefixOf "vim:" ) title) <||> className =? "Gvim"
      , icon    = "apps/vim.xpm"
      }

    -- Scratchpad
    , nullApp
      { cmd     = spawn $ xterm "xterm-scratchpad" "screen -dRRS scratchpad"
      , appType = Summon "scratchpad" apps
      , key     = (i, xK_Return)
      , query   = appName =? "xterm-scratchpad"
      , hook    = Just doCenterFloat
      , icon    = "apps/utilities-terminal.xpm"
      }

    -- Emacs
    , nullApp
      { cmd     = spawn "emacs"
      , appType = JumpTo
      , key     = (i, xK_e)
      , query   = className =? "Emacs" <||> fmap (isPrefixOf "emacs:") title
      , icon    = "apps/emacs.xpm"
      }

    -- Gmail
    , nullApp
      { cmd     = spawn "prism gmail"
      , appType = Summon "gmail" apps
      , key     = (u, xK_j)
      , query   = q_prism <&&> fmap ("Gmail" `isPrefixOf`) title
      , hook    = Just prismFloat
      , icon    = "apps/gmail.xpm"
      }

    -- Google Calendar
    , nullApp
      { cmd     = spawn "prism google.calendar"
      , appType = Summon "gcal" apps
      , key     = (u, xK_k)
      , query   = q_prism <&&> fmap (\ x -> isPrefixOf "madsnoe.dk Calendar" x 
                                         || isPrefixOf "Google Calendar" x) title
      , hook    = Just prismFloat
      , icon    = "apps/google-calendar.xpm"
      }

    -- Remember The Milk
    , nullApp
      { cmd     = spawn "prism remember.the.milk"
      , appType = Summon "rtm" apps
      , key     = (u, xK_l)
      , query   = q_prism <&&> fmap (isPrefixOf "Remember The Milk") title
      , hook    = Just prismFloat
      , icon    = "apps/rtm.xpm"
      }

    -- Ordbogen.com
    , nullApp
      { cmd     = spawn "prism ordbogen.com"
      , appType = Summon "ordbogen" apps
      , key     = (u, xK_semicolon)
      , query   = let prefix x = isPrefixOf "ordbogen" x || isPrefixOf "Ordbogen" x
                  in  q_prism <&&> fmap prefix title
      , hook    = Just $ doCenterFloat' (4/10) (5/6)
      , icon    = "apps/ordbogen.xpm"
      }

    -- Nautilus
    , nullApp
      { cmd     = spawn "nautilus ~"
      , appType = JumpTo
      , key     = (i, xK_d)
      , query   = className =? "Nautilus"
      , icon    = "apps/file-manager.xpm"
      }

    -- Eclipse
    , nullApp
      { cmd     = spawn "eclipse"
      , appType = JumpTo
      , key     = (u, xK_g)
      , query   = let eclipse = className =? "Eclipse" 
                      splash  = title =? "." <&&> ( className =? "" <||> appName =? "." ) 
                  in  eclipse <||> splash
      , icon    = "apps/eclipse.xpm"
      }

    -- XDvi
    , nullApp
      { query   = className =? "XDvi"
      , icon    = "apps/adobe.pdf.xpm"
      }

    -- Xpdf
    , nullApp
      { query   = className =? "Xpdf"
      , icon    = "apps/adobe.pdf.xpm"
      }

    -- Evince
    , nullApp
      { query   = className =? "Evince"
      , icon    = "apps/evince.xpm"
      }

    -- Acroread
    , nullApp
      { query   = className =? "Acroread"
      , icon    = "apps/adobe-reader.xpm"
      }

    -- MPlayer
    , nullApp
      { query   = className =? "MPlayer"
      , icon    = "apps/gnome-mplayer.xpm"
      }

    -- VLC
    , nullApp
      { query   = title =? "VLC media player"
      , icon    = "apps/vlc.xpm"
      }

    -- Gimp
    , nullApp
    { query     = className =? "Gimp"
    , icon      = "apps/gimp.xpm"
    }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2" <&&> fmap (isSuffixOf "OpenOffice.org Writer") title
      , icon    = "apps/ooo-writer.xpm"
      }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2" <&&> fmap (isSuffixOf "OpenOffice.org Calc") title
      , icon    = "apps/ooo-calc.xpm"
      }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2" <&&> fmap (isSuffixOf "OpenOffice.org Base") title
      , icon    = "apps/ooo-base.xpm"
      }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2" <&&> fmap (isSuffixOf "OpenOffice.org Draw") title
      , icon    = "apps/ooo-draw.xpm"
      }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2" <&&> fmap (isSuffixOf "OpenOffice.org Impress") title
      , icon    = "apps/ooo-impress.xpm"
      }

    -- OpenOffice
    , nullApp
      { query   = className =? "OpenOffice.org 3.2"
      , icon    = "apps/ooo-gulls.xpm"
      }

    -- VirtualBox
    , nullApp
      { query   = className =? "VirtualBox"
      , icon    = "apps/vmware.xpm"
      }

    -- XChat
    , nullApp
      { query   = className =? "Xchat"
      , icon    = "apps/xchat-gnome.xpm"
      }

    
    -- Gnucash
    , nullApp
      { appType = JumpTo
      , query   = className =? "Gnucash"
      , icon    = "apps/gnucash-icon.xpm"
      }

    
    -- Audacity
    , nullApp
      { cmd     = spawn "audacity"
      , appType = JumpTo
      , query   = className =? "Audacity"
      , icon    = "apps/audacity.xpm"
      }

    
    -- Gnome-session
    , nullApp

      { query   = className =? "Gnome-session"
      , icon    = "apps/gnome-shutdown.xpm"
      }

    
    -- Rhythmbox
    , nullApp
      { query   = className =? "Rhythmbox"
      , icon    = "apps/rhythmbox.xpm"
      }


-- MARK --

    ]


-- Auxiliary functions

terminalWithTitle p = className =? "XTerm" <&&> fmap p title

q_typing_mon  = className =? "Gnome-typing-monitor"
q_nautilus_f  = className =? "Nautilus" <&&> fmap (not . isSuffixOf " - File Browser") title
q_eclipse_spl = title     =? "." <&&> ( className =? "" <||> appName =? "." )
q_prism       = className =? "Prism"
q_xterms      = className =? "XTerm"

prismFloat         = doCenterFloat' (8/10) (5/6)
doCenterFloat' w h = doRectFloat $ RationalRect ((1 - w)/2) ((1 - h)/2) w h

