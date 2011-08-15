
-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Commands
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
-- 
-- Settings for XMonad.Actions.Commands. 
-- 
-------------------------------------------------------------------------- }}}

module Commands where 

-- Haskell modules
import qualified Data.Map as M
import Data.IORef (IORef)
import Data.List
import Data.Maybe
import System.Exit (exitWith, ExitCode(..) )

-- XMonad modules
import XMonad
import XMonad.Actions.Commands hiding (workspaceCommands)
import XMonad.Actions.WindowGo
import qualified XMonad.StackSet as W

-- Custom modules
import App
import Config
import DMenu

-- | Given a list of command\/action pairs, prompt the user to choose a
--   command and return the corresponding action.
-- runCommand :: [(String, X ())] -> X ()
runCommand = do
  let m = commandMap $ dmenuCommands
  choice <- dmenu (M.keys m)
  fromMaybe (return ()) (M.lookup choice m)

-- | Commands for DMenu.
dmenuCommands :: [(String, X ())]
dmenuCommands = 
        [ ("view-summon"        , windows $ W.view summonWorkspaceTag)
        , ("view-hidden"        , windows $ W.view hiddenWorkspaceTag)
        -- , ("restart"           , restart "xmonad" True)
        , ("restart-no-resume" , restart "xmonad" False)
        , ("refresh"           , refresh)
        , ("quit"              , io $ exitWith ExitSuccess)
        ]


-- | Commands for ServerMode.
--   TODO: integrate with dzen.
smCommands :: X [(String, X ())]
smCommands = do
    wsCmds <- workspaceCommands
    return $ take 10 (cycle wsCmds) ++ otherCommands
  where

    otherCommands = 
        [ ("focus-vim" , raiseNext q_vims)
        ]

    q_vims = className =? "Gvim" <||> (className =? "XTerm" <&&> fmap (isPrefixOf "vim:") title)

-- | Generate a list of commands to switch to.
workspaceCommands :: X [(String, X ())]
workspaceCommands = do
    ws <- asks $ workspaces . config
    return $ map makeEntry ws
  where
    makeEntry w = ("view-" ++ w, windows $ W.view w)

-- -- | Generate a list of commands dealing with multiple screens.
-- screenCommands :: [(String, X ())]
-- screenCommands = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
--                       | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
--                       , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
--                  ]

