------------------------------------------------------------------------------
-- |
-- Module      :  Dzen
-- Copyright   :  (c) Mads N Noe 2010
-- Maintainer  :  mail (@) madsnoe.dk
-- License     :  as-is
--
-- Functions for spawning dzen instances.
--
------------------------------------------------------------------------------

module Panel
    ( spawnPanels
    , killPanels
    , getScreenCount
    ) where

-- Haskell modules
import Control.Monad
import Data.List
import Foreign.C.Types (CInt)
import GHC.IOBase (Handle)
import System.Cmd
import System.Environment (getEnv)
import System.Posix.Files(fileExist)

-- XMonad modules
import Control.Monad
import Graphics.X11.Xlib
import Graphics.X11.Xinerama
import XMonad
import XMonad.Util.Run(spawnPipe)

-- Custom modules
import Config
import Utils

-- | Run before each restart of xmonad to ensure that there
--   will only be the expected panel instances running.
killPanels :: X ()
killPanels = do
    spawn' "killall conky-cli"
    spawn' "killall hbar"
    spawn' "killall trayer"
    return ()

-- | Spawn the applications that make the upper panel.
spawnPanels :: IO ([Handle])
spawnPanels = do
    count <- getScreenCount'
    pipes <- mapM (spawnDzenOnScreen count) [0..count-1]
    spawnTrayer
    return pipes

spawnTrayer = spawn' $ intercalate " "
    [ "trayer"
    , "--edge"            , "top"
    , "--align"           , "right"
    , "--widthtype"       , "pixel"
    , "--width"           , show wTrayer
    , "--heighttype"      , "pixel"
    , "--height"          , height
    , "--margin"          , show $ wHbar + wConky
    , "--transparent"     , "true"
    , "--alpha"           , "0"
    , "--tint"            , convert $ defaultBG
    , "--SetDockType"     , "true"
    , "--SetPartialStrut" , "true"
    , "--expand"          , "true"
    ]
  where
    convert ('#':xs) = '0':'x':xs
    convert xs = xs

-- | spawn' two dzen instances at the top of the screen, reading input
--   from xmonad and hbar respectively.
spawnDzenOnScreen count screen = do

    -- Unfortunately, only one instance of trayer is allowed.
    let wTrayerMaybe = if screen == count - 1 then wTrayer else 0

    (sx, sy, sw, sh) <- getScreenDim screen
    pipes <- spawnPipe $ dzen
        sy              -- vertical position
        sx              -- horizontal position
        (sw - wHbar - wTrayerMaybe - wConky) -- horizontal width
        'l'             -- text align
        ""              -- no actions
    spawnDzenWithConky $ dzen
            sy          -- vertical position
            (sx + sw - wHbar - wConky)  -- horizontal position
            wConky      -- horizontal width
            'r'         -- text align
            ""          -- no actions
    spawn' $ hbar ++ dzen
        sy              -- vertical position
        (sx + sw - wHbar)  -- horizontal position
        wHbar              -- horizontal width
        'r'             -- text align
        ""              -- no actions
    return pipes

  where
    spawnDzenWithConky dest =
        fileExist conkyrc >>=
            (flip when $ do_ $ spawn' $ dzenWithConky conkyrc dest)

    dzenWithConky conkyrc dest = intercalate " " ["conky-cli -c", conkyrc, "|", dest]


-- | Return a string that launches dzen with the given configuration.
dzen :: Num a => a           -- ^ vertical position
              -> a           -- ^ horizontal position
              -> a           -- ^ horizontal width
              -> Char        -- ^ text align
              -> String      -- ^ actions
              -> String
dzen y x w ta e =
        intercalate " "
            [ "dzen2"
            , "-x"  , show x
            , "-w"  , show w
            , "-y"  , show y
            , "-h"  , height
            , "-fn" , quote font
            , "-bg" , quote defaultBG
            , "-fg" , quote defaultFG
            , "-ta" , [ta]
            , "-e"  , quote e
            ]

-- | Get the number of available screens.
getScreenCount :: Num a => X a
getScreenCount = io getScreenCount'

getScreenCount' :: Num a => IO a
getScreenCount' = do
    d <- openDisplay ""
    screens  <- getScreenInfo d
    return $ fromInteger $ toInteger $ length screens

-- | Return the dimensions (x, y, width, height) of screen n.
getScreenDim :: Num a => Int -> IO (a, a, a, a)
getScreenDim n = do
    d <- openDisplay ""
    screens  <- getScreenInfo d
    closeDisplay d
    let rn = screens!!(min (abs n) (length screens - 1))
    case screens of
        []        -> return $ (0, 0, 1024, 768) -- fallback
        [r]       -> return $ (fromIntegral $ rect_x r , fromIntegral $ rect_y r , fromIntegral $ rect_width r , fromIntegral $ rect_height r )
        otherwise -> return $ (fromIntegral $ rect_x rn, fromIntegral $ rect_y rn, fromIntegral $ rect_width rn, fromIntegral $ rect_height rn)

-- | Run the command in the background, ensuring that the
--   value returned is always 0. This is to avoid making
--   spawn break a sequence of commands due to a return
--   value indicating that an error has occured.
spawn' x = spawn $ x ++ "&"

