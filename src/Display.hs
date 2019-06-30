module Display where

import qualified Board
import qualified Game
-- import qualified Config
import qualified Pictures as P


import Graphics.Gloss
import Graphics.Gloss.Game



-- Instead of int Game.State
showAt :: Int -> Picture -> Picture
showAt _ = translate 0.1 10 


splash :: Picture
splash = P.splash

board :: Picture
board =  P.board

redC :: Picture
redC = P.red_circle

-- circle state = showAt (Game.position state) $ P.red_circle
