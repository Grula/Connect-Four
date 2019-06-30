module Display where

import qualified Board
-- import qualified Game
-- import qualified Config
import qualified Pictures as P


import Graphics.Gloss
import Graphics.Gloss.Game

showAt :: Board.Position -> Picture -> Picture
showAt (x, y) = translate x y


splash :: Picture
splash = P.splash

board :: Picture
board =  P.board

-- circle state = showAt (Game.position state) $ P.red_circle
