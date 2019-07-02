module Display where

import qualified Board
import qualified Game
-- import qualified Config
import qualified Pictures as P


import Graphics.Gloss
import Graphics.Gloss.Game



-- Instead of int Game.State
showAt :: (Float, Float) -> Picture -> Picture
showAt (x, y) = translate x y

redC :: Game.ItemState -> Picture
redC state =  
		let pl = Game.player state
		in if (pl == 1 ) then showAt (Game.position state) $ P.red_circle
		   else showAt (Game.position state) $ P.blue_circle


splash :: Picture
splash = P.splash

board :: Picture
board =  P.board

-- redC :: Picture
-- redC = P.red_circle

blueC :: Picture
blueC = P.blue_circle

-- circle state = showAt (Game.position state) $ P.red_circle
