module Display where

import qualified Board
import qualified Game
import qualified Config
import qualified Pictures as P


import Graphics.Gloss
import Graphics.Gloss.Game



-- Instead of int Game.State
showAt :: (Float, Float) -> Picture -> Picture
showAt (x, y) = translate x y

piece :: Game.ItemState -> Picture
piece state =  
  let pl = Game.player state
  in if (pl == 1 ) then showAt (Game.position state) $ P.red_circle
     else showAt (Game.position state) $ P.blue_circle


splash :: Picture
splash = P.splash
splashB = P.splashBlue
splashR = P.splashRed



-- redC :: Picture
-- redC = P.red_circle

blueC :: Picture
blueC = P.blue_circle

-- circle state = showAt (Game.position state) $ P.red_circle

-- fullImage :: Picture -> (Int, Int) -> Picture
-- fullImage picture windowSize =
--            let (_, (picWidth, picHeight)) = boundingBox picture
--                (winWidth, winHeight)      = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
--                horizontalScale            = winWidth / picWidth
--                verticalScale              = winHeight / picHeight
--                scaleFactor                = max horizontalScale verticalScale
--            in scale scaleFactor scaleFactor $ picture

board :: Picture
board = translate 0 (-(fromIntegral $ snd Config.windowSize) / 14) P.board