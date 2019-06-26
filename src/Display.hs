module Display where



import qualified Board
import qualified Game
import qualified Config
import qualified Pictures


import Graphics.Gloss
import Graphics.Gloss.Game

-- showAt :: Board.Position -> Picture -> Picture
-- showAt (x, y) = translate (blockSize * x + boardOffsetHorizontal) (blockSize * y + boardOffsetVertical)

board :: Picture
board =  Pictures.board