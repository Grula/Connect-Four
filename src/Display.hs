module Display where

import qualified Board
import qualified Game
import qualified Config
import qualified Pictures


import Graphics.Gloss
import Graphics.Gloss.Game

board :: Picture
board =  Pictures.board