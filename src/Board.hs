module Board where

import qualified Pictures

import Graphics.Gloss

import Data.Maybe
import Data.List


type Position = (Int, Int)

boardData = [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
			 [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
			 [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
			 [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
			 [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
			 [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
			]
