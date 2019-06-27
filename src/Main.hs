module Main where

import Graphics.Gloss
import Graphics.Gloss.Game 
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.ViewPort

import qualified Display
import qualified Board
import qualified Game
import qualified Config
import qualified Pictures

render :: Game.State -> Picture
render state = Pictures.board

main :: IO ()
main = let size       = Config.windowSize
           position   = (0, 0)
           fps        = 30
           background = white
           window     = InWindow "Connect Four" size position
       in Graphics.Gloss.Game.play
              window
              background
              fps
              Game.initialState
              render
              Game.handleEvent
              [Game.update]

