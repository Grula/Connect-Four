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


background :: Picture
background = Pictures.board

render :: Game.State -> Picture
render state = Pictures.board

main :: IO ()
main = let size       = Config.windowSize
           position   = (10, 10)
           fps        = 30
           background = black
           window     = InWindow "Connect Four" size position
           -- updates    = \ seconds state -> case Game.mode state of
           --                                     Game.ModeSplash  -> state
           --                                     Game.ModeStill   -> state
           --                                     _                -> Game.update seconds state
       in Graphics.Gloss.Game.play
              window
              background
              fps
              Game.initialState
              render
              Game.handleEvent
              [Game.update]

