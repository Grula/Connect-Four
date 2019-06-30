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
render state = 
    let splashScreen = Display.splash
        boardScreen = Display.board
        redCircle = Display.redC
    in case Game.mode state of
        Game.ModeSplash -> splashScreen
        Game.ModeClick  -> Display.showAt 1 redCircle
        _ ->            boardScreen

main :: IO ()
main = let size       = Config.windowSize
           position   = (0, 0)
           fps        = 30
           background = white
           window     = InWindow "Connect Four" size position
           updates    = \ _ state -> case Game.mode state of
                                                Game.ModeSplash -> state
                                                _               -> Game.update state
       in Graphics.Gloss.Game.play
              window
              background
              fps
              Game.initialState
              render
              Game.handleEvent
              [updates]

