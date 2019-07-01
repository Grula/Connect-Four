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
        redCircle = Display.redC
        blueCircle = Display.blueC
        content = pictures [ Display.board
                          , redCircle
                          , redCircle
                          , redCircle
                          ]
    in case Game.mode state of
        Game.ModeSplash -> splashScreen
        _ ->				pictures [content]             
        -- _ ->            pictures [ content 
        						 -- ]

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

