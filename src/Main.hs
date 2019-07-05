module Main where

import Graphics.Gloss
import Graphics.Gloss.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.ViewPort

import qualified Display as D
import qualified Board
import qualified Game
import qualified Config
import qualified Pictures as P

-- circlesAround:: [(Float,Float)] -> Picture
-- circlesAround coords = pictures $ fmap (\(x,y)->  translate x y $ P.grey_circle) coords



render :: Game.State -> Picture
render state = let 
                    splashScreen = D.splash
                    redW = D.splashR
                    blueW = D.splashB 
                    blueCircle = D.blueC
                    content = pictures $ fmap D.piece $ Game.objectsState state
               in case Game.mode state of
                    Game.ModeSplash -> splashScreen
                    Game.ModeWonBlue -> blueW
                    Game.ModeWonRed -> redW
                    _ ->	pictures [content, D.board]

main :: IO ()
main = let size       = Config.windowSize
           position   = (0, 0)
           fps        = 60
           background = light aquamarine
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

