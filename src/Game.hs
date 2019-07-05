module Game where

import qualified Board as B
import qualified Config as C
import qualified Logic as Lg

import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Matrix as M
import Debug.Trace
import Graphics.Gloss.Game

data ItemState = ItemState { position  :: B.Position
                           , indices :: (Int, Int)
                           , player :: Int -- -1, 1
                           } deriving Show

data Mode = ModeSplash
          | ModeStart
          | ModeWonBlue
          | ModeWonRed
          | ModeClick
          | ModeDrop
          | ModeInit
          | ModeCheck
          deriving(Show, Eq)

data State = State { objectsState  :: [ItemState]
             , currentPlayer :: Lg.Item
             , mode          :: Mode
             , windowSize    :: (Int, Int)
             , contentScale  :: Float
             , itemMatrix    :: M.Matrix Lg.Item
             } deriving Show

-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeInit }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) state = 
    let dbg1 = traceShow (x, y)
        player = currentPlayer state
        oldMatrix = itemMatrix state
        (_, column) = coordsToIndices (x, y)
    in case Lg.firstFreeIndices column oldMatrix of
        Nothing -> state
        Just (i, j) -> 
            let
                newMatrix = M.setElem player (i, j) oldMatrix
                m = mode state
                dbg2 = traceShow ((i, j))
            in if m `elem` [ModeWonBlue, ModeWonRed, ModeDrop]
                then state
            else
               dbg1 $
               state { mode = ModeDrop
                     , objectsState = addState i j state
                     , currentPlayer = if player == Lg.R then Lg.B else Lg.R
                     , itemMatrix = newMatrix
                     }
handleEvent _ state = state

width = fromIntegral $ fst C.windowSize
height' = fromIntegral $ snd C.windowSize 
-- height needs to be adjusted to the board, which is 7x6 boxes
height = height' * 6 / 7
-- coordinates of centers from each box,
-- adjusted for offset from top of the window
y_osa = L.take 6 [height/2 - height/6, height/2 - height/6 - height/6..]
x_osa = L.take 7 [-width/2 + width/14, -width/2 + width/14 + width/7..]
coordsMatrix = M.matrix (length y_osa) (length x_osa) (\(i, j) -> (x_osa !! (j-1), y_osa !! (i-1)))
coords = [(x, y) | x <- x_osa, y <- y_osa]

addState :: Int -> Int -> State -> [ItemState]
addState i j state =
    let objects = objectsState state
        (x, _) = M.getElem i j coordsMatrix
        exists = any (\item ->  (indices item) == (1, j)) objects
    in if exists 
        then objects 
        else ItemState { indices = (i, j), position = (x, height/2), player = negate $ player $ head$ Game.objectsState state } : objects

-- x = (-141.5, 158.5)::(Float, Float)




coordsToIndices :: (Float, Float) -> (Int, Int)
coordsToIndices (x, y) = let
                            distances = fmap (\r -> abs(x-r)) x_osa
                            min_distance_x = minimum distances
                            mmin_index = L.findIndex (==min_distance_x) distances
                            min_index = Mb.fromMaybe (-1) mmin_index
                            distances_y = fmap (\r -> abs(y-r)) y_osa
                            min_distance_y = minimum distances_y
                            mmin_index_y = L.findIndex (==min_distance_y) distances_y
                            min_index_y = Mb.fromMaybe (-1) mmin_index_y
                         in (min_index_y+1, min_index+1) --fixed indexes: [1..]

coordsToReal :: (Float, Float) -> (Float, Float)
coordsToReal (x, y) = let (i, j) = coordsToIndices (x, y)
                      in ((x_osa !! (j-1)), (y_osa !! (i-1))) --hack




-- mat = M.fromList 7 6 coords
-- tmat = M.transpose mat
-- ova matrica tmat predstavlja matricu koordinata krugova sa ekrana ispravnim redosledom






initialState :: State
initialState = State { objectsState = [ItemState { position = (-2000,-2000) -- hack avoid this
                                                 , indices = (-1, -1)
                                                 , player = 1
                                                 }]
                     , mode          = ModeSplash
                     , windowSize    = C.windowSize
                     , contentScale  = 1
                     , currentPlayer = Lg.B
                     , itemMatrix = M.matrix 6 7 $ \(_, _) -> Lg.U
                     }


speed = 10.0 :: Float


topItemInColumn_y :: Int -> [ItemState] -> Float
topItemInColumn_y col items = let res = L.find (\t -> col == snd (indices t)) items
                              in case res of
                                Just r -> snd (position r) + height/6
                                Nothing -> last y_osa

-- Game update
update :: Game.State -> Game.State
update oldState = 
    let 
        m = mode oldState
        (dropItem:rest) = objectsState oldState
      in case m of
            ModeDrop -> 
                let (x, y) = position dropItem
                    (i, j) = indices dropItem
                    dbg1 = traceShow (x, y)
                    minH = topItemInColumn_y j rest 
                in if y - speed > minH
                    then oldState {objectsState = dropItem{position = (x, y - speed)} : rest}
                    else oldState {mode = ModeCheck, objectsState = dropItem{position = (x, y_osa !! (i-1))} : rest}
            ModeCheck -> 
                let 
                    (i, j) = indices dropItem
                    mat = itemMatrix oldState
                    player = currentPlayer oldState
                in 
                    if (Lg.fourDiag (i, j) mat) || (Lg.fourInARow i mat) || (Lg.fourInACol j mat)
                        then if player == Lg.R then oldState { mode = ModeWonBlue } else oldState { mode = ModeWonRed }
                        else oldState {mode = ModeStart}
            _ -> oldState
