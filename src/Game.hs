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
						   , player :: Int -- -1, 1
						   } deriving Show

data Mode = ModeSplash
		  | ModeStart
		  | ModeWonBlue
		  | ModeWonRed
      | ModeClick
		  deriving(Show, Eq)

data State = State { objectsState  :: [ItemState]
				   , currentPlayer :: Lg.Item
				   , mode          :: Mode
				   , windowSize    :: (Int, Int)
				   , contentScale  :: Float
                   , itemMatrix    :: M.Matrix Lg.Item
				   } deriving Show

-- addState :: B.Position -> ItemState
-- addState coordinates = ItemState { position = coordinates
-- 								 , player = 1 -- TODO: make player whos turn it is
--

-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) state =
    let dbg1 = traceShow (x, y)
        player = currentPlayer state
        oldMatrix = itemMatrix state
        (_, column) = coordsToIndices (x, y)
        (i, j) = Lg.firstFreeIndices column oldMatrix
        newMatrix = if (i > 0) then M.setElem player (i, j) oldMatrix else oldMatrix
        m = mode state
        dbg2 = traceShow ((i, j))
	 in if m == ModeWonRed || m == ModeWonBlue
            then state
        else if (Lg.fourDiag (i, j) newMatrix) || (Lg.fourInARow i newMatrix) || (Lg.fourInACol j newMatrix)
            then if player == Lg.R then state { mode = ModeWonRed } else state { mode = ModeWonBlue }
        else
    	 	dbg1 $ dbg2 $
    	 	state { mode = ModeClick
             , objectsState =  if (i>0) then addState i j state else objectsState state
    		   	  , currentPlayer = if player == Lg.R then Lg.B else Lg.R
                  , itemMatrix = newMatrix
			      }
handleEvent _ state = state

y_osa = [159.5, 159.5-63.5..(-158.5)]
x_osa = [-202.5, -202.5+66.. 195.5]
coordsMatrix = M.matrix (length y_osa) (length x_osa) (\(i, j) -> (x_osa !! (j-1), y_osa !! (i-1)))
coords = [(x, y) | x <- x_osa, y <- y_osa]

addState :: Int -> Int -> State -> [ItemState]
addState i j state =
    let objects = objectsState state
        coords = M.getElem i j coordsMatrix
        exists = any (\item ->  (position item)==coords) objects
    in if exists
        then objects
        else [ItemState { position = coords, player = negate $ player $ head$ Game.objectsState state }] ++ objects

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
												 , player = 1
												 }
									  ]
				     , mode         = ModeSplash
				     , windowSize   = C.windowSize
				     , contentScale = 1
				     , currentPlayer = Lg.B
                     , itemMatrix = M.matrix 6 7 $ \(_, _) -> Lg.U
                     }

resetItemMatrix::M.Matrix Lg.Item -> M.Matrix Lg.Item
resetItemMatrix mat = let n = M.ncols mat
                          m = M.nrows mat
                      in M.matrix m n $ \(_, _) -> Lg.U

-- Game update
update :: Game.State -> Game.State
update oldState = oldState

-- isItemPositionValid position =

-- objectsUpdate :: Game.State -> [ItemState]
-- objectsUpdate oldState =  [ItemState { position = (0,0)
-- 									 , player = 1
-- 									 }
-- 						  ]
-- objectsUpdate oldState =
-- 	let newItemState = case

