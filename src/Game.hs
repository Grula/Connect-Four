module Game where

import qualified Board as B
import qualified Config as C

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
		  | ModeWon
		  | ModeLost
		  | ModeClick
		  deriving(Show, Eq)

data State = State { objectsState  :: [ItemState]
				   , currentPlayer :: Int
				   , mode          :: Mode
				   , windowSize    :: (Int, Int)
				   , contentScale  :: Float
				   } deriving Show

-- addState :: B.Position -> ItemState
-- addState coordinates = ItemState { position = coordinates
-- 								 , player = 1 -- TODO: make player whos turn it is
-- 								 }

-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) state = let dbg1 = traceShow (x, y)
																	 in
																	 	dbg1 $
																	 	state { mode = ModeClick
																		   	  , objectsState = addState x y state
																		   	  , currentPlayer = negate $ currentPlayer state
																		      }
handleEvent _ state = state

-- Search in matrix for four connected dots
existsFour state item = True

addState :: Float -> Float -> State ->[ItemState]
addState x y state = [ItemState { position = coordsToReal (x,y), player = negate $ player $ head$ Game.objectsState state }] ++ objectsState state


y_osa = [159.5, 159.5-63.5..(-158.5)]
x_osa = [-202.5, -202.5+66.. 195.5]

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

coords = [(x,y) | x <- x_osa, y <- y_osa]

circlesAround:: [(Float,Float)] -> Picture
circlesAround coords = pictures $ fmap (\(x,y)-> color blue $ translate x y $ circle 28) coords



mat = M.fromList 7 6 coords
tmat = M.transpose mat
-- ova matrica tmat predstavlja matricu koordinata krugova sa ekrana ispravnim redosledom






initialState :: State
initialState = State { objectsState = [ItemState { position = (-2000,-2000) -- hack avoid this
												 , player = 1
												 }
									  ]
				     , mode         = ModeSplash
				     , windowSize   = C.windowSize
				     , contentScale = 1
				     , currentPlayer = 1
                     }


-- Game update
update :: Game.State -> Game.State
update oldState =
		let newState  = oldState
			-- dbg = traceShow $ currentPlayer oldState
		in if	existsFour newState Game.objectsState then oldState { mode = ModeWon }
		   else  newState

-- isItemPositionValid position =

-- objectsUpdate :: Game.State -> [ItemState]
-- objectsUpdate oldState =  [ItemState { position = (0,0)
-- 									 , player = 1
-- 									 }
-- 						  ]
-- objectsUpdate oldState =
-- 	let newItemState = case

