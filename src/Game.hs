module Game where

import qualified Board as B
import qualified Config as C

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

data State = State { objectsState :: [ItemState]
				   , mode         :: Mode
				   , windowSize   :: (Int, Int)
				   , contentScale :: Float
				   } deriving Show

				  

-- addState :: B.Position -> ItemState
-- addState coordinates = ItemState { position = coordinates
-- 								 , player = 1 -- TODO: make player whos turn it is
-- 								 }

-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) state = state { mode = ModeClick
																		   , objectsState = [ItemState { position = (x,y)
																		   							   , player = 1
																		   							   }
																		   					]
																		   }
handleEvent _ state = state

-- Search in matrix for four connected dots
existsFour state item = True


initialState :: State
initialState = State { objectsState = [ItemState { position = (-2000,-2000) -- hack avoid this
												 , player = 1
												 }
									  ]
				     , mode         = ModeSplash
				     , windowSize   = C.windowSize
				     , contentScale = 1
                     }


-- Game update
update :: Game.State -> Game.State
update oldState = 
		let newState  = oldState { Game.objectsState = []
								 } 
		in if	existsFour newState Game.objectsState then oldState { mode = ModeWon }
		   else newState

-- isItemPositionValid position =

-- objectsUpdate :: Game.State -> [ItemState]
-- objectsUpdate oldState =  [ItemState { position = (0,0)
-- 									 , player = 1
-- 									 }
-- 						  ]
-- objectsUpdate oldState = 
-- 	let newItemState = case 
				