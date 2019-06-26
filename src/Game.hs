module Game where

import qualified Board
import qualified Config

import Graphics.Gloss.Game

data ItemState = ItemState { position  :: Board.Position
						   -- , fallingSpeed :: Float
						   } deriving Show

data Mode = ModeStart
		  | ModeWon
		  | ModeLost
		  | ModeClick
		  deriving(Show, Eq)

data State = State { objectsState :: [ItemState]
				   , mode         :: Mode
				   , windowSize   :: (Int, Int)
				   , contentScale :: Float
				   } deriving Show

-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }
handleEvent (EventKey (MouseButton LeftButton) Down _ _) state = state { mode = ModeClick }
handleEvent _ state = state

-- Search in matrix for four connected dots
existsFour state item = True

initialState :: State
initialState = State { objectsState = []
				     , mode         = ModeStart
				     , windowSize   = Config.windowSize
				     , contentScale = 1
                     }


-- Game update
update :: Float -> Game.State -> Game.State
update _ oldState = 
		let newState  = oldState { Game.objectsState = objectsUpdate oldState
								 } 
		in if	existsFour newState Game.objectsState then oldState { mode = ModeWon }
		   else newState

-- isItemPositionValid position =

objectsUpdate :: Game.State -> [ItemState]
objectsUpdate oldState =  [ItemState { position = (0,0)}]
-- 		