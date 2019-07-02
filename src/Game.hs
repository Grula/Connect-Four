module Game where

import qualified Board as B
import qualified Config as C

import Debug.Trace
import Graphics.Gloss.Game
import qualified Data.Vector as V
import System.Random
--import Logic
import qualified Data.Matrix as M
data ItemState = ItemState { position  :: B.Position
                           , player :: Int -- -1, 1, 0
                           , col :: Color
                           } deriving (Show, Eq)

data Mode = ModeSplash
		  | ModeStart
		  | ModeWon
		  | ModeLost
		  | ModeClick
		  deriving(Show, Eq)

data State = State { objectsState :: V.Vector ItemState
				   , mode         :: Mode
				   , windowSize   :: (Int, Int)
				   , contentScale :: Float
				   } deriving (Show, Eq)



-- Key events
-- Respoond when mouse is clicked
handleEvent :: Event -> State -> State
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { mode = ModeStart }
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,y)) state = let dbg1 = traceShow (x,y)
                                                                         dbg2 = traceShow $ coordinatesToPosition (x,y)
                                                                         (i,j) = (1.0,1.0)
                                                                     in dbg1 $ dbg2 $ state { mode = ModeClick
																 , objectsState = (objectsState state){- V.// [(round i ,ItemState { position = (i,j) , col=red
                                                                                                    , player = 1 })] -}
                                                                                     }
handleEvent _ state = state

v = V.fromList $ [ItemState (1,1) 1 red, ItemState (1,2) 0 red]
nv = v V.// [(floor 1.23, ItemState (2,3) 6 blue)]



-- Search in matrix for four connected dots
existsFour state item = True


--1.fiksirati tablu: da vidim odakle dokle ide sta
--2. u koju strukturu cu da organizujem ItemState ove, mora u matricu moju
--3. da nadjem nacin za preslikavanje :: next {xx}
--4. kako da zarotiram ovu tablu? [problem su + i - strana]
--5. kastovanje Float -> Int i Int to Float
--6. Matrica ItemStateova
coordinatesToPosition:: (Float,Float) -> (Int, Int) --na osnovu koordinata pozicija u matrici
-- y ide od -200 do 200 (oblast velicine 400/7) ->i
-- x ide od -240 do 240                        -> j
--
--
-- dat horizontal i vertikal, odstojanje do najblizeg elementa u vektoru
coordinatesToPosition (x,y) = let x_translated = (x + 240.0)
                                  j = round $ x_translated / (480/7);
                              in (j, 1)

-- matrix to List


initialState :: State
--initialState = State { objectsState = []
--				     , mode         = ModeSplash
--				     , windowSize   = C.windowSize
--				     , contentScale = 1
--                     }

configMat = [ "bbbbrrb",
              "bbbbrrb",
              "bbbbbbb",
              "bbbbbbb",
              "bbbbbbb",
              "bbbbbbb"
            ]

--zato sto se nalazi na centru, gde su + i - vrednosti


initialState = State (fmap (\(c,x,y)-> ItemState (x,y) (0) (if c=='r' then red else blue)) con) ModeSplash C.windowSize 1

zipWJ = zip configMat vertical'

fzipWJ = fmap (\(s, j)-> (zip3 s (repeat j) horizontal')) zipWJ

con = V.fromList $ concat fzipWJ

--new Coords

--vertical = [-240 +circleSize  + 10, (-240 + circleSize + 1+240/7).. 240 - circleSize - 10]

vertical' = [-200, -200+65.. 200]
horizontal' = [170, 100.. (-170)]




coords = [(x,y) | y <- horizontal', x <- vertical']

listOfColors = V.fromList $ fmap (\((x,y), c)-> (c,x,y)) $ zip coords $ concat  configMat



initialState' = State (fmap (\(c,x,y)-> ItemState (x,y) (0) (if c=='r' then red else blue)) listOfColors) ModeSplash C.windowSize 1






circleSize = 28
--na osnovu koordinata i boje i Solid/Wired nacrtaj krug
circleAt:: (Float,Float) -> Color -> Char -> Picture
circleAt (x,y) col c = let which = if c == 's' then circleSolid else circle
                       in translate x y $ color col $ which circleSize

--listOfPics  = fmap (\(col, i)-> circleAt (i*70,0) (if col=='r' then red else blue)) configPos


--funkcija koja vraca sliku view ta slika ce biti prikazana
-- bitno je da vraca sliku
--
container::Picture
container = color red $ rectangleWire 480 400

--circle' = color blue $ circle 32

view::State -> Picture
view state = let matrix = objectsState state
                 mappedToPics =  fmap (\i -> circleAt (fst $ position i, snd $ position i)(col i) 's') matrix
            in  pictures $  container : (V.toList $ mappedToPics)

update:: Float ->State ->State
update time state = let c = 1
                        newItems = (objectsState state) -- V.// [(0, ItemState (1.0,1.0) (1) (red))]
                    in  state { objectsState = newItems }

-------------------------------------
  -----------------------------------

-- Game update
--update :: Game.State -> Game.State
--update oldState =
--		let newState  = oldState { Game.objectsState = []
--								 }
--		in if	existsFour newState Game.objectsState then oldState { mode = ModeWon }
--		   else newState

-- isItemPositionValid position =

-- objectsUpdate :: Game.State -> [ItemState]
-- objectsUpdate oldState =  [ItemState { position = (0,0)
-- 									 , player = 1
-- 									 }
-- 						  ]
-- objectsUpdate oldState =
-- 	let newItemState = case

