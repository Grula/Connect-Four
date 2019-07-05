module Logic
   where

import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.Maybe as Mb
import qualified Data.List as L

import Debug.Trace

data Item = R | B | U deriving Show
instance Eq Item where
                        (==) R R = True
                        (==) B B = True
                        (==) _ _ = False
-- preimenovane funkcije
--
makeMat = M.zero 6 7

--konstrukcija matrice

--type MatrixErr = Either String (M.Matrix Item)
--predstavlja ili matricu ili poruku o gresci

type MatrixErr = Either String (MatrixItem)

set' (i,j) e mat =
  let n = M.nrows mat
      m = M.ncols mat
  in if (i `elem` [1..n]) && (j `elem` [1..m]) then Right $ M.setElem e (i, j) mat
                               else Left "Error: Index out of bounds\n"

--fromMatrixErr koji ako je Matrix validan vraca ga a ako nije neku default vrednost

fromMatrixErr:: MatrixItem -> MatrixErr -> MatrixItem
fromMatrixErr def mat = case mat of
                         Left _ -> def
                         Right x -> x


type MatrixItem = M.Matrix Item
--matrica ::Int-> Matrix

--matrica n = M.matrix n n $ \(i,j)->elem
--            where elem=U

-- postavi (i, j) element na elem, bez provere funkcija
set i j elem mat = M.setElem elem (i,j) mat


-- data je kolona treba postaviti na  prvu undef vrednost dati element


--sample = set 6 5 1 $ set 6 6 1 $ set 3 3 (-1) $ set 4 3 (-1) $ set 5 3 (-1) $ set 6 3 (-1) $ set 6 2 (-1) $ set 6 4 1 $ M.zero 6 7


sample = set 6 5 B $ set 6 6 B $ set 3 3 R  $ set 4 3 R $ set 5 3 R $ set 6 3 R
         $ set 6 2 R $ set 6 4 B $ M.matrix 6 7 $ \(i,j)-> U


--ovde obavezno obraditi gresku
--ovde StrErr a -- ili poruka o gresci ili vrednost


-- setFirstFree' c elem mat =
--   let col = M.getCol c mat
--       m = M.ncols mat
--       list = V.toList col
--       hd = (head $ head $ L.group list)
--       posU = if hd `notElem` [R,B] then
--                               length( head $ L.group list)
--                                                  else -1;
--   in if posU == (-1) then Left "Can't set element" else (set' (posU, c) elem mat)

--ne radi jer je nedefinisano ponasanje Eq za

firstFreeIndices:: Int -> M.Matrix Item -> Mb.Maybe (Int,Int)
firstFreeIndices c mat =
  let col = M.getCol c mat
      columnList = V.toList col
      listofUs = L.takeWhile (\i -> i `notElem` [R,B]) columnList
  in if listofUs == [] then Nothing else Just (length listofUs, c)

setFirstFree :: Int -> Item -> MatrixItem -> MatrixErr
setFirstFree c elem mat =
  let col=M.getCol c mat
      m = M.ncols mat
      listofUs = L.takeWhile (\i -> i `notElem` [R,B]) $ V.toList col
  in if listofUs == [] then Left "Can't set element" else Right $ M.setElem elem (length listofUs,c) mat




-- za datu poziciju i, j i datu matricu proveriti da li se nalaze 4 spojene, dijagonaln vertikalno i horizontalno

--dat vektor da li ima 4 elementa e

type IntErr = Either String Int

-- unosi se red a vraca se item ili porka da takvog nema
-- fourInARow r mat=
--   let row = M.getRow r mat
--       list = V.toList row
--       g = L.group list
--       mf = L.find (\l -> length(l) >=4) g
--   in case mf of
--         Nothing -> Left "For in a row not found\n"
--         Just (x:xs) -> Right x

-- --obrada maybe a sa caseovima
-- fourInACol c mat=
--   let col = M.getCol c mat
--       list = V.toList col
--       g = L.group list
--       mf = L.find (\l -> length(l) >=4) g
--   in case mf of
--         Nothing -> Left "For in a row not found\n"
--         Just (x:xs) -> Right x

fourInARow r mat=
  let row = M.getRow r mat
      list = V.toList row
      g = L.group list
      mf = L.find (\l -> length(l) >=4) g
  in if Mb.isNothing mf
      then False
      else True

fourInACol c mat=
  let col = M.getCol c mat
      list = V.toList col
      g = L.group list
      mf = L.find (\l -> length(l) >=4) g
  in if Mb.isNothing mf
      then False
      else True

-- funkcija koja mapira matricu
-- Red -> -1
-- Blue -> 1
-- U -> 0

--trazi int -> Int -> Int ne znam zasto
slika :: Int -> Item
slika e
  | e==0 = U
  | e==(-1) = R
  | e == 1 = B
  | otherwise = U

--slika' item = case item of
--               Red -> 1
--               Blue -> 2
--               U -> 0

--mappedSample = fmap slika' sample


--konverzija u listu listi


--data (i,j) vratiti da li se dijagonalno nalaze 4 elementa e

-- konverzija matrice u listu listi

--toList:: Matrix a -> [[a]]

-- dijagonalna provera
--data je (i,j) pozicija i matrica, ako se nalazi dijagonalno 4 elementa od te pozicije, vraca taj element, u suprotnom poruku da nema 4 dijagonalno
safeGet = M.safeGet

-- Gets elements from matrix mat from indices in the list
safeGetElems :: [(Int, Int)] -> M.Matrix a -> [Maybe a]
safeGetElems [] _ = []
safeGetElems ((i, j) : rest) mat = let elem = safeGet i j mat
                                   in if Mb.isNothing elem
                                    then [Nothing]
                                    else elem : (safeGetElems rest mat)

-- Gets diagonals of element at (i, j)
getDiags :: (Int, Int) -> M.Matrix a -> ([Maybe a], [Maybe a])
getDiags (i, j) mat = let
                          n = M.nrows mat
                          m = M.ncols mat
                          indices1 = (reverse (zip [i,i-1..1] [j,j-1..1])) ++ (zip [i+1..n] [j+1..m])
                          indices2 = (reverse (zip [i,i-1..1] [j..m])) ++ (zip [i+1..n] [j-1, j-2..1])
                      in (safeGetElems indices1 mat, safeGetElems indices2 mat)

fourDiag :: (Eq a, Show a) => (Int, Int) -> M.Matrix a -> Bool
fourDiag (i, j) mat = let
                        (diag_list_1, diag_list_2) = getDiags (i, j) mat
                        g1 = L.group diag_list_1
                        g2 = L.group diag_list_2
                        mf1 = L.find (\l -> length(l) >= 4) g1
                        mf2 = L.find (\l -> length(l) >= 4) g2
                        dbg1 = traceShow (diag_list_1)
                      in if Mb.isNothing mf1 && Mb.isNothing mf2
                        then dbg1 $ False
                        else dbg1 $ True

--sample1 = [U,U,U,U, Red,Red]
--sample2 = [Red,Blue,Red,Red,Red,Red]
--sample3 = [U,U,U,U, U, U ]


someFunc :: IO ()
someFunc = putStrLn "someFunc"
