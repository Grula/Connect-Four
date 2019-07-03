module Logic
   where

import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.Maybe as Mb
import qualified Data.List as L

-- preimenovane funkcije
--
makeMat = M.zero 6 7
setF = setFirstFree'




--data Item = Blue | Red | U deriving (Show, Eq)

--konstrukcija matrice

--type MatrixErr = Either String (M.Matrix Item)
--predstavlja ili matricu ili poruku o gresci

type MatrixErr = Either String (M.Matrix Int)

set' (i,j) e mat =
  let n = M.nrows mat
      m = M.ncols mat
  in if (i `elem` [1..n]) && (j `elem` [1..m]) then Right $ M.setElem e (i, j) mat
                               else Left "Error: Index out of bounds\n"

--fromMatrixErr koji ako je Matrix validan vraca ga a ako nije neku default vrednost

fromMatrixErr:: MatrixInt -> MatrixErr -> MatrixInt
fromMatrixErr def mat = case mat of
                         Left _ -> def
                         Right x -> x


type MatrixInt = M.Matrix Int
--matrica ::Int-> Matrix

--matrica n = M.matrix n n $ \(i,j)->elem
--            where elem=U

-- postavi (i, j) element na elem, bez provere funkcija
set i j elem mat = M.setElem elem (i,j) mat


-- data je kolona treba postaviti na  prvu undef vrednost dati element




sample = set 6 5 1 $ set 6 6 1 $ set 3 3 (-1) $ set 4 3 (-1) $ set 5 3 (-1) $ set 6 3 (-1) $ set 6 2 (-1) $ set 6 4 1 $ M.zero 6 7






-- za ove funkcije treba obraditi slucajeve ako se unese neispravan indeks
setFirstFree r mat=
  let vec = M.getCol r mat
      list = V.toList vec
      --mlastU = (V.findIndex (\e -> e /= U)) - 1 vec --indeksiranje u vektoru krece od 0
      --last = Mb.fromMaybe -1 mlastU

   in mat


--ovde obavezno obraditi gresku
--ovde StrErr a -- ili poruka o gresci ili vrednost

--setFirstFree':: Int ->Int->Matrix ->MatrixErr
setFirstFree' c elem mat =
  let col = M.getCol c mat
      m = M.ncols mat
      list = V.toList col
      posU = if (head $ head $ L.group list) == 0 then
                                                  length( head $ L.group list)
                                                 else -1;
  in if posU == (-1) then Left "Can't set element" else (set' (posU, c) elem mat)



-- za datu poziciju i, j i datu matricu proveriti da li se nalaze 4 spojene, dijagonaln vertikalno i horizontalno

--dat vektor da li ima 4 elementa e

type IntErr = Either String Int

-- unosi se red a vraca se item ili porka da takvog nema
fourInARow r mat=
  let row = M.getRow r mat
      list = V.toList row
      g = L.group list
      mf = L.find (\l -> length(l) >=4) g
  in case mf of
        Nothing -> Left "For in a row not found\n"
        Just (x:xs) -> Right x

--obrada maybe a sa caseovima
fourInACol c mat=
  let col = M.getCol c mat
      list = V.toList col
      g = L.group list
      mf = L.find (\l -> length(l) >=4) g
  in case mf of
        Nothing -> Left "For in a row not found\n"
        Just (x:xs) -> Right x


--treba da vidim kako izbrojati tacno da li ih ima 4 u redu
--ne moze ovako


-- funkcija koja mapira matricu
-- Red -> -1
-- Blue -> 1
-- U -> 0

-- treba namapirati celo sranje,
--trazi int -> Int -> Int ne znam zasto
--slika :: Int -> Item -> Int
--slika _ e = case e of
    --        Red -> 1
  --          Blue -> 2
      --      U -> 0

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


fourDiag (i,j) mat =

  let upLeft = (safeGet i j mat):(safeGet (i-1) (j-1) mat):(safeGet (i-2) (j-2) mat):(safeGet (i-3) (j-3) mat):[];
      upRight = (safeGet i j mat):(safeGet (i-1) (j+1) mat):(safeGet (i-2) (j+2) mat):(safeGet (i-3) (j+3) mat):[];
      downLeft = (safeGet i j mat):(safeGet (i+1) (j-1) mat):(safeGet (i+2) (j-2) mat):(safeGet (i+3) (j-3) mat):[];
      downRight = (safeGet i j mat):(safeGet (i+1) (j+1) mat):(safeGet (i+2) (j+2) mat):(safeGet (i+3) (j+3) mat):[];
      together = all (== head upLeft) upLeft : all (== head upRight) upRight : all (== head downLeft) downLeft : all (== head downRight) downRight :[];
      in  if (or together) then Right (mat M.! (i,j))
                           else Left "Diagonal not found!"


--sample1 = [U,U,U,U, Red,Red]
--sample2 = [Red,Blue,Red,Red,Red,Red]
--sample3 = [U,U,U,U, U, U ]


someFunc :: IO ()
someFunc = putStrLn "someFunc"
