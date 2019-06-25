module Lib where
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.Maybe as Mb
import qualified Data.List as L
data Item = Blue | Red | U deriving (Show, Eq)

--konstrukcija matrice

matrica ::Int-> M.Matrix Item

matrica n = M.matrix n n $ \(i,j)->elem
            where elem=U

-- postavi (i, j) element na elem
set i j elem mat = M.setElem elem (i,j) mat


-- data je kolona treba postaviti na  prvu undef vrednost dati element

sample = set 6 5 Red $ set 6 6 Red $ set 3 3 Blue $ set 4 3 Blue $ set 5 3 Blue $ set 6 3 Blue $ set 6 2 Red $ set 6 4 Red $ matrica 6




-- za ove funkcije treba obraditi slucajeve ako se unese neispravan indeks
setFirstFree r mat=
  let vec = M.getCol r mat
      list = V.toList vec
      --mlastU = (V.findIndex (\e -> e /= U)) - 1 vec --indeksiranje u vektoru krece od 0
      --last = Mb.fromMaybe -1 mlastU

   in mat


--ovde obavezno obraditi gresku
--ovde StrErr a -- ili poruka o gresci ili vrednost


setFirstFree' c elem mat =
  let col = M.getCol c mat
      mfirst = V.findIndex (\e->e/=U) col
      val = Mb.fromMaybe (-1) mfirst
   in set (val) c elem mat






-- za datu poziciju i, j i datu matricu proveriti da li se nalaze 4 spojene, dijagonaln vertikalno i horizontalno

--dat vektor da li ima 4 elementa e
--BUG! ne valja ovo
fourInARow r e mat=
  let row = M.getRow r mat
      list = V.toList row
      grupe = L.group list
      first = L.find (\g -> (length g) >= 4) grupe
   in head ( Mb.fromJust first) == e

--treba da vidim kako izbrojati tacno da li ih ima 4 u redu
--ne moze ovako


-- funkcija koja mapira matricu
-- Red -> -1
-- Blue -> 1
-- U -> 0

-- treba namapirati celo sranje,
--trazi int -> Int -> Int ne znam zasto
slika :: Int -> Item -> Int
slika _ e = case e of
            Red -> -1
            Blue -> 2
            U -> 0

--data (i,j) vratiti da li se dijagonalno nalaze 4 elementa e


--mapTable mat = let n = ncols mat
someFunc :: IO ()
someFunc = putStrLn "someFunc"



