{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = MyCell String
    deriving (Eq, Ord)

instance Show Cell where
    --show cell = undefined
    show (MyCell str) = str

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}
{-A.assocs  => [(indice, valoare)]
  A.elems   => [valoare]
  A.indices => [indice] -}
type MyBlock = (Position, Position)

data Level = MyLevel MyBlock Position (A.Array Position Cell) [Position]
    deriving (Eq, Ord)

{-
    *** Opțional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiati explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou.
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n".
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n".
-}


instance Show Level where
    show lvl = "\n" ++ (foldl myPrint "" (A.assocs a)) ++ endMessage
        where
            MyLevel (pos1, pos2) pos a _ = lvl
            (_, (_, c)) = A.bounds a
            endMessage
                    | pos1 == pos && pos2 == pos = "Congrats! You won!\n"
                    | continueGame lvl           = ""
                    | otherwise                  = "Game Over"
            myPrint acc (pos@(_, el_c), el) = acc ++ str ++ dummy-- pos == (el_r, el_c)
                where
                    str
                        | pos == pos1 || pos == pos2 = [block]
                        | otherwise                  = show el
                    dummy
                        | c == el_c = "\n"
                        | otherwise = ""
                    --  endMessage
                    -- | pos1 == pos && pos2 == pos = "Congrats! You won!\n"
                    -- | continueGame lvl           = ""
                    -- | otherwise                  = "Game Over"
                --  | pos1 == pos || pos2 == pos = acc ++ if c == el_c then [block] ++ "\n" else [block]
                --  | el_c == c  = acc ++ show el ++ "\n"
                --  | otherwise  = acc ++ show el

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel (m, n) p = MyLevel (p, p) (-1, -1) (A.array ((k, l), (m, n)) [((x, y), MyCell [emptySpace]) | x <- [k .. m], y <- [l .. n]]) []
    where
        (k, l) = (0, 0)

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile ch pos (MyLevel b won ar s) = case ch of
                                    'H' -> MyLevel b won (ar A.// [(pos, MyCell [hardTile])]) s
                                    'S' -> MyLevel b won (ar A.// [(pos, MyCell [softTile])]) s
                                    'W' -> MyLevel b pos (ar A.// [(pos, MyCell [winningTile])]) s
--  | ch == 'H' = MyLevel b (ar A.// [(pos, MyCell [hardTile])])
--  | ch == 'S' = MyLevel b (ar A.// [(pos, MyCell [softTile])])
--  | otherwise = MyLevel b (ar A.// [(pos, MyCell [winningTile])])

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos positions (MyLevel b won ar s) = MyLevel b won (ar A.// [(pos, MyCell [switch])]) s
--  | pos == head positions = MyLevel b won (ar A.// [(pos, MyCell [emptySpace])]) s
--  | otherwise addSwitch pos (tail positions) (MyLevel b won ar s)

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}


activate :: Cell -> Level -> Level
activate (MyCell str) (MyLevel b@(pos1, pos2) won ar s)
    | ar A.!(fst b) == MyCell [switch] || ar A.!(snd b) == MyCell [switch] = MyLevel b won ar1 s
    | otherwise                                                            = MyLevel b won ar s
        where
            ar1
                | ar A.!(head s) == MyCell [emptySpace] = (ar A.//[(pos1, MyCell [hardTile]) | pos1 <- s])
                | otherwise = (ar A.//[(pos1, MyCell [emptySpace]) | pos1 <- s])


{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move dir (MyLevel ((k, l), (m, n)) won ar s)
        | dir == North && k == m && l /= n = MyLevel ((k - 1, l), (m - 1, n))  won ar s
        | dir == North && k /= m && l == n = if k < m then (MyLevel ((k-1, l), (m-2, n)) won ar s) else MyLevel ((k-2, l), (m-1, n)) won ar s
        | dir == North && k == m && l == n = MyLevel ((k-2, l), (m-1, n)) won ar s
        | dir == South && k == m && l /= n = MyLevel ((k+1, l), (m+1, n)) won ar s
        | dir == South && k /= m && l == n = if k < m then (MyLevel ((k+2, n), (k+1, n)) won ar s) else MyLevel ((k+1, l), (m+2, n)) won ar s
        | dir == South && k == m && l == n = MyLevel ((k+2, l), (m+1, n)) won ar s
        | dir == East  && k == m && l /= n = if l < n then (MyLevel ((k, n + 2), (m, n + 1)) won ar s) else MyLevel ((k, l + 1), (m, n + 2)) won ar s
        | dir == East && k /= m && l == n = MyLevel ((k, n + 1), (m, n + 1)) won ar s
        | dir == East && k == m && l == n = MyLevel ((k, n+2), (m, n+1)) won ar s
        | dir == West && k == m && l /= n = if l < n then (MyLevel ((k, l - 1), (m, n - 2)) won ar s) else (MyLevel ((k, l - 2), (m, n - 1)) won ar s)
        | dir == West && k /= m && l == n = MyLevel ((k, n-1), (m, n-1)) won ar s
        | dir == West && k == m && l == n = MyLevel ((k, l-2), (m, n-1)) won ar s
        | otherwise = MyLevel ((k, l), (m, n)) won ar s

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (MyLevel b won ar s)
            | fst b /= won && snd b /= won                                                 = True
            | ar A.!(fst b) == MyCell [emptySpace] && ar A.!(snd b) == MyCell [emptySpace] = False
            | ar A.!(fst b) /= MyCell [softTile] && ar A.!(snd b) /= MyCell [softTile]     = False

            | otherwise
                                                                                           = False


{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined
