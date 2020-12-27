{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import qualified Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = EmptyCell | StartUp | StartDown | StartLeft | StartRight | WinUp | WinDown | WinLeft | WinRight |
            HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight | EmptySpace
	deriving (Eq, Ord)


instance Show Cell where 
   show cell
        | cell == EmptyCell = [emptyCell]   --string-uri
        | cell == StartUp = [startUp]
        | cell == StartDown = [startDown]
        | cell == StartLeft = [startLeft]
        | cell == StartRight = [startRight]
        | cell == WinUp = [winUp]
        | cell == WinDown = [winDown]
        | cell == WinLeft = [winLeft]
        | cell == WinRight = [winRight]
        | cell == HorPipe = [horPipe]
        | cell == VerPipe = [verPipe]
        | cell == TopLeft = [topLeft]
        | cell == BotLeft = [botLeft]
        | cell == BotRight = [botRight]
        | cell == EmptySpace = [emptySpace]
        | otherwise = [topRight]

{-
    Tip de date pentru reprezentarea nivelului curent
-}


data Level = EmptyLevel | Lvl { arrCells :: (A.Array Position Cell) } --vector de celule din nivel cu pozitiile aferente
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level 
    where show EmptyLevel = ""
          show (Lvl cells) = [endl] ++ unlines [concat [show (elemAt cells x y) | y <- [0..highY]] | x <- [0..highX]]
            where 
                (highX, highY) = snd (A.bounds cells)
                elemAt arr x y = arr A.! (x, y)
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (r, c) = Lvl newLevel
    where 
        newLevel = A.array ((0, 0), (r, c)) [((i, j), EmptySpace) | i <- [0..r], j <- [0..c]] 
 
{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (_, _) EmptyLevel = EmptyLevel
addCell (cellType, pos) (Lvl cells)
    | fst pos > highX || snd pos > highY || fst pos < 0 || snd pos < 0 = Lvl cells
    | cellType == verPipe && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, VerPipe)])
    | cellType == horPipe && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, HorPipe)])
    | cellType == topLeft && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, TopLeft)])
    | cellType == botLeft && elemAt cells pos == EmptySpace &&fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, BotLeft)])
    | cellType == topRight && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, TopRight)])
    | cellType == botRight && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, BotRight)])
    | cellType == startUp && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, StartUp)])
    | cellType == startDown && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, StartDown)])
    | cellType == startLeft && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, StartLeft)])
    | cellType == startRight && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, StartRight)])
    | cellType == winUp && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, WinUp)])
    | cellType == winDown && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, WinDown)])
    | cellType == winLeft && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, WinLeft)])
    | cellType == winRight && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, WinRight)])
    | cellType == emptyCell && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, EmptyCell)])
    | cellType == emptySpace && elemAt cells pos == EmptySpace && fst pos <= highX && snd pos <= highY && fst pos >= 0 && snd pos >= 0 = Lvl (cells A.// [(pos, EmptySpace)])
    | otherwise = Lvl cells
        where 
            elemAt arr p = arr A.! p
            (highX, highY) = snd (A.bounds cells)

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel high list = foldr addCell newLevel list
    where newLevel = emptyLevel high

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell _ _ EmptyLevel = EmptyLevel
moveCell pos dir (Lvl cells)
    | fst pos < 0 || snd pos < 0 || fst pos > highX || fst pos > highY = Lvl cells
    | elemAt cells pos == StartUp || elemAt cells pos == StartDown || elemAt cells pos == StartLeft || elemAt cells pos == StartRight = Lvl cells
    | elemAt cells pos == WinLeft || elemAt cells pos == WinRight || elemAt cells pos == WinUp || elemAt cells pos == WinDown = Lvl cells
    | dir == North &&  fst pos - 1 >= 0 && fst pos - 1 <= highX && elemAt cells (fst pos - 1, snd pos) == EmptySpace = Lvl (cells A.// [((fst pos - 1, snd pos), elemAt cells pos), (pos, EmptySpace)])
    | dir == South &&  fst pos + 1 >= 0 && fst pos + 1 <= highX && elemAt cells (fst pos + 1, snd pos) == EmptySpace = Lvl (cells A.// [((fst pos + 1, snd pos), elemAt cells pos), (pos, EmptySpace)])
    | dir == West &&  snd pos - 1 >= 0 && snd pos - 1 <= highY && elemAt cells (fst pos, snd pos  - 1) == EmptySpace = Lvl (cells A.// [((fst pos, snd pos - 1), elemAt cells pos), (pos, EmptySpace)])
    | dir == East &&  snd pos + 1 >= 0 && snd pos + 1 <= highY && elemAt cells (fst pos, snd pos + 1) == EmptySpace = Lvl (cells A.// [((fst pos, snd pos + 1), elemAt cells pos), (pos, EmptySpace)])
    | otherwise = Lvl cells
        where
            elemAt arr p = arr A.! p
            (highX, highY) = snd (A.bounds cells)



{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir
    | cell1 == HorPipe && cell2 == HorPipe && dir == West = True
    | cell1 == HorPipe && cell2 == HorPipe && dir == East = True
    | cell1 == HorPipe && cell2 == BotLeft && dir == West = True
    | cell1 == HorPipe && cell2 == BotRight && dir == East = True
    | cell1 == HorPipe && cell2 == TopLeft && dir == West = True
    | cell1 == HorPipe && cell2 == TopRight && dir == East = True
    | cell1 == HorPipe && cell2 == StartLeft && dir == East = True
    | cell1 == HorPipe && cell2 == StartRight && dir == West = True
    | cell1 == HorPipe && cell2 == WinLeft && dir == East = True
    | cell1 == HorPipe && cell2 == WinRight && dir == West = True
    | cell1 == VerPipe && cell2 == VerPipe && dir == South = True
    | cell1 == VerPipe && cell2 == VerPipe && dir == North = True
    | cell1 == VerPipe && cell2 == TopLeft && dir == North = True
    | cell1 == VerPipe && cell2 == BotLeft && dir == South = True
    | cell1 == VerPipe && cell2 == BotRight && dir == South = True
    | cell1 == VerPipe && cell2 == TopRight && dir == North = True
    | cell1 == VerPipe && cell2 == StartUp && dir == South = True
    | cell1 == VerPipe && cell2 == StartDown && dir == North = True
    | cell1 == VerPipe && cell2 == WinUp && dir == South = True
    | cell1 == VerPipe && cell2 == WinDown && dir == North = True
    | cell1 == TopLeft && cell2 == HorPipe && dir == East = True
    | cell1 == TopLeft && cell2 == VerPipe && dir == South = True
    | cell1 == TopLeft && cell2 == BotLeft && dir == South = True
    | cell1 == TopLeft && cell2 == BotRight && dir == South = True
    | cell1 == TopLeft && cell2 == BotRight && dir == East = True
    | cell1 == TopLeft && cell2 == TopRight && dir == East = True
    | cell1 == TopLeft && cell2 == StartUp && dir == South = True
    | cell1 == TopLeft && cell2 == StartLeft && dir == East = True
    | cell1 == TopLeft && cell2 == WinUp && dir == South = True
    | cell1 == TopLeft && cell2 == WinLeft && dir == East = True
    | cell1 == BotLeft && cell2 == HorPipe && dir == East = True
    | cell1 == BotLeft && cell2 == VerPipe && dir == North = True
    | cell1 == BotLeft && cell2 == TopLeft && dir == North = True
    | cell1 == BotLeft && cell2 == TopRight && dir == North = True
    | cell1 == BotLeft && cell2 == TopRight && dir == East = True
    | cell1 == BotLeft && cell2 == StartDown && dir == North = True
    | cell1 == BotLeft && cell2 == WinDown && dir == North = True
    | cell1 == BotLeft && cell2 == BotRight && dir == East = True
    | cell1 == BotLeft && cell2 == StartLeft && dir == East = True
    | cell1 == BotLeft && cell2 == WinLeft && dir == East = True
    | cell1 == BotRight && cell2 == HorPipe && dir == West = True
    | cell1 == BotRight && cell2 == VerPipe && dir == North = True
    | cell1 == BotRight && cell2 == TopLeft && dir == North = True
    | cell1 == BotRight && cell2 == BotLeft && dir == West = True
    | cell1 == BotRight && cell2 == TopRight && dir == North = True
    | cell1 == BotRight && cell2 == StartDown && dir == North = True
    | cell1 == BotRight && cell2 == StartRight && dir == West = True
    | cell1 == BotRight && cell2 == WinRight && dir == West = True
    | cell1 == BotRight && cell2 == WinDown && dir == North = True
    | cell1 == TopRight && cell2 == HorPipe && dir == West = True
    | cell1 == TopRight && cell2 == VerPipe && dir == South = True
    | cell1 == TopRight && cell2 == BotLeft && dir == West = True
    | cell1 == TopRight && cell2 == TopLeft && dir == West = True
    | cell1 == TopRight && cell2 == BotRight && dir == South = True
    | cell1 == TopRight && cell2 == StartUp && dir == South = True
    | cell1 == TopRight && cell2 == StartRight && dir == West = True
    | cell1 == TopRight && cell2 == WinUp && dir == South = True
    | cell1 == TopRight && cell2 == WinRight && dir == West = True
    | cell1 == StartUp && cell2 == VerPipe && dir == North = True
    | cell1 == StartUp && cell2 == TopLeft && dir == North = True
    | cell1 == StartUp && cell2 == TopRight && dir == North = True
    | cell1 == StartUp && cell2 == WinDown && dir == North = True
    | cell1 == StartDown && cell2 == VerPipe && dir == South = True
    | cell1 == StartDown && cell2 == BotLeft && dir == South = True
    | cell1 == StartDown && cell2 == BotRight && dir == South = True
    | cell1 == StartDown && cell2 == WinUp && dir == South = True
    | cell1 == StartLeft && cell2 == HorPipe && dir == West = True
    | cell1 == StartLeft && cell2 == TopLeft && dir == West = True
    | cell1 == StartLeft && cell2 == BotLeft && dir == West = True
    | cell1 == StartLeft && cell2 == WinRight && dir == West = True
    | cell1 == StartRight && cell2 == HorPipe && dir == East = True
    | cell1 == StartRight && cell2 == TopRight && dir == East = True
    | cell1 == StartRight && cell2 == BotRight && dir == East = True
    | cell1 == StartRight && cell2 == WinLeft && dir == East = True
    | cell1 == WinUp && cell2 == VerPipe && dir == North = True
    | cell1 == WinUp && cell2 == StartDown && dir == North = True
    | cell1 == WinUp && cell2 == TopLeft && dir == North = True
    | cell1 == WinUp && cell2 == TopRight && dir == North = True
    | cell1 == WinDown && cell2 ==  VerPipe && dir == South = True
    | cell1 == WinDown && cell2 == BotLeft && dir == South = True
    | cell1 == WinDown && cell2 == BotRight && dir == South = True
    | cell1 == WinDown && cell2 == StartUp && dir == South = True
    | cell1 == WinLeft && cell2 == HorPipe && dir == West = True
    | cell1 == WinLeft && cell2 == TopLeft && dir == West = True
    | cell1 == WinLeft && cell2 == BotLeft && dir == West = True
    | cell1 == WinLeft && cell2 == StartRight && dir == West = True
    | cell1 == WinRight && cell2 == HorPipe && dir == East = True
    | cell1 == WinRight && cell2 == TopRight && dir == East = True
    | cell1 == WinRight && cell2 == BotRight && dir == East = True
    | cell1 == WinRight && cell2 == StartRight && dir == East = True
    | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
findStart :: [(Position, Cell)] -> Position
findStart cellList = if snd (head cellList) == StartDown || snd (head cellList) == StartUp ||
    snd (head cellList) == StartLeft || snd (head cellList) == StartRight then fst (head cellList)
    else findStart (tail cellList)

findNext :: Position -> Position -> Level -> Bool
findNext p1 p2 EmptyLevel = False
findNext crtPos prevPos (Lvl cells)
    | elemAt crtPos cells == WinRight || elemAt crtPos cells == WinLeft || elemAt crtPos cells == WinUp || elemAt crtPos cells == WinDown = True
    | fst crtPos - 1 >= 0 && fst crtPos - fst prevPos /= 1 && connection (elemAt crtPos cells) (elemAt (fst crtPos - 1, snd crtPos) cells) North == True = findNext (fst crtPos - 1, snd crtPos) crtPos (Lvl cells)
    | fst crtPos + 1 <= highX && fst crtPos - fst prevPos /= -1 && connection (elemAt crtPos cells) (elemAt (fst crtPos + 1, snd crtPos) cells) South == True = findNext (fst crtPos + 1, snd crtPos) crtPos (Lvl cells)
    | snd crtPos - 1 >= 0 && snd crtPos - snd prevPos /= 1 && connection (elemAt crtPos cells) (elemAt (fst crtPos, snd crtPos - 1) cells) West == True = findNext (fst crtPos, snd crtPos - 1) crtPos (Lvl cells)
    | snd crtPos + 1 <= highY && snd crtPos - snd prevPos /= -1 && connection (elemAt crtPos cells) (elemAt (fst crtPos, snd crtPos + 1) cells) East == True = findNext (fst crtPos, snd crtPos + 1) crtPos (Lvl cells)
    | otherwise = False
    where 
        elemAt pos arr = arr A.! pos
        (highX, highY) = snd (A.bounds cells)


wonLevel :: Level -> Bool
wonLevel EmptyLevel = False
wonLevel (Lvl cells) = findNext crtPos crtPos (Lvl cells)
    where crtPos = findStart cellList
          cellList = A.assocs cells


instance ProblemState Level (Position, Directions) where    -- a = (Position, Directions)
   
    isGoal EmptyLevel = False
    isGoal (Lvl cells) = if wonLevel (Lvl cells) == True then True else False

    successors (Lvl cells) = [((pos, dir), moveCell pos dir (Lvl cells)) | dir <- [North, South, East, West], pos <- indexes, moveCell pos dir (Lvl cells) /= (Lvl cells)]

        where 
            indexes = A.indices cells

    reverseAction = undefined
