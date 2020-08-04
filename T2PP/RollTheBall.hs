{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
			 TypeSynonymInstances, FlexibleInstances,
			 InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A


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
data Cell = Cell {tip :: Char}
	deriving (Eq, Ord)

{-
	Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {levelCells :: (A.Array Position Cell)} --TODO
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


cellAt :: (A.Array Position Cell) -> (Int, Int) -> Cell
cellAt arr pos = arr A.! pos

instance Show Level where 
	show (Level lvl) = foldl (\x y -> if ((snd (fst y)) == 0)
		then x ++ [endl] ++ [(tip (snd y))]
		else x ++ [(tip (snd y))]) "" (A.assocs lvl) ++ [endl]

{-
	*** TODO ***
	Primește coordonatele colțului din dreapta jos a hărții.
	Intoarce un obiect de tip Level în care tabla este populată
	cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (row, column) = (Level (A.array ((0, 0), (row, column)) [((i, j), (Cell emptySpace)) | i <- [0..row], j <- [0..column]]))

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

maxRow :: (A.Array Position Cell) -> Int
maxRow = fst . snd . A.bounds

maxCol :: (A.Array Position Cell) -> Int
maxCol = snd . snd . A.bounds

addCell :: (Char, Position) -> Level -> Level
addCell (cell, pos@(x, y)) lvl = if (x < 0 || x > (maxRow (levelCells lvl)) ||
	y < 0 || y > (maxCol (levelCells lvl))) then lvl
	else (Level ((levelCells lvl) A.// [(pos, (Cell cell))]))


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
createLevel pos [] = emptyLevel pos
createLevel pos list = foldr (\x y -> (Level ((levelCells y) A.// [((snd  x), (Cell (fst x)))]))) (emptyLevel pos) list


{-
	*** TODO ***

	Mișcarea unei celule în una din cele 4 direcții 
	Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
	Celulele de tip start și win sunt imutabile.

	Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.

-}


swapCell :: Position -> Position -> Cell -> Level -> Level
swapCell initialPos finalPos cell lvl = (Level ((levelCells lvl) A.// [(finalPos, cell), (initialPos, (Cell emptySpace))]))

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(x,y) direction lvl
	| (elem (tip cell) startCells || elem (tip cell) winningCells) = lvl

	| (direction == West && (y - 1) >= 0) = if (cellAt (levelCells lvl) (x, y - 1) == (Cell emptySpace))
		then (swapCell pos (x, y - 1) cell lvl) else lvl

	| (direction == East && ((y + 1) <= (maxCol (levelCells lvl)))) =
		if ((cellAt (levelCells lvl) (x, y + 1)) == (Cell emptySpace))
		then (swapCell pos (x, y + 1) cell lvl) else lvl

	| (direction == North && (x - 1) >= 0) = if ((cellAt (levelCells lvl) (x - 1, y)) == (Cell emptySpace))
		then (swapCell pos (x - 1, y) cell lvl) else lvl

	| (direction == South && ((x + 1) <= (maxRow (levelCells lvl)))) =
		if ((cellAt (levelCells lvl) (x + 1, y)) == (Cell emptySpace))
		then (swapCell pos (x + 1, y) cell lvl) else lvl

	| otherwise = lvl
		where cell = (cellAt (levelCells lvl) pos)
{-
	*** HELPER ***

	Verifică dacă două celule se pot conecta.
	Atenție: Direcția indică ce vecin este a
	doua celulă pentru prima.

	ex: connection botLeft horPipe East = True (╚═)
		connection horPipe botLeft East = False (═╚)
-}

verticalUpPipes :: [Char]
verticalDownPipes = [startDown, verPipe, topLeft, topRight, winDown]

verticalDownPipes :: [Char]
verticalUpPipes = [startUp, verPipe, botLeft, botRight, winUp]

horizontalLeftPipes :: [Char]
horizontalLeftPipes = [startLeft, horPipe, topRight, botRight, winLeft]

horizontalRightPipes :: [Char]
horizontalRightPipes = [startRight, horPipe, topLeft, botLeft, winRight]

connection :: Cell -> Cell -> Directions -> Bool
connection firstCell nextCell direction 
	| elem (tip firstCell) verticalDownPipes && elem (tip nextCell) verticalUpPipes && direction == South = True
	| elem (tip firstCell) verticalUpPipes && elem (tip nextCell) verticalDownPipes && direction == North = True
	| elem (tip firstCell) horizontalLeftPipes && elem (tip nextCell) horizontalRightPipes && direction == West = True
	| elem (tip firstCell) horizontalRightPipes && elem (tip nextCell) horizontalLeftPipes && direction == East = True
	| otherwise = False

{-
	*** TODO ***

	Va returna True dacă jocul este câștigat, False dacă nu.
	Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
	de tip inițial la cea de tip final.
	Este folosită în cadrul Interactive.
-}

findStart :: (A.Array Position Cell) -> Position -> Position
findStart arr pos@(x, y)
	| x > (maxRow arr) = (-1, -1)
	| elem (tip (cellAt arr pos)) startCells = pos
	| (y + 1) > (maxCol arr) = findStart arr (x + 1, 0)
	| otherwise = findStart arr (x, y + 1)

followPath :: Level -> Position -> Position -> Bool
followPath lvl pos@(x, y) (xp, yp)
	| elem (tip cell) winningCells = True
	| (x + 1) <= (maxRow arr) && connection cell (cellAt arr (x + 1, y)) South && (x + 1) /= xp = followPath lvl (x + 1, y) pos
	| (x - 1) >= 0 && connection cell (cellAt arr (x - 1, y)) North && (x - 1) /= xp = followPath lvl (x - 1, y) pos
	| (y + 1) <= (maxCol arr) && connection cell (cellAt arr (x, y + 1)) East && (y + 1) /= yp = followPath lvl (x, y + 1) pos
	| (y - 1) >= 0 && connection cell (cellAt arr (x, y - 1)) West && (y - 1) /= yp = followPath lvl (x, y - 1) pos
	| otherwise = False
	where 
		arr = (levelCells lvl)
		cell = (cellAt arr pos)

wonLevel :: Level -> Bool
wonLevel lvl = followPath lvl pos pos
	where pos = findStart (levelCells lvl) (0, 0)

instance ProblemState Level (Position, Directions) where
	successors lvl = foldl (\x ((a, b), cell) -> if ((tip cell) == emptySpace) then x ++ (findMove arr (a, b - 1) East) ++
			(findMove arr (a, b + 1) West) ++ (findMove arr (a - 1, b) South) ++ (findMove arr (a + 1, b) North) else x) [] (A.assocs (levelCells lvl))
		where 
			arr = (levelCells lvl)
			findMove arr1 posF@(a, b) dir = if (a > (maxRow arr1) || a < 0 || b > (maxCol arr1) || b < 0 || elem (tip (cellAt arr1 posF))
				winningCells || elem (tip (cellAt arr1 posF)) startCells || cellAt arr1 posF == (Cell emptySpace)) then []
			else [((posF, dir), (moveCell posF dir (Level arr1)))]
			
	isGoal lvl = wonLevel lvl 

	reverseAction (action@((x, y), dir), lvl)
		| dir == North = (((x - 1, y), South), moveCell (x - 1, y) South lvl)
		| dir == South = (((x + 1, y), North), moveCell (x + 1, y) North lvl)
		| dir == East = (((x, y + 1), West), moveCell (x, y + 1) West lvl)
		| dir == West = (((x, y - 1), East), moveCell (x, y - 1) East lvl)
		| otherwise = (action, lvl) 

