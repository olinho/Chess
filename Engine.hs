module Engine where
import Text.Read
import Data.Char
import Control.Monad
import Data.List
import Data.Tree
import Data.Maybe
import System.IO.Unsafe
import Data.Time.Clock

-- ******************** data, types *****************

data Figure = Figure FType FColor deriving (Eq, Read, Show)

data FColor = White | Black | Empty deriving (Eq, Read, Show)
-- **********Pion**Wieża**Skoczek***Goniec***
data FType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Read, Show)

type Square = Maybe Figure 

type Board = [[Square]]

type Position = (Int, Int)

-- ***************** output functions **********************

showFigure :: Figure -> Char
showFigure (Figure Pawn White)     =   'P'
showFigure (Figure Rook White)     =   'R'
showFigure (Figure Knight White)   =   'N'
showFigure (Figure Bishop White)   =   'B'
showFigure (Figure Queen White)    =   'Q'
showFigure (Figure King White)     =   'K'
showFigure (Figure Pawn Black)     =   'p'
showFigure (Figure Rook Black)     =   'r'
showFigure (Figure Knight Black)   =   'n'
showFigure (Figure Bishop Black)   =   'b'
showFigure (Figure Queen Black)    =   'q'
showFigure (Figure King Black)     =   'k'

rdFigure :: Char -> Figure
rdFigure 'P' = (Figure Pawn White)
rdFigure 'R' = (Figure Rook White)
rdFigure 'N' = (Figure Knight White)
rdFigure 'B' = (Figure Bishop White)
rdFigure 'Q' = (Figure Queen White)
rdFigure 'K' = (Figure King White)
rdFigure 'p' = (Figure Pawn Black)
rdFigure 'r' = (Figure Rook Black)
rdFigure 'n' = (Figure Knight Black)
rdFigure 'b' = (Figure Bishop Black)
rdFigure 'q' = (Figure Queen Black)
rdFigure 'k' = (Figure King Black)

-- *******functions working on square*****
showSquare :: Square -> Char
showSquare Nothing = '-'
showSquare (Just fig) = showFigure (fig)

rdSquare :: Char -> Square
rdSquare '-' =	Nothing
rdSquare fig = Just (rdFigure fig)

-- functions working on board ****
rdBoard2 :: String -> Board
rdBoard2 = map readRow . lines
	where readRow = map rdSquare

convertLetterToUnicode :: Char -> IO ()
convertLetterToUnicode 'p' = putChar '\9817' 
convertLetterToUnicode 'r' = putChar '\9814'
convertLetterToUnicode 'n' = putChar '\9816'
convertLetterToUnicode 'b' = putChar '\9815'
convertLetterToUnicode 'q' = putChar '\9813'
convertLetterToUnicode 'k' = putChar '\9812'
convertLetterToUnicode 'P' = putChar '\9823'
convertLetterToUnicode 'R' = putChar '\9820'
convertLetterToUnicode 'N' = putChar '\9822'
convertLetterToUnicode 'B' = putChar '\9821'
convertLetterToUnicode 'Q' = putChar '\9819'
convertLetterToUnicode 'K' = putChar '\9818'
convertLetterToUnicode '-' = putChar '-'
convertLetterToUnicode '\n' = putChar '\n'

getBoardShowUnicode :: Board -> IO ()
getBoardShowUnicode brd = (mapM_ convertLetterToUnicode brdAsStr)
	where
		brdAsStr = '\n' : getBoardShowString brd

getBoardShowString :: Board -> String
getBoardShowString = unlines . map showRow
	where showRow = map showSquare

getBoardShowRow :: (Board, Int) -> String
getBoardShowRow (brd,n) = getBoardShowString (brd !! (n-1) : [])

getBoardShowColumn :: (Board, Int) -> String 
getBoardShowColumn (brd,n) = getBoardShowString [map (!! (n-1)) brd]

-- ****** return figure on specific position **
getBoardShowField :: (Board, Position) -> Char
getBoardShowField (brd, (col,row)) = getBoardShowRow(brd,row) !! (col-1)

-- **** return color of figure in given pos ***
showColorOfFigureInPos :: Board -> Position -> FColor
showColorOfFigureInPos brd pos = result
	where
		whitePos = colorPos brd White
		blackPos = colorPos brd Black
		result = 
			if pos `elem` whitePos then White
			else
				if pos `elem` blackPos then Black
				else Empty

-- **** return table of positions in which are figures of certain color
colorPos :: Board -> FColor -> [Position]
colorPos brd color = helpColorPos (brd, color, 0)

helpColorPos :: (Board,FColor,Int) -> [Position]
helpColorPos (brd, color, 64) = []
helpColorPos (brd, color ,n)
	| (fst c) == True = (snd c):(helpColorPos (brd,color,n+1))
	| (fst c) == False = []++(helpColorPos (brd,color,n+1))
	| otherwise  = []
	where
		b = tableBoolAndPos(brd, color)
		c = b !! n
tableBoolAndPos :: (Board, FColor) -> [(Bool, Position)]
tableBoolAndPos (brd, color)
	| color == Black = (zip [(isLower $ getBoardShowField (brd, (convertNumberToPosition x)) )|x<-[1..64]] (map (convertNumberToPosition )[1..]))
	| color == White = (zip [(isUpper $ getBoardShowField (brd, (convertNumberToPosition x)) )|x<-[1..64]] (map (convertNumberToPosition )[1..]))

-- 1-(1,1) .. 8-(8,1), 9-(1,2), ..64-(8,8)
convertNumberToPosition :: Int -> Position
convertNumberToPosition n = head $ zip [col] [row]
	where
		dzielnik = 8
		b = n `div` dzielnik + 1
		c = n `mod` (dzielnik)
		col = 
			if c == 0 then 8 else c
		row = 
			if c == 0 then b-1 else b


cosek = (map (`elem` ['a'..'z']) (getBoardShowString initialBoard))

-- ********* availableMoves for a particular figure *******
figureMoves :: Char -> Position -> [Position]
figureMoves 'P' (col,row) 
	| row == 1 = []
	| row == 7 = [(col, row-1),(col, row-2)]
	| otherwise = [(col, row-1)]
figureMoves 'R' (col,row) = vertMoves ++ horizMoves
	where
		horizMoves = zip [x | x<-[1..8], x /= col] (replicate 7 row)
		vertMoves =  zip (replicate 7 col) [y | y<-[1..8], y /= row]
figureMoves  'K' (col,row) = deleteMovesOverBorders result
	 where
	 	higher = zip [col-1,col,col+1] (replicate 3 (row+1))
	 	equals = zip [col-1,col+1] [row,row]
	 	lower = zip [col-1,col,col+1] (replicate 3 (row-1))
	 	result = higher ++ equals ++ lower
figureMoves 'B' (col,row) = deleteMovesOverBorders result
	where
		a = zip [(col+1)..8] [(row+1)..8]
		b = zip [(col-1),(col-2)..1] [(row+1)..8]
		c = zip [(col-1),(col-2)..1] [(row-1),(row-2)..1]
		d = zip [(col+1)..8] [(row-1),(row-2)..1]
		result = a++b++c++d
figureMoves 'Q' (col,row) = result
	where
		a = figureMoves 'B' (col,row)
		b = figureMoves 'R' (col,row)
		result = a ++ b 
figureMoves 'N' (col,row) = deleteMovesOverBorders result
	where
		result = ((col+2), (row+1)):((col+2), (row-1)):((col+1), (row+2)):((col+1), (row-2)) : ((col-2), (row+1)):((col-2), (row-1)):((col-1), (row+2)):((col-1), (row-2)):[]
figureMoves 'p' (col,row) 
	| row == 8 = []
	| row == 2 = [(col,row+1),(col,row+2)]
	| otherwise = [(col, row+1)]
figureMoves 'r' (col,row)  = vertMoves ++ horizMoves
	where
		horizMoves = zip [x |x<-[1..8], x /= col] (replicate 7 row )
		vertMoves =  zip (replicate 7 col) [y |y<-[1..8], y /= row]
figureMoves 'k' (col,row) = deleteMovesOverBorders result
	 where
	 	higher = zip [col-1,col,col+1] (replicate 3 (row+1))
	 	equals = zip [col-1,col+1] [row,row]
	 	lower = zip [col-1,col,col+1] (replicate 3 (row-1))
	 	result = higher ++ equals ++ lower 
figureMoves 'b' (col,row) = deleteMovesOverBorders result
	where
		a = zip [(col+1)..8] [(row+1)..8]
		b = zip [(col-1),(col-2)..1] [(row+1)..8]
		c = zip [(col-1),(col-2)..1] [(row-1),(row-2)..1]
		d = zip [(col+1)..8] [(row-1),(row-2)..1]
		result = a++b++c++d
figureMoves 'q' (col,row) = result
	where
		a = figureMoves 'b' (col,row)
		b = figureMoves 'r' (col,row)
		result = a ++ b 
figureMoves 'n' (col,row) = deleteMovesOverBorders result
	where
		result = ((col+2), (row+1)):((col+2), (row-1)):((col+1), (row+2)):((col+1), (row-2)) : ((col-2), (row+1)):((col-2), (row-1)):((col-1), (row+2)):((col-1), (row-2)):[]
--deleting moves that go beyond border
deleteMovesOverBorders :: [Position] -> [Position]
deleteMovesOverBorders tab = helpDeleteMoves tab 0

helpDeleteMoves :: [Position] -> Int -> [Position]
helpDeleteMoves tab n
	| length tab == n = []
	| (((first < 1) || (first > 8)) || ((second < 1) || (second > 8))) = [] ++ (helpDeleteMoves tab (n+1))
	| (((first > 0) && (first < 9)) || ((second > 0) && (second < 9))) = element : (helpDeleteMoves tab (n+1))
	where 
		element = tab !! n
		first = fst (element)
		second = snd (element)

-- czy zwracana lista złożona jest z samych zanków określonych w argumencie
ifOnlyGivenLetter :: String -> Char -> Bool
ifOnlyGivenLetter str character
	| length (filter (/=character) str) == 0 = True
	| otherwise = False

--does figure moved
rightWhiteRook = False
rightBlackRook = False
leftWhiteRook = False
leftBlackRook = False
whiteKing = False
blackKing = False
-- czy mała roszada jest możliwa
availableShortCastling :: Board -> FColor -> Bool
availableShortCastling brd White = result
	where
		correctSetOfFigures = "K--R"
		realSetOfFigures = [getBoardShowField (brd, (y,8))|y<-[5..8]]
		thisPosInNextIterations = [getBoardShowField (x,(y,1))|x<-(genMovesForColor brd Black),y<-[2..4]]
		fieldNotAttacked = (ifOnlyGivenLetter thisPosInNextIterations '-') || (ifOnlyGivenLetter thisPosInNextIterations 'K')
		didRookMove = not leftWhiteRook
		doesCorrectSetOfFigures = (correctSetOfFigures==realSetOfFigures)
		result = doesCorrectSetOfFigures && didRookMove && fieldNotAttacked && (not whiteKing)

availableShortCastling brd Black = result
	where
		correctSetOfFigures = "k--r"
		realSetOfFigures = [getBoardShowField (brd, (y,1))|y<-[5..8]] 
		thisPosInNextIterations = [getBoardShowField (x,(y,1))|x<-(genMovesForColor brd White),y<-[5..7]]
		fieldNotAttacked = ((length (filter (/='-') ((filter (/='k') thisPosInNextIterations)))) == 0)
		didRookMove = not rightBlackRook
		doesCorrectSetOfFigures = (correctSetOfFigures==realSetOfFigures)
		result = doesCorrectSetOfFigures && didRookMove && fieldNotAttacked && (not blackKing)

-- czy duża roszada jest możliwa
availableLongCastling :: Board -> FColor -> Bool
availableLongCastling brd White = result
	where
		correctSetOfFigures = "R---K"
		realSetOfFigures = [getBoardShowField (brd, (y,8))|y<-[1..5]]
		thisPosInNextIterations = [getBoardShowField (x,(y,1))|x<-(genMovesForColor brd Black),y<-[4..6]]
		fieldNotAttacked = (ifOnlyGivenLetter thisPosInNextIterations '-') || (ifOnlyGivenLetter thisPosInNextIterations 'K')
		didRookMove = not rightWhiteRook
		doesCorrectSetOfFigures = (correctSetOfFigures==realSetOfFigures)
		result = doesCorrectSetOfFigures && didRookMove && fieldNotAttacked && (not whiteKing)

availableLongCastling brd Black = result
	where
		correctSetOfFigures = "r---k"
		realSetOfFigures = [getBoardShowField (brd, (y,1))|y<-[1..5]]
		thisPosInNextIterations = [getBoardShowField (x,(y,1))|x<-(genMovesForColor brd White),y<-[3..5]]
		fieldNotAttacked = (ifOnlyGivenLetter thisPosInNextIterations '-') || (ifOnlyGivenLetter thisPosInNextIterations 'k')
		didRookMove = not leftBlackRook
		doesCorrectSetOfFigures = (correctSetOfFigures==realSetOfFigures)
		result = doesCorrectSetOfFigures && didRookMove && fieldNotAttacked && (not blackKing)

------------- OPERATIONS CONNECTED WITH TREES --------
-- aktualna plansza
-- kto ma ruch
type State = (Board, FColor)
-- wartość przechowywana przez węzeł
-- głebokość drzewa(ile poziomów pozostało do końca)
-- make tree of all possible board in n-depth
makeTree :: State -> Int -> Tree State
makeTree (brd,color) n
	| n == 0 = Node (brd,color) []
	| otherwise = Node (brd,color) [result | newBrd<-(genMovesForColor brd color),let result = (makeTree (newBrd, newColor) (n-1))]
	where
		newColor = case color of 
			White -> Black
			Black -> White

findBestState :: State -> State
findBestState state = result
	where
		brd = fst state
		color = snd state
		listOfNewBrd = genStatesForState state

		stateAndVal = [(st,val) | st<-listOfNewBrd, let val = minmax (makeTree st 2)]  -- drzewo o poziomie 1
		listOfValues = map snd stateAndVal
		
		maxVal = maximum listOfValues
		stateWithMaxVal = map fst (filter (\(x,y) -> y==maxVal) stateAndVal)
		minVal = minimum listOfValues
		stateWithMinVal = map fst (filter (\(x,y) -> y==minVal) stateAndVal)
		result = head stateWithMinVal

--underFindBestState :: State -> UTCTime -> State
--underFindBestState


--state'sy zbudowane z plansz powstałych z wejściowej i przeciwnego koloru
--dzieci danego węzła 
genStatesForState :: State -> [State]
genStatesForState (brd, color) 
	| color == White = zip listOfNewBrd (replicate 2000 Black)
	| color == Black = zip listOfNewBrd (replicate 2000 White)
	| otherwise = []
	where
		listOfNewBrd = genMovesForColor brd color
		


--maksymalizuje dla kolejnego ruchu białych
--minimalizuje dla kolejnego ruchu czarnych
minmax :: Tree State -> Int
minmax (Node a []) = getValueOfFitness a
minmax (Node (brd, White) b) = bestVal
	where
		list = map minmax b
		bestVal = maximum list
minmax (Node (brd, Black) b) = bestVal 
	where
		list = map minmax b
		bestVal = minimum list

showIOFromTree :: Tree State -> IO ()
showIOFromTree tree = result
	where
		listOfBoard = map fst (flatten tree)
		result = mapM_ putStrLn (map getBoardShowString listOfBoard)

showStateInIO :: State -> IO ()
showStateInIO (brd,col) = getBoardShowUnicode (brd)

getValueOfFitness :: State -> Int
getValueOfFitness a 
	| color == White = (getValueOfStates a)  - (getValueOfFiguresInColor (fst a)  Black)
	| color == Black = (getValueOfStates a)  - (getValueOfFiguresInColor (fst a)  White)
	| otherwise = 0
	where
		color = snd a

--- ***************** NIEUŻYTKI ****************** -------

getSmallerTree :: Tree State -> Int -> Tree State
getSmallerTree (Node a [b]) n
	| n == 0 = Node a []
	| otherwise = Node a [(getSmallerTree (c) (n-1))|c<-[b]]

-- przykładowe obiekty
tmpTree = makeTree (initialBoard4,White) 1
tmpState = (flatten tmpTree) !! 1
startState = (flatten (makeTree (initialBoard2, White) 1)) !! 0


-- state pocztkowy
-- ostatni poziom w drzewie
-- zwracany najlepiej przystosowany state
bestFitnessState :: State -> [State] -> State
bestFitnessState state stateTab = result
	where
		stateCol = snd state
		fitnessList = map getValueOfStates stateTab
		statesAndFitnessList = zip stateTab fitnessList
		maxFitness = maximum fitnessList
		result = head $ map fst (filter (\(st,fit) -> fit==maxFitness) statesAndFitnessList)

compareTwoStates :: State -> State -> Int
compareTwoStates fstState sndState = fstFitness - sndFitness
	where
		fstBrd = fst fstState
		fstCol = snd fstState
		fstFitness = getValueOfFiguresInColor fstBrd fstCol 

		sndBrd = fst sndState
		sndCol = snd sndState
		sndFitness = getValueOfFiguresInColor sndBrd sndCol

getParent :: Tree State -> State -> State
getParent tree (brd,color)
	| (notElem (brd,color) listAfterFlattenTree) = (brd,color)
	| otherwise = parent
	where
		listAfterFlattenTree = flatten tree
		indexOfVal = head $ map snd (filter (\(x,y) -> x==True) (reverse (zip (map (==brd) (map fst listAfterFlattenTree)) [0..])))

		listOfLevelsInTree = levels tree
		tabOfTabOfBoolAndNum = (zip (map (filter (==True)) (map (map (==brd)) (map (map fst) listOfLevelsInTree))) [0..])
		tabOfBool = map (any (==True)) (map fst tabOfTabOfBoolAndNum)
		levelWithBrd = snd . head $ reverse $ filter (\(x,y) -> x==True) (zip tabOfBool [0..])
		
		statesOnParentLevel = listOfLevelsInTree !! (levelWithBrd-1)
		parent = helpGetParent listAfterFlattenTree statesOnParentLevel (indexOfVal-1)

helpGetParent :: [State] -> [State] -> Int -> State
helpGetParent listAfterFlattenTree listOfPrevLevelStates n = result
	where
		prevElem = listAfterFlattenTree !! n
		result = 
			if (elem prevElem listOfPrevLevelStates) then prevElem
				else helpGetParent listAfterFlattenTree listOfPrevLevelStates (n-1)

getChildren :: Tree State -> State -> Int
getChildren tree (brd,color) = result
	where
		listAfterFlattenTree = flatten tree
		indexOfVal = head $ map snd (filter (\(x,y) -> x==True) (reverse (zip (map (==brd) (map fst listAfterFlattenTree)) [0..])))
		
		listOfLevelsInTree = levels tree

		result = indexOfVal

getPredecessor :: Tree State -> State -> State
getPredecessor tree state 
	| parent == grandparent = state
	| otherwise = getPredecessor tree parent
	where
		--odfiltruj elementy z listy które zawierają state 
		--a następnie znajdź poziom, na którym on jest
		stateLvl = fromJust $ findIndex (/=[]) (map (filter (==state)) (levels tree))
		parent = getParent tree state
		grandparent = getParent tree parent
		result = stateLvl


--pobranie przodka z drugiego poziomu (level 1)
getLastPredecessor :: Tree State -> State -> State
getLastPredecessor tree (brd,color)
	| levelWithBrd == 1 = (brd,color)
	| isParentInSecondLevel = parent
	| otherwise = getLastPredecessor tree parent
	where
		listOfLevelsInTree = levels tree
		tabOfTabOfBoolAndNum = (zip (map (filter (==True)) (map (map (==brd)) (map (map fst) listOfLevelsInTree))) [0..])
		tabOfBool = map (any (==True)) (map fst tabOfTabOfBoolAndNum)
		levelWithBrd = snd . head $ reverse $ filter (\(x,y) -> x==True) (zip tabOfBool [0..])
		parent = getParent tree (brd,color)

		statesOnSecondLevel = listOfLevelsInTree !! (1)
		isParentInSecondLevel = elem parent statesOnSecondLevel

getParentOfLevel:: Tree State -> State -> Int -> State
getParentOfLevel tree (brd,color) n 
	| (notElem (brd,color) listAfterFlattenTree) = (brd,color)
	| otherwise = parent
	where
		listAfterFlattenTree = flatten tree
		indexOfVal = head $ map snd (filter (\(x,y) -> x==True) (reverse (zip (map (==brd) (map fst listAfterFlattenTree)) [0..])))

		listOfLevelsInTree = levels tree
		levelWithBrd = n
		
		statesOnParentLevel = listOfLevelsInTree !! (n-1)
		parent = helpGetParent listAfterFlattenTree statesOnParentLevel (indexOfVal-1)

tryGetBranch :: Tree State -> State -> [State]
tryGetBranch tree (brd,color) 
	| levelWithState == 1 = [(brd,color)]
	| isParentInSecondLevel = [parent]
	| otherwise = [parent] ++ (tryGetBranch tree parent)
	where
		listOfLevelsInTree = levels tree
		tabOfTabOfBoolAndNum = (zip (map (filter (==True)) (map (map (==(brd,color))) listOfLevelsInTree)) [0..])
		tabOfBool = map (any (==True)) (map fst tabOfTabOfBoolAndNum)
		levelWithState = snd . head $ reverse $ filter (\(x,y) -> x==True) (zip tabOfBool [0..])
		parent = getParent tree (brd,color)

		statesOnSecondLevel = listOfLevelsInTree !! 1
		isParentInSecondLevel = elem parent statesOnSecondLevel

--gałąź rozpoczynająca się od liścia podanego w argumencie
getBranch :: Tree State -> State -> [State]
getBranch tree (brd,color) 
	| levelWithState == 1 = [(brd,color)]
	| isParentInSecondLevel = [parent]
	| otherwise = [parent] ++ (getBranch tree parent)
	where
		listOfLevelsInTree = levels tree
		tabOfTabOfBoolAndNum = (zip (map (filter (==True)) (map (map (==(brd,color))) listOfLevelsInTree)) [0..])
		tabOfBool = map (any (==True)) (map fst tabOfTabOfBoolAndNum)
		levelWithState = snd . head $ reverse $ filter (\(x,y) -> x==True) (zip tabOfBool [0..])
		parent = getParent tree (brd,color)

		statesOnSecondLevel = listOfLevelsInTree !! (1)
		isParentInSecondLevel = elem parent statesOnSecondLevel

getBranchToTabOfBoard :: Tree State -> State -> [Board]
getBranchToTabOfBoard tree (brd,color) = map fst (getBranch tree (brd,color))

branchAsTabOfBoard = getBranchToTabOfBoard tmpTree tmpState

---------- KONIEC NIEUŻYTKÓW -------------


getValueOfStates :: State -> Int
getValueOfStates (brd,color) = result
	where
		--lista pozycji, które zajmują figury o kolorze podanym w argumencie
		listOfPosOfFigInCol = colorPos brd color
		tab = [result | nextPos<-listOfPosOfFigInCol, let result = (valueOfFigureInPos brd nextPos)*(valueOfPosition nextPos)]
		result = sum tab
------------------- OPERATIONS ON BOARD ------------------
--value of figure of given color
getValueOfFiguresInColor :: Board -> FColor -> Int
getValueOfFiguresInColor brd color = result
	where
		--lista pozycji, które zajmują figury o kolorze podanym w argumencie
		listOfPosOfFigInCol = colorPos brd color
		tab = [result | nextPos<-listOfPosOfFigInCol, let result = (valueOfFigureInPos brd nextPos)*(valueOfPosition nextPos)]
		result = sum tab

--generate all moves for specific team to string
showGeneratingBoardInLines :: Board -> FColor -> IO ()
showGeneratingBoardInLines brd color = mapM_ putStrLn (genMovesForColorToString brd color)

--generate all moves for specific team to string
genMovesForColorToString :: Board -> FColor -> [String]
genMovesForColorToString brd color = map (getBoardShowString) result
	where
		result = genMovesForColor brd color
		--result = helpGenMovesForColor brd color 0

--generate all moves for specific team
genMovesForColor :: Board -> FColor -> [Board]
genMovesForColor brd color 
	| doesAvailableShortCastling && (not doesAvailableLongCastling) = boardAfterShortCastling : result
	| doesAvailableLongCastling && (not doesAvailableShortCastling) = boardAfterLongCastling : result
	| doesAvailableLongCastling && doesAvailableShortCastling = boardAfterShortCastling : boardAfterLongCastling : result 
	| otherwise = result
	where
		doesAvailableShortCastling = availableShortCastling brd color
		doesAvailableLongCastling = availableLongCastling brd color
		boardAfterShortCastling = 
			if doesAvailableShortCastling then
				if color == Black && (not blackKing)
					then rdBoard2 (moveFigure (brd, (5,1), (7,1)))
					else rdBoard2 (moveFigure (brd, (5,8), (7,8)))
				else [[]]
		boardAfterLongCastling = 
			if doesAvailableLongCastling then
				if color == Black
					then rdBoard2 (moveFigure (brd, (5,1), (3,1)))
					else rdBoard2 (moveFigure (brd, (5,8), (3,8)))
				else [[]]
		result = helpGenMovesForColor brd color 0

helpGenMovesForColor :: Board -> FColor -> Int -> [Board]
helpGenMovesForColor brd color n 
	| length listOfBusyPos == n+1 = allBoardForThisFigure
	| length listOfBusyPos == n = []
	| otherwise = allBoardForThisFigure ++ (helpGenMovesForColor brd color (n+1))
	where
		listOfBusyPos =
			if color == White then colorPos brd White
				else colorPos brd Black
		posWithFigure = listOfBusyPos !! n
		allBoardForThisFigure = genMovesForFigInPos brd posWithFigure

--czy jest możliwe posunięcie z fromPos to toPos na podanej planszy brd
isAvailableMove :: Board -> Position -> Position -> Bool
isAvailableMove brd fromPos toPos = elem toPos (tryFindAvailablePos brd fromPos)

-- generate all moves
genMoves :: Board -> [Board]
genMoves brd = result
	where
		result = (genMovesForColor brd White)++(genMovesForColor brd Black)

-- it takes a board and position and returns array of boards
-- which can be created after moving figure at given position
genMovesForFigInPos :: Board -> Position -> [Board]
genMovesForFigInPos brd pos = helpGenMovesForFigInPos brd pos 0
-- jeśli na podanej pozycji nie ma figury to zwracamy pustą tablicę
helpGenMovesForFigInPos :: Board -> Position -> Int -> [Board]
helpGenMovesForFigInPos brd oldPos n 
	| character == '-' = []
	| length availablePos == n = []
	| otherwise = result : (helpGenMovesForFigInPos brd oldPos (n+1))
	where
		character = getBoardShowField (brd, oldPos)
		availablePos = tryFindAvailablePos brd oldPos
		newPos = availablePos !! n
		boardAsStr = moveFigure (brd, oldPos, newPos)
		result = rdBoard2 boardAsStr


-- generowanie ruchów w wyniku czego otrzymujemy tablicę plansz
-- z których każda została utworzona po wykonaniu ruchu
genMovesForFigInPosToString :: Board -> Position -> [String]
genMovesForFigInPosToString brd pos = helpGenMovesForFigInPosToString brd pos 0
-- jeśli na podanej pozycji nie ma figury to zwracamy pustą tablicę
helpGenMovesForFigInPosToString :: Board -> Position -> Int -> [String]
helpGenMovesForFigInPosToString brd oldPos n 
	| character == '-' = []
	| length availablePos == n = []
	| otherwise = boardAsStr : (helpGenMovesForFigInPosToString brd oldPos (n+1))
	where
		character = getBoardShowField (brd, oldPos)
		availablePos = tryFindAvailablePos brd oldPos
		newPos = availablePos !! n
		boardAsStr = moveFigure (brd, oldPos, newPos)

-- plansza
-- pozycja figury
tryFindAvailablePos brd (col,row) 
	| (character == 'N') || (character == 'n') = filter (`notElem` myTeamFigurePositions) (figureMoves character (col,row))
	| (character == 'P') || (character == 'p') = additionalFieldForPawn ++ result2
	| character == 'K' = result
	| character == '-' = []
	| otherwise = result
	where
		character = getBoardShowField (brd, (col,row))
		figureColor = showColorOfFigureInPos brd (col,row)
		characterInPos = getBoardShowField (brd, (col,row))
		
		figurePosOnEmptyBoard = figureMoves characterInPos (col,row)
		
		listOfBlackFigPos = colorPos brd Black
		listOfWhiteFigPos = colorPos brd White
		listOfAllBusyPos = listOfWhiteFigPos++listOfBlackFigPos

		nearestBlockedPos = findNearestBlockedPos brd (col,row)
		nearestBlockedPosByEnemy = 
			if figureColor == White 
				then filter (`elem` nearestBlockedPos) listOfBlackFigPos
			else 
				if figureColor == Black
					then filter (`elem` nearestBlockedPos) listOfWhiteFigPos
				else nearestBlockedPos
		myTeamFigurePositions = 
			if figureColor == White then filter (`notElem` [(col,row)]) listOfWhiteFigPos
			else 
				if figureColor == Black then filter (`notElem` [(col,row)]) listOfBlackFigPos
				else []

		listOfBlockedPosAfterAddingVector = (correctListOfBlockedPos brd (col,row)) ++ myTeamFigurePositions
		listOfBlockedPosForPawn = listOfBlockedPosAfterAddingVector ++ nearestBlockedPosByEnemy
		result = helpForReallyAvailablePositions brd (col,row) listOfBlockedPosAfterAddingVector figurePosOnEmptyBoard 0
		
		--posAvlForKing = filter (\(c,d) -> c `elem` [1..8] && d `elem` [1..8] && (c,d)/=(col,row)) [(a,b) | a<-[col-1..col+1], b<-[row-1..row+1]]
		--newBrds = 
			--if figureColor == White then genMovesForColor brd Black else genMovesForColor brd White

		--[getBoardShowField (b,pos) | b <- newBrds, pos <- result)]

		attackedFieldByPawn = 
			if figureColor == Black 
		 		then  filter (\(c,d) -> c `elem` [1..8] && d `elem` [1..8]) [(col-1,row+1),(col+1,row+1)]
		 		else filter (\(c,d) -> c `elem` [1..8] && d `elem` [1..8]) [(col-1,row-1),(col+1,row-1)]
		additionalFieldForPawn = 
			if figureColor == White
				then concat [filter (==x) listOfBlackFigPos | x<-attackedFieldByPawn]
				else concat [filter (==x) listOfWhiteFigPos | x<-attackedFieldByPawn]
		result2 = helpForReallyAvailablePositions brd (col,row) listOfBlockedPosForPawn figurePosOnEmptyBoard 0
	
-- sprawdza czy dane pole jest atakowane
isFieldAttacked :: Board -> Position -> Bool
isFieldAttacked brd (col,row) = result
	where
		character = getBoardShowField (brd, (col,row))
		figureColor = showColorOfFigureInPos brd (col,row)

		enemyColor = 
			if figureColor == White then Black else White
		brdsAfterEnemyMoves = genMovesForColor brd enemyColor
		result = ((length $ filter (/=character) [getBoardShowField (x, (col,row)) | x<-(genMovesForColor brd enemyColor)]) > 0)

-- pozycje faktycznie dostępne dla danej figury
-- po odfiltrowaniu ruchów zablokowanych przez inne figury
-- NIE DZIAŁA POPRAWNIE
reallyAvailablePositions :: Board -> Position -> [Position]
reallyAvailablePositions brd (col,row) 
	| (character == 'N') || (character == 'n') = filter (`notElem` heldPositions) (figureMoves character (col,row))
	| otherwise = result
	where
		character = getBoardShowField (brd, (col,row))
		krotki = 
			if isUpper character then (tableBoolAndPos (brd, White) )
			else (tableBoolAndPos (brd, Black))
		heldPositions = map snd (filter fst krotki)
		blockedPositions = correctListOfBlockedPos brd (col,row)
		positionHeldByEnemy =
			-- jeśli moja figura jest biała
			if (isUpper character) then map snd (filter fst (tableBoolAndPos (brd, Black)))
			else map snd (filter fst (tableBoolAndPos (brd, White)))
		nearestBlockedPos = findNearestBlockedPos brd (col,row)
		sameElements = filter (`elem` nearestBlockedPos) positionHeldByEnemy
		changedPositions = helpCorrectListOfBlockedPos (col,row) sameElements sameElements 1
		positionBlockedByMyTeam = filter (`notElem` positionHeldByEnemy) sameElements 
		searchingBlockedPos = positionBlockedByMyTeam ++ changedPositions
		
		availablePos = figureMoves character (col,row)
		result = helpForReallyAvailablePositions brd (col,row) nearestBlockedPos availablePos 0


-- argumenty :
-- tablica
-- pozycja figury
-- tablica zajętych pozycji
-- tablica pozycji aktualnie dostępnych
-- kolejny indeks
helpForReallyAvailablePositions :: Board -> Position -> [Position] -> [Position] -> Int -> [Position]
helpForReallyAvailablePositions brd (col,row) tabBusy tabAvailable n 
	| length tabBusy == n+1 = newTabAvailable
	| length tabBusy == n = []
	| otherwise = helpForReallyAvailablePositions brd (col,row) tabBusy newTabAvailable (n+1)
	where
		blockedPos = tabBusy !! n
		newTabAvailable = helpBlockedPositions (col,row) blockedPos tabAvailable 

-- pierwszy argument to pozycja mojej figury
-- drugi argument to pozycja innej figury, która stoi mi na drodze
-- trzeci argument to aktualnie dostępne pola dla mojej figury (przed sprawdzeniem kolizji)		
-- filtruję dostępne pozycje i zwracam nową tablicę
helpBlockedPositions :: Position -> Position -> [Position] -> [Position]
helpBlockedPositions (col,row) (a,b) availablePos
	| (col == a) || (row == b) = result
	| (abs (col-a) == abs (row-b)) && (row<b) && (col<a)	= result1
	| (abs (col-a) == abs (row-b)) && (row<b) && (col>a)	= result2 
	| (abs (col-a) == abs (row-b)) && (row>b) && (col>a)	= result3
	| (abs (col-a) == abs (row-b)) && (row>b) && (col<a)	= result4
	| otherwise = availablePos
	where
		roznica = 
			if col == a then 8-b+1 else 8-a+1
		blockedPos = 
			if col == a then zip (replicate roznica col) [b..8]
			else zip [a..8] (replicate roznica row)
		blockedPos2 = 
			if col == a then zip (replicate b col) [1..b]
			else zip [1..a] (replicate a row) 
		result = 
			if col == a then
				-- przeszkoda na prawo od mojej figury
				if row < b then filter (`notElem` blockedPos) availablePos
				-- przeszkoda na lewo od mojej figury
				else filter (`notElem` blockedPos2) availablePos
			else
				-- przeszkoda powyżej mojej figury
				if col < a then filter (`notElem` blockedPos) availablePos
				-- przeszkoda poniżej mojej figury
				else filter (`notElem` blockedPos2) availablePos
		blockedSkos1 = zip [a..8] [b..8]
		result1 = filter (`notElem` blockedSkos1) availablePos
		blockedSkos2 = zip [a,(a-1)..1] [b..8]
		result2 = filter (`notElem` blockedSkos2) availablePos
		blockedSkos3 = zip [a,(a-1)..1] [b,(b-1)..1]
		result3 = filter (`notElem` blockedSkos3) availablePos
		blockedSkos4 = zip [a..8] [b,(b-1)..1]
		result4 = filter (`notElem` blockedSkos4) availablePos

-- find nearest to given position held positions in row and column
-- on the left, right, above and under given position
-- znajduję zajęte pozycje w kolumnie i w rzędzie podanym w argumencie
-- położone najbliżej podanej pozycji
findNearestBlockedPos :: Board -> Position -> [Position]
findNearestBlockedPos brd (col,row)
	| (character == 'P') || (character == 'p') = nearestAbove ++ nearestUnder
	| (character == 'R') || (character == 'r') = nearestOnTheLeft ++ nearestOnTheRight ++ nearestAbove ++ nearestUnder
	| (character == 'B') || (character == 'b') = nearestRightAbove++nearestLeftAbove++nearestLeftUnder++nearestRightUnder
	| (character == 'Q') || (character == 'q') || (character == 'K') || (character == 'k') = result
	| otherwise = []
	where
		character = getBoardShowField (brd, (col,row))
		krotki = (tableBoolAndPos (brd, White) ) ++ (tableBoolAndPos (brd, Black))
		heldPositions = map snd (filter fst krotki)
		onTheRigth = filter (`elem` (zip [(col+1)..8] (replicate (8-col+1) row))) heldPositions 
		heldColumnsOnThisRowOnTheRight = sort (map fst onTheRigth)
		nearestOnTheRight =
			if col == 8	then []
			else 
				if heldColumnsOnThisRowOnTheRight == [] then []
					else [((head heldColumnsOnThisRowOnTheRight), row)]
		onTheLeft = filter (`elem` (zip [1..(col-1)] (replicate (col-1) row))) heldPositions
		heldColumnsOnThisRowOnTheLeft = sort (map fst onTheLeft)
		nearestOnTheLeft =
			if col == 1 then []
			else 
				if heldColumnsOnThisRowOnTheLeft == [] then []
				else [((last heldColumnsOnThisRowOnTheLeft), row)]
		above = filter (`elem` (zip (replicate (8-row) col) [(row+1)..8])) heldPositions
		heldRowsOnThisColumnAbovePos = sort (map snd above)
		nearestAbove = 
			if row == 8 then []
			else
				if heldRowsOnThisColumnAbovePos == [] then []
				else [(col, (head heldRowsOnThisColumnAbovePos))]
		under = filter (`elem` (zip (replicate (row-1) col) [1..(row-1)])) heldPositions
		heldRowsOnThisColumnUnderPos = sort (map snd under)
		nearestUnder = 
			if row == 1 then []
			else
				if heldRowsOnThisColumnUnderPos == [] then []
				else [(col, (last heldRowsOnThisColumnUnderPos))]

		skosPrawoGora1 = zip [(col+1)..8] [(row+1)..8]
		skosPrawoGora2 = filter (`elem` skosPrawoGora1) heldPositions
		skosPrawoGora3 = sort (map fst skosPrawoGora2)
		skosPrawoGora4 = head skosPrawoGora3
		skosPrawoGora5 = head $ sort (map snd skosPrawoGora2)
		nearestRightAbove = 
			if (col == 8) || (row == 8) then []
			else 
				if (length skosPrawoGora3 == 0) then []
				else [(skosPrawoGora4, skosPrawoGora5)]

		skosLewoGora1 = zip [(col-1),(col-2)..1] [(row+1)..8]
		skosLewoGora2 = filter (`elem` skosLewoGora1) heldPositions
		skosLewoGora3 = sort (map fst skosLewoGora2)
		skosLewoGora4 = last skosLewoGora3
		skosLewoGora5 = head $ sort (map snd skosLewoGora2)
		nearestLeftAbove = 
			if (col == 1) || (row == 8) then []
			else 
				if (length skosLewoGora3 == 0) then []
				else [(skosLewoGora4, skosLewoGora5)]

		skosLewoDol1 = zip [(col-1),(col-2)..1] [(row-1),(row-2)..1]
		skosLewoDol2 = filter (`elem` skosLewoDol1) heldPositions
		skosLewoDol3 = sort (map fst skosLewoDol2)
		skosLewoDol4 = last skosLewoDol3
		skosLewoDol5 = last $ sort (map snd skosLewoDol2)
		nearestLeftUnder = 
			if (col == 1) || (row == 1) then []
			else 
				if (length skosLewoDol3 == 0) then []
			else [(skosLewoDol4, skosLewoDol5)]

		skosPrawoDol1 = zip [(col+1)..8] [(row-1),(row-2)..1]
		skosPrawoDol2 = filter (`elem` skosPrawoDol1) heldPositions
		skosPrawoDol3 = sort (map fst skosPrawoDol2)
		skosPrawoDol4 = head skosPrawoDol3
		skosPrawoDol5 = last $ sort (map snd skosPrawoDol2)
		nearestRightUnder = 
			if (col == 8) || (row == 1) then []
			else
				if (length skosPrawoDol3 == 0) then []
				else [(skosPrawoDol4, skosPrawoDol5)]

		result = nearestOnTheLeft ++ nearestOnTheRight ++ nearestAbove ++ nearestUnder++nearestRightAbove++nearestLeftAbove++nearestLeftUnder++nearestRightUnder

-- otrzymuję pozycję figury i listę aktualnie zablokowanych pozycji
-- ponieważ moja figura może zająć pozycje, na których
-- stoją figury przeciwnika, to zmieniam pozycje przez nie zajęte
-- na kolejne pozycje, znajdujące się zaraz za pozycją danej figury
-- zwracana lista nowych pozycji związanych z pozycjami przeciwnika
correctListOfBlockedPos :: Board -> Position -> [Position]
correctListOfBlockedPos brd pos = result
	where
		character = getBoardShowField (brd, pos)
		positionHeldByEnemy =
			-- jeśli moja figura jest biała
			if (isUpper character) then map snd (filter fst (tableBoolAndPos (brd, Black)))
			else map snd (filter fst (tableBoolAndPos (brd, White)))
		result = helpCorrectListOfBlockedPos pos positionHeldByEnemy positionHeldByEnemy 1

-- pozycja figury
-- pozycje zablokowane, tablica niemodyfikowana
-- tablica, którą modyfikuję
-- indeks
helpCorrectListOfBlockedPos :: Position -> [Position] -> [Position] -> Int -> [Position]
helpCorrectListOfBlockedPos pos startingList currentList n
	| length startingList == n = nub newList
	| length startingList < n = nub currentList
	| otherwise = helpCorrectListOfBlockedPos pos startingList newList (n+1)
	where
		otherFigurePos = getNElement n startingList 
		newList = addVectorToPos pos otherFigurePos currentList

-- pozycja mojej figury
-- pozycja innej figury
-- aktualna lista pozycji, w której znajdują się pozycje innych figur 
-- zmieniam pozycję na kolejną leżącą na prostej przechodzącej 
-- przez pozycje podane w argumentach
-- np. (1,1) -> (5,1) -> (6,1)
-- np. (6,5) -> (4,3) -> (3,2)
addVectorToPos :: Position -> Position -> [Position] -> [Position]
addVectorToPos (col,row) (a,b) currentList
	| cond1 && cond5 = changeElementInListOfPos (a,b) above currentList
	| cond1 && cond6 = changeElementInListOfPos (a,b) under currentList
	| cond2 && cond4 = changeElementInListOfPos (a,b) right currentList
	| cond3 && cond4 = changeElementInListOfPos (a,b) left currentList
	| cond2 && cond5 && cond7 = changeElementInListOfPos (a,b) rightAbove currentList     
	| cond3 && cond5 && cond7 = changeElementInListOfPos (a,b) leftAbove currentList  
	| cond3 && cond6 && cond7 = changeElementInListOfPos (a,b) leftUnder currentList  
	| cond2 && cond6 && cond7 = changeElementInListOfPos (a,b) rightUnder currentList  
	| otherwise = currentList
	where
		cond1 = (col == a)
		cond2 = (col < a)
		cond3 = (col > a)
		cond4 = (row == b)
		cond5 = (row < b)
		cond6 = (row > b)
		cond7 = (abs (row-b) == abs (col-a))
		inOtherWay = (0,0)
		above = 
			if b == 8 then inOtherWay
			else (a,b+1)
		under =
			if b == 1 then inOtherWay
			else (a,b-1)
		right = 
			if a == 8 then inOtherWay
			else (a+1,b)
		left =
			if a == 1 then inOtherWay
			else (a-1,b)
		rightAbove = 
			if (a == 8) || (b == 8) then inOtherWay
			else (a+1,b+1)
		leftAbove =
			if (a == 1) || (b == 8) then inOtherWay
			else (a-1,b+1)
		leftUnder =
			if (a == 1) || (b == 1) then inOtherWay
			else (a-1,b-1)
		rightUnder =
			if (a == 8) || (b == 1) then inOtherWay
			else (a+1,b-1) 

changeElementInListOfPos :: Position -> Position -> [Position] -> [Position]
changeElementInListOfPos t1 t2 currentList
	| t2 == (0,0) = result
	| otherwise = newList
	where
		countOfT1Elements = length $ filter (==(t1)) currentList
		newList =
			if (countOfT1Elements > 1) 
				then ((filter (`notElem` [t1]) currentList) ++ [t2] ++ [t1])
				else ((filter (`notElem` [t1]) currentList) ++ [t2])
		result = 
			if (length $ filter (==t1) currentList) > 1 then currentList
			else filter (`notElem` [t1]) currentList

-- **function written by me, not working properly **
rdBoard :: String -> Board
rdBoard	[] = []
rdBoard brdAsString 
	| (head brdAsString == '\n') =  rdBoard (tail brdAsString)
	| otherwise = [rdSquare (head brdAsString)] : rdBoard (tail brdAsString)

-- ************ FUNCTION TO MODIFY BOARD ****************

setPosition :: (Board, Char, Position) -> String
setPosition (brd, character, (col,row)) = getBoardShowString ([(brd !! x)|x<-[0..(row-2)]]++[[(brd !! (row-1))!!x|x<-[0..(col-2)]]++[rdSquare character]++[(brd !! (row-1))!!x|x<-[col..7]]]++[(brd !! x)|x<-[row..7]])
setPosition2 :: (Board, Char, Position) -> String
setPosition2 (brd, character, (col,row)) =
	if (row == 1)
	then
		if col == 1
		then
			getBoardShowString ([[rdSquare character]++[(brd !! 0)!!x|x<-[1..7]]] ++ [brd !! x|x<-[1..7]])
		else	
			 getBoardShowString ([[(brd !! 0)!!x|x<-[0..(col-2)]]++[rdSquare character]++[(brd !! 0)!!x|x<-[col..7]]] ++ [(brd !! x)|x<-[1..7]])
	else	
		if col == 1
		then
			getBoardShowString ([(brd !! x)|x<-[0..(row-2)]]++[[rdSquare character]++[(brd !! (row-1))!!x|x<-[1..7]]]++[(brd !! x)|x<-[row..7]])
		else	
			getBoardShowString ([(brd !! x)|x<-[0..(row-2)]]++[[(brd !! (row-1))!!x|x<-[0..(col-2)]]++[rdSquare character]++[(brd !! (row-1))!!x|x<-[col..7]]]++[(brd !! x)|x<-[row..7]])
resetPosition :: (Board, Position) -> String
resetPosition (brd, (col,row)) = setPosition(brd, '-', (col,row))

moveFigure :: (Board, Position, Position) -> String
moveFigure (brd, (fromCol,fromRow), (toCol,toRow)) 
	| cond1 = moveFigure ((rdBoard2 brd3), (8,1), (6,1))
	| cond2 = moveFigure ((rdBoard2 brd3), (8,8), (6,8))
	| cond3 = moveFigure ((rdBoard2 brd3), (1,1), (4,1))
	| cond4 = moveFigure ((rdBoard2 brd3), (1,8), (4,8))
	| otherwise = brd3
	where
		brd2 = rdBoard2 (setPosition (brd, character, (toCol,toRow)))
		brd3 = resetPosition (brd2, (fromCol,fromRow))
		cond1 = (character == 'k' && fromCol == 5 && fromRow == 1 && toCol == 7 && toRow == 1 )
		cond2 = (character == 'K' && fromCol == 5 && fromRow == 8 && toCol == 7 && toRow == 8 )
		cond3 = (character == 'k' && fromCol == 5 && fromRow == 1 && toCol == 3 && toRow == 1 )
		cond4 = (character == 'K' && fromCol == 5 && fromRow == 8 && toCol == 3 && toRow == 8 )
		character = getBoardShowField (brd, (fromCol, fromRow))


-- ********* figure as an argument ******
valueOfFigure :: Figure -> Int
valueOfFigure (Figure Pawn _)	=	1
valueOfFigure (Figure Rook _)	=	8
valueOfFigure (Figure Knight _)	=	3
valueOfFigure (Figure Bishop _)	=	3
valueOfFigure (Figure Queen _)	=	20
valueOfFigure (Figure King _)	=	1

-- *** figure's character as an argument ***
valueOfFigure2 :: Char -> Int
valueOfFigure2 character = valueOfFigure (rdFigure (character))

-- figure position as argument
valueOfFigureInPos :: Board -> Position -> Int
valueOfFigureInPos brd position = result
	where
		character = getBoardShowField (brd, position)
		result = valueOfFigure2 character

-- value of pos 
valueOfPosition :: Position -> Int
valueOfPosition (col,row)
	| elem (col,row) area1 = 4 
	| elem (col,row) area2 = 3
	| elem (col,row) area3 = 2	
	| elem (col,row) area4 = 1	
	| otherwise = 0
		where
			area1 = [(x,y) | x<-[4,5], y<-[4,5]]
			area2 = zip (replicate 4 3) [3..6] ++ zip (replicate 4 6) [3..6] ++ zip [4,5] (replicate 2 3) ++ zip [4,5] (replicate 2 6)
			area3 = zip (replicate 6 2) [2..7] ++ zip (replicate 6 7) [2..7] ++ zip [3..6] (replicate 4 2) ++ zip [3..6] (replicate 4 7)
			area4 = zip (replicate 8 1) [1..8] ++ zip (replicate 8 8) [1..8] ++ zip [2..7] (replicate 6 1) ++ zip [2..7] (replicate 6 8)
			 


-- *********** additional functions ********** 
--pobierz n-ty element z listy
getNElement :: Int -> [a] -> a
getNElement n list = last $ take n list

-- usuń n-ty element z listy
deleteNElementFromList :: Int -> [a] -> [a]
deleteNElementFromList n list = frontList ++ endList
	where
		frontList = take (n-1) list
		endList = drop n list

-- ******** figures and initial board ********************
-- ****** figures ********
--W - white 
--B - black
pawnW   = 	Figure Pawn White
rookW   = 	Figure Rook White
knightW = 	Figure Knight White
bishopW = 	Figure Bishop White
queenW  =	Figure Queen White
kingW   = 	Figure King White
pawnB   = 	Figure Pawn Black
rookB   = 	Figure Rook Black
knightB = 	Figure Knight Black
bishopB = 	Figure Bishop Black
queenB  =	Figure Queen Black
kingB   = 	Figure King Black

-- **** initial squares ****
-- big letter = white
-- small letter = black
sqr_P = Just pawnW
sqr_R = Just rookW
sqr_N = Just knightW
sqr_B = Just bishopW
sqr_Q = Just queenW
sqr_K = Just kingW
sqr_p = Just pawnB
sqr_r = Just rookB
sqr_n = Just knightB
sqr_b = Just bishopB
sqr_q = Just queenB
sqr_k = Just kingB

-- ****** initial board *******
initialBoardStr = init (unlines ["rnbqkbnr",[ 'p'|_<-[1..8]],['.'|_<-[1..8]],['.'|_<-[1..8]],['.'|_<-[1..8]],['.'|_<-[1..8]],"PPPPPPPP","RNBQKBNR"])
initialBoard = [[Just (Figure Rook Black),Just (Figure Knight Black),Just (Figure Bishop Black),Just (Figure Queen Black),Just (Figure King Black),Just (Figure Bishop Black),Just (Figure Knight Black),Just (Figure Rook Black)],[Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black)],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White)],[Just (Figure Rook White),Just (Figure Knight White),Just (Figure Bishop White),Just (Figure Queen White),Just (Figure King White),Just (Figure Bishop White),Just (Figure Knight White),Just (Figure Rook White)]]
initialBoard2 = [[Just (Figure Rook Black),Just (Figure Knight Black),Just (Figure Bishop Black),Nothing,Just (Figure King Black),Nothing,Nothing,Just (Figure Rook Black)],[Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black),Just (Figure Pawn Black)],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Just (Figure Queen Black),Nothing,Nothing,Just (Figure Pawn White),Nothing,Nothing,Nothing],[Nothing,Nothing,Just (Figure Pawn White),Just (Figure Pawn White),Nothing,Nothing,Just (Figure Knight Black),Nothing],[Just (Figure Pawn White),Just (Figure Pawn White),Nothing,Nothing,Nothing,Just (Figure Pawn White),Just (Figure Pawn White),Just (Figure Pawn White)],[Just (Figure Rook White),Just (Figure Knight White),Just (Figure Bishop White),Just (Figure Queen White),Just (Figure King White),Just (Figure Bishop White),Just (Figure Knight White),Just (Figure Rook White)]]
initialBoard3 = [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Just (Figure Queen Black),Nothing,Nothing,Nothing,Nothing,Just (Figure Pawn White),Just (Figure King White)],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just (Figure Knight Black),Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]
initialBoard4 = [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Just (Figure Pawn Black),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just (Figure King White)],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just (Figure Knight Black),Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Just (Figure Knight White),Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

boardView = do
	contents <- readFile "plansza"
	let wlist = lines contents
	let board = wlist
	putStr (head board)
	putStr contents

board = [[sqr_r,sqr_n,sqr_b,sqr_q,sqr_k,sqr_b,sqr_n,sqr_r],[sqr_p|_<-[1..8]],[Nothing|_<-[1..8]],[Nothing|_<-[1..8]],[Nothing|_<-[1..8]],[Nothing|_<-[1..8]],[sqr_P|_<-[1..8]],[sqr_R,sqr_N,sqr_B,sqr_Q,sqr_K,sqr_B,sqr_N,sqr_R]]