module Game2 where
import Text.Read
import Data.Char
import Control.Monad
import Data.List
import Data.Tree
import Data.Maybe
import System.IO
import Data.Maybe
import System.Environment
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Engine

--data fileLetter = a|b|c|d|e|f|g|h
--data rankNumber = 1|2|3|4|5|6|7|8
--data promotedTo = q|r|b|n
board = initialBoard
convertFromACN :: Char -> Char -> Char -> Char -> (Position, Position)
convertFromACN c1 c2 c3 c4 = ((col1,row1), (col2,row2))
	where
		col1 = ((+(-96)) . ord) c1
		row1 = ((+(-48)) . ord) c2
		col2 = ((+(-96)) . ord) c3
		row2 = ((+(-48)) . ord) c4

main :: IO ()
main = do
	underMain initialBoard White
underMain :: Board -> FColor -> IO ()
underMain brd color = do
	putStrLn "Aktualna plansza"
	getBoardShowUnicode brd
	putStr "Wykonaj ruch "
	print color
	c1 <- getChar
	c2 <- getChar
	c3 <- getChar
	c4 <- getChar
	let (fromPos, toPos) = convertFromACN c1 c2 c3 c4
	let avlStates = genStatesForState (brd, color)
	let avlBrds = map fst avlStates
	let newBrd = rdBoard2 $ (moveFigure (brd, fromPos, toPos))
	let board = newBrd
	let newColor = if color == White then Black else White
	--if (elem newBrd avlBrds) then getBoardShowUnicode newBrd else do underMain brd color
	let stAfterCmpMv = findBestState (newBrd,newColor)
	underMain (fst stAfterCmpMv) color
	--underMain newBrd newColor
