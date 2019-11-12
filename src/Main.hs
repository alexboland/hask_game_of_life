module Main where

import Control.Monad
import Control.Monad.Reader

type Cell = ((Int, Int), Bool)

type Board = [Cell]

initBoard :: Board
initBoard = [((x, y), False) | x <- [0..20], y <- [0..20]]

nbrs :: Cell -> Reader Board [Cell]
nbrs ((x', y'), alive') = do
		board <- ask
		return [((x, y), alive) | ((x, y), alive) <- board, (abs (x' - x)) < 2, (abs (y' - y)) < 2, (x, y) /= (x', y')]

liveNbrs :: Cell -> Reader Board Int
liveNbrs cell = do
	myNbrs <- nbrs cell
	return (length [cell | ((x, y), alive) <- myNbrs, alive == True] )

iterateCell :: Cell -> Reader Board Cell
iterateCell ((x, y), alive) =
	do
		lNbrs <- liveNbrs ((x, y), alive)
		if alive && lNbrs > 2 && lNbrs < 4
			then return ((x, y), True)
		else	
			if alive == False && lNbrs == 3
				then return ((x, y), True)
			else return ((x, y), False)

iterateBoard :: Board -> Board
iterateBoard board = [(runReader (iterateCell ((x, y), alive)) board) | ((x, y), alive) <- board]

showBoard :: Board -> String
showBoard board = foldl (++) "" (map (\((x, y), alive) -> "(" ++ (show x) ++ "," ++ (show y) ++ "): " ++ (show alive) ++ " | " ) board)

gameLoop :: Board -> Int -> IO ()
gameLoop board iterations
	| iterations > 0 = (print (showBoard board)) >>= ( \y -> (putStrLn "") >>= (\x -> (gameLoop (iterateBoard board) (iterations - 1) ) ))
	| otherwise = putStrLn "all done!"


main :: IO ()
main = gameLoop initBoard 10