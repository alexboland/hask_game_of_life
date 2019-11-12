module Main where

import Control.Monad
import Control.Monad.State

type Cell = ((Int, Int), Bool)

type Board = [Cell]

initBoard :: Board
initBoard = [((x, y), False) | x <- [0..20], y <- [0..20]]

nbrs :: Cell -> Board -> [Cell]
nbrs ((x', y'), alive') board = [((x, y), alive) | ((x, y), alive) <- board, (abs (x' - x)) < 2, (abs (y' - y)) < 2, (x, y) /= (x', y')]

liveNbrs :: Cell -> Board -> Int
liveNbrs cell board = (length [cell | ((x, y), alive) <- (nbrs cell board), alive == True] )

iterateCell :: Cell -> Board -> Cell
iterateCell ((x, y), alive) board
	| alive && (liveNbrs cell board) > 2 && (liveNbrs cell board) < 4 = ((x, y), True)
	| (alive == False) && (liveNbrs cell board) == 3 = ((x, y), True)
	| otherwise = ((x, y), False)
	where
		cell = ((x, y), alive)

iterateBoard :: Board -> Board
iterateBoard board = [iterateCell ((x, y), alive) board | ((x, y), alive) <- board]

showBoard :: Board -> String
showBoard board = foldl (++) "" (map (\((x, y), alive) -> "(" ++ (show x) ++ "," ++ (show y) ++ "): " ++ (show alive) ++ " | " ) board)

gameLoop :: Board -> Int -> IO ()
gameLoop board iterations
	| iterations > 0 = (print (showBoard board)) >>= ( \y -> (putStrLn "") >>= (\x -> (gameLoop (iterateBoard board) (iterations - 1) ) ))
	| otherwise = putStrLn "all done!"


main :: IO ()
main = gameLoop initBoard 10

--main = putStrLn (fst (runState someState ["a"]))

-- main = inputLoop "nobody"
