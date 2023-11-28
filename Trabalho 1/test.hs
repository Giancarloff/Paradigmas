import Data.List

type SudokuBoard = [[Int]]
-- Tipos para modelagem do tabuleiro
data Ordem = Lt | Gt | Na deriving (Eq) -- Ordem entre células, onde Na é not applicable

ordemLinhas :: [[Ordem]]
ordemLinhas = [
    [Lt, Na, Lt],
    [Gt, Na, Gt],
    [Gt, Na, Lt],
    [Gt, Na, Gt]
    ]

ordemColunas :: [[Ordem]]
ordemColunas = [
    [Lt, Na, Lt],
    [Gt, Na, Gt],
    [Gt, Na, Lt],
    [Gt, Na, Gt]
    ]

-- Function to extract a subregion from a Sudoku board as a single list
extractSubregion :: SudokuBoard -> Int -> Int -> [Int]
extractSubregion board row col =
    let boardSize = length board
        subregionSize = floor (sqrt (fromIntegral boardSize))
        subregionRow = row `div` subregionSize
        subregionCol = col `div` subregionSize
        subregionStartRow = subregionRow * subregionSize
        subregionStartCol = subregionCol * subregionSize
        subregion = [board !! i !! j | i <- [subregionStartRow..subregionStartRow + subregionSize - 1], j <- [subregionStartCol..subregionStartCol + subregionSize - 1]]
    in subregion

    -- Métodos auxiliars
isListOrdered :: [Int] -> [Ordem] -> Bool
isListOrdered [_] [] = True
isListOrdered [] [] = True
isListOrdered (x:y:intTail) (op:opTail)
    | x == 0 || y == 0 = isListOrdered (y:intTail) opTail
    | op == Gt && x > y = isListOrdered (y:intTail) opTail
    | op == Lt && x < y = isListOrdered (y:intTail) opTail
    | op == Na = isListOrdered (y:intTail) opTail
    | otherwise = False

main :: IO ()
main = do
    let sudokuBoard = [ [1, 2, 3, 4],
                        [0, 0, 0, 0],
                        [9, 10, 11, 12],
                        [13, 14, 15, 16] ]

        row = 2
        col = 2

        subregion = extractSubregion sudokuBoard row col
    putStrLn "Subregion:"
    print subregion

    print (isListOrdered (sudokuBoard !! 1) (ordemColunas !! 1))