import Data.List (transpose)
-- Vergleichssudoku

-- Tipos para modelagem do tabuleiro
data Ordem = Lt | Gt | Na deriving (Eq) -- Ordem entre células, onde Na é not applicable
type Sudoku = [[Int]] -- Células do sudoku
type OrdemLinhas = [[Ordem]] -- Matriz de ordenação de células numa linha
type OrdemColunas = [[Ordem]] -- Matriz de ordenação das colunas
-- Nota: Obtém-se a ordem das colunas transpondo o sudoku.
type Vergleichssudoku = (Sudoku, OrdemLinhas, OrdemColunas, Int, Int) -- Tabuleiro completo
-- Nota:                       Altura da subregião ----------^    ^---------- Largura da subregião

-- Acesso aos dados
getSudoku :: Vergleichssudoku -> Sudoku
getSudoku (s, _, _, _, _) = s

getOrdemLinhas :: Vergleichssudoku -> OrdemLinhas
getOrdemLinhas (_, ol, _, _, _) = ol

getOrdemColunas :: Vergleichssudoku -> OrdemColunas
getOrdemColunas (_, _, oc, _, _) = oc

getAlturaSub :: Vergleichssudoku -> Int
getAlturaSub (_, _, _, h, _) = h

getLarguraSub :: Vergleichssudoku -> Int
getLarguraSub (_, _, _, _, w) = w

-- Elementos do tabuleiro

tamanhoSudoku :: Int -- Um sudoku NxN tem tamanho N
tamanhoSudoku = 4

sudoku :: Sudoku
sudoku = [
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]
    ]

ordemLinhas :: OrdemLinhas
ordemLinhas = [
    [Lt, Na, Lt],
    [Gt, Na, Gt],
    [Gt, Na, Lt],
    [Gt, Na, Gt]
    ]

ordemColunas :: OrdemColunas
ordemColunas = [
    [Gt, Na, Gt],
    [Lt, Na, Lt],
    [Lt, Na, Lt],
    [Gt, Na, Gt]
    ]

-- Montando o Vergleichssudoku
tabuleiroVS :: Sudoku -> OrdemLinhas -> OrdemColunas -> Int -> Int -> Vergleichssudoku
tabuleiroVS s ol oc h w = (s, ol, oc, h, w)

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

hasNoRepeats :: [Int] -> Bool
hasNoRepeats [] = True
hasNoRepeats (a:p) = notElem a p || hasNoRepeats p

-- Extrai a subregião de um elemento
extractSubregion :: Sudoku -> Int -> Int -> [Int]
extractSubregion board row col =
    let boardSize = length board
        subregionSize = floor (sqrt (fromIntegral boardSize))
        subregionRow = row `div` subregionSize
        subregionCol = col `div` subregionSize
        subregionStartRow = subregionRow * subregionSize
        subregionStartCol = subregionCol * subregionSize
        subregion = [board !! i !! j | i <- [subregionStartRow..subregionStartRow + subregionSize - 1], j <- [subregionStartCol..subregionStartCol + subregionSize - 1]]
    in subregion

-- Verificar se uma tentativa específica é válida
tentativaValida :: Vergleichssudoku -> Int -> Int -> Int -> Bool
tentativaValida vs row col num =
    notElem num (getSudoku vs !! row)
    && notElem num (transpose (getSudoku vs) !! col)
    && notElem num (extractSubregion (getSudoku vs) row col)
    && isListOrdered (getSudoku (atualizarTabuleiro vs row col num) !! row) (ordemLinhas !! row)
    && isListOrdered (transpose (getSudoku (atualizarTabuleiro vs row col num)) !! col) (ordemColunas !! col)

-- Resolução por backtracking
resolver :: Vergleichssudoku -> Vergleichssudoku
resolver vs = tentarNumeros vs 0 0 
    where
        tentarNumeros :: Vergleichssudoku -> Int -> Int -> Vergleichssudoku
        -- Verificações preliminares e ajustes antes de própriamente tentar os númeors
        tentarNumeros vs row col
            | col >= tamanhoSudoku = tentarNumeros vs (row + 1) 0
            | row >= tamanhoSudoku = vs
            | getSudoku vs !! row !! col /= 0 = tentarNumeros vs row (col + 1)
            | otherwise = tentarN vs row col 1
            where
                tentarN :: Vergleichssudoku -> Int -> Int -> Int -> Vergleichssudoku
                -- Verifica se a tentativa é válida e então propaga ela na execução
                tentarN vs row col num
                    | num > tamanhoSudoku = vs
                    | tentativaValida vs row col num = tentarNumeros (atualizarTabuleiro vs row col num) row (col + 1)
                    | otherwise = tentarN vs row col (num + 1)

-- Coloca um novo número no tabuleiro do Sudoku
atualizarTabuleiro :: Vergleichssudoku -> Int -> Int -> Int -> Vergleichssudoku
atualizarTabuleiro (s, ol, oc, h, w) row col num =
    let newRow = take col (s !! row) ++ [num] ++ drop (col + 1) (s !! row)
        newSudoku = take row s ++ [newRow] ++ drop (row + 1) s
    in (newSudoku, ol, oc, h, w)

-- Facilitador do print
printSudoku :: Vergleichssudoku -> IO ()
printSudoku vs = mapM_ (putStrLn . unwords . map show) (getSudoku vs)

-- Execução principal
main :: IO ()
main = do
    let vs = tabuleiroVS sudoku ordemLinhas ordemColunas 2 2
    print (tentativaValida vs 0 0 1)
    let resolvido = (resolver vs)
    printSudoku resolvido
