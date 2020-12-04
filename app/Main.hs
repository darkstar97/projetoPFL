import Data.List     (elemIndices, intercalate, transpose)
import System.IO     (BufferMode(..), hSetBuffering, stdin)
import System.Random (randomRIO)

-- cada lista interna é uma linha
-- um 0 é um elemento vazio
type Board = [[Int]]

data Direction = North | East | South | West

slideLeft :: Board -> Board
slideLeft = map slideRow
  where slideRow [ ] = [ ]
        slideRow [x] = [x]
        slideRow (x:y:zs)
          | x == 0 = slideRow (y : zs) ++ [0]
          | y == 0 = slideRow (x : zs) ++ [0] -- pras coisas combinarem quando tiver zeros entre elas
          | x == y = (x + y) : slideRow zs ++ [0]
          | otherwise = x : slideRow (y : zs)

slide :: Direction -> Board -> Board
slide North = transpose . slideLeft . transpose
slide East  = map reverse . slideLeft . map reverse
slide South = transpose . map reverse . slideLeft . map reverse . transpose
slide West  = slideLeft

-- checa se o player ganhou
completed :: Board -> Bool
completed b = any (elem 2048) b

-- checa se o jogo acabou por falta de movimento
stalled :: Board -> Bool
stalled b = all stalled' b && all stalled' (transpose b)
  where stalled' row = notElem 0 row && noNeighbors row
        noNeighbors [ ] = True
        noNeighbors [_] = True
        noNeighbors (x:y:zs)
          | x == y    = False
          | otherwise = noNeighbors (y:zs)

-- espaços vazios
emptyTiles :: Board -> [(Int, Int)]
emptyTiles = concatMap (uncurry search) . zip [0..3]
  where search n = zip (replicate 4 n) . elemIndices 0

-- substitui um elemento
updateTile :: (Int, Int) -> Int -> Board -> Board
updateTile (rowI, columnI) value = updateIndex (updateIndex (const value) columnI) rowI
  where updateIndex fn i list = take i list ++ fn (head $ drop i list) : tail (drop i list)

-- coloca um elemento num espaço vazio
-- maioria das vezes é um 2, um pouco das vezes é um 4
addTile :: Board -> IO Board
addTile b = do
  let tiles = emptyTiles b
  newPoint <- randomRIO (0, length tiles - 1) >>= return . (tiles !!)
  newValue <- randomRIO (1, 10 :: Int) >>= return . \x -> if x == 1 then 4 else 2
  return $ updateTile newPoint newValue b

-- game loop principal
gameloop :: Board -> IO ()
gameloop b = do
    putStrLn "---------------------"
    putStrLn $ boardToString b
    putStrLn "---------------------"
    if stalled b
        then putStrLn "Game over."
        else if completed b
            then putStrLn "You won!"
            else do
              input <- getChar
              putStrLn "" -- alinhamento
              let b1 = maybe b (`slide` b) $ lookup input $ zip "wasd" [North, West, South, East]
              if b1 == b -- título
                  then gameloop b1
                  else addTile b1 >>= gameloop

-- imprime o tabuleiro
boardToString :: Board -> String
boardToString = init . unlines . map (vertical . map (pad . showSpecial))
  where vertical = ('|' :) . (++ "|") . intercalate "|"
        showSpecial 0 = ""
        showSpecial n = show n
        pad s = replicate (4 - length padTemp) ' ' ++ padTemp
          where padTemp = s ++ if length s < 3 then " " else ""

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    let board = replicate 4 (replicate 4 0)
    b1 <- addTile board >>= addTile -- coloca dois elementos no começo
    gameloop b1