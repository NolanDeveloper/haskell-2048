import Text.Printf
import System.Random
import System.IO

type Row = [Int]
type Grid = [Row]
data Direction = DR | DD | DL | DU deriving Enum

gridToList :: Grid -> [Int]
gridToList = concat

gridFromList :: [Int] -> Grid
gridFromList  [a, b, c, d,
               e, f, g, h,
               i, j, k, l,
               m, n, o, p] = 
    [[a, b, c, d],
     [e, f, g, h],
     [i, j, k, l],
     [m, n, o, p]]

showRow :: Row -> String
showRow r = "|" ++ (concat $ fmap showCell r) ++ "\n"
    where
    showCell 0 = "    |"
    showCell n = printf "%4d|" (2 ^ n :: Int)

showGrid :: Grid -> String
showGrid g = delim ++ (concat $ fmap ((++ delim) . showRow) g)
    where delim = "+----+----+----+----+\n"

shiftRight :: Row -> Row 
shiftRight r = padding ++ nonZero
    where
    nonZero = filter (/= 0) r
    nNonZero = length nonZero
    padding = take (4 - nNonZero) $ repeat 0

{-

Probably better option would be shrink to left

consume :: Int -> Row -> Maybe Row
consume 0 _ = Nothing
consume n (0:xs) = (0 :) <$> consume n xs
consume n r@(x:xs) = if n == x then Just (0 : xs)
                     else Nothing

shrinkLeft :: Row -> Row
shrinkLeft [] = []
shrinkLeft (x:xs) = case consume x xs of
                    Just xs' -> (x + 1) : shrinkLeft xs'
                    Nothing -> x : shrinkLeft xs
-}

shrinkRight :: Row -> Row
shrinkRight [a, b, c, d] = 
    if c ~~ d then
        if a ~~ b then [0, 0, a + 1, c + 1]
        else           [0, a,     b, c + 1]
    else
        if b ~~ c then [0, a, b + 1, d]
        else
            if a ~~ b then [0, a + 1, c, d]
            else           [a,     b, c, d]
    where
    x ~~ y = x == y && 0 /= x

moveRight :: Grid -> Grid
moveRight g = fmap (shrinkRight . shiftRight) g

-- Rotates Grid 90 degrees clockwise 
turn :: Grid -> Grid
turn [[a, b, c, d],
      [e, f, g, h],
      [i, j, k, l],
      [m, n, o, p]] = 
    [[m, i, e, a],
     [n, j, f, b],
     [o, k, g, c],
     [p, l, h, d]]

turnN :: Int -> Grid -> Grid
turnN n g = iterate turn g !! n

move :: Direction -> Grid -> Grid
move dir g = turnN n $ moveRight $ turnN n' g
    where
    n = fromEnum dir
    n' = (4 - n) `mod` 4

addNewNumber :: Int -> Int -> Grid -> Grid
addNewNumber index number grid = 
    gridFromList $ insertOnZero index' gridAsList
    where
    gridAsList = gridToList grid
    nZeroes = length $ filter (== 0) gridAsList
    index' = index `mod` nZeroes
    insertOnZero 0 (0:xs) = number:xs
    insertOnZero i (0:xs) = 0 : insertOnZero (i - 1) xs
    insertOnZero i (x:xs) = x : insertOnZero i xs

putRandomNumber :: StdGen -> Grid -> (Grid, StdGen)
putRandomNumber gen grid = (addNewNumber r n' grid, gen'')
    where
    (r, gen') = random gen :: (Int, StdGen)
    (n, gen'') = randomR (0, 3) gen'
    n' = (n `div` 3) + 1

repeatM :: IO Bool -> IO ()
repeatM m = do
    b <- m
    if b then do
        repeatM m
        return ()
    else return ()

loose :: Grid -> Bool
loose g = null $ filter (/= 0) $ gridToList g

getOneOf :: [Char] -> IO Char
getOneOf acceptable = do
    c <- getChar
    if c `elem` acceptable then
        return c
    else do
        c' <- getOneOf acceptable
        return c'

charToDirection :: Char -> Direction
charToDirection 'h' = DL
charToDirection 'j' = DD
charToDirection 'k' = DU
charToDirection 'l' = DR

initialGrid :: StdGen -> (Grid, StdGen)
initialGrid gen = (three, g3)
    where
    emptyGrid = replicate 4 $ replicate 4 0
    (one, g1) = putRandomNumber gen emptyGrid
    (two, g2) = putRandomNumber gen one
    (three, g3) = putRandomNumber gen two

game :: Grid -> StdGen -> IO ()
game grid gen = do
    putStrLn $ showGrid grid
    c <- getOneOf "hjkl"
    let dir = charToDirection c
    let gridAfterMove = move dir grid
    if not $ loose gridAfterMove then do
        let (newGrid, gen') = putRandomNumber gen gridAfterMove
        game newGrid gen'
    else 
        return ()

main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    stdGen <- getStdGen
    let (grid, gen) = initialGrid stdGen
    game grid gen
