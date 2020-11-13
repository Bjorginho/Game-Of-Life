-- INF122 Oblig 2, André Bjørgum
-- 10 (s 0 0, b 2 2) 4 3 5 2 5 3

module Oblig2 where
import Data.Char ( isAlpha, isDigit )
import Control.Concurrent (threadDelay)
import Data.List ( (\\))

type Rule = (Char, Int, Int)
type Pos = (Int, Int)
type Objects = [Pos]

-- Main function 
main :: IO ()
main = do 
    cls 
    -- Initial input 
    gameOfLife 0 ('s', 0, 0) ('b', 0, 0) [] 

-- Gameloop 
gameOfLife :: Int -> Rule -> Rule -> [Int] -> IO()
gameOfLife n s b cells  = do
    clearInputField (n)
    goto(0, n + 2)
    putStr "Input: "
    input <- getLine

    let currentCells = tuplesToList(remDup(intsToTuple(cells)))
    let inp = words input
    let args = tail inp 
    let reset = do 
        gameOfLife n s b cells  

    if (null inp) 
        then do 
            life n (currentCells) s b 1 
    else do 
     
        case (head (inp)) of 

            "c" -> 
                if (length args == 1 && onlyDigits(head args)) 
                    then do 
                        let size = read(args !! 0) :: Int 
                        grid size
                        gameOfLife size s b [] 
                else if not(onlyDigits(head(args))) 
                    then 
                        exception n "Invalid input (must be int)" >> reset
                else 
                    exception n "Only one argument is required" >> reset

            "n" -> do 
                let checkArgs = map (onlyDigits) args 
                if (isEven args && not(length args == 0) && all (==True) checkArgs) 
                    then do 
                        let cellsList = map (read :: String -> Int) args
                        let cells = intsToTuple(cellsList)
                        printObjects cells "O"
                        let newCells = cellsList ++ currentCells
                        gameOfLife n s b newCells
                else if (not(all (==True) checkArgs)) then 
                    exception n "Insert integers" >> reset
                
                else if (length args == 0)
                    then 
                        exception n "You didnt insert values" >> reset 
                else 
                    exception n "Must be even elements" >> reset 
            
            "e" -> do
                let checkArgs = map (onlyDigits) args
                if (isEven args && not(length args == 0) && all (==True) checkArgs ) 
                    then do 
                        let remList = map (read :: String -> Int) args
                        let remTuples = intsToTuple(remList)
                        printObjects remTuples "."
                        let newCells = currentCells \\ remList
                        gameOfLife n s b newCells
                else if (not(all (==True) checkArgs)) then 
                    exception n "Insert integers" >> reset
                else 
                    exception n "Must be even elements " >> reset
        
            "b" -> do
                let x = args !! 0 
                let y = args !! 1
                if (length args == 2 && onlyDigits(x) && onlyDigits(y)) 
                    then do
                        let a = read x :: Int 
                        let b = read y :: Int 
                        if((read "0" <= a) && (a <= b))
                            then do                 
                                let new_bRule = ('b', a, b)
                                message n ("B rule changed to " ++ show(new_bRule))
                                gameOfLife n s new_bRule currentCells 
                        else do
                            exception n "Remember: (0 <= m <= n)" >> reset
                else 
                    exception n "Invalid inputs" >> reset

            "s" -> do
                let x = args !! 0 
                let y = args !! 1
                if (length args == 2 && onlyDigits x && onlyDigits y) 
                    then do
                        let a = read x :: Int 
                        let b' = read y :: Int  -- b is the rule (called b')
                        if((read "0" <= a) && (a <= b'))
                            then do 
                                let new_sRule = ('s', a, b')
                                message n ("S rule changed to " ++ show(new_sRule))
                                gameOfLife n new_sRule b currentCells 
                        else do 
                            exception n "Remember: (0 <= m <= n)" >> reset
                else 
                    exception n "Invalid input arguments"  >> reset

            "w" -> 
                if (null args) 
                    then do 
                        message n ("Living cells: " ++ show (intsToTuple currentCells))
                        gameOfLife n s b currentCells
                else do 
                    exception n "No arguments valid." >> reset
            
            "r" -> 
                if (length args == 1)  
                    then do
                        let filename = removePunc(args !! 0)
                        let f = tokenize filename ""  "."
                        if (length f == 2) 
                            then do 
                                let name = show (head f)
                                let suffix = show (f !! 1)
                                let filename = removePunc((name ++ "." ++ suffix))
                                readF filename
                        else do
                            exception n "Not correct input, make sure you type correct filename, remember suffix!" >> reset                           
                else do 
                    exception n "Type in filename with suffix" >> reset

            "?" -> 
                if (null args) 
                    then do
                        message n ("Rules = " ++ show s ++ " and " ++ show b)
                        gameOfLife n s b currentCells
                else do
                    exception n "No arguments here." >> reset

            -- TO CHANGE SPEED CHANGE THREADDELAY IN LIFE FUNCTION (ln 275)
            "l" -> 
                if(length args == 1 && onlyDigits(args !! 0)) 
                    then do 
                        let x = read(args !! 0) :: Int 
                        if (x > 0) then do 
                            life n (currentCells) s b x 
                        else 
                            exception n "x must be 1 or greater. " >> reset 
                else 
                    exception n "You need to type an integer as argument." >> reset 

            "quit"-> cls >> return()

            _ -> exception n "Invalid input " >> reset

-- HELP FUNCTIONS

-- Duplicate string (used to clear fields)
duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

-- Clear input field
clearInputField :: Int -> IO ()
clearInputField nR = goto(0, nR + 2) >> putStrLn (duplicate " " 250)

-- Print error message on terminal
exception :: Int -> String -> IO ()
exception n msg = do
    goto(0, n+3)
    putStrLn (duplicate " " 250)
    goto(0, n+3)
    putStrLn ("Error: " ++ msg) 

-- Print message on terminal 
message :: (Show a1, Num a1) => a1 -> String -> IO ()
message n msg = do 
    goto(0, n+3)
    putStrLn (duplicate " " 250)
    goto(0, n+3)
    putStrLn msg 

-- Print board 
printBoard size objects = do 
    grid size 
    printObjects objects "O"

-- Unpack tuples
tupToList :: (a, a) -> [a]
tupToList (x,y) = [x, y]

tuplesToList :: [(a, a)] -> [a]
tuplesToList xs = concat (map (tupToList) xs)

-- Remove duplicates 
remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs) = x : remDup (filter (/= x) xs)

-- Remove item from list if in list 
remItem :: Eq a => a -> [a] -> [a]
remItem _ [] = []
remItem i (x:xs) 
    | i == x = remItem x xs 
    | otherwise = x : remItem i xs 

remElements :: Eq a => [a] -> [a] -> [a]
remElements xs y = (\\) xs y 

-- Check if current cell is alive 
isAlive :: Pos -> Objects -> Bool
isAlive pos cells = elem pos cells 

-- Check if current cell is empty 
isEmpty :: Pos -> Objects -> Bool 
isEmpty pos cells = not (isAlive pos cells)

-- Alive neigbours
aliveNeigbours :: Pos -> Objects -> Int -> Int
aliveNeigbours cell cells nR = length (filter (==True) ([isAlive x cells | x <- (neigbours cell nR)]))

-- Survivors 

survivors :: Int -> Objects -> Int -> Int -> Objects
survivors nR cells n m  = [cell | cell <- cells, elem (aliveNeigbours cell cells nR) [n..m]]

-- Births 

births :: Int -> Objects -> Int -> Int -> Objects
births nR cells n m  = [(x, y) | x <- [1..nR], y <- [1..nR], isEmpty (x,y) cells, elem (aliveNeigbours (x,y) cells nR) [n..m]]

-- Check if Objects contains cells 
boardEmpty :: Foldable t => t a -> Bool
boardEmpty objects = if (length objects == 0) then True else False 

-- Next generation
nextGen :: Int -> Objects  -> Rule -> Rule -> Objects
nextGen nR cells sRule bRule = survivors nR cells (fst(getValues(sRule))) (snd(getValues(sRule)))  ++ births nR cells (fst(getValues(bRule))) (snd(getValues(bRule))) 

-- Get n and m in a tuple
getValues :: Rule -> (Int, Int)
getValues (_, x, y) = (x, y)

-- Visualize x- generations 
life :: Int -> [Int] -> Rule -> Rule -> Int -> IO ()
life n cells sRule bRule x  = do 
    
    let currGen = intsToTuple(cells)                                -- Tuple
    let nextG = nextGen n (remDup(intsToTuple(cells))) sRule bRule  -- Tuple
    let dyingCells = remElements (intsToTuple(cells)) (nextG)       -- Tuple 
    let currGen_list = tuplesToList(currGen)                        -- List 

    if(currGen == nextG) then do 
        message n "A stable configuration is reached."
        gameOfLife n sRule bRule currGen_list

    else if (x == 0) then do 
        gameOfLife n sRule bRule currGen_list
    
    else if (boardEmpty cells) then do 
        message n "Board is now empty!"
        gameOfLife n sRule bRule currGen_list
    
    else do  
        printObjects (dyingCells) "."
        printObjects (nextG) "O"
        threadDelay 9000                                           -- To change speed change threadDelay (1000000 = 1 second)
        life n (tuplesToList(nextG)) sRule bRule (x-1)

-- Clear terminal and place cursor on top 
cls :: IO ()
cls = do 
    clear
    goto(0, 0)

removePunc xs = [ x | x <- xs, not (x `elem` "\"\'")]

-- Check isEven, for argument input in gameOfLife 
isEven :: [a] -> Bool
isEven xs = if mod (length xs) 2 == 0 then True else False

-- Convert list of integers to tuples 
intsToTuple :: [Int] -> [(Int, Int)]
intsToTuple [] = []
intsToTuple (x:y:xs) = (x , y) : intsToTuple xs 

-- Place list of tuples on Objects
printObjects :: [(Int, Int)] -> String -> IO()
printObjects [] _ = return () 
printObjects (x:xs) s = do
    printObject(x) s 
    printObjects xs s 

-- Print object on board
printObject :: (Int, Int) -> String -> IO()
printObject (x,y) s = do
    goto(x * 3 + 2, y + 1)
    putStr s 

-- Remove first n elements from list
remFirstEls :: Int -> [a] -> [a]
remFirstEls n xs
    | ((n <= 0) || null xs) = xs
    | otherwise = remFirstEls (n-1) (tail xs)

-- Get neigbours to a given (x,y) coordinate with boardsize
neigbours :: (Num a, Ord a) => (a, a) -> a -> [(a, a)]
neigbours (x,y) n = 
    if (x == 1 && y == 1) then ([(x+1, y), (x+1, y+1), (x, y+1)])                                           -- Øverst til venstre 
    else if (x == 1 && y > 1 && not(y==n)) then ([(x, y-1), (x+1, y-1), (x+1, y), (x+1, y+1), (x, y+1)])    -- venstre kolonne 
    else if (x == 1 && y == n) then ([(x, y-1), (x+1, y-1), (x+1, y)])                                      -- Nederst til venstre
    else if (x > 1 && y == 1 && not(x==n)) then ([(x-1, y), (x-1, y+1), (x, y+1), (x+1, y), (x+1,y+1)])     -- Øverste rad
    else if (x == n && y == 1) then ([(x-1, y), (x-1, y+1), (x, y+1)])                                      -- Øverst til høyre 
    else if (x == n && y > 1 && not(y==n)) then ([(x, y-1), (x-1, y-1), (x-1, y), (x-1, y+1), (x, y+1)])    -- høyre kolonne
    else if (x == n && y == n) then ([(x, y-1), (x-1, y-1), (x-1, y)])                                      -- nederst til høyre
    else if (x > 1 && y == n) then ([(x-1, y), (x-1, y-1), (x, y-1), (x+1, y-1), (x+1, y)])                 -- nederst
    else ([(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)])         -- ellers 


-- Open and read a file, file input have syntax -> String (Char, Int, Int) (Char, Int, Int) [String] 
readF :: String -> IO ()
readF filename = 
    do
    x <- readFile filename 
    let str = tokensFile (filter (/= '\n') x)
    let n =  read ((head str)) :: Int
    let cells = (map (read :: String -> Int)(remFirstEls 7 str)) 
 
    if (isEven(cells)) then do         
        let firstChar = (show (str !! 1)) !! 1
        if (firstChar == 's') then do
            let sRule = (firstChar, (read (str !! 2) :: Int), (read (str !! 3) :: Int))
            let b = (show (str !! 4)) !! 1
            if (b == 'b') then do 
                let bRule = (b, (read (str !! 5) :: Int), (read (str !! 6) :: Int))
                grid n
                printObjects (intsToTuple(cells)) "O"
                gameOfLife n sRule bRule cells 
            else do 
                exception n ("Invalid char in file (" ++ filename ++ ")") >> gameOfLife n ('s', 0, 0) ('b', 0, 0) cells 

        else if (firstChar == 'b') then do
            let bRule = (firstChar, (read (str !! 2) :: Int), (read (str !! 3) :: Int))
            let s = (show (str !! 4)) !! 1
            if (s == 's') then do 
                let sRule = (s, (read (str !! 5) :: Int), (read (str !! 6) :: Int))
                grid n
                printObjects (intsToTuple(cells)) "O"
                gameOfLife n sRule bRule cells 
            else do 
                exception n ("Invalid char in file (" ++ filename ++ ")")

        else do 
            exception n "Invalid file structure." >> gameOfLife 0 ('s', 0, 0) ('b', 0, 0) cells 
    else do 
        exception n "Something wrong with file, make sure you have even elements or correct rule setup."
    
-- Tokenize from Oblig 1
onlyDigits :: Foldable t => t Char -> Bool
onlyDigits y = all isDigit y

onlyAlpha :: Foldable t => t Char -> Bool
onlyAlpha y = all isAlpha y

tokenize :: Eq a => [a] -> [a] -> [a] -> [[a]]
tokenize [] t s = []
tokenize (x:xs) t s 
        | elem x t = [x] : tokenize xs t s
        | elem x s = tokenize xs t s
        | otherwise = (takeWhile (notin (t++s)) (x:xs)) : tokenize (dropWhile (notin (t++s)) (x:xs)) t s
notin :: (Foldable t, Eq a) => t a -> a -> Bool
notin xs = \x -> not(elem x xs) 

tokensFile xs = tokenize xs "" "(), "

-- GRID FUNCTIONS, week 9 

clear :: IO ()
clear = putStr "\ESC[2J"

ve :: Integer
ve = 3

-- Construct an user friendly grid in Terminal 
grid :: Int -> IO()
grid n = do
    clear
    writeTop n
    mapM_ (\i -> writeRow i n) [1..n]
    putStrLn ""

writeTop :: (Num a, Enum a, Ord a, Show a) => a -> IO ()
writeTop n =
    writeAt (ve + 1, 0) ((concat [formatN i ++ " " | i <- [1..n]]) ++ "\n")

formatN :: (Ord a, Num a, Show a) => a -> [Char]
formatN n = if (n < 10) then (" " ++ show(n)) else (show n)

writeRow :: (Show a1, Ord a1, Num a1, Num a2, Enum a2) => a1 -> a2 -> IO ()
writeRow i n = do
    writeAt(if i > 9 then (ve - 2) else ve - 1, 1 + i) (show i)
    mapM_ (\i -> putStr "  .") [1..n]
    putStr ""

writeAt :: (Show a1, Show a2) => (a2, a1) -> String -> IO ()
writeAt (x,y) xs = do
    goto(x,y)
    putStr xs

goto :: (Show a1, Show a2) => (a2, a1) -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")