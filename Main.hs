-- Some syntax stuff Ive learned:

-- "::" is read as "has a type of"
-- Use "<-" when you want to bind results of I/O actions to names 

-- Use "let" to bind pure expressions to names

-- Functions have types. We can choose to give them an explicit type declaration. 
-- This is generally considered to be good practice

-- The parameters are separated with "->" 
-- The return type is the last item in the declaration
-- Ex: "Function1 :: Int -> Int -> Int -> Int" has 3 parameters that are of the "Int" type. 
-- The last "Int" is the return type.


-- data & types

-- I have learned that the "data" keyword represents a new type, and the "type" keyword represents
-- a type synonym.  Like how a "String" is just a list of "Char"
-----------------------------------------------------------------------------------------------------------------
type Coordinates = (Int,Int)

type Board = [String]

--pure functions
-----------------------------------------------------------------------------------------------------------------

--checks a player's coordinates to see if they won the game
checkWin :: Board -> String -> Bool
checkWin board turn
    = 
        if ((board !! 0) == turn) && ((board !! 1) == turn) && ((board !! 2) == turn)
            then True
        else if ((board !! 3) == turn) && ((board !! 4) == turn) && ((board !! 5) == turn)
            then True
        else if ((board !! 6) == turn) && ((board !! 7) == turn) && ((board !! 8) == turn)
            then True
        else if ((board !! 0) == turn) && ((board !! 3) == turn) && ((board !! 6) == turn)
            then True
        else if ((board !! 1) == turn) && ((board !! 4) == turn) && ((board !! 7) == turn)
            then True
        else if ((board !! 2) == turn) && ((board !! 5) == turn) && ((board !! 8) == turn)
            then True
        else if ((board !! 0) == turn) && ((board !! 4) == turn) && ((board !! 8) == turn)
            then True
        else if ((board !! 2) == turn) && ((board !! 4) == turn) && ((board !! 6) == turn)
            then True
        else
            False

checkTie :: Board -> Bool
checkTie board
    =
        if (countEmptySpots board 0) == 0
            then
                if checkWin board "O" == True
                    then False
                else if checkWin board "X" == True
                    then False
                else
                    True
        else
            False

changeTurns :: String -> String
changeTurns n
    =
        if n == "O"
            then "X"
        else
            "O"

showStringInList :: [String] -> Int -> String
showStringInList someList num
    = 
        head (drop num someList)

coordinatesToInt :: Coordinates -> Int
coordinatesToInt somePoint
    =
        (fst somePoint) + 3 * (snd somePoint)

updateBoard :: Board -> Int -> Int -> String -> Board
updateBoard oldBoard x y turn
    =
       (fst (splitAt (coordinatesToInt (x,y)) oldBoard)) ++ [turn] ++ (drop 1 (snd (splitAt (coordinatesToInt (x,y)) oldBoard)))

countEmptySpots :: Board -> Int -> Int
countEmptySpots board count
    =
        if length board > 0
            then
                if (head board) == "*"
                    then
                        countEmptySpots (drop 1 board) (count + 1)
                else
                    countEmptySpots (drop 1 board) count
        else
            count

validMove :: String -> Int -> Int -> Board -> Bool
validMove turn x y board
    =
        if (board !! coordinatesToInt (x,y)) == "*"
            then
                True
        else
            False



--input/output functions
------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Hello, welcome to this completely normal Tic Tac Toe game."

    putStrLn "\nPlayer 1, please enter your name."
    player1 <- getLine

    putStrLn "\nPlayer 2, please enter your name."
    player2 <- getLine

    putStrLn ("\n" ++ player1 ++ " will use X's and " ++ player2 ++ " will use O's.")
   
    putStrLn("\nWho will go first?")
    firstPlayer <- getLine

    chooseFirstPlayer player1 player2 firstPlayer        

chooseFirstPlayer :: String -> String -> String -> IO ()
chooseFirstPlayer player1 player2 firstPlayer
    = do
        if firstPlayer == player1
            then do
                putStrLn ("\nAlright. " ++ player1 ++ " will go first.")
                -- play the game
                playGame "X"
        else if firstPlayer == player2
            then do
                putStrLn ("\nAlright. " ++ player2 ++ " will go first.")
                -- play the game
                playGame "O"
        else
            do
                putStrLn ("\nIt appears there was a typo. Please try again.")
                putStrLn("\nWho will go first?")
                newFirstPlayer <- getLine
                chooseFirstPlayer player1 player2 newFirstPlayer

playGame :: String -> IO ()
playGame currentTurn
    = do
        let board = ["*","*","*","*","*","*","*","*","*"]

        putStr ("\n\n --------------------------------------------------------------")
        putStrLn ("\nThe board is set up like this:")
        printBoard ["0,0","1,0","2,0","0,1","1,1","2,1","0,2","1,2","2,2"]
        putStrLn ("\nWhen taking your turn, please use the form (x,y). (no parenthesis)")

        playTurn currentTurn board

        putStrLn ("\nPlay again? (Type 'Yes' to continue)")
        playAgain <- getLine
                
        if playAgain == "Yes"
            then do
                putStrLn ("\nLoser goes first.")
                playGame (changeTurns currentTurn)
        else
            do
                putStrLn ("\nThanks for playing!")
              
playTurn :: String -> Board -> IO ()
playTurn  currentTurn board 
    = do
        putStrLn ("\n\n --------------------------------------------------------------")
        putStrLn ("\nThis is the current board.")
        printBoard board

        if checkTie board == True
            then do
                putStrLn("\nWow! Its a tie!")
        else if checkWin board currentTurn == True
            then do
                putStrLn("\nCongratulations Player " ++ currentTurn ++ "! You are the winner!")
        else if checkWin board (changeTurns currentTurn) == True
            then do
                putStrLn("\nCongratulations Player " ++ (changeTurns currentTurn) ++ "! You are the winner!")
        else
            do
            putStrLn ("\nPlease place an " ++ currentTurn ++ " on the board.")

            putStrLn ("\nX coordinate?")
            x <- getLine

            putStrLn ("\nY coordinate?")
            y <- getLine

            let newBoard = updateBoard board (read x) (read y) currentTurn

            if ((validMove currentTurn (read x) (read y) board) == True)
                then do
                    putStrLn("\nAn " ++ currentTurn ++ " was placed on space (" ++ x ++ "," ++ y ++ ")")
                    playTurn (changeTurns currentTurn) newBoard
            else
                do
                putStrLn("\nThat is not a valid move! Try again.")
                playTurn currentTurn board

printBoard :: [String] -> IO()
printBoard board
    = do
        putStrLn ("\n " ++ showStringInList board 0 ++ " " ++ showStringInList board 1 ++ " " ++ showStringInList board 2)
        putStrLn (" " ++ showStringInList board 3 ++ " " ++ showStringInList board 4 ++ " " ++ showStringInList board 5)
        putStrLn (" " ++ showStringInList board 6 ++ " " ++ showStringInList board 7 ++ " " ++ showStringInList board 8)



                
                
 