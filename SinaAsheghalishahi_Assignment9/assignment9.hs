-- assignment9.hs
{-
Name: Sina Asheghalishahi
Date Created: 05/06/2025
Program: EECS 468 Assignment 09
Description: Haskell program that implements the game of nim for 2 players.
Input: NA (i.e. nim func. is called, and players take turns inputting entries)
Output: Player that wins the game
Collaborators: NA
Other Sources: NA
-}

-- import entities to check whether char values are digits, and to convert char digits to ints, resp.
import Data.Char (isDigit)
import Data.Char (digitToInt)

-- declare data type Board, which is a list of ints
type Board = [Int]
-- initial func. defined, which just returns an initial pre-defined board
initial :: Board
initial = [5,4,3,2,1]

-- dsiplayBoard func. defined, which takes in a board and the first index, and recursively prints its contents
-- Input: Board and first index as int
-- Output: IO outputs displaying the board
displayBoard :: Board -> Int -> IO ()
-- base case prints new-line char
displayBoard [] _ = putChar '\n'
-- rec. case takes in board and index position
displayBoard (x:xs) position = do
                        -- prints each row of asterisks
                        putStr (show position ++ ": " ++ replicate x '*' ++ "\n")
                        -- recurses with remainder of board and next index
                        displayBoard xs (position+1)


-- isGameFinished func. defined, which takes a board as input, and outputs a bool for whether the game is over
-- Input: Board type
-- Output bool for whether game has ended
isGameFinished :: Board -> Bool
-- checks wether the list w/ zeroes from the board is equal in length to the board
isGameFinished board = length [x | x <- board, x == 0] == length board


-- updateBoard func. defined, which takes in a board, a given row, and a given # of stars, and outputs an updated board
-- Input: Board type and 2 ints
-- Output: Board type of updated board list
updateBoard :: Board -> Int -> Int -> Board
-- returns updated board with value at given row # reduced by given # of stars
updateBoard board row stars = take row board ++ [(board !! row) - stars] ++ drop (row+1) board


-- isValid func. defined, which takes in a board with a given row # and # of stars, and returns bool based on whether row and stars numbers are valid
-- Input: Board type and 2 ints
-- Output: bool based on whether given row and star numbers are valid for given board
isValid :: Board -> Int -> Int -> Bool
-- checks wether given row # is within length of board, and # of stars is within current amt. of stars at given row # in board
isValid board row stars = row >= 0 && row < length board && stars <= (board !! row)

-- play func. defined, which implements game main loop 
-- Input: Board type and player # as int
-- Output: IO outputs that progress game to completion
play :: Board -> Int -> IO ()
-- do block declared for main loop
play board player =  do
                        -- new-line char printed
                        putChar '\n'
                        -- board is displayed via displayBoard func. call
                        displayBoard board 1
                        -- check if game is finished already
                        if isGameFinished board
                            then do
                                -- if so, then check if current player is #1
                                if player == 1
                                    -- if so, then print that #2 won
                                    then putStrLn "Player 2 wins!\n"
                                    -- otherwise, print that #1 won
                                    else putStrLn "Player 1 wins!\n"
                                -- if game has not ended, continue w/ loop
                            else do
                                -- print current player
                                putStrLn ("Player " ++ show player)
                                -- prompt row #
                                putStr "Enter a row number: "
                                -- collect row # until new-line is encountered
                                row <- getLine
                                -- prompt for # of stars
                                putStr "Stars to remove: "
                                -- collect # of stars
                                stars <- getLine
                                -- check whether collected row and stars numbers are digits
                                if all isDigit row && all isDigit stars
                                    -- if so, then continue w/ loop
                                    then do
                                        -- converts row and stars numbers to ints
                                        let intRow = (read row :: Int)-1
                                        let intStars = read stars :: Int

                                        -- check whether row and stars numbers are valid for current board
                                        if isValid board intRow intStars
                                            -- if so, then continue w/ loop
                                            then do
                                                -- check is current player is #1
                                                if player == 1
                                                    -- if so, then recurse w/ updated board and player 2
                                                    then play (updateBoard board intRow intStars) 2
                                                    -- otherwise, recurse w/ updated board and player 1
                                                    else play (updateBoard board intRow intStars) 1
                                            -- if row and stars numbers are invalid for current board, then execute error case
                                            else do
                                                -- print error message to output
                                                putStrLn "Invalid Input: Inputs out of bounds\n"
                                                -- recurse w/ current board and player #
                                                play board player
                                    -- if row and stars numbers are not digits, then execute error case
                                    else do
                                        -- print error message to output
                                        putStrLn "Invalid Input: Non-integers entered\n"
                                        -- recurse w/ same board and player #
                                        play board player

-- nim func. defined, which runs main game loop
nim :: IO ()
-- nim call invokes play func. w/ initial board and player #1
nim = play initial 1

