{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (stateToString
  ,switchSide
  ,locToPiece
  ,stringToLoc
  ,isValidPath
  ,isNoObtascle
  ,doMoving
  ,getPiecesOnPath
  ,pathGen
  ) where
import Tafl.Core
--------------Game Logic-----------

-- DATA FORMATING & CONVERSION

--Converting PieceType and Repesentative Character/String

charToPiece :: Char -> Piece
charToPiece char
 | char == 'O' = O
 | char == 'G' = G
 | char == 'L' = L
 | char == 'X' = X
 | char == ' ' = E
 | otherwise = R

pieceToString :: Piece ->String
pieceToString pType
 | pType == O = "O"
 | pType == G = "G"
 | pType == L = "L"
 | pType == X = "X"
 | pType == E = " "
 | otherwise = "R"

--Converting src & des strings into Loc type
stringToLoc:: String -> Loc
stringToLoc (col:row) = (rowIndex, colIndex)
  where
    rowIndex = 8 - ((read row :: Int) - 1)
    colIndex = fromEnum col - 97

--Getting the chess piece at a specific location
locToPiece:: GameState -> Loc -> Piece
locToPiece inState (letter, no) = charToPiece (gameBoard inState !! no !! letter)

--Checking if the location is in the range (0~8)
isValidLoc :: Loc -> Bool
isValidLoc (rowIndex, colIndex) =  rowIndex >= 0 && rowIndex <= 8 && colIndex >= 0 && colIndex <= 8

-- MOVE FUNCTIONALITY

--Switching side after each move
switchSide :: GameState -> GameState
switchSide inState = inState{gameTurn = if gameTurn inState == Lambdas then Objects else Lambdas}

--check if a path is valid
  --start with a piece
  --end not on central (5,5) aka (e5)
  --both locations are in range
  --moving in a staright line
  --no obtacles on the path
  --if it cross central (5,5)
    --if lambda has move then can move accross
      --note : cross central + hasLambdaMoved == ok

isValidPath :: GameState ->Loc ->Loc -> Bool
isValidPath inState src des = isAPiece inState src && isNotToCastle des
                              && isValidLoc src && isValidLoc des
                              && isMovingStraight src des && isNoObtascle inState src des
                              && (not (isCrossCentral $pathGen src des) || hasLambdaMoved inState)

  --TO MOVE
  --set src to E
  --set des to <TYPE>
  --return GameState
doMoving:: GameState -> Loc -> Loc -> GameState
doMoving inState src des = newGameState
  where
    srcType = locToPiece inState src
    newGameBoard = placePiece (placePiece (gameBoard inState) src E) des srcType
    newGameState = inState{gameBoard =newGameBoard}
--Subfunction placePiece --> Placing a piece on the board
placePiece :: Board -> Loc -> Piece -> Board
placePiece inBoard (rowIndex, colIndex) newType = newGameBoard
  where
    targetRow =  inBoard !! rowIndex
    modifiedRow = take colIndex targetRow ++ pieceToString newType ++ drop (colIndex+1) targetRow
    newGameBoard = take rowIndex inBoard ++ [modifiedRow] ++ drop (rowIndex+1) inBoard

-- Subfunction :getting the path between 2 locations
pathGen :: Loc -> Loc -> [Loc]
pathGen (srcRow, srcCol) (desRow, desCol)
  |srcRow == desRow =
    if srcCol <desCol
      then drop 1 $ zip [srcRow,srcRow..srcRow][srcCol..desCol]
    else init $ zip [srcRow,srcRow..srcRow][desCol..srcCol]
  |srcRow < desRow = drop 1 $ zip [srcRow..desRow][srcCol,srcCol..srcCol]
  |otherwise  =  init $ zip [desRow..srcRow][srcCol,srcCol..srcCol]

--Subfunction getPiecesOnPath --> get all the pieces on the path
getPiecesOnPath :: GameState -> [Loc] -> [Piece]
getPiecesOnPath inState path = map (locToPiece inState) path

--Subfunction :Cheking is there is any obstacle on the path (Not Empty AKA anything but E)
isNoObtascle :: GameState -> Loc ->Loc ->Bool
isNoObtascle inState src des = all (== E) (getPiecesOnPath inState (pathGen src des))



--Subfunction :Checking if there is a piece at a specific location
isAPiece :: GameState ->  Loc -> Bool
isAPiece inState loc = (/=) E $ locToPiece inState loc

--Subfunction :Checking if the piece would move AKA not diagonal or stay the same place
isMoving :: Loc -> Loc -> Bool
isMoving (srcRow, srcCol) (desRow, desCol) = (srcRow /= srcCol) || (srcRow /= srcCol)

--Subfunction :Checking if the path between 2 locations is a straight line
isMovingStraight :: Loc -> Loc -> Bool
isMovingStraight (srcRow, srcCol) (desRow, desCol) = (srcRow == desRow) || (srcCol == desCol)

--Subfunction :Checking if the desination is not the castle
isNotToCastle :: Loc -> Bool
isNotToCastle (desRow, desCol) = not( (==) (5,5) (desRow, desCol))

--Sub function : isCrossCentral --> Check if the path crosses e5
isCrossCentral :: [Loc] ->Bool
isCrossCentral path = (5, 5) `elem` path

--Subfunction : Check if the central lambda has moved away from the castle
hasLambdaMoved :: GameState ->Bool
hasLambdaMoved inState = (/=) ((gameBoard inState !! 5) !! 5) 'L'

-- TO STRING METHODS
--Board to string

boardToString :: [String] -> String
boardToString inBoard =  " abcdefghi \n" ++ unlines(zipWith (++) ["9","8","7","6","5","4","3","2","1"] inBoard)

--State to string
stateToString :: GameState -> String
stateToString inState = do
                          let boardString = boardToString $ gameBoard inState
                          show(gameTurn inState)++" to play\n"++boardString
