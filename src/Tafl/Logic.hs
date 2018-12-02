{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (stateToString
  ,locToPiece
  ,stringToLoc
  ,isValidPath
  ,isMoveUnobstructed
  ,doMoving
  ) where
import Tafl.Core
--------------Game Logic-----------

--Board to string

boardToString :: [String] -> String
boardToString inBoard =  " abcdefghi \n" ++ unlines(zipWith (++) ["9","8","7","6","5","4","3","2","1"] inBoard)

--State to string
stateToString :: GameState -> String
stateToString inState = do
                          let boardString = boardToString $ gameBoard inState
                          show(gameTurn inState)++" to play\n"++boardString

--Converting PieceType and Repesentative Character
charToPiece :: Char -> Piece
charToPiece char
 | char == 'O' = O
 | char == 'G' = G
 | char == 'L' = L
 | char == 'X' = X
 | char == ' ' = E
 | otherwise = E

stringToPiece:: String -> Piece
stringToPiece str
 | str == "O" = O
 | str == "G" = G
 | str == "L" = L
 | str == "X" = X
 | str == " " = E
 | str == "E" = E
 | otherwise = Err

pieceToChar :: Piece ->Char
pieceToChar pType
 | pType == O = 'O'
 | pType == G = 'G'
 | pType == L = 'L'
 | pType == X = 'X'
 | pType == E = ' '
 | otherwise = ' '

pieceToString :: Piece ->String
pieceToString pType
 | pType == O = "O"
 | pType == G = "G"
 | pType == L = "L"
 | pType == X = "X"
 | pType == E = " "
 | otherwise = " "

--Converting src & des strings into Loc type
stringToLoc:: String -> Loc
stringToLoc (col:row) = (rowIndex, colIndex)
  where
    rowIndex = 8 - ((read row :: Int) - 1)
    colIndex = fromEnum col - 97

--Getting the chess piece at a specific location
locToPiece:: GameState -> Loc -> Piece
locToPiece inState (letter, no) = charToPiece $(gameBoard inState !! no) !! letter

--Checking if the location is in the range (0~8)
isValidLoc :: Loc -> Bool
isValidLoc (rowIndex, colIndex) =  rowIndex >= 0 && rowIndex <= 8 && colIndex >= 0 && colIndex <= 8

--Checking if there is a piece at a specific location
isAPiece :: GameState ->  Loc -> Bool
isAPiece inState loc = (/=) E $ locToPiece inState loc

--Checking if the piece would move AKA not diagonal or stay the same place
isMoving :: Loc -> Loc -> Bool
isMoving (srcRow, srcCol) (desRow, desCol) = (srcRow /= srcCol) || (srcRow /= srcCol)

--Checking if the path between 2 locations is a straight line
isMovingStraight :: Loc -> Loc -> Bool
isMovingStraight (srcRow, srcCol) (desRow, desCol) = (srcRow == desRow) || (srcCol == desCol)

--Checking if the desination is not the castle
isNotToCastle :: Loc -> Bool
isNotToCastle (desRow, desCol) = not( (==) (5,5) (desRow, desCol))

hasLambdaMoved :: GameState ->Bool
hasLambdaMoved inState = (/=) ((gameBoard inState !! 5) !! 5) 'L'


--Cheking is there is any obstacle on the path (Not Empty AKA anything but E)
isNoObtascle :: GameState -> Loc ->Loc ->Bool
isNoObtascle inState src des = all (== E) (getPiecesOnPath inState (pathGen src des))

isMoveUnobstructed :: GameState -> Loc ->Loc -> Bool
isMoveUnobstructed st (a, b) (x, y) =
  -- check all squares in path are empty
  foldl (\res sq -> res && (locToPiece st sq == E)) True (pathGen (a, b) (x, y))
    -- where
    --   -- list of squares between src and dst, including dst
    --   path = if a == x
    --     -- +1 and -1 omits the src square from the path
    --     then [show (((gameBoard st) !! a) !! i)| i <- [((min (b+1) y))..(max (b-1) y)]]
    --     else [show (((gameBoard st) !! i) !! b)| i <- [((min (a+1) x))..(max (a-1) x)]]

  -- Subfunction : pathGen --> getting the path between 2 locations (smaller coordinates first)
pathGen :: Loc -> Loc -> [Loc]
pathGen (srcRow, srcCol) (desRow, desCol) =
  if srcRow == desRow
    then if srcCol <desCol
      then drop 1 $ zip [srcRow,srcRow..srcRow][srcCol..desCol]
      else init $ zip [srcRow,srcRow..srcRow][desCol..srcCol]
  else if srcRow < desRow
    then drop 1 $ zip [srcRow..desRow][srcCol,srcCol..srcCol]
    else init $ zip [desRow..srcRow][srcCol,srcCol..srcCol]

  --Subfunction getPiecesOnPath --> get all the pieces on the path
getPiecesOnPath :: GameState -> [Loc] -> [Piece]
getPiecesOnPath inState path = map (locToPiece inState) path

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
                              && isMovingStraight src des && isMoveUnobstructed inState src des
                              && (if isCrossCentral $pathGen src des then hasLambdaMoved inState else True)

--Sub function : isCrossCentral --> Check if the path crosses e5
isCrossCentral :: [Loc] ->Bool
isCrossCentral path = elem (5,5) path

--TO MOVE
--check if the path is valid
--set src to E
--set des to <TYPE>
--return GameState
doMoving:: GameState -> Loc -> Loc -> GameState
doMoving inState src des = newGameState
  where
    srcType = locToPiece inState src
    desType = locToPiece inState des
    newGameState = placePiece (placePiece inState src E) des srcType

  --Subfunction placePiece --> Placing a piece on the board
placePiece :: GameState -> Loc -> Piece -> GameState
placePiece inState (rowIndex, colIndex) newType = newState
  where
    targetRow =  gameBoard inState !! rowIndex
    modifiedRow = (take colIndex targetRow) ++ pieceToString newType ++ (drop (colIndex+1) targetRow)
    newGameBoard = (take rowIndex (gameBoard inState)) ++ [modifiedRow] ++ (drop(rowIndex+1)(gameBoard inState))
    newState = GameState{
    inGame = inGame inState,
    inTestMode = inTestMode inState,
    gameTurn = gameTurn inState,
    gameBoard = newGameBoard
    }
