{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    ( -- Game type
      Game
    , gameBoard
    , gameScore
    , gameGoal
    -- Game API
    , newGame
    , makeMove
    , addNewNumber
    , isWon
    , isLost
    , showBoard
    , printBoard
    -- Misc types
    , Direction(..)
    , Board
    ) where

import           Control.Applicative
import           Control.Lens         hiding (holes)
import           Control.Monad
import           Control.Monad.Random (getRandomR)
import           Control.Monad.Writer
import           Data.Maybe           (isJust, catMaybes)
import           Data.List
import qualified Data.Text as T

data Direction = East | West | North | South deriving Eq

type Pos   = (Int,Int) -- (Row,Col)

type Cell  = Maybe Int
type Row   = [Cell]
type Board = [Row]

data Game = Game
    { _gameBoard :: Board
    , _gameScore :: Int
    , _gameGoal  :: Maybe Int
    }
makeLenses ''Game

newGame :: Int -> Maybe Int -> IO Game
newGame size goal = do
    board <- newBoard
    return $ Game
        { _gameBoard = board
        , _gameScore = 0
        , _gameGoal  = goal
        }
  where
    newBoard :: IO Board
    newBoard = addNewNumber' emptyBoard >>= addNewNumber'

    emptyBoard :: Board
    emptyBoard = replicate size $ replicate size Nothing

isWon :: Game -> Bool
isWon game = maybe False isWon' $ game^.gameGoal
  where
    isWon' :: Int -> Bool
    isWon' goal = elem goal . catMaybes . concat $ game^.gameBoard

isLost :: Game -> Bool
isLost = isLoserBoard . view gameBoard

-- Given a direction to move, update the game's board, score, and state.
-- Game board is updated purely - only cell shifting occurs. A new random cell
-- is not added.
makeMove :: Direction -> Game -> Game
makeMove dir game = game & gameBoard .~ newBoard
                         & gameScore %~ (+ score)
  where
    (newBoard,score) = shiftBoard dir (game^.gameBoard)

-- Transform a board by adding a new cell with 90% chance to be a 2 and
-- 10% chance to be a 4. Assumes there is at least one empty cell.
addNewNumber :: Game -> IO Game
addNewNumber game = do
    board' <- addNewNumber' (game^.gameBoard)
    return (gameBoard .~ board' $ game)

addNewNumber' :: Board -> IO Board
addNewNumber' board = do
    (pos,val) <- newCell (availablePositions board)
    return $ updateBoard pos (Just val) board
  where
    newCell :: [Pos] -> IO (Pos,Int)
    newCell holes = do
        pos <- getRandomElem holes
        pct <- getRandomPercent
        if pct < 0.9
            then return (pos,2)
            else return (pos,4)

    getRandomElem :: [a] -> IO a
    getRandomElem xs = (xs !!) `liftM` getRandomR (0, length xs - 1)

    getRandomPercent :: IO Float
    getRandomPercent = getRandomR (0,1)

updateBoard :: Pos -> Cell -> Board -> Board
updateBoard (row,col) val = (ix row . ix col) .~ val

showBoard :: Board -> String
showBoard = T.unpack . T.unlines . fmap formatRow
    where formatRow = T.intercalate "|" . fmap (T.center 4 ' ' . formatCell)
          formatCell (Just x) = T.pack $ show x
          formatCell _ = T.empty

shiftBoard :: Direction -> Board -> (Board, Int)
shiftBoard dir = unwrap . f dir
  where
    f :: Direction -> Board -> Writer (Sum Int) Board
    f West  = shiftBoardWest
    f East  = shiftBoardEast
    f North = shiftBoardNorth
    f South = shiftBoardSouth

    -- Unwrap a board shift to a nicer result tuple.
    unwrap :: Writer (Sum Int) Board -> (Board, Int)
    unwrap = fmap getSum . runWriter

shiftBoardWest, shiftBoardEast, shiftBoardNorth, shiftBoardSouth :: Board -> Writer (Sum Int) Board
shiftBoardWest  = mapM shiftRowWest
shiftBoardEast  = mapM $ horizontallyMirror shiftRowWest
shiftBoardNorth = diagonallyMirror shiftBoardWest
shiftBoardSouth = diagonallyMirror shiftBoardEast

-- Apply an effectful function on a list in reverse.
horizontallyMirror :: Functor f => ([a] -> f [a]) -> [a] -> f [a]
horizontallyMirror f = fmap reverse . f . reverse

-- Apply an effectful function on a matrix on the transpose.
diagonallyMirror :: Functor f => ([[a]] -> f [[a]]) -> [[a]] -> f [[a]]
diagonallyMirror f = fmap transpose . f . transpose

-- | Shift a row West.
shiftRowWest :: Row -> Writer (Sum Int) Row
shiftRowWest row = (++ nothings) <$> collapseNonempty justs
  where
    justs, nothings :: [Cell]
    (justs, nothings) = partition isJust row

    -- Collapse a Row's nonempty cells and tell the score. Maintain
    -- the size of the input list. Examples:
    --      [Just 4, Just 4]                 = tell 8, return [Just 8, Nothing]
    --      [Just 2, Just 2, Just 2, Just 2] = tell 8, return [Just 4, Just 4, Nothing, Nothing]
    collapseNonempty :: [Cell] -> Writer (Sum Int) [Cell]
    collapseNonempty (Just x:Just y:zs) | x == y = do
        tell $ Sum (x+y)
        rest <- collapseNonempty zs
        return $ Just (x+y):rest ++ [Nothing]
    collapseNonempty (x:xs) = (x:) <$> collapseNonempty xs
    collapseNonempty [] = return []

availablePositions :: Board -> [Pos]
availablePositions = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices Nothing)

isLoserBoard :: Board -> Bool
isLoserBoard board = all (board ==) $ [boardW, boardA, boardS, boardD]
  where
    boardW, boardA, boardS, boardD :: Board
    boardW = fst . shiftBoard North $ board
    boardA = fst . shiftBoard West  $ board
    boardS = fst . shiftBoard South $ board
    boardD = fst . shiftBoard East  $ board

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard
