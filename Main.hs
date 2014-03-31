{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Monoid
import System.Console.Haskeline
import System.Random

import Game
import TwentyFortyEight

main :: IO ()
main = do
    game <- newGame 4 (Just 2048)

    -- Human player
    runInputT defaultSettings $ twentyFortyEight game playerHuman

    -- Random AI
    --gen <- newStdGen
    --twentyFortyEight game (playerRandom gen)

    -- Up-left-up-right AI
    --twentyFortyEight game playerUpLeftUpRight

    -- Another up-left-up-right AI
    --twentyFortyEight game playerUpLeftUpRight2

playerHuman :: Player (InputT IO) ()
playerHuman = forever $ do
    getBoard >>= liftIO . printBoard
    lift (getInputChar "wasd: ") >>= \case
        Just 'w' -> void $ moveDirection North
        Just 'a' -> void $ moveDirection West
        Just 's' -> void $ moveDirection South
        Just 'd' -> void $ moveDirection East
        _        -> return ()

-- Play random spaces every half second.
playerRandom :: MonadIO m => StdGen -> Player m ()
playerRandom = evalStateT $ forever $ do
    uniform [North, South, East, West] >>= lift . void . moveDirection
    liftIO $ threadDelay 500000
  where
    uniform :: Monad m => [a] -> StateT StdGen m a
    uniform xs = (xs !!) `liftM` randomIndex xs

    randomIndex :: Monad m => [a] -> StateT StdGen m Int
    randomIndex xs = do
        (n,gen) <- randomR (0, length xs - 1) `liftM` get
        put gen
        return n

-- Pro strat: up-left-up-right until you lose.
playerUpLeftUpRight :: Player IO ()
playerUpLeftUpRight = forever $ do
    b1 <- moveDirection North
    b2 <- moveDirection West
    b3 <- moveDirection North
    b4 <- moveDirection East
    -- Unstuck ourselves when necessary
    when (allFalse [b1,b2,b3,b4]) $
        void $ moveDirection South
  where
    allFalse :: [Bool] -> Bool
    allFalse = not . getAny . mconcat . map Any

-- Another pro strat: up/right until you can't move, then up/left until you
-- can't move, and repeat.
playerUpLeftUpRight2 :: Player IO ()
playerUpLeftUpRight2 = forever $ upLeft
  where
    upLeft :: Player IO ()
    upLeft = forever $ do
        b1 <- moveDirection North
        b2 <- moveDirection West
        unless (b1 || b2) $ do
            unstuck East
            upRight

    upRight :: Player IO ()
    upRight = forever $ do
        b1 <- moveDirection North
        b2 <- moveDirection East
        unless (b1 || b2) $ do
            unstuck West
            upLeft

    -- Try dir. If that doesn't work, go South.
    unstuck :: Direction -> Player IO ()
    unstuck dir = do
        b <- moveDirection dir
        unless b $ void $ moveDirection South
