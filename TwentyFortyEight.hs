{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE GADTSyntax      #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module TwentyFortyEight
    ( Player
    , getBoard
    , moveDirection
    , twentyFortyEight
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Free
import Control.Monad.Trans (MonadIO, liftIO)

import Game

data PlayerF :: * -> * where
    GetGame       ::              (Game -> next) -> PlayerF next
    MoveDirection :: Direction -> (Bool -> next) -> PlayerF next
  deriving Functor

type Player m = FreeT PlayerF m

getGame :: Monad m => Player m Game
getGame = liftF $ GetGame id

-- Get the game board.
getBoard :: Monad m => Player m Board
getBoard = view gameBoard <$> getGame

-- Move in the given direction. Returns True if the board changed, False otherwise.
moveDirection :: Monad m => Direction -> Player m Bool
moveDirection dir = liftF (MoveDirection dir id)

twentyFortyEight :: MonadIO m => Game -> Player m () -> m ()
twentyFortyEight game player = runFreeT player >>= \case
    Pure _                        -> return ()
    Free (GetGame next)           -> twentyFortyEight game (next game)
    Free (MoveDirection dir next) ->
        if | isWon game'          -> liftIO $ putStrLn $ "You win! Score: " ++ show (game^.gameScore)
           | isLost game'         -> liftIO $ putStrLn $ "You lose. Score: " ++ show (game^.gameScore)
           | oldBoard == newBoard -> twentyFortyEight game' (next False)
           | otherwise            -> liftIO (addNewNumber game') >>= \game'' -> twentyFortyEight game'' (next True)
      where
        game' :: Game
        game' = makeMove dir game

        oldBoard, newBoard :: Board
        oldBoard = game^.gameBoard
        newBoard = game'^.gameBoard
