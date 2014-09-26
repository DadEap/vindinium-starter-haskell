module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import AI

bot :: Bot
--bot = randomBot

myBot :: Bot
myBot state = case (dirFromPath <$> (pathToTilePred board (heroPos hero) othersGold)) of
    Nothing -> return Stay
    Just d  -> return d
    where
        hero  = stateHero state
        board = gameBoard $ stateGame state
        othersGold t = case t of
            MineTile Nothing -> True
            MineTile (Just hid) -> hid /= (heroId hero)
            otherwise      -> False


randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

bot = randomBot

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx


