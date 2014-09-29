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
myBot state = do
    liftIO $ putStrLn $ show answer
    liftIO $ putStrLn $ show (heroPos hero)
    liftIO $ putStrLn $ show (tileAt board (heroPos hero) )
    return answer
    where
        answer = (case (dirFromPath <$> (pathRecurseFastIt board (heroPos hero) othersGold)) of
            Nothing -> Stay
            Just d -> d)
        hero  = stateHero state
        board = gameBoard ( stateGame state )
        turn  = gameTurn (stateGame state)
        othersGold sb sp = case (tileAt sb sp) of
            MineTile Nothing -> True
            MineTile (Just hid) -> hid /= (heroId hero)
            otherwise      -> False


randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

bot = myBot

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx


