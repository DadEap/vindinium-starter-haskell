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
    liftIO $ putStrLn $ show (stateViewUrl state)
    liftIO $ putStrLn $ show answer
    liftIO $ putStrLn $ show (heroPos hero)
    liftIO $ putStrLn $ show (tileAt board (heroPos hero) )
    return answer
    where
        answer = (case (dirFromPath <$> (pathRecurseFastIt board (heroPos hero) getTarget)) of
            Nothing -> Stay
            Just d -> d)
        hero  = stateHero state
        board = gameBoard ( stateGame state )
        turn  = gameTurn (stateGame state)
        heroes = gameHeroes ( stateGame state )
        hero2 = heroes !! 1
        hero3 = heroes !! 2
        hero4 = heroes !! 3
        hurtedHero = mostHurtedHero $ tail $ gameHeroes ( stateGame state )
        justHurtedHero = fromJust hurtedHero
        lifeThreshold = 40
        {-
        getTarget = if (heroLife hero)  <= 50 && (heroGold hero >= 2) 
            then getHealth
            else othersGold
        getHealth sb sp = case (tileAt sb sp) of
            TavernTile -> True
            otherwise  -> False
        othersGold sb sp = case (tileAt sb sp) of
            MineTile Nothing -> True
            MineTile (Just hid) -> hid /= (heroId hero)
            otherwise      -> False
        -}
        getTarget = if (heroLife hero)  <= 50 && (heroGold hero >= 2) 
            then getHealth 
            else if ( isHeroHurted hero hero2 lifeThreshold)
                then killHero2 
            else if ( isHeroHurted hero hero3 lifeThreshold)
                then killHero3
            else if ( isHeroHurted hero hero4 lifeThreshold)
                then killHero4
                else othersGold
        killHero2 sb sp = case (tileAt sb sp) of
            HeroTile (HeroId 2) -> True
            otherwise -> False
        killHero3 sb sp = case (tileAt sb sp) of
            HeroTile (HeroId 3) -> True
            otherwise -> False
        killHero4 sb sp = case (tileAt sb sp) of
            HeroTile (HeroId 4) -> True
            otherwise -> False
        getHealth sb sp = case (tileAt sb sp) of
            TavernTile -> True
            otherwise  -> False
        othersGold sb sp = case (tileAt sb sp) of
            MineTile Nothing -> True
            MineTile (Just hid) -> hid /= (heroId hero)
            otherwise      -> False

isHeroHurted :: Hero -> Hero -> Integer -> Bool
isHeroHurted h e v = ((enemyLife <= v && isCloseNeighbor currentPos enemyPos) || (enemyLife < currentLife && isNeighbor currentPos enemyPos))
    where
        enemyLife = heroLife e
        currentLife = heroLife h
        enemyPos = heroPos e
        currentPos = heroPos h


mostHurtedHero :: [Hero] -> Maybe HeroId
mostHurtedHero [] = Nothing
mostHurtedHero (h:hs)
    | heroLife h <= 60 = Just (heroId h)
    | otherwise = maxH
    where maxH = mostHurtedHero hs            

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

bot = myBot

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx


