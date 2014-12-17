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
bot = myBot
--bot = randomBot

myBot :: Bot
myBot state = do
    liftIO $ putStrLn $ show (stateViewUrl state)
    liftIO $ putStrLn $ show answer
    liftIO $ putStrLn $ show (heroPos hero)
    liftIO $ putStrLn $ "Turn : " ++ show turn
    liftIO $ putStrLn $ "Heroes : " ++ show (map heroName heroes)
    liftIO $ putStrLn $ "Gold   : " ++ show (map heroGold heroes)
    liftIO $ putStrLn $ "Mines : " ++ show (map heroMineCount heroes)
    liftIO $ putStrLn $ "Mine Percent   : " ++ show ((fromIntegral $ heroMineCount hero)*100 / (fromIntegral totalMineCount)) ++ "%"
    return answer
    where
        answer = (case (dirFromPath <$> (pathRecurseFastIt board (heroPos hero) getTarget)) of
            Nothing -> Stay
            Just d -> d)
        hero  = stateHero state
        board = gameBoard ( stateGame state )
        turn  = gameTurn (stateGame state)
        heroes = gameHeroes $ stateGame state
        enemyHeroes = filter (\h -> hero /= h) $ gameHeroes ( stateGame state )
        hero2 = enemyHeroes !! 0
        hero3 = enemyHeroes !! 1
        hero4 = enemyHeroes !! 2
        lifeThreshold = 50
        isDrinking = isCloseTavern board (heroPos hero)
        totalMineCount = getTotalMineCount board heroes
        got50PercentMines = (fromIntegral $ heroMineCount hero) / (fromIntegral totalMineCount) > ((fromIntegral 45) / (fromIntegral 100))

        getTarget = if (got50PercentMines && (heroLife hero < 60))
            then getHealth
            else if (got50PercentMines && (heroLife hero > 60) && isDrinking)
                then getStay
            else if ( isHeroHurted hero hero2 lifeThreshold)
                then if (heroId hero2 == HeroId 1)
                    then killHero1
                else if (heroId hero2 == HeroId 2)
                    then killHero2
                else if (heroId hero2 == HeroId 3)
                    then killHero3
                else killHero4
            else if ( isHeroHurted hero hero3 lifeThreshold)
                then if (heroId hero3 == HeroId 1)
                    then killHero1
                else if (heroId hero3 == HeroId 2)
                    then killHero2
                else if (heroId hero3 == HeroId 3)
                    then killHero3
                else killHero4
            else if ( isHeroHurted hero hero4 lifeThreshold)
                then if (heroId hero4 == HeroId 1)
                    then killHero1
                else if (heroId hero4 == HeroId 2)
                    then killHero2
                else if (heroId hero4 == HeroId 3)
                    then killHero3
                else killHero4
            else if (heroLife hero <= 50)
                then getHealth
            else if (heroLife hero < 70 && isDrinking)
                then getHealth
            else othersGold
        getStay sb sp = case (tileAt sb sp) of
            otherwise -> False
        killHero1 sb sp = case (tileAt sb sp) of
            HeroTile (HeroId 1) -> True
            otherwise -> False
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
            otherwise  -> False


isHeroHurted :: Hero -> Hero -> Integer -> Bool
isHeroHurted h e v = ((enemyLife + 20 <= currentLife && isCloseNeighbor currentPos enemyPos) || (enemyLife < currentLife && isNeighbor currentPos enemyPos))
    where
        enemyLife = heroLife e
        currentLife = heroLife h
        enemyPos = heroPos e
        currentPos = heroPos h

mostHurtedHero :: [Hero] -> Hero
mostHurtedHero heroes = foldl1 (\h1 h2 -> if heroLife h1 > heroLife h2 then h2 else h1) heroes

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx


