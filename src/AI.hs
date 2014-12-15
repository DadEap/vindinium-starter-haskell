module AI where

import Data.List
import Data.Maybe

import Control.Monad.State

import Vindinium

diffPos :: Pos -> Pos -> Pos
diffPos pa pb = Pos (posX pa - posX pb) (posY pa - posY pb)

isNeighbor :: Pos -> Pos -> Bool
isNeighbor pa pb = abs ((posX pa) - (posX pb)) + abs ((posY pa) - (posY pb)) == 1

isCloseNeighbor :: Pos -> Pos -> Bool
isCloseNeighbor pa pb = abs ((posX pa) - (posX pb)) + abs ((posY pa) - (posY pb)) <= 3

neighborDir :: Pos -> Pos -> Dir
neighborDir pa pb = case ( ((posX pb) - (posX pa)), ((posY pb) - (posY pa)) ) of
    (-1,  0) -> North
    (1,   0) -> South
    (0,  1) -> East
    (0, -1) -> West
    (0,  0) -> Stay
---neighborDir pa pb = case ( ((posX pb) - (posX pa)), ((posY pb) - (posY pa)) ) of
--    (-1,  0) -> North
--    (1,   0) -> South
--    (0,  1) -> East
--    (0, -1) -> West
--    (0,  0) -> Stay


inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) = (x < s) && (y < s) && (x >= 0) && (y >= 0)
                    where s = boardSize b

tileAt :: Board -> Pos -> Tile
tileAt b (Pos x y) = (boardTiles b) !! ( x*(boardSize b) + y)

testBoard = Board 3 [ FreeTile, FreeTile, FreeTile,
                      WoodTile, FreeTile, WoodTile,
                      FreeTile, FreeTile, FreeTile ]

walkable :: Board -> Pos -> Bool
walkable b p = case (tileAt b p) of
    FreeTile    -> True
    otherwise   -> False

getNeighbors :: Board -> Pos -> [Pos]
getNeighbors b p = filter (inBoard b) (map (\(x,y) -> Pos (x + (posX p)) (y + (posY p)) ) [ (1, 0), (-1, 0), (0, 1), (0, -1) ] )

pathRecurse :: Board -> [Pos] -> Pos -> (Board -> Pos -> Bool) -> Int -> Maybe [Pos]
pathRecurse b visited pa tilePred maxLen
    | tilePred b pa == True   = Just (visited++[pa])
    | null neighbors          = Nothing
    | null subpaths           = Nothing
    | (length visited) > maxLen = Nothing
    | otherwise               = Just ( minimumBy (comparePath) subpaths )
        where subpaths        = catMaybes ( map (\n -> pathRecurse b (visited++[pa]) n tilePred maxLen) neighbors)
              neighbors       = filter (\x -> (walkable b x) && not (x `elem` visited)) (getNeighbors b pa)
              comparePath x y = compare (length x) (length y)

pathToPos :: Board -> Pos -> Pos -> Maybe [Pos]
pathToPos b pa pb = pathRecurse b [] pa (\bin p -> (p == pb)) 10

pathToTile :: Board -> Pos -> Tile -> Maybe [Pos]
pathToTile b pa tile = pathRecurse b [] pa (\bin p -> (tileAt bin p == tile)) 10

pathToTilePred :: Board -> Pos -> (Tile -> Bool) -> Maybe [Pos]
pathToTilePred b pa tilePred = pathRecurse b [] pa (\bin p -> (tilePred (tileAt bin p))) 3

dirsFromPath :: [Pos] -> [Dir]
dirsFromPath (x:[]) = []
dirsFromPath a@(_:xs) = map (uncurry neighborDir) (zip a xs)

dirFromPath :: [Pos] -> Dir
dirFromPath []      = Stay
dirFromPath (x:[])  = Stay
dirFromPath path    = head $ dirsFromPath path

type TilePred = Board -> Pos -> Bool

data PathState = PathState {
    pathStateVisited :: [Pos],
    pathStateQueue   :: [ [Pos] ]
}

pathRecurseFast :: Board -> Pos -> TilePred -> Control.Monad.State.State PathState (Maybe [Pos])
pathRecurseFast b pa tilePred = do
    pathState <- get
    case (pathStateQueue pathState) of
        [] -> return Nothing
        otherwise -> processQueue b pa tilePred pathState

skipRest :: Board -> Pos -> TilePred -> PathState -> Pos -> Control.Monad.State.State PathState (Maybe [Pos])
skipRest b pa tilePred pathState currHead = do {
    put ( PathState (currHead:visited) (tail $ pathStateQueue pathState ) );
    (pathRecurseFast b pa tilePred)
} where visited   = pathStateVisited pathState

processQueue :: Board -> Pos -> TilePred -> PathState -> Control.Monad.State.State PathState (Maybe [Pos])
processQueue b pa tilePred pathState
    | tilePred b currHead       = return (Just (reverse currPath))
    | not (walkable b currHead) && currHead /= pa = skipRest b pa tilePred pathState currHead
    | null neighbors            = skipRest b pa tilePred pathState currHead
    | currHead `elem` visited   = skipRest b pa tilePred pathState currHead
    | otherwise = do {
        put $ PathState (currHead:visited) ( (tail (pathStateQueue pathState)) ++ [ (n:currPath) | n <- neighbors ] );
        pathRecurseFast b pa tilePred
    }
    where visited   = pathStateVisited pathState
          currPath  = head (pathStateQueue pathState)
          currHead  = head currPath
          neighbors = filter (\x -> not (x `elem` visited)) (getNeighbors b currHead)

pathRecurseFastIt :: Board -> Pos -> TilePred -> Maybe [Pos]
pathRecurseFastIt b pa tilePred = evalState (pathRecurseFast b pa tilePred) (PathState [] [[pa]])

posPred p = (\b pa -> pa == p)

testBoard2 = Board {boardSize = 10, boardTiles = [WoodTile,WoodTile,HeroTile (HeroId 1),FreeTile,FreeTile,FreeTile,FreeTile,HeroTile (HeroId 4),WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,MineTile Nothing,FreeTile,FreeTile,MineTile (Just (HeroId 4)),WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,TavernTile,FreeTile,FreeTile,FreeTile,FreeTile,TavernTile,WoodTile,WoodTile,WoodTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,WoodTile,WoodTile,WoodTile,WoodTile,FreeTile,WoodTile,WoodTile,FreeTile,WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,FreeTile,WoodTile,WoodTile,FreeTile,WoodTile,WoodTile,WoodTile,WoodTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,WoodTile,WoodTile,WoodTile,TavernTile,FreeTile,FreeTile,FreeTile,HeroTile (HeroId 2),TavernTile,WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,MineTile Nothing,FreeTile,FreeTile,MineTile (Just (HeroId 3)),WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,FreeTile,FreeTile,FreeTile,FreeTile,FreeTile,HeroTile (HeroId 3),WoodTile,WoodTile]}

