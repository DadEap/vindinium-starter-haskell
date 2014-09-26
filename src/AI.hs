module AI where

import Data.List
import Data.Maybe

import Vindinium

diffPos :: Pos -> Pos -> Pos
diffPos pa pb = Pos (posX pa - posX pb) (posY pa - posY pb)

isNeighbor :: Pos -> Pos -> Bool
isNeighbor pa pb = abs ((posX pa) - (posX pb)) + abs ((posY pa) - (posY pb)) == 1

neighborDir :: Pos -> Pos -> Dir
neighborDir pa pb = case ( ((posX pb) - (posX pa)), ((posY pb) - (posY pa)) ) of
    (1,  0) -> North
    (-1, 0) -> South
    (0,  1) -> East
    (0, -1) -> West
    (0,  0) -> Stay

inBoard :: Board -> Pos -> Bool
inBoard b p = ((posX p) < (boardSize b)) && ((posY p) < (boardSize b)) && (posX p) >= 0 && (posY p) >= 0

tileAt :: Board -> Pos -> Tile
tileAt b p = (boardTiles b) !! ( (posY p)*(boardSize b) + (posX p) )

testBoard = Board 3 [ FreeTile, FreeTile, FreeTile,
                      WoodTile, FreeTile, WoodTile,
                      FreeTile, FreeTile, FreeTile ]

walkable :: Board -> Pos -> Bool
walkable b p = case (tileAt b p) of
    FreeTile    -> True
    MineTile _  -> True
    TavernTile  -> True
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

