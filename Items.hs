
module Items where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Item = Item {
    position :: Position,
    itemType :: ItemType,
    eaten :: Bool

} deriving (Show, Eq)

type Position = (Float, Float)

--initialItems :: [Item]

--spawnFruit :: StdGen -> Position -> (Item, StdGen)

data ItemType = SmallDot | BigDot | Fruit | Cherry | Strawberry | Orange | Apple | Melon deriving (Show, Eq)

points :: ItemType -> Int
points SmallDot = 10
points BigDot = 50
points Cherry = 100
points Strawberry = 300
points Orange = 500
points Apple = 700
points Melon = 1000

eatenItem :: GameState -> Maybe ItemType -> GameState
eatenItem gameState item = 
    case item of
        Just SmallDot -> gameState { score = score gameState + points SmallDot, veld = newveld}
        Just BigDot -> gameState { score = score gameState + points BigDot, player = (player gameState) {hunting = 10}, veld = newveld}
        Just Cherry -> gameState { score = score gameState + points Cherry, veld = newveld}
        Just Strawberry -> gameState { score = score gameState + points Strawberry, veld = newveld}
        Just Orange -> gameState { score = score gameState + points Orange, veld = newveld}
        Just Apple -> gameState { score = score gameState + points Apple, veld = newveld}
        Just Melon -> gameState { score = score gameState + points Melon, veld = newveld}
        Nothing -> gameState
    where 
        newveld = replaceAt "n" x y (veld gameState)
        (x,y) = cell (player gameState)

replaceAt :: String -> Int -> Int -> [[String]] -> [[String]]
replaceAt _ _ _ [] = []
replaceAt s 0 0 ((x:xs):ys) = ((s:xs):ys)
replaceAt s i 0 ((x:xs):ys) = ((x: replaceAt2 s (i-1) xs): ys)
replaceAt s i j ((x:xs):ys) = ((x:xs): replaceAt s i (j-1) ys)

replaceAt2 :: String -> Int -> [String] -> [String]
replaceAt2 _ _ [] = []
replaceAt2 s 0 (x:xs) = (s:xs)
replaceAt2 s i (x:xs) = (x: replaceAt2 s (i-1) xs)