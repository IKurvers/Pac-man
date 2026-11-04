
module Items where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import system.Random

data Item = Item {
    position :: Position,
    itemType :: ItemType,
    eaten :: Bool

} deriving (Show, Eq)

type Position = (Float, Float)

initialItems :: [Item]

spawnFruit :: StdGen -> Position -> (Item, StdGen)

data ItemType = SmallDot | BigDot | Fruit deriving (Show, Eq)

data FruitType = Cherry | Strawberry | Orange | Apple | Melon deriving (Show, Eq)

points :: ItemType -> Int
points SmallDot = 10
points BigDot = 50

fruitPoints :: FruitType -> Int
fruitPoints Cherry = 100
fruitPoints Strawberry = 300
fruitPoints Orange = 500
fruitPoints Apple = 700
fruitPoints Melon = 1000

-- toevoegen: huidige locatie
eatenItem :: GameState -> Item -> GameState
eatenItem gameState item = case itemType item of
    SmallDot -> gameState { score = score gameState + points SmallDot, veld = newveld}
    BigDot -> gameState { score = score gameState + points BigDot, hunting = 10, veld = newveld}
    Fruit -> gameState { score = score gameState + fruitPoints (fruitType item), veld = newveld}
        where newveld = replaceAt "n" x y veld

replaceAt :: String -> Int -> Int -> Grid -> Grid
replaceAt _ _ _ [] = []
replaceAt s 0 0 ((x:xs):ys) = ((s:xs):ys)
replaceAt s i 0 ((x:xs):ys) = ((x: replaceAt s (i-1) 0 xs): ys)
replaceAt s i j ((x:xs):ys) = ((x:xs): replaceAt s i (j-1) ys)
