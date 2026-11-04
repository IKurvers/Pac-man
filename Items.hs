
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

eatenItem :: GameState -> Item -> GameState
eatenItem gameState item = case itemType item of
    SmallDot -> gameState { score = score gameState + points SmallDot}
    BigDot -> gameState { score = score gameState + points BigDot, hunting = 10 }
    Fruit -> gameState { score = score gameState + fruitPoints (fruitType item) }

{-
delSmallDot :: Grid -> Grid
delSmallDot [] = []
delSmallDot 
-}