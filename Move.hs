module Move (Move(Move, Nil), successorMoves, makeMove) where

import Data.Maybe
import qualified Data.Map.Strict as Hmap
import State
import Car

data Move = Nil | Move Char Int deriving (Show)

instance Eq Move where
    Nil == Nil = True
    _ == _ = False

takeCarFromMove::Move->Char
takeCarFromMove (Move c _) = c

takeSteps::Move->Int
takeSteps (Move _ s) = s


checkMoveCar::State->[Car]->[(Move, Int)]
checkMoveCar _ [] = []
checkMoveCar s (x:xs)
                | (d == 'V') = (checkMoveVertical s x)++(checkMoveCar s xs)
                |otherwise = (checkMoveHorizontal s x)++(checkMoveCar s xs)
                    where d = direction x


checkMoveVertical:: State->Car->[(Move,Int)]
checkMoveVertical s c = (checkMoveUp grid id (rw-1) clm 0)++(checkMoveDown chopped id clm 0 h)
                    where
                        grid = stateGrid s
                        h = height s
                        id = name c
                        rw = row c
                        clm = column c
                        sc = size c
                        chopped = reverse (take (h -(rw + sc)) (reverse grid))

checkMoveUp:: [String]->Char->Int->Int->Int->[(Move,Int)]
checkMoveUp x id cur clm stps
                 | cur < 0 = []
                 | (x!!cur)!!clm == '.' = ((Move id (stps-1)), 1):(checkMoveUp x id (cur-1) clm (stps-1))
                 | otherwise = []

checkMoveDown:: [String]->Char->Int->Int->Int->[(Move,Int)]
checkMoveDown [] _ _ _  _ = []
checkMoveDown (x:xs) id clm stps h
                 | (x!!clm) == '.' = ((Move id (stps +1), 1)):(checkMoveDown xs id  clm (stps+1) h)
                 | otherwise = []


checkMoveHorizontal::State->Car->[(Move, Int)]
checkMoveHorizontal s c = (checkMoveLeft (grid!!rw) id (clm-1) 0 )++(checkMoveRight (grid!!rw) id (clm+sc) 1)
                where grid = stateGrid s
                      w = width s
                      id = name c
                      rw = row c
                      clm = column c
                      sc = size c

checkMoveLeft::String->Char->Int->Int->[(Move, Int)]
checkMoveLeft x id cur stps
                | cur < 0 = []
                | x!!cur == '.' = ((Move id (stps-1)), 1):(checkMoveLeft x id (cur-1) (stps-1))
                | otherwise = []

checkMoveRight::String->Char->Int->Int->[(Move, Int)]
checkMoveRight x id cur stps
                | cur >= (length x) = []
                | x!!cur == '.' = (Move id stps, 1):(checkMoveRight x id (cur+1) (stps+1))
                | otherwise = []



makeMove:: State->Move->State
makeMove s m = State h w new_l pin new_map
            where steps = takeSteps m
                  id = takeCarFromMove m
                  h = height s
                  w = width s
                  ls = stateGrid s
                  pin = pinRow s
                  cars = takeMap s
                  car = fromJust $Hmap.lookup id cars
                  new_car = changeCar car steps
                  new_map = Hmap.insert id new_car cars
                  new_l = changeStateList ls car steps

successorMoves :: State -> [(Move, Int)]
successorMoves s = checkMoveCar s l
            where l = map snd (Hmap.toList (takeMap s))
