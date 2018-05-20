module Car (Car(Car), name, row, column, size, direction, correlateCars, createCarsList, traceCar, changeCar, changePos) where

import Data.List
import Data.Maybe

data Car = Car Char (Int, Int) Int Char deriving (Show)

name::Car->Char
name (Car n _ _ _ ) = n

row::Car->Int
row (Car _ (r,_) _ _) = r

column::Car->Int
column (Car _ (_,c) _ _) = c

size::Car->Int
size (Car _ _ s _) = s

direction::Car->Char
direction (Car _ _ _ o) = o

correlateCars::[Char]->[((Int,Int),(Char,Int))]->[(Char,Car)]
correlateCars [] [] = []
correlateCars (x:xs) (((row,clm),(d,s)):ys) = (x,Car x (row,clm) s d):(correlateCars xs ys)

class CarSize a where
    carSize::Char->[a]->Int->Int

instance CarSize Char where
    carSize car row pos
        | pos >= (length row) = 0
        | row!!(pos) == car = (carSize car row (pos+1)) + 1
        | otherwise = 0

instance CarSize String where
    carSize _ [] _ = 0
    carSize car (x:xs) pos
        | x!!(pos) == car = (carSize car xs pos) + 1
        | otherwise = 0

createCarsList::String->[Char]
createCarsList s1 = Data.List.delete '.' (Data.List.delete '\n' (nub(s1)))

traceCar::[String]->Int->Char->((Int,Int),(Char,Int)) --((row,col),direction,Size of "big" dimension)
traceCar (x:xs) rwNdx car
    | (elem car x == True) = ((rwNdx,clNdx),dirSize car (x:xs) clNdx)
    | otherwise = traceCar xs (rwNdx+1) car
        where clNdx = fromJust $elemIndex car x


dirSize::Char->[String]->Int->(Char,Int)
dirSize car (x:xs) pos
    | (pos /=( (length x)-1) && x!!(pos+1)==car) = ('H',(carSize car x (pos+2))+2) --it expands horizontally
    | otherwise = ('V',(carSize car xs pos)+1)

changePos::Car->Car->Car
changePos c1 c2 = Car (name c1) (row c2, column c2) (size c1) (direction c1)

changeCar::Car->Int->Car
changeCar c stp
    | d == 'H' = Car id (rw, clm + stp) s d
    | otherwise = Car id (rw + stp, clm) s d
        where
            id = name c
            rw = row c
            clm = column c
            d = direction c
            s = size c
