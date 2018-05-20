module State (State(State), height, width, stateGrid, pinRow, takeMap, readState, writeState, findRow, changeStateList, finalState) where

import Data.List
import qualified Data.Map.Strict as Hmap
import Data.List.Split
import Car

data State = State Int Int [String] Int (Hmap.Map Char Car) deriving (Show)

instance Eq State where
    (State w1 h1 g1 p1 hm1) == (State w2 h2 g2 p2 hm2) = g1==g2

instance Ord State where 
    compare (State _ _ g1 _ _) (State _ _ g2 _ _) = compare g1 g2 

height::State -> Int
height (State h _ _ _ _) = h

width::State -> Int
width (State _ w _ _ _) = w

stateGrid::State -> [String]
stateGrid (State _ _ s _ _) = s

pinRow::State->Int
pinRow (State _ _ _ pin _)= pin

takeMap::State->(Hmap.Map Char Car)
takeMap (State _ _ _ _ car_map ) = car_map

class WriteDots a where
    writeDots::[a]->Char->Int->Maybe Int->[a]

instance WriteDots Char where
    writeDots [] _ _ _ = []
    writeDots (x:xs) id s Nothing
        | s == 0 = (x:xs)
        | x == id = '.':(writeDots xs id (s-1) Nothing)
        | otherwise = x:(writeDots xs id s Nothing)

instance WriteDots String where
    writeDots [] _ _ _ = []
    writeDots (x:xs) id s (Just clm)
        |s == 0 = (x:xs)
        --x!!(clm)==id = (x!!(clm)='.'):(writeDots xs id (s-1) (Just clm))
        |x!!(clm)==id = (writeDots x id 1 Nothing):(writeDots xs id (s-1) (Just clm))
        |otherwise = x:(writeDots xs id s (Just clm))


class WriteChar a where
    writeChar::[a]->Char->Int->Int->Int->Maybe Int->[a]

instance WriteChar Char where
    writeChar [] _ _ _ _ _ = []
    writeChar (x:xs) id s start cur Nothing
        | s == 0 = (x:xs)
        | cur >= start = id:(writeChar xs id (s-1) start (cur+1) Nothing)
        | otherwise = x:(writeChar xs id s start (cur+1) Nothing)


instance WriteChar String where
    writeChar [] _ _ _ _ _ = []
    writeChar (x:xs) id s clm start (Just cur)
        | s == 0 = (x:xs)
        --cur>=start = (x!!(clm)=id):(writeChar xs id (s-1) clm start (Just (cur+1)) )
        |cur >= start = (writeChar x id 1 clm 0 Nothing):(writeChar xs id (s-1) clm start (Just (cur+1)) )
        | otherwise = x:(writeChar xs id s clm start (Just (cur+1)) )

findRow::[String]->Int->Char->Int
findRow (l:ls) currRow symbol
    | (elem symbol l == True) = currRow
    | otherwise = findRow ls (currRow + 1) symbol


readState::String -> State
readState str = State h w new_l pin mapedCars
    where ls = splitOn "\n" str
          h = (length ls) -1
          w = length (head ls)
          new_l = take h ls
          pin=findRow new_l 0 '='
          carsList = createCarsList str
          tracedCars = Data.List.map (traceCar new_l 0) carsList
          correlated = correlateCars carsList tracedCars
          mapedCars = Hmap.fromList correlated


writeState::State -> String
writeState (State _ _ [] _ _) = ""
writeState (State h w (x:xs) pin m) = (x ++ "\n") ++ (writeState (State h w xs pin m))

finalState::State -> Bool
finalState s
    | last (stateGrid(s)!!pinRow(s)) == '=' = True
    | otherwise = False

changeStateList::[String]->Car->Int->[String]
changeStateList (x:xs) car stp
                    |d=='H' = replace (x:xs) (writeChar (writeDots ((x:xs)!!rw) id s Nothing) id s (clm+stp) 0 Nothing) rw 0
                    |d == 'V' = writeChar (writeDots (x:xs) id s (Just clm)) id s clm (rw + stp) (Just 0)
                where id = name car
                      rw = row car
                      clm = column car
                      s = size car
                      d = direction car

replace::[String]->String->Int->Int->[String]
replace (x:xs) rplc row cur
    |cur==row = rplc:xs
    |otherwise = x:(replace xs rplc row (cur+1))
