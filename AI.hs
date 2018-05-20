module AI (solver, solve, solve_astar,makeMap,constructSolution,heuristic,countObstacles,costAppend,notInPath,astar_helper) where

import Car
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Hmap
import qualified Data.Set as Set
import Heap
import Move
import State

-------------------DUMB SOLVING-------------------------------------
makeMap::State->[State]->[Move]->(Hmap.Map State (State, Move))
makeMap _ [] [] = Hmap.fromList []
makeMap p (x:xs) (y:ys) = Hmap.union (Hmap.fromList [(x, (p,y))] ) (makeMap p xs ys)

solver::[State]->(Set.Set State)->(Hmap.Map State (State, Move))->[Move]
solver unexplored explored path
    | null unexplored = []
    | finalState cs = constructSolution cs path
    | Set.member cs explored = solver (tail unexplored) explored path 
    | otherwise = result 
        where cs = head unexplored
              moves = map fst $successorMoves cs
              successorStates = map (makeMove cs) moves
              new_path = Hmap.union path (makeMap cs successorStates moves) --don't care if a state is replaced with v' 
              result = solver ((tail unexplored) ++ successorStates) (Set.insert cs explored) new_path

constructSolution::State->(Hmap.Map State (State, Move))->[Move]
constructSolution h m 
     | (snd val) == Move.Nil = []
     | otherwise = (snd val):(constructSolution (fst val) (Hmap.delete h m))
        where val = fromJust (Hmap.lookup h m)

solve::State->[Move]
solve s = reverse $solver [s] (Set.fromList []) (Hmap.fromList [(s, (s, Move.Nil))])



-------------------A-STAR-------------------------------------
heuristic:: State->Int
heuristic s = (10*(w - pos) + obs)
    where w = width s
          h1= takeMap s
          c1=fromJust $Hmap.lookup '=' h1
          pos= (column c1)+ (size c1)
          carRow=row c1
          rem = drop (pos) ((stateGrid s)!!carRow)
          obs=countObstacles rem

countObstacles:: String->Int
countObstacles []=0
countObstacles (x:xs)
        |x=='.' = countObstacles xs
        |otherwise = (countObstacles xs) + 1


solve_astar::State->[Move]
solve_astar s = reverse $astar_helper (Heap (heuristic s) (s,0) []) (Set.fromList []) (Hmap.fromList [(s, (s, Move.Nil))] ) 
--pathcost=g
astar_helper::(Heap (State, Int))->(Set.Set State)->(Hmap.Map State (State,Move))->[Move]
astar_helper unexplored explored path
    | Heap.isEmpty unexplored = []
    | finalState cs = constructSolution cs path
    | Set.member cs explored = astar_helper popped explored path
    | otherwise = result 
        where top_el = top unexplored
              f = fst top_el
              cs = fst (snd top_el)
              g = snd (snd top_el)   
              moves = map fst (successorMoves cs)
              popped = Heap.pop unexplored
              successorStates = map (makeMove cs) moves
              h_costs = map heuristic successorStates
              cost_list = map (+(g+1)) h_costs
              new_uxp = Heap.heapFromList cost_list (map (costAppend (g+1)) successorStates) popped 
              --notIn = notInPath successorStates moves path
              --new_path = Hmap.union path (makeMap cs successorStates moves) 
              new_path = notInPath cs successorStates moves path
              result = astar_helper new_uxp (Set.insert cs explored) new_path

costAppend:: Int->State->(State,Int)
costAppend g s=(s,g)

notInPath::State->[State]->[Move]->(Hmap.Map State (State, Move))->(Hmap.Map State (State, Move))
notInPath c [] [] m = m
notInPath c (x:xs) (y:ys) m
    | not (Hmap.member x m) = notInPath c xs ys new_m
    | otherwise = notInPath c xs ys m
        where new_m = Hmap.insert x (c,y) m
    
