import AI
import Car
import Heap
import Move
import Prelude
import State

rush_hour::String->[Move]
rush_hour x = solve $readState x

printSolution s [] = putStrLn (writeState s)
printSolution s (m:ms) = do {putStrLn (writeState s);
                             printSolution (makeMove s m) ms}
