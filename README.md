# PrinciplesOfComputerLanguages_Proj3

Implementation of the Rush Hour game in haskell for the Principles of Computer Languages class.  The game uses 2 ways to solve, BFS and A-star. We also implemented the heuristic function used for A-star. 

## Run

For the BFS 

```
    state = readState "...a\n==.a\n....\n....\n"
    printSolution state (solve state)
```

For the A-star

```
    state = readState "...a\n==.a\n....\n....\n"
    printSolution state (solve_astar state)
```
