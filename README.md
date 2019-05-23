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

The input should be a string of the form "...a\n==.a\n....\n....\n" where \n denotes the change of line. The car we are moving is denoted as '=', the empty spaces are denoted as '.' and the rest of the cars can be any other symbol. 

You can find more info about the game [here](https://en.wikipedia.org/wiki/Rush_Hour_(puzzle))
