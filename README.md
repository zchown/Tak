# TAK

Tak is a game from Patrick Rothfuss's Kingkiller Chronicle.
A more thorough of the game can be found here: [ustak](https://ustak.org/)

## Description
This is really three projects in one. 

#### TakServer
Tak server includes multiple parts. First located in the Lib directory there is a Haskell implementation of Tak. This includes a pure representation of the board and ways to generate legal moves in a position. As well as a statefull way of representing the board and generating legal moves in a position. The stateful way is faster but is not as easy to work with.
The game server located in the app directory is the brains of this Tak project. It communicated with any potential AI players and the front end. It can keep track of multiple games at once and the keeps track of the state of those games updating the players of that game when the state changes.
    TakServer also includes some basic board evaluation and search algorithms for a basic game tree search ai. This is located in the ai directory.

#### TakGraphics
This is the front end for the Tak Game. It uses BabylonJs to create a 3D graphics visualization of the board. It also includes a description of how to play the game and lets users input their moves in [PTN format](https://ustak.org/portable-tak-notation/)

#### LambdaTak
LambdaTak aims to be a stronger AI for the game Tak. It reimplements the game logic in C for a significant speedup. Currently it's strongest version uses an alpha-beta search in a negamax framework. It includes optimizations such as zobrist hashing to do lookups in a transposition table using linear probing in the case of collisions. It also includes a move ordering heuristic making use of killer moves and history heuristics. It also includes a quiescence search to avoid the horizon effect. It also includes a basic evaluation function that uses a combination of piece count, piece count on the board, and piece count in the stack. It makes use of late move reductions to significantly reduce the number of nodes searched. The evaluation function could be stronger but takes into consideration flat count differential, centrality, game stage, stack mobility and threats of taking pieces. The most interesting part of the evaluation is the use of the connectivity index which can quickly look through a board position and make a guess at how close a player is to making a road, which is an algorithm I came up with when doing dfs proved far to slow.

Currently wip is working on neural networks and reinforcement learning to improve the strength of the AI as the search space is just far to large for alpha-beta to be effective. At this point a monte carlo tree search has been implemented but without a policy network it is basically just a random search even though it does use the move ordering heuristics from the alpha-beta search.

### Running
#### TakServer
TakServer is most easily run using [Cabal](https://www.haskell.org/cabal/).
It has the executables Tak for the Game Server, TakAI to run the AI and Tak-test to run the test suite.

#### TakGraphics
Running `npm run dev` should result in being able to go to http://localhost:5173/ to play Tak assuming that the game server is already running.

#### LambdaTak
Uses a cmake build system. 
Requirements:
CUnit for testing
Janson for json parsing
libwebsockets for websockets

