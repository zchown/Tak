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
        Currently very work in progress. But aims to be a better AI to play Tak. A long term goal will be setting it up to play online using the playtak api.

### Installing
    #### TakServer
        TakServer is most easily run using [Cabal](https://www.haskell.org/cabal/).
        It has the executables Tak for the Game Server, TakAI to run the AI and Tak-test to run the test suite.

    #### TakGraphics
        Running `npm run dev` should result in being able to go to http://localhost:5173/ to play Tak assuming that the game server is already running.

    #### LambdaTak
        Uses a cmake build system. And requires Cunit to run its tests.

