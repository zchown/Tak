# TAK

## Overview
Tak is a two-player abstract strategy game inspired by Patrick Rothfuss's Kingkiller Chronicle series. This project offers a complete digital implementation of the game, including server architecture, graphical interface, and AI opponents.

For comprehensive rules and strategy guides, visit the official US Tak Association: [ustak.org](https://ustak.org/)

A full project write up can be found here https://medium.com/@zanchown/writing-an-ai-player-for-the-game-tak-4ab5c7d17b68

## Project Components

This repository contains three integrated projects:

### 1. TakServer

At the heart of this implementation is a Haskell-based game server with several components:

- **Game Logic Library** (in `/Lib`): Contains both pure and stateful implementations of Tak's rules and mechanics:
  - The pure representation offers excellent composability and reasoning properties
  - The stateful implementation delivers superior performance for time-critical operations

- **Game Server** (in `/app`): Functions as the central coordinator, managing:
  - Multiple concurrent game sessions
  - Communication between AI and human players
  - Real-time game state synchronization
  - Event broadcasting to connected clients

- **Basic AI** (in `/ai`): Includes foundation-level board evaluation and game tree search algorithms.

### 2. TakGraphics

The frontend interface built with BabylonJS provides:

- 3D visualization of the game board and pieces
- Simple tutorial on game rules and strategy
- Support for move input using the standard [Portable Tak Notation (PTN)](https://ustak.org/portable-tak-notation/)

### 3. LambdaTak

An advanced AI implementation focused on competitive play:

- **C-based Game Implementation**: Reimplements core game logic for significant performance improvements
  - Makes use of bitboards for efficient move generation
  - Utilizes precomputed tables for fast move validation
    - Found in `/lib/sorcery.c` and `/lib/magic.h`
  - Currently packs drops into a u16 for efficient storage and manipulation
    - Future plans to pack the whole move into a u16 for further optimization
  - Uses flood-fill algorithm for road detection
  - Has a good test suite in CUnit
  - Has a perft function for debugging and performance testing

- **C-based Engine**: 
- **Search Optimizations**:
  - Alpha-beta pruning within a negamax framework
  - Transposition tables with Zobrist hashing and linear probing
  - Sophisticated move ordering using killer moves and history heuristics
  - Late move reductions to efficiently prune the search space

- **Evaluation Function**: Considers multiple strategic factors:
  - Piece count differentials (on board and in stacks)
  - Positional elements like centrality and stack mobility
  - Game phase adaptation
  - Threat assessment
  - Connectivity index (a novel algorithm for efficiently estimating road-building potential)
    - This feature is useful as a dfs-based approach is computationally expensive
    - The connectivity index is a heuristic that estimates the number of moves required to build a road
    - Player is incentivized to build pieces that connect across the board
    - Also insentivizes the player to block the opponent with standing stones

- **Neural Network Integration**:
  - NeuralNeworks.c is a handmade neural network implementation
    - It used the Accelerate framework to speed things up but has been replaced because it was still to slow and only supported basic dense networks
  - The new implementation uses TensorFlow for training and BNNS through the Accelerate framework for inference
    - The training loop lives in neuralNetTrainer.c there are some artifacts from the old implementation left in
    - pythonTrainer.c then uses sockets to pass data back and forth between the trainer and a python process that is running the training of the model
    - The model is compiled to a BNNS model so that it can be used in the monteCarloGraphSearch

- **Monte Carlo Graph Search:**
  - A Monte Carlo Graph Search is included in monteCarloGraphSearch.c
    - mostly based on the alphaZero paper [https://arxiv.org/pdf/2012.11045]
    - It uses a BNNS neural network to do inference and get priors for the search
    - It has a training mode
        - In training mode moves are chosen stochastically based on visits
        - The visits are then used to update the policy network
        - Uniform noise is added at every step to the networks priors to encourage exploration
  
## Setup and Running

### TakServer
1. Ensure you have [Cabal](https://www.haskell.org/cabal/) installed
2. Available executables:
   - `Tak`: Launches the main game server
   - `TakAI`: Runs the AI component
   - `Tak-test`: Executes the test suite

### TakGraphics
1. Navigate to the TakGraphics directory
2. Run `npm run dev`
3. Access the game interface at http://localhost:5173/
4. Note: This requires the game server to be running
5. Also ensure that your browser supports WebGL

### LambdaTak
Uses CMake build system with the following dependencies:
- CUnit (for testing)
- Janson (for JSON parsing)
- libwebsockets (for WebSocket communication)
- Accelerate (for neural network)

1. Navigate to the LambdaTak directory
2. To build project in debug mode:
   `cmake -B build-debug -DCMAKE_BUILD_TYPE=Debug
   cmake --build build-debug`
3. To build project in release mode:
    `cmake -B build-release -DCMAKE_BUILD_TYPE=Release
    cmake --build build-release`
4. To run the project:
    `./build-debug/LambdaTak` 
    or
    `./build-release/LambdaTak`
5. To run the tests:
    `./build-debug/LambdaTak-test`
6. To run the neural network trainer:
    `./build-release/trainTak`
7. To run training:
    First run the python script `pythonTrainer.py` in the `LambdaTak` directory
    Then run the trainer with `./build-release/trainTak`
