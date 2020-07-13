# Connect4-Network-Game

Project implementing [Connect Four game](https://en.wikipedia.org/wiki/Connect_Four). Consists of 3 programs:

* Server - runs the game.
* HumanPlayer - allows user to connect to the server and play game via simple console interface.
* ComputerPlayer - AI based on Monte Carlo Tree Search. Connects to the server and plays game automatically.

## Prerequisites

Project uses [stack](https://haskellstack.org).

## Usage

From root directory:

* building - stack build
* run Server - stack exec Server-exe [port]  
* run HumanPlayer - stack exec Client-exe [host] [port]  
* run ComputerPlayer - stack exec Client-MCTS-exe [host] [post]  
