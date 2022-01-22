# Taktikus

## Game Rules
- Played by 2 players (white and black, respectively)
- On a 8x8 chess-like board (with no colors)
- Each player starts with 8 pieces in their first row
- Each turn a player moves one piece (let white start)
- The pieces have a rook-like movement (diagonal or horizontal as far as the player want), but cannot jump over a piece (either color)
- The objective is to capture the opponent's pieces
- Capturing occurs in 2 ways:
    1. by capturing one or more of the opponents marbles (in an unbroken row or column) between two of one’s own marbles. 
    2. moving a marble into an empty space in-between two of the opponents marbles, which are both captured.
- Game ends:
    When one player is left with 1 or less pieces on the board

[See More Details Here](https://boardgamegeek.com/boardgame/80811/taktikus)


## Installation And Execution

### Requisites
- SICStus Prolog

### Execution
1. Direct into the `/src` folder;
2. Open SICStus Prolog command line interface (run `sicstus`);
3. Run `compile('game.pl').` to compile the game application;
4. To start the game, run `play.`


## How to play

We endup choosing a very straight foward and simple way to play, once it's your turn all the player need to type in is the current position of a pawn and the desired new position.

### Movement
- Each turn a player is asked to input their movement, the input should be similar to the standard method for recording and describing the *moves in a game of chess for a pawn* (e.g. to move a piece on position B1 to B3, input `b1b3.`, everything together without spaces)
