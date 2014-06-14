GameNumber
====
GameNumber is simple game with AI players

Controls
----
| Key | Effect
| --- | ---
| Left Click | do action on cell
| Right Click | centering cell
| F2 | save game
| F3 | load game
| p | pause \ unpause
| s | shield action
| + | increase game speed
| - | decrease game speed

Termins
----
Cell _value_ - number in cell

_Strength_ in cell - sum of values of all surrounding owned cells (without the cell)

Rules
----
Game played on board of 64x64 cell.

Number of players - 16 : human player + 15 AI

When game starts, each player owns a cell with a value of 1.

Players get a free points for a time proportional to the sum of the values of its cells.

Free points can be used for:

1. one point actions
  - conquer unowned cells if player has some strength in it
  - increase value of owned cells (up to 9, limited by strength)
  - conquer others cell if player has greather strength
  - charge shield
2. two point actions
  - attack non shielded cell (cell value decreased by 1)
  - attack shielded cell (target players free points decreased by 1, when it lesser that -10, shield stop working)

Shield
----
Shield protects players cell from attacks for cost of free points.

Shield must be charged up to 128 and then activated.

Activated shield can be disabled to accumulate free points and then turned back on without spending free points.
