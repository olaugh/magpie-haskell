# Magpie Haskell

A Scrabble engine in Haskell, ported from [MAGPIE](https://github.com/domino14/magpie).

## Features

- **GADDAG-based move generation** - Fast word lookup using GADDAG data structure
- **Shadow pruning** - Efficient best-move search by pruning anchors that can't beat the current best
- **Leave values (KLV)** - Equity-based move selection using pre-computed leave values
- **Exchange generation** - Generates and evaluates tile exchanges when beneficial
- **Threaded autoplay** - Run many games in parallel for testing and analysis
- **Multiple lexica** - Supports CSW21, CSW24, NWL20, NWL23, TWL06, and more

## Building

Requires GHC 9.x and Cabal.

```bash
cabal build
```

## Usage

```bash
cabal run magpie -- data/lexica/CSW21.kwg
```

### Commands

- `gen <rack>` - Generate all legal moves for a rack
- `play` - Start an interactive game
- `autoplay [n] [-t threads] [-s seed]` - Auto-play n games
- `anagram <rack>` - Find anagrams
- `quit` - Exit

### Example

```
$ cabal run magpie -- data/lexica/CSW21.kwg
magpie> autoplay 100 -t 10
==================================================
Autoplay Results
==================================================
Games:          100
Threads:        10
Time:           2.47 seconds
Games/sec:      40.5

P1 wins:        56 (56.0%)
P2 wins:        43 (43.0%)
Ties:           1 (1.0%)

Avg turns:      23.5
Avg P1 score:   454.1
Avg P2 score:   437.1
Avg P1 bingos:  2.35
Avg P2 bingos:  2.19
Avg P1 exchg:   0.15
Avg P2 exchg:   0.08
```

## Data Files

The `data/lexica/` directory contains:
- `.kwg` - GADDAG word graph files
- `.klv2` - Leave value files (for equity calculation)
- `.txt` - Plain text word lists

## Architecture

```
src/Magpie/
├── Types.hs            # Core types (MachineLetter, Rack, Move, etc.)
├── KWG.hs              # GADDAG/KWG loading and traversal
├── KLV.hs              # Leave value loading and lookup
├── Board.hs            # Board representation and cross-sets
├── MoveGen.hs          # Move generation with shadow pruning
├── Shadow.hs           # Shadow anchor generation for pruning
├── LeaveMap.hs         # O(1) leave value lookup via subset indexing
├── LetterDistribution.hs # Letter distributions for different languages
├── Game.hs             # Game state and move execution
├── Autoplay.hs         # Threaded autoplay for testing
└── Equity.hs           # Fixed-point equity type
```

## Equity Calculation

Moves are evaluated by equity = score + leave_value:
- **Score**: Points from the play
- **Leave value**: Expected future value of remaining tiles (from KLV file)

Equity is stored as fixed-point Int32 with 1000x resolution for efficient computation.

## License

MIT
