# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Magpie is a Scrabble engine in Haskell, ported from the C implementation at github.com/domino14/magpie. It performs move generation using GADDAG traversal, scoring, and multi-threaded game simulation.

## Build Commands

```bash
cabal build                    # Build library and executables
cabal test                     # Run all test suites
cabal test movegen-test        # Run specific test suite
cabal run magpie -- data/lexica/CSW24.kwg data/letterdistributions/english.csv
```

## REPL Commands

After starting the executable:
- `gen <rack>` - Generate top 20 moves for rack on empty board (e.g., `gen SATINE?`)
- `play` - Interactive single game
- `autoplay [n] [-t threads] [-s seed]` - Multi-game simulation
- `anagram <rack>` - Find all words from rack tiles
- `quit` - Exit

Rack notation: uppercase letters A-Z, `?` for blank tiles.

## Architecture

### Core Data Types (src/Magpie/Types.hs)
- **MachineLetter**: 8-bit tile (0=blank, 1-26=A-Z, high bit 0x80 marks played blank)
- **Rack**: Unboxed vector of tile counts for O(1) lookup
- **Board**: 2D vector of Squares with cross-sets (bitmasks of valid perpendicular tiles)
- **Move**: Position, direction, tiles, score, and equity

### Key Modules
- **MoveGen.hs**: GADDAG-based move generation with cross-set validation
- **Board.hs**: Board representation, cross-set computation, CGP format parsing
- **KWG.hs**: Kurnia Word Graph format - 32-bit nodes encoding DAWG/GADDAG
- **KLV.hs**: Leave value lookup for move equity calculation
- **Game.hs**: Game state, tile bag, turn management
- **Autoplay.hs**: STM-based concurrent game simulation
- **Shadow.hs**: Shadow algorithm for computing score upper bounds (WIP - needs extension sets)

### Binary Formats
- **KWG**: Dictionary format with 32-bit nodes (bits 0-21: arc index, bit 22: last sibling, bit 23: accepts word, bits 24-31: tile)
- **KLV**: KWG header followed by leave values
- **CGP**: Compact game position text format

## Data Files

- `data/lexica/*.kwg` - Dictionary files (CSW24, NWL20, etc.)
- `data/lexica/*.klv2` - Leave value files
- `data/letterdistributions/*.csv` - Tile counts and point values per language
- `data/layouts/*.txt` - Board bonus square layouts

## Performance Notes

- Uses unboxed vectors and strict evaluation throughout
- Cross-sets encoded as 64-bit bitmasks for O(1) validation
- Empty board optimization skips redundant vertical move generation
- Threaded RTS enabled with `-N` for autoplay parallelism
