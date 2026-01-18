# Haskell vs C Implementation Analysis

This document analyzes the key differences between this Haskell port and the original MAGPIE C implementation.

## Overview

The Haskell implementation is a faithful port of MAGPIE's algorithms, but uses idiomatic Haskell data structures and patterns. The core algorithms (GADDAG traversal, shadow scoring, cross-set computation) are structurally identical, but the implementation details differ significantly due to language paradigms.

---

## File-by-File Analysis

### Types.hs vs C Types

| Aspect | C (MAGPIE) | Haskell |
|--------|------------|---------|
| Machine Letter | `uint8_t` | `newtype MachineLetter = MachineLetter Word8` |
| Rack | Mutable `uint8_t rack[RACK_SIZE]` | Immutable `VU.Vector Int` with copy-on-write |
| Move tiles | Fixed array `uint8_t tiles[BOARD_DIM]` | List `[MachineLetter]` (heap allocated) |
| Square | Packed struct with bit fields | Record with strict fields |
| Equity | `int32_t` (1000x fixed-point) | `newtype Equity = Equity Int32` |

**Key Differences:**
- **Rack operations**: C mutates in-place; Haskell creates new vectors via `VU.//` operator. This is O(n) in Haskell vs O(1) in C.
- **Move representation**: C uses fixed-size arrays; Haskell uses linked lists which require allocation per tile.
- **Type safety**: Haskell newtypes provide compile-time distinction between `MachineLetter`, `Equity`, etc.

**Performance Impact**: Rack and move representation are significant sources of allocation overhead.

---

### KWG.hs vs kwg.c

| Aspect | C | Haskell |
|--------|---|---------|
| Node storage | Raw `uint32_t*` array | `VU.Vector Word32` (unboxed) |
| Node access | Direct pointer arithmetic | `VU.unsafeIndex` (bounds-checked in debug) |
| Word lookup | Iterative with pointer increment | Recursive with tail-call optimization |

**Key Differences:**
- **Memory layout**: Both use contiguous 32-bit words. Haskell's unboxed vector has identical memory layout to C array.
- **Traversal**: Structurally identical algorithms. Haskell uses pattern matching where C uses bit manipulation.

**Performance Impact**: Minimal. The unboxed vector should perform comparably to C arrays.

---

### Board.hs vs board.c

| Aspect | C | Haskell |
|--------|---|---------|
| Board storage | `Square board[BOARD_DIM][BOARD_DIM]` (2D) | `V.Vector Square` (flat, row-major) |
| Square access | `board[row][col]` | `boardSquares V.! (row * dim + col)` |
| Cross-set computation | In-place mutation | ST monad with freeze |
| Vertical moves | Direction-parameterized accessors | Board transpose |

**Key Differences:**

1. **Flat vs 2D storage**: Haskell uses a flat vector with manual index calculation. This is slightly more work but equivalent performance.

2. **Mutation strategy**: C mutates the board directly. Haskell:
   ```haskell
   computeCrossSets kwg ld dir board = runST $ do
     squares <- V.thaw (boardSquares board)  -- O(n) copy
     -- ... mutations ...
     squares' <- V.freeze squares            -- O(n) copy
     return board { boardSquares = squares' }
   ```

3. **Vertical move generation**:
   - C: Uses direction parameter to swap row/col access throughout
   - Haskell: Creates a transposed board copy, generates moves as if horizontal, then transposes move coordinates back

   The Haskell approach is simpler but creates an extra board copy.

**Performance Impact**: Board transpose creates unnecessary allocation. Direction-parameterized accessors would be more efficient but add code complexity.

---

### LetterDistribution.hs vs letter_distribution.c

| Aspect | C | Haskell |
|--------|---|---------|
| Score storage | `int scores[DIST_SIZE]` | `VU.Vector Int` |
| Score type | `int` (points) | `Int` (points, not Equity) |
| Lookup | Direct array access | `VU.unsafeIndex` |

**Key Differences:**

1. **Score as Equity**: C stores tile scores as Equity (1000x fixed-point) from the start, avoiding conversions during scoring. Haskell stores as plain `Int` and converts when computing final equity.

2. **Impact**: Every tile scored requires `fromIntegral score * 1000` conversion in Haskell. With thousands of moves per game, this adds up.

**Recommendation**: Store `ldScoresEquity :: VU.Vector Equity` alongside `ldScores` to avoid runtime conversions.

---

### Shadow.hs vs shadow.c

| Aspect | C | Haskell |
|--------|---|---------|
| MoveGen state | Mutable struct | Immutable record with updates |
| Unrestricted multipliers | Fixed arrays | Lists |
| State threading | Pointer mutation | Record update syntax |

**Key Differences:**

1. **State management**: C mutates a single `MoveGen` struct throughout. Haskell creates new `MoveGen` records at each step:
   ```haskell
   gen1 = gen0 { mgTilesPlayed = mgTilesPlayed gen0 + 1, ... }
   gen2 = gen1 { mgShadowPerpAdditionalScore = ... }
   ```
   GHC can often optimize these away, but deep record updates can still cause allocations.

2. **Multiplier tracking**: C uses fixed-size arrays sorted in-place. Haskell uses lists with insertion sort:
   ```haskell
   insertSortedMul x (y:ys)
     | x >= y = x : y : ys
     | otherwise = y : insertSortedMul x ys
   ```
   Lists are O(n) insertion vs O(log n) for binary search + O(1) amortized for arrays.

3. **Score computation**: Algorithm is identical - assigns highest-scoring tiles to highest-multiplier positions.

**Performance Impact**: The immutable state threading is the main overhead. Could use `ST` monad internally like `generateBestMove` does.

---

### MoveGen.hs vs movegen.c

| Aspect | C | Haskell |
|--------|---|---------|
| Core algorithm | `recursive_gen` | `recursiveGen` / `recursiveGenAcc` |
| Strip (placed tiles) | Fixed array `strip[BOARD_DIM]` | `IntMap MachineLetter` |
| Move collection | Callback function | List accumulator / ST-based best tracking |
| Best move tracking | Mutable best pointer | `STRef (Move, Equity)` |

**Key Differences:**

1. **Strip data structure**:
   - C: `uint8_t strip[15]` - fixed array, O(1) access/update
   - Haskell: `IntMap MachineLetter` - tree map, O(log n) access/update

   The ST-based version uses `MVU.MVector Word8` which matches C performance.

2. **Algorithm structure**: The `recursiveGen` function is structurally identical to C's `recursive_gen`:
   - Check if current square is occupied (play through) or empty (try letters)
   - For empty squares, iterate over all letters in the GADDAG node
   - For each valid letter, call `goOn` to update state and continue
   - Record moves when word boundaries are valid

3. **Move generation modes**:
   - `generateMovesWithScores`: Returns all moves as a list (allocates per move)
   - `generateBestMove`: Uses ST monad, only tracks best move found

4. **Vertical move handling**:
   - C: Single code path with direction parameter
   - Haskell: Separate `recursiveGenBestST` and `recursiveGenBestSTTransposed` functions

   The Haskell approach duplicates code but avoids runtime direction checks.

5. **Accumulator pattern**: Haskell uses `recursiveGenAcc` with explicit accumulator to avoid O(n^2) list concatenation:
   ```haskell
   recursiveGenAcc ... !acc =
     ... buildMove ... : acc  -- O(1) cons
   ```

**Performance Impact**:
- `generateBestMove` with ST monad should be competitive with C
- `generateMovesWithScores` has significant overhead from list/Move allocations
- IntMap Strip is slower than array but only used in non-ST path

---

### KLV.hs vs klv.c

| Aspect | C | Haskell |
|--------|---|---------|
| Leave value storage | `float[]` converted to fixed-point | `VU.Vector Int32` (pre-converted) |
| Word count memoization | Pre-computed array | ST-computed `VU.Vector Word32` |
| Lookup algorithm | Iterative DAWG traversal | Recursive DAWG traversal |

**Key Differences:**

1. **Value storage**: Both use 1000x fixed-point `Int32`. Haskell pre-converts from float at load time.

2. **Word count computation**: Both use memoization. Haskell computes in ST monad with topological traversal.

3. **Lookup**: Algorithm is identical - traverse KWG accumulating word index.

**Performance Impact**: Minimal difference. KLV lookup is already efficient.

---

## Summary of Performance-Critical Differences

### High Impact
1. **Rack operations**: Copy-on-write VU.Vector vs in-place mutation
2. **Move list allocation**: List `[MachineLetter]` vs fixed array
3. **Board transpose**: Creates full board copy for vertical moves
4. **Score-to-Equity conversion**: Not pre-computed in LetterDistribution

### Medium Impact
5. **Shadow state threading**: Immutable record updates vs mutation
6. **Strip as IntMap**: O(log n) vs O(1) in list-based path
7. **Multiplier lists**: O(n) insertion vs O(1) array append

### Low Impact
8. **KWG traversal**: Unboxed vectors are efficient
9. **KLV lookup**: Pre-computed, efficient
10. **GADDAG algorithm**: Structurally identical

---

## Recommendations for Optimization

### Already Implemented
- ST-based `generateBestMove` with mutable rack and strip
- Best-move-only tracking to avoid Move list allocation
- Shadow pruning with equity comparison

### Should Implement
1. **Store scores as Equity in LetterDistribution**
   - Add `ldScoresEquity :: VU.Vector Equity`
   - Avoid `fromIntegral * 1000` in hot path

2. **Direction-parameterized board access** (instead of transpose)
   - More complex code but avoids board copy
   - Could use type-level direction for zero-cost abstraction

3. **Unboxed Move representation for internal use**
   - Fixed-size array for tiles instead of list
   - Only convert to user-facing `Move` at output

### Consider for Future
4. **Mutable shadow state** (ST-based shadow scoring)
5. **Pre-allocated multiplier arrays** instead of lists
6. **Compact rack representation** (bit-packed counts)
