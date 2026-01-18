# Magpie Haskell Implementation Notes

## Current Status

The core move generation engine is functional and produces correct results:

- **All 10 test suites pass** including comprehensive move generation tests against known good values
- **ST-based best-move-only tracking** implemented in `generateBestMove` for efficient single-best-move queries
- **Shadow pruning** working correctly with proper equity comparison
- **Clean codebase** with zero compiler warnings

### Key Components

| Component | Status | Notes |
|-----------|--------|-------|
| KWG/GADDAG loading | Complete | Binary format compatible with wolges |
| KLV leave values | Complete | Fixed-point arithmetic (1000x resolution) |
| Cross-set computation | Complete | Per-direction cross-sets for valid perpendicular plays |
| Anchor generation | Complete | Shadow-based with upper bound scoring |
| Move generation | Complete | Full recursive_gen algorithm ported from MAGPIE C |
| Best move selection | Complete | ST-based with shadow pruning |
| Autoplay | Complete | Threaded game simulation |

## Areas to Polish Before WMP Implementation

### High Priority

1. **Performance Profiling**
   - Current benchmark shows ~9 games/sec (measured with background load)
   - Need clean measurement without competing processes
   - Profile to identify remaining bottlenecks
   - Target should be significantly higher for practical WMP simulation

### Completed Optimizations

2. **Equity Throughout** ✓
   - Added `ldScoresEq :: VU.Vector Equity` to LetterDistribution
   - Added `ldScoreEquity` accessor for pre-computed 1000x fixed-point scores
   - Avoids repeated `fromIntegral * 1000` conversions in hot path

3. **Vertical Move Generation** ✓
   - Implemented direction-aware board accessors (`getLetterDir`, `getCrossSetDir`, etc.)
   - Unified `recursiveGenBestSTDir` handles both directions without board transpose
   - Eliminates O(n²) board copy for vertical moves

4. **Leave Value Computation** ✓
   - Using `klvGetLeaveValueFromTiles` which computes leave value directly from tiles
   - Avoids allocating intermediate `Rack` for each move considered
   - KLV lookup remains efficient (DAWG traversal)

### Medium Priority

5. **Move Representation**
   - Current `Move` type uses `[MachineLetter]` for tiles (list allocation)
   - Consider unboxed vector or fixed-size array for hot path
   - The `moveTiles` field is only needed for final output, not during search

### Low Priority

6. **Code Organization**
   - `MoveGen.hs` is getting large (~1000 lines)
   - Consider splitting: `MoveGen.hs` (public API), `MoveGen/RecursiveGen.hs` (core algorithm)
   - Keep related code together for now; split if it becomes unwieldy

7. **Documentation**
   - Add haddock comments to exported functions
   - Document the recursive_gen algorithm flow
   - Add examples for common use cases

## WMP Implementation Considerations

When implementing Winning Move Percentage (WMP):

1. **Simulation Engine**
   - Need fast game simulation from any position
   - `generateBestMove` is the right foundation
   - May want to precompute/cache some board state

2. **Monte Carlo Sampling**
   - Random bag draws for opponent's tiles
   - May need fast random tile selection
   - Consider: deterministic seeds for reproducibility

3. **Parallelization**
   - WMP is embarrassingly parallel (independent simulations)
   - Current autoplay threading model can be reused
   - Consider work-stealing for better load balancing

4. **Accuracy vs Speed Tradeoff**
   - More simulations = more accurate WMP
   - Need to balance against computation time
   - May want configurable simulation count

## Testing Notes

Current test coverage:
- `movegen-test`: Core move generation correctness
- `crossset-test`: Cross-set computation
- `shadow-test`: Anchor and shadow infrastructure
- `shadow-movegen-test`: Shadow-pruned best move
- `equity-test`: Equity and KLV functionality
- `klv-values-test`: Leave value lookups

Additional tests needed for WMP:
- Game simulation correctness
- WMP calculation accuracy (compare to reference implementation)
- Performance regression tests
