# Testing Strategy for Slew

This document outlines the testing approach for the Slew TUI application and provides guidance for writing new tests.

## Overview

Slew is a Brick-based TUI for Slurm job management. The codebase has been refactored to improve testability while maintaining minimal changes to existing functionality.

## Testing Architecture

### 1. **Pure Logic Extraction**

Core business logic has been extracted into pure modules that don't depend on IO or UI state:

- **`Logic.JobFiltering`**: Pure functions for searching, filtering, and sorting jobs
  - `filterJobs`: Search jobs by various fields (ID, name, account, state, partition)
  - `sortJobsBy`: Sort jobs by different categories (account, CPU, time, etc.)
  - `sortListByCat`: Comparator functions for job sorting

- **`Logic.EventHandlers`**: Pure functions for event transformation
  - `scontrolCommand`: Maps Command types to slurm command strings
  - `shouldTriggerRefresh`: Determines if commands require queue refresh

### 2. **IO Abstraction via Typeclasses**

The `Types.SlurmAPI` module provides a typeclass abstraction for Slurm operations:

```haskell
class Monad m => SlurmAPI m where
    pollJobs :: m (Either String [Job])
    cancelJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    -- ... other operations
```

Two implementations are provided:
- `RealSlurmAPI`: Calls actual squeue/scontrol commands
- `MockSlurmAPI`: Mock implementation for testing (future use)

### 3. **Test Infrastructure**

Tests are organized by module:
- `test/Logic/JobFilteringSpec.hs`: Tests for job filtering and sorting
- `test/Logic/EventHandlersSpec.hs`: Tests for event handling logic

Tests use:
- **HSpec** for test structure and assertions
- **QuickCheck** for property-based testing
- **hspec-discover** for automatic test discovery

## Running Tests

```bash
# Run all tests
cabal test

# Run specific test suite
cabal test slew-test

# Run with verbose output
cabal test --test-show-details=direct
```

## What Is Tested

### ✅ Currently Tested

1. **Job Filtering Logic**
   - Search by job ID, name, account, state, partition
   - Case-insensitive matching
   - Partial string matching
   - Multiple field searches
   - Edge cases (empty search terms, no matches)

2. **Job Sorting Logic**
   - Sorting by all categories (Account, CPUs, StartTime, EndTime, JobName, UserName, Memory)
   - Correct ordering of Quantity types (Unset < Set < Infinite)
   - Comparator consistency

3. **Event Command Generation**
   - Correct slurm command generation for all operations
   - Command-to-refresh mapping

4. **Property-Based Tests**
   - Idempotence of filtering operations
   - QuickCheck automated test generation

### ⚠️ Not (Yet) Tested

Due to the incremental approach for a side project, these areas are not yet tested but could be added:

1. **UI State Management**: Direct testing of Brick EventM state transformations
2. **Polling Logic**: The actual squeue polling and error handling
3. **TUI Rendering**: Visual components (by design - difficult to test effectively)
4. **File Operations**: The tail file functionality
5. **Integration Tests**: End-to-end testing with real slurm commands

## Writing New Tests

### For Pure Logic

When adding new pure functions, follow this pattern:

```haskell
module MyModule.MyLogicSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import MyModule.MyLogic

spec :: Spec
spec = do
    describe "myFunction" $ do
        it "handles the base case" $ do
            myFunction input `shouldBe` expectedOutput
        
        it "handles edge cases" $ do
            myFunction edgeInput `shouldBe` edgeOutput
    
    describe "Property: myFunction properties" $ do
        it "is idempotent" $
            property $ \x -> myFunction (myFunction x) == myFunction x
```

### For Stateful Logic

For functions that work with app state but have extractable logic:

1. Extract the pure transformation logic into Logic modules
2. Test the pure logic
3. Keep the EventM wiring in UI modules (harder to test, but minimal)

## Design Principles

### Minimal Changes Philosophy

The refactoring follows these principles:

1. **Extract, Don't Rewrite**: Move existing logic to pure functions rather than rewriting
2. **Test What Matters**: Focus on business logic, not framework plumbing
3. **Incremental Progress**: Each phase adds value independently
4. **Side Project Realistic**: Balance testability with time constraints

### What Makes This Approach Practical

- No major architectural changes required
- Tests add confidence without blocking development
- Pure logic is easy to test and maintain
- IO abstraction enables future testing expansion
- Pattern is familiar to Haskell developers

## Future Improvements

If time allows, consider:

1. **Integration Tests**: Test with mock slurm commands using shell scripts
2. **State Machine Testing**: Property-based tests for state transitions
3. **Error Handling**: More thorough testing of error conditions
4. **Performance**: Benchmark tests for large job lists
5. **Mock Implementations**: Expand MockSlurmAPI for full integration testing

## References

This approach is inspired by testing patterns in established Haskell projects:

- **Pandoc**: Extensive pure function testing, IO at the edges
- **Stack**: Typeclass abstractions for system operations
- **Cabal**: Property-based testing for build logic
- **Brick Examples**: Separation of state logic from UI rendering

## Contributing

When contributing:

1. Add tests for any new pure logic functions
2. Extract logic from IO when possible
3. Use property-based tests for laws and invariants
4. Keep tests focused and fast
5. Update this document with new testing patterns

## Test Coverage Goals

Current coverage (rough estimates):
- Logic.JobFiltering: ~90% (most paths covered)
- Logic.EventHandlers: ~80% (command generation covered)
- Overall: Focus on high-value, frequently-used code

The goal is not 100% coverage, but confidence in the most important logic paths.
