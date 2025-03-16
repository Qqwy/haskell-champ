# Run these commands using `just`
# c.f. https://github.com/casey/just
# And/or use this file as a reference to see common commands

# Run all tests
test:
  cabal test

# Run only the documentation tests
doctest:
  cabal run doctest

# Run only the main test suite
unit_test:
  cabal run tests

# Build the libary and its dependencies
build:
  cabal build

# Start a GHCi shell with Champ loaded
repl:
  cabal repl

# Run benchnmarks
bench:
  cabal bench --benchmark-options="+RTS -T -RTS --csv bench/benches.csv --timeout=5s"

# Plot previously-run benchmarks
plot_benches:
  ./bench/plot_benches.sh

# Generate documentation
docs:
  cabal haddoc
