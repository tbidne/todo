set -e

export LANG="C.UTF-8"

cabal bench --benchmark-options \
  '+RTS -T -RTS --csv benchmarks/baseline_bench.csv --svg benchmarks/baseline_bench.svg --baseline benchmarks/baseline_bench.csv'
