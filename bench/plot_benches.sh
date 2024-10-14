#!/bin/bash
cd "$(dirname "$0")"
nix-shell -p julia --command "julia ./plot_benches.jl"
