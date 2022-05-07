#!/bin/bash
set -euo pipefail
dune build
dune test
dune-release tag -d
dune-release