#!/bin/bash
set -euo pipefail
dune build @doc
rm -fr docs
cp -R _build/default/_doc/_html docs