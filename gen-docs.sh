#!/bin/bash

#
# Note: this script is essentially obsolete. Docs are produced and
# uploaded automatically by `dune-release` to gh-pages site associated 
# with our github repo: https://kdvolder.github.io/knights_tour
#
# The docs are also automatically produced by opam on ocaml.org
# here: https://ocaml.org/p/knights_tour
#
# The script is still kept because it is convenient for producing a
# local preview of the docs; useful mainly while working
# on the docs prior to a release.

set -euo pipefail

# Forcing a clean build, this may be overkill... but anyhow :-)
rm -fr _build
dune build

# Build docs and replace old docs with new
dune build @doc
rm -fr docs
cp -R _build/default/_doc/_html docs