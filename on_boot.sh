#!/bin/bash
echo $HOME
source $HOME/.profile
set -euox pipefail
cd $HOME/git/knights_tour
eval $(opam env)
current_time=$(date "+%Y.%m.%d-%H.%M.%S")
mkdir saves/$current_time
cd saves/$current_time
random_hexo_puzzle
solve_file > solutions.txt
