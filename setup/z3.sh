#!/bin/bash

wget https://github.com/Z3Prover/z3/archive/refs/tags/z3-4.13.0.tar.gz
tar -xvzf z3-4.13.0.tar.gz
cd z3-z3-4.13.0

# OCaml binding
python3 scripts/mk_make.py --ml
cd build; make -j 4
sudo make install

# Python binding
cd ..; mkdir $home/mylib
python3 scripts/mk_make.py --python --prefix=$home --pypkgdir=$home/mylib
export PYTHONPATH=$home/mylib:$PYTHONPATH
cd build; make -j 4
sudo make install
