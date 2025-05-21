#!/bin/bash

sudo rm -rf ~/.opam
sudo rm -rf $(which opam)
sudo rm -rf $(which ocaml)

sudo yes '' | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init -y
eval $(opam env --switch=default)
opam switch create 5.1.1
eval $(opam env --switch=5.1.1)
brew reinstall gmp pkg-config
opam install -y conf-m4.1 ocamlfind ocamlbuild zarith batteries
eval $(opam env)
