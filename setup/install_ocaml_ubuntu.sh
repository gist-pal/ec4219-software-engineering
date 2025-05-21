#!/bin/bash

# check if sudo is installed
if ! command -v sudo &> /dev/null; then
  apt-get update && apt-get upgrade -y
  apt-get install -y --no-install-recommends sudo
fi

sudo rm -rf ~/.opam && sudo rm -rf $(which opam) && sudo rm -rf $(which ocaml)

sudo apt-get install -y --no-install-recommends wget gcc build-essential curl unzip bubblewrap ocaml-findlib vim libgmp-dev m4 pkg-config
yes '' | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init -y
eval $(opam env --switch=default)
opam switch create 5.1.1
eval $(opam env --switch=5.1.1)
opam install -y conf-m4.1 ocamlfind ocamlbuild zarith batteries
eval $(opam env)
