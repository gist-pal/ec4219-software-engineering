#!/bin/bash

apt-get update && apt-get upgrade
apt-get install -y  --no-install-recommends sudo wget gcc build-essential curl unzip bubblewrap ocaml-findlib vim
yes '' | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init -y
eval $(opam env --switch=default)
opam switch create 5.1.1
eval $(opam env --switch=5.1.1)
opam install -y conf-m4.1 ocamlfind ocamlbuild zarith
eval $(opam env)
