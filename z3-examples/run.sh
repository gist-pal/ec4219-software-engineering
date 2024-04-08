#!/bin/bash

rm -rf "$1".native
ocamlbuild ./"$1".native -use-ocamlfind -tag thread -Is .
./"$1".native
