#!/bin/bash

ocamlbuild ./main.native -use-ocamlfind -tag thread -Is .
./main.native
