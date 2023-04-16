#!/bin/bash

dune clean
dune build
cp $(find . -name "*Asm_alias.s") alias.s
cp $(find . -name "*Asm_normal.s") normal.s
dune exec ./profile.exe