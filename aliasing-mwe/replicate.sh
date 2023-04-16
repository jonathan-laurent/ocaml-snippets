#!/bin/bash

dune clean
dune build
cp $(find . -name "*Asm_alias.s") alias.s
cp $(find . -name "*Asm_normal.s") normal.s
cp $(find . -name "*alias.0.inlining.org") alias.0.inlining.org
dune exec ./profile.exe