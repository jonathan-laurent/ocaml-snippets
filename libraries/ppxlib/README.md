# Resources for writing PPX extensions

## Learning resources

- http://rgrinberg.com/posts/deriving-slowly/
- https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem
- [Manual](https://ocaml-ppx.github.io/ppxlib/ppxlib/index.html)
- [Parsetree](https://github.com/ocaml/ocaml/blob/trunk/parsing/parsetree.mli)

## Helpful commands

```sh
ocamlfind ppx_tools/dumpast -e "{set = (fun x -> x.field)}"
ocamlfind ppx_tools/dumpast file.ml
ocamlc -dparsetree file.ml
```