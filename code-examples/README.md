## How to test your code on the OCaml toplevel

1. Install `utop`, the alternative OCaml Toplevel program.

```bash
opam install utop
```

[Here](https://opam.ocaml.org/blog/about-utop/) is the brief explanation about `utop`. You may find `utop` be more convenient than the basic OCaml Toplevel since it has a much improved interface. For example,

* it automatically shows all valid names in the namespace
* and supports auto-completion (Use `Tab` key)

2. On the project root directory (i.e. the directory that has the `dune-project` file of that project, in our case, `ec4219-software-engineering/code-examples`), type

```bash
dune utop
``` 

This command runs `utop`, with loading all your libraries. For more information, type `dune utop --help`.

You can access to each elements of examples in this directory by `Ocaml_example.Ex[1-6].<name>`, `Z3_example.Ex[1-3].<name>` and `SignAnalysis.<name>`.

Example:

```bash
utop > Ocaml_example.Ex2.reverse [1; 2; 3] ;;
- : int list = [3; 2; 1]
```

```bash
utop > Z3_example.Util.check_sat_print Z3_example.Ex2.final ;;
SAT
(define-fun y () Int
  0)
(define-fun x () Int
  7)
- : unit = ()
```