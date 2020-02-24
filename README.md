# ocamlcfg

Control Flow Graph (CFG) represenation for OCaml native code compiler.

This library defines the intermediate representation and provides
the following basic functionality:

- build a CFG from OCaml native compiler's intermediate representation
called Linear
- convert CFG back to Linear
- dead block elimination
- print
- marshal
