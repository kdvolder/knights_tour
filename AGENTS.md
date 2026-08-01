# Knights Tour Project Instructions

## Dune Test Behavior
`dune test` (or `dune runtest`) produces **no output when all tests pass**. Silence = success. Only on failure does it print diffs/errors.

## OCaml Semicolons Between Toplevel `let` Bindings
**Never put semicolons between toplevel `let` bindings.** A semicolon after a `let` binding tells the parser it's part of an expression sequence, which breaks the toplevel structure. The error manifests as a confusing "Syntax error" at EOF or much later in the file, not at the actual location.

```ocaml
(* GOOD — no semicolons between toplevel lets *)
let x = 5

let foo param = param + 5

(* BAD — semicolons break toplevel structure *)
let x = 5;

let foo param = param + 5;
```

Inside expressions (match arms, `if/then`, function bodies), semicolons are still needed to separate statements. The rule only applies between top-level `let` definitions at the same indentation level.
