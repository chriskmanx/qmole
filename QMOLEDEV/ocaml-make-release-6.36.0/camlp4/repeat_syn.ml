(*pp camlp4orf *)

open Camlp4.PreCast.Syntax

EXTEND Gram
  expr:
    [[ "repeat"; e1 = expr; "until"; e2 = expr; "done" ->
      <:expr< do { $e1$; while not $e2$ do { $e1$; } } >> ]];
END
