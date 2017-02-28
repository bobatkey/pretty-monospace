#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
  Pkg.describe "pretty-monospace" @@ fun c ->
  Ok [ Pkg.mllib "lib/pretty-monospace.mllib"
     ; Pkg.test "test/test"
     ]
