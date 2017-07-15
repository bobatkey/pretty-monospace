# Pretty-monospace: Related Work

- https://github.com/brandonbloom/fipp "A Common Lisp Pretty Printing
  System" (I think this was referenced from Brandon Bloom's fipp
  thing)

- http://defoe.sourceforge.net/folio/knuth-plass.html

- http://i.stanford.edu/pub/cstr/reports/cs/tr/79/770/CS-TR-79-770.pdf
  "Pretty Printing" by Derek Oppen, 1980 As far as I know, this is the
  original attempt at defining a generic pretty-printing library. It
  introduces the concept of 'groups' that are broken all at once, or
  not at all. A streaming algorithm is given that processes the input
  with latency limited to the

- https://github.com/freebroccolo/pretty.rs A implementation of pretty
  printing in Rust, by Darin Morrison and Jon Sterling. Follows the
  Lindig implementation.

- http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
  Wadler's "prettier printer" paper. Derives an implementation from
  the specification (though somewhat sketchily in places). This
  algorithm remeasures nested groups when making decisions about
  whether to flatten a group or not.

- [http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4753F995936BCBD605D00BBC55292C70?doi=10.1.1.34.2200&rep=rep1&type=pdf]
  Lindig's "Strictly Pretty". An implementation of Wadler's algorithm
  in OCaml that "inlines" some of the laziness, but retains the
  exponential worst case time.

- http://pauillac.inria.fr/~fpottier/pprint/ François Pottier's pretty
  printing library. Like Oppen's system, and Bloom's `fipp` library
  and the Chitil and Kiselyov et al algorithms, this library doesn't
  take into account trailing text on the same line as a group we are
  making a decision about. Wadler's library, and Lindig's, take into
  account trailing context on the same line, giving better layouts and
  avoiding the problem described in one of fipp's issues.

- https://www.cs.kent.ac.uk/people/staff/oc/pretty.html Olaf Chitil's
  page on his purely functional versions of pretty printing. Later
  followed up by Kiselyov et al in their dequeue based system. The
  final system with the co-routines looks very similar to the
  Oppen-style solution I have ended up with.

- http://blog.vjeux.com/2017/javascript/anatomy-of-a-javascript-pretty-printer.html
  How the Javascript `prettier` thing works.

- "Correct-by-Construction Pretty-Printing" by Nils Anders Danielsson.
  http://www.cse.chalmers.se/~nad/publications/danielsson-correct-pretty.html

- "Format: Add functions for printing conditionally on line breaks"
  https://github.com/ocaml/ocaml/pull/1229
  Similar to the 'alignment_spaces' stuff.

## To be added

- `prettier`
- Box model stuff
- Ocaml's `Format` module

