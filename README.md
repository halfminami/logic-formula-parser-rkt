# Logic Formula Parser

simple logic formula parser in Typed Racket!

## Usage

Run:
```sh
racket truth_table.rkt
```
then you will be prompted to input a formula.

If input `1^x1^x1*x2^x3` ($1 \oplus x_1 \oplus x_1 \cdot x_2 \oplus x_3$),
the result would be like:
```
$ racket truth_table.rkt
input formula: 1^x1^x1*x2^x3
tree is (((1) ^ (x1)) ^ ((x1) * (x2))) ^ (x3)
variable order [default x1 x2 x3]:

x1x2x3 | 1^x1^x1*x2^x3
=======+==============
 0 0 0 |       1
 0 0 1 |       0
 0 1 0 |       1
 0 1 1 |       0
-------+--------------
 1 0 0 |       0
 1 0 1 |       1
 1 1 0 |       1
 1 1 1 |       0
-------+--------------
```
It parses the given formula, sorts the variables
and draws the truth table.

### `lib`

Functions that are used by the main module are provided via submod.

You can calculate `x*!y` ($x \cdot \bar{y}$)
where `x=true` and `y=false` in REPL like so:
```racket
> (require (submod "truth_table.rkt" lib))
> (define tokens (tokenize "x*!y"))
> (define tree (make-tree tokens))
> (define variables (variable-list tree))
> (define bit-mapping (make-bit-mapping variables (length variables)))
> (evaluate tree (bit-mapping #b10))
#t
```

### Unit Test

Files in `truth_table/` have unit tests.
```sh
raco test truth_table/
```
(I'm a novice and the test may not be correct.
If there's no unusual output it's ok.)

