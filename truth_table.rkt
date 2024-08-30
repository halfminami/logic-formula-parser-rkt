#lang typed/racket/base

#|
Draws a truth table from a logic formula (`!((x*y+z)*((!x^y)*z))` for example).

A variable starts with an alphabet which may be followed by either alphabets or numbers.
Spaces between tokens have no meaning, so you can add it for clarity.
Symbols for logical operations are:
```
Operation : Symbol
OR        : +
XOR       : ^
AND       : *
NOT       : !
```
The order is `NOT > AND > XOR > OR` (NOT has the highest precedence).
It's a good idea to wrap the formula with parentheses to specify the precedence.
                                                                                           
Here's a [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) for logic formulas.
```
<Formula> ::= <OrExpr>

<OrExpr>  ::= <OrExpr> <Or> <XorExpr> | <XorExpr>
<XorExpr> ::= <XorExpr> <Xor> <AndExpr> | <AndExpr>
<AndExpr> ::= <AndExpr> <And> <NotExpr> | <NotExpr>
<NotExpr> ::= <Not> <Atomic> | <Atomic>
<Atomic>  ::= <Variable> | <Constant> | "(" <Formula> ")"

<Variable> ::= <Alphabet> | <Variable> <Alphabet> | <Variable> <Number>
<Or>       ::= "+"
<Xor>      ::= "^"
<And>      ::= "*"
<Not>      ::= "!"

<Constant> ::= "0" | "1"
<Alphabet> ::= [a-zA-Z]
<Number>   ::= [0-9]
```
|#

(require "tokenize.rkt")

#;
(define/contract (padstring-left s w pad)
  (-> string? exact-nonnegative-integer? string? string?)
  (~a s #:min-width w #:align 'right #:pad-string pad))

(module+ main
  (displayln "yo")
  (displayln (tokenize "!x1^x2")))
