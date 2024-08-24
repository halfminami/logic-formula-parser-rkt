#lang racket

#|
Draws a truth table from a logic formula (`!((x*y+z)*((!x^y)*z))` for example).

A variable starts with an alphabet which may be followed by either alphabets or numbers.
Spaces between tokens have no meaning, so you can add it for clarity.
Symbols for logical operations are:
```
Operation : Symbol
OR        : +
AND       : *
XOR       : ^
NOT       : !
```
The order is `NOT > XOR > AND > OR` (NOT has the highest precedence).
It's a good idea to wrap the formula with parentheses to specify the precedence.
                                                                                           
Here's a [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) for logic formulas.
```
<Formula> ::= <OrExpr>
                                                                                           
<OrExpr> ::= <OrExpr> <Or> <AndExpr> | <AndExpr>
<AndExpr> ::= <AndExpr> <And> <XorExpr> | <XorExpr>
<XorExpr> ::= <XorExpr> <Xor> <NotExpr> | <NotExpr>
<NotExpr> ::= <Not> <Atomic> | <Atomic>
<Atomic> ::= <Variable> | "(" <Formula> ")"
                                                                                           
<Variable> ::= <Alphabet> | <Variable> <Alphabet> | <Variable> <Number>
<Or> ::= "+"
<And> ::= "*"
<Xor> ::= "^"
<Not> ::= "!"

<Alphabet> ::= [a-zA-Z]
<Number> ::= [0-9]
```
|#

(require "tokenize.rkt")

(define/contract (padstring-left s w pad)
  (-> string? exact-nonnegative-integer? string? string?)
  (~a s #:min-width w #:align 'right #:pad-string pad))

(module+ main
  (displayln "yo"))
