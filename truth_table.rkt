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

(provide format-table)

(module+ lib
  (provide tokenize
           make-tree
           variable-list
           custom-order-list
           make-bit-mapping
           evaluate))

(require "truth_table/tokenize.rkt"
         "truth_table/tree.rkt"
         "truth_table/traverse.rkt")

(require racket/format
         racket/string
         racket/match)

(: main (-> String Void))
(define (main formula)
  (define tree (make-tree (tokenize formula)))
  (printf "tree is ~a~n" (tree-to-string tree))
  
  (define sorted (variable-list tree))
  (printf "variable order [default ~a]: "
          (string-join sorted))  
  (define custom-order-str (cast
                            (match (read-line)
                              [eof #:when (eof-object? eof)
                                   ""]
                              [str str])
                            String))
  
  (define custom-order (string-split custom-order-str))
  (define v-list (custom-order-list custom-order
                                    sorted))

  (printf "~n~a" (format-table tree v-list formula)))

(module+ main
  (printf "input formula: ")
  (define str (read-line))
  (cond
    [(eof-object? str) (displayln "no formula read. exit")]
    [else (main str)]))

(: format-table (-> node (Listof String) String String))
(define (format-table tree v-list formula-str)
  ;; printing
  (define each-len (map (lambda (s) (string-length s))
                        v-list))
  
  (define v-list-str (string-join v-list ""))
  
  (define v-list-str-len (string-length v-list-str))
  (define formula-str-len (string-length formula-str))
  
  (define vert-sep (format "~a+~a~n"
                           (many-dash (add1 v-list-str-len))
                           (many-dash (add1 formula-str-len))))
  (define vert-sep= (format "~a+~a~n"
                            (many-dash (add1 v-list-str-len) #\=)
                            (many-dash (add1 formula-str-len) #\=)))

  ;; evaluating
  (define v-list-len (length v-list))
  (define bit-mapping (make-bit-mapping v-list v-list-len))

  (: gen (-> Nonnegative-Integer Nonnegative-Integer (Listof String)))
  (define (gen bit end)
    (cond
      [(= bit end) '()]
      [else
       (define set (evaluate tree (bit-mapping bit)))
       (cons (string-append
              (format "~a | ~a~n"
                      (format-bits each-len bit v-list-len)
                      (padstr (format-bool set)
                              formula-str-len
                              'center))
              (cond
                [(= 3 (modulo bit 4)) vert-sep]
                [else ""]))
             (gen (add1 bit) end))]))

  (apply
   string-append
   (format "~a | ~a~n" v-list-str formula-str)
   vert-sep=
   (gen 0 (bin-power 2 v-list-len))))

(: padstr (->* (String Nonnegative-Integer) ((U 'left 'center 'right)) String))
(define (padstr s w [align 'right])
  (~a s #:min-width w #:align align))

;; string of binary number padded
(: format-bits (-> (Listof Nonnegative-Integer) Integer Integer
                   String))
(define (format-bits each-len bit len)
  (: gen (-> (Listof Nonnegative-Integer) Integer Integer
                   (Listof String)))
  (define (gen each-len bit len)
    (match each-len
      [(list) '()]
      [(list head tail ...)
       (define set (bitwise-bit-set? bit len))
       (cons (padstr (format-bool set)
                     head)
             (gen tail bit (sub1 len)))]))
  (string-join (gen each-len bit (sub1 len)) ""))

;; #f/#t to 0/1
(: format-bool (-> Boolean String))
(define (format-bool b) (if b "1" "0"))

(: many-dash (->* (Nonnegative-Integer) (Char) String))
(define (many-dash n [ch #\-]) (make-string n ch))

(: bin-power (-> Natural Natural Natural))
(define/match (bin-power r ex)
  [(_ 0) 1]
  [(r ex) (* (if (odd? ex) r 1)
             (bin-power (* r r) (quotient ex 2)))])
