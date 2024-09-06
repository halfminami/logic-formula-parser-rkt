#lang typed/racket/base

;; TODO: figure out how to test (not failing on failure)
;; I can't seem to handle values well..

(provide tokenize
         (struct-out token)
         TokenType)

(require racket/match
         racket/list)

(module+ test
  (require typed/rackunit
           typed/rackunit/text-ui))

;; - - - - - - - - - - - - -
;; parsing variable names
;; - - - - - - - - - - - - -

;; formula -> (latter variable name), (rest of formula)
(: consume-alphabet-or-number (-> (Listof Char)
                                  (List (Listof Char) (Listof Char))))
(define (consume-alphabet-or-number li)
  (define-values (left right)
    (splitf-at li
               (lambda ([c : Char])
                 (or (char<=? #\a c #\z)
                     (char<=? #\A c #\Z)
                     (char<=? #\0 c #\9)))))
  (list left right))

;; formula -> (variable name), (rest of formula)
(: consume-variable (-> (Listof Char)
                        (List (Listof Char) (Listof Char))))
(define/match (consume-variable s)
  [((list)) (error 'consume-variable
                 "expected a variable name, read an empty formula")]
  [((list head tail ...))
   (cond
     [(or (char<=? #\a head #\z)
          (char<=? #\A head #\Z))
      (match-define (list val rst)
        (consume-alphabet-or-number tail))
      (list (cons head val) rst)]
     [else (error 'consume-variable
                  "variable name should start with alphabet, read '~c'"
                  head)])])

(module+ test
  (define sl string->list)
  (define ret list)
  
  (define consume-variable-tests
    (test-suite
     "consume-variable"
     (test-suite
      "parse a variable name"
      (test-case
          "can consume a variable name to the end"
        (check-equal? (consume-variable (sl "a"))
                      (ret (sl "a") '()))
        (check-equal? (consume-variable (sl "abc"))
                      (ret (sl "abc") '()))
      
        (check-equal? (consume-variable (sl "x0"))
                      (ret (sl "x0") '()))
        (check-equal? (consume-variable (sl "x0a"))
                      (ret (sl "x0a") '())))

      (test-case
          "can stop parsing"
        (check-equal? (consume-variable (sl "a*b"))
                      (ret (sl "a") (sl "*b")))
        (check-equal? (consume-variable (sl "x1+x2"))
                      (ret (sl "x1") (sl "+x2")))))

     (test-suite
      "reject invalid input"
      (test-case
          "fails on invalid call"
        (check-exn exn:fail?
                   (lambda () (consume-variable (sl "6x"))))
        (check-exn exn:fail?
                   (lambda () (consume-variable (sl " w")))))

      (test-exn "fails on empty formula" exn:fail?
                (lambda () (consume-variable (sl ""))))))))

;; - - - - - - - - - - - -
;; parsing the formula
;; - - - - - - - - - - - -

(define-type TokenType (U 'or
                          'and
                          'xor
                          'not
                          'p-open
                          'p-close
                          'variable
                          'one
                          'zero))

(struct token ([type : TokenType]
               [str  : String])
  #:transparent)

;; no #\space in input char list
(: list-to-token (-> (Listof Char) (Listof token)))
(define/match (list-to-token li)
  [((list)) '()]
  [((list head tail ...))
   (case head
     [(#\+) (cons (token 'or      "+") (list-to-token tail))]
     [(#\*) (cons (token 'and     "*") (list-to-token tail))]
     [(#\^) (cons (token 'xor     "^") (list-to-token tail))]
     [(#\!) (cons (token 'not     "!") (list-to-token tail))]
     [(#\() (cons (token 'p-open  "(") (list-to-token tail))]
     [(#\)) (cons (token 'p-close ")") (list-to-token tail))]
     [(#\1) (cons (token 'one     "1") (list-to-token tail))]
     [(#\0) (cons (token 'zero    "0") (list-to-token tail))]
     [else
      (match-define (list val rst)
        (consume-variable li))
      (cons (token 'variable (list->string val))
            (list-to-token rst))])])

;; convert formula string into token list
(: tokenize (-> String (Listof token)))
(define (tokenize s)
  (define no-space (remove* '(#\space) (string->list s)))
  (match no-space
    [(list) (error 'tokenize "expression should not be empty")]
    [(var a) (list-to-token a)]))

(module+ test
  (define tokenize-tests
    (test-suite
     "tokenize"
     (test-suite
      "can tokenize"
      (test-case
          "normal formula"
        (check-equal? (tokenize "xz^yz^xz")
                      (list (token 'variable "xz")
                            (token 'xor      "^")
                            (token 'variable "yz")
                            (token 'xor      "^")
                            (token 'variable "xz")))
        (check-equal? (tokenize "(x1*z+x2)")
                      (list (token 'p-open   "(")
                            (token 'variable "x1")
                            (token 'and      "*")
                            (token 'variable "z")
                            (token 'or       "+")
                            (token 'variable "x2")
                            (token 'p-close  ")")))
        (check-equal? (tokenize "10x1(x1+1)")
                      (list (token 'one      "1")
                            (token 'zero     "0")
                            (token 'variable "x1")
                            (token 'p-open   "(")
                            (token 'variable "x1")
                            (token 'or       "+")
                            (token 'one      "1")
                            (token 'p-close  ")"))))
      (test-case
          "ignore spaces"
        (check-equal? (tokenize " xz ^ yz ^ x z ")
                      (list (token 'variable "xz")
                            (token 'xor      "^")
                            (token 'variable "yz")
                            (token 'xor      "^")
                            (token 'variable "xz")))))

     (test-suite
      "reject invalid input"
      (test-exn "should reject empty formula"
                exn:fail? (lambda () (tokenize "")))
      (test-exn "should ignore spaces"
                exn:fail? (lambda () (tokenize "    ")))))))

(module+ test
  (void (run-tests
         (make-test-suite "all"
                          (list
                           consume-variable-tests
                           tokenize-tests)))))

