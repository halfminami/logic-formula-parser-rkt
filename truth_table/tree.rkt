#lang typed/racket/base

(provide make-tree
         tree-to-string
         (struct-out node))

(require racket/match)

(require (only-in "tokenize.rkt"
                  token token? token-type TokenType))

(module+ test
  (require typed/rackunit
           typed/rackunit/text-ui))

;; - - - - - - - - - - - - -
;; matching parentheses
;; - - - - - - - - - - - - -

;; skips to the next closing parenthesis
;; the left part is not reversed
(: next-close (-> (Listof token)
                  (List (Listof token) (Listof token))))
(define/match (next-close li)
  [((list)) (error 'next-close "expected matching closing parenthesis")]
  [((list head tail ...))
   (case (token-type head)
     [(p-close)                         ; a match
      (list (list head) tail)]
     [(p-open)                          ; another opening parenthesis
      (match-define (list closed next)
        (consume-parens li))
      (match-define (list left right)
        (next-close next))
      (list (append closed left) right)]
     [else
      (match-define (list left right)
        (next-close tail))
      (list (cons head left) right)])])

;; splits at the end of the matching closing parenthesis
;; the left part is not reversed, starts with `(`
(: consume-parens (-> (Listof token)
                      (List (Listof token) (Listof token))))
(define/match (consume-parens li)
  [((list)) (error 'consume-parens "expected non empty list of tokens")]
  [((list head tail ...))
   (case (token-type head)
     [(p-open)
      (match-define (list left right)
        (next-close tail))
      (list (cons head left) right)]
     [else (error 'consume-parens
                  "expected starting with 'p-open', read '~a'"
                  head)])])

(module+ test
  (require (only-in "tokenize.rkt" tokenize))
  
  (define consume-parens-tests
    (test-suite
     "consume-parens"
     (test-suite
         "can match parens"
         (test-case
             "can match to the end"
           (check-equal? (consume-parens (tokenize "(hello)"))
                         (list (tokenize "(hello)") '()))
           (check-equal? (consume-parens (tokenize "(x2*(x3)+x1)"))
                         (list (tokenize "(x2*(x3)+x1)") '()))
           (check-equal? (consume-parens (tokenize "(1+(yo))"))
                         (list (tokenize "(1+(yo))") '()))
           (check-equal? (consume-parens (tokenize "((x)+z)"))
                         (list (tokenize "((x)+z)") '()))
           (check-equal? (consume-parens (tokenize "(((()())(()())))"))
                         (list (tokenize "(((()())(()())))") '())))
         
         (test-case
             "can stop"
           (check-equal? (consume-parens (tokenize "(x+y)*(x+z)"))
                         (list (tokenize "(x+y)") (tokenize "*(x+z)")))
           (check-equal? (consume-parens (tokenize "(())()"))
                         (list (tokenize "(())") (tokenize "()")))))
     
     (test-suite
      "reject invalid input"
      (test-case
          "fails on runaway"
        (check-exn exn:fail?
                   (lambda () (consume-parens (tokenize "(()"))))
        (check-exn exn:fail?
                   (lambda () (consume-parens (tokenize "(")))))

      (test-exn "fails on empty list" exn:fail?
                (lambda () (consume-parens '())))))))

;; - - - - - - - - -
;; making tree
;; - - - - - - - - -

(struct node ([token    : token]
              [children : (Listof node)])
  #:transparent)

;; splitting at the last token for infix operators
;; the left part is not reversed
(: split-token-right (-> TokenType
                         (-> (Listof token)
                             (Option (List (Listof token) token (Listof token))))))
(define ((split-token-right tt) li)
  (match li
    [(list) #f]
    [(list head tail ...)
     (define head-type (token-type head))
     (case head-type
       [(p-open)                        ; skip parentheses
        (match-define (list closed next)
          (consume-parens li))
        (define next-match
          ((split-token-right tt) next))
        (match next-match
          [#f #f]
          [(list left tkn right)
           (list (append closed left) tkn right)])]
       [else
        (define this-match
          (cond
            [(equal? head-type tt)
             (list '() head tail)]
            [else #f]))
        (define next-match
          ((split-token-right tt) tail))
        (match next-match
          [#f this-match]
          [(list left tkn right)
           (list (cons head left) tkn right)])])]))

(module+ test
  (define t-or (token 'or "+"))
  (define split-or
    (split-token-right 'or))
  (define split-token-right-tests
    (test-suite
     "split-token-right"
     (test-case
         "one token"
       (check-equal? (split-or (tokenize "a+b"))
                     (list (tokenize "a") t-or (tokenize "b")))
       (check-equal? (split-or (tokenize "(x+y)+z"))
                     (list (tokenize "(x+y)") t-or (tokenize "z")))
       (check-equal? (split-or (tokenize "x+(y+z)"))
                     (list (tokenize "x") t-or (tokenize "(y+z)"))))
     
     (test-case
         "multiple tokens"
       (check-equal? (split-or (tokenize "x+y+z"))
                     (list (tokenize "x+y") t-or (tokenize "z")))
       (check-equal? (split-or (tokenize "((x+y)*x1)+(x^x)+(y+z)+(z+w)+(x+(1+0)+y)"))
                     (list (tokenize "((x+y)*x1)+(x^x)+(y+z)+(z+w)") t-or (tokenize "(x+(1+0)+y)"))))

     (test-case
         "no token"
       (check-equal? (split-or '()) #f)
       (check-equal? (split-or (tokenize "x*y^z")) #f)))))

(define split-at-or  (split-token-right 'or))
(define split-at-xor (split-token-right 'xor))
(define split-at-and (split-token-right 'and))

(define-type ExprFunc (-> (Listof token) node))

(: or-expr ExprFunc)
(define (or-expr li)
  (match (split-at-or li)
    [#f (xor-expr li)]
    [(list l t r)
     (node t (list (or-expr l) (xor-expr r)))]))
(define formula-expr or-expr)

(: xor-expr ExprFunc)
(define (xor-expr li)
  (match (split-at-xor li)
    [#f (and-expr li)]
    [(list l t r)
     (node t (list (xor-expr l) (and-expr r)))]))

(: and-expr ExprFunc)
(define (and-expr li)
  (match (split-at-and li)
    [#f (not-expr li)]
    [(list l t r)
     (node t (list (and-expr l) (not-expr r)))]))

(: not-expr ExprFunc)
(define/match (not-expr li)
  [((list)) (error 'not-expr "expected <NotExpr>, read empty list")]
  [((list head tail ...))
   (case (token-type head)
     [(not) (node head (list (atom-expr tail)))]
     [else (atom-expr li)])])

(: atom-expr ExprFunc)
(define/match (atom-expr li)
  [((list)) (error 'atom-expr "expected <Atomic>, read empty list")]
  [((list head))
   (define type (token-type head))
   (case type
     [(variable one zero) (node head '())]
     [else (error 'atom-expr
                  "expected <Variable> or <Constant>, read token '~a'"
                  type)])]
  [((list whole ...))               ; formula
   ;; wanted to match something like:
   #;(list #{head : token} #{body : (Listof token)} ... #{tail : token})
   (match-define (list head body tail)
     (head-body-tail whole))
   (define head-type (token-type head))
   (define tail-type (token-type tail))
   (cond
     [(and (equal? head-type 'p-open)
           (equal? tail-type 'p-close))
      (formula-expr body)]
     [else (error 'atom-expr
                  "expected (<Formula>), but first token was '~a' and last was '~a'"
                  head-type
                  tail-type)])])

;; split list into three parts
(: head-body-tail (All (T) (-> (Listof T) (List T (Listof T) T))))
(define/match (head-body-tail li)
  [((list))     (error 'head-body-tail "length too short (0)")]
  [((list _))   (error 'head-body-tail "length too short (1)")]
  [((list _ _)) (error 'head-body-tail "length too short (2)")]
  [((list head rest ...))
   (match-define (list body tail)
     (body-tail rest))
   (list head body tail)])

;; length >= 2
;; last will do..?
(: body-tail (All (T) (-> (Listof T) (List (Listof T) T))))
(define/match (body-tail li)
  [((list head tail)) (list (list head) tail)]
  [((list head rest ...))
   (match-define (list body tail)
     (body-tail rest))
   (list (cons head body) tail)])

(: make-tree (-> (Listof token) node))
(define (make-tree li)
  (formula-expr li))

(module+ test
  (define make-tree-tests
    (test-suite
     "make-tree"
     (test-case
         "valid formula"
       (check-equal? (make-tree (tokenize "x"))
                     (node (token 'variable "x") '()))
       (check-equal? (make-tree (tokenize "1+0"))
                     (node (token 'or "+")
                           (list (node (token 'one "1") '())
                                 (node (token 'zero "0") '()))))
       (check-equal? (make-tree (tokenize "1*(x+y)+z+w"))
                     (node (token 'or "+")
                           (list (node (token 'or "+")
                                       (list (node (token 'and "*")
                                                   (list (node (token 'one "1") '())
                                                         (node (token 'or "+")
                                                               (list (node (token 'variable "x") '())
                                                                     (node (token 'variable "y") '())))))
                                             (node (token 'variable "z") '())))
                                 (node (token 'variable "w") '()))))
       (check-equal? (make-tree (tokenize "1^1*x^1*y"))
                     (node (token 'xor "^")
                           (list (node (token 'xor "^")
                                       (list (node (token 'one "1") '())
                                             (node (token 'and "*")
                                                   (list (node (token 'one "1") '())
                                                         (node (token 'variable "x") '())))))
                                 (node (token 'and "*")
                                       (list (node (token 'one "1") '())
                                             (node (token 'variable "y") '()))))))
       (check-equal? (make-tree (tokenize "!x1*!x2+!x3^!x4"))
                     (node (token 'or "+")
                           (list (node (token 'and "*")
                                       (list (node (token 'not "!")
                                                   (list (node (token 'variable "x1") '())))
                                             (node (token 'not "!")
                                                   (list (node (token 'variable "x2") '())))))
                                 (node (token 'xor "^")
                                       (list (node (token 'not "!")
                                                   (list (node (token 'variable "x3") '())))
                                             (node (token 'not "!")
                                                   (list (node (token 'variable "x4") '()))))))))
       (check-equal? (make-tree (tokenize "(((x12)))"))
                     (node (token 'variable "x12") '())))
     
     (test-case
         "invalid formula"
       (check-exn exn:fail?
                  (lambda () (make-tree (tokenize "!!x"))))
       (check-exn exn:fail?
                  (lambda () (make-tree (tokenize "x2*+1"))))
       (check-exn exn:fail?
                  (lambda () (make-tree (tokenize "10x"))))))))

;; quick simple string representation of a tree
(: tree-to-string (-> node String))
(define (tree-to-string n)
  (match-define (node (token type str) children) n)
  (case type
    [(or and xor) (format "(~a) ~a (~a)"
                          (tree-to-string (car children))
                          str
                          (tree-to-string (cadr children)))]
    [(not) (format "~a (~a)"
                   str
                   (tree-to-string (car children)))]
    [else (format "~a" str)]))

(module+ test
  (void (run-tests
         (make-test-suite "all"
                          (list
                           consume-parens-tests
                           split-token-right-tests
                           make-tree-tests)))))
