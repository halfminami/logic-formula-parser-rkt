#lang racket

;; will provide the tokenize function!

(module+ test
  (require rackunit
           rackunit/text-ui))

;; - - - - - - - - - -
;; constants
;; - - - - - - - - - -

(define OR #\+)
(define AND #\*)
(define XOR #\^)
(define NOT #\!)
(define P-OPEN #\()
(define P-CLOSE #\))

;; return value type
(struct/contract consume-result
                 ([val (listof char?)]  ; parsed string
                  [rst (listof char?)]) ; rest of formula
                 #:transparent)

;; - - - - - - - - - - - - -
;; parsing variable names
;; - - - - - - - - - - - - -

;; formula -> (latter variable name), (rest of formula)
(define/contract (consume-alphabet-or-number li)
  (-> (listof char?) consume-result?)

  (match li
    [(list)                             ; end of input
     (consume-result (list) li)]

    [(list head tail ...)
     (cond
       [(or (char-alphabetic? head)
            (char<=? #\0 head #\9))

        (match (consume-alphabet-or-number tail)
          [(consume-result val rst)
           (consume-result (cons head val) rst)])]

       [else                            ; end of name
        (consume-result (list) li)])]))

;; formula -> (variable name), (rest of formula)
(define/contract (consume-variable s)
  (-> (listof char?) consume-result?)

  (match s
    [(list) (error "variable name expected, read end of input")]

    [(list head tail ...)
     (cond
       [(char-alphabetic? head)

        (match (consume-alphabet-or-number tail)
          [(consume-result val rst)
           (consume-result (cons head val) rst)])]

       [else (error "variable name should start with alphabet")])]))

(module+ test
  (define (ce li ex)
    (check-equal? (apply consume-variable li) ex))
  (define sl string->list)
  (define ret consume-result)
  
  (define consume-variable-tests
    (list
     (test-suite
      "parse a variable name"
      (test-case
          "can consume a variable name to the end"
        (ce (list (sl "a"))
            (ret (sl "a") '()))
        (ce (list (sl "abc"))
            (ret (sl "abc") '()))
      
        (ce (list (sl "x0"))
            (ret (sl "x0") '()))
        (ce (list (sl "x0a"))
            (ret (sl "x0a") '())))

      (test-case
          "can stop parsing"
        (ce (list (sl "a*b"))
            (ret (sl "a") (sl "*b")))
        (ce (list (sl "x1+x2"))
            (ret (sl "x1") (sl "+x2"))))

      (test-case
          "fails on invalid call"
        (check-exn exn:fail?
                   (lambda () (consume-variable (sl "6x"))))
        (check-exn exn:fail?
                   (lambda () (consume-variable (sl ""))))))))

  (for-each (lambda (x) (run-tests x))
            consume-variable-tests))
