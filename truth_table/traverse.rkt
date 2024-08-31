#lang typed/racket/base

(provide variable-list
         custom-order-list
         make-bit-mapping
         evaluate)

(require (except-in "tree.rkt"
                    make-tree tree-to-string)
         (except-in "tokenize.rkt"
                    tokenize))

(require racket/list
         racket/match
         racket/promise)

(module+ test
  (require typed/rackunit
           typed/rackunit/text-ui)
  (require (only-in "tokenize.rkt" tokenize)
           (only-in "tree.rkt" make-tree)))

;; - - - - - - - - - - - - -
;; extract variable names
;; - - - - - - - - - - - - -
;; a variable is a leaf

;; contains duplicates and empty lists
(: leaf-variable-list (-> node (Listof (U String Null))))
(define (leaf-variable-list n)
  (define children (node-children n))
  (case (token-type (node-token n))
    [(one zero) '(())]
    [(variable) (list (token-str (node-token n)))]
    [(not) (leaf-variable-list (car children))]
    [else
     (define-values (left right)
       (values (car children) (cadr children)))
     (append (leaf-variable-list left)
             (leaf-variable-list right))]))

;; returns unique sorted list of variables
(: variable-list (-> node (Listof String)))
(define (variable-list r)
  (define flattened
    (cast (flatten (leaf-variable-list r))
          (Listof String)))
  (sort (remove-duplicates flattened)
        string<?))

(module+ test
  (define variable-list-tests
    (test-suite
     "variable-list"
     (test-case
         "can list variables"
       (check-equal? (variable-list (make-tree (tokenize "x1+x2")))
                     (list "x1" "x2"))
       (check-equal? (variable-list (make-tree (tokenize "x+y*(z^w)")))
                       (list "w" "x" "y" "z")))

     (test-case
         "can remove duplicates"
       (check-equal? (variable-list (make-tree (tokenize "x1^x1*x2+x1")))
                     (list "x1" "x2"))
       (check-equal? (variable-list (make-tree (tokenize "x+x*x^x*!x")))
                     (list "x")))

     (test-case
         "no variable"
       (check-equal? (variable-list (make-tree (tokenize "0+1*0")))
                     '())))))

;; unique sorted list preserving custom ordering at the beginning
(: custom-order-list (-> (Listof String) (Listof String)
                         (Listof String)))
(define (custom-order-list custom lst)
  (match lst
    [(list) custom]
    [(list head tail ...)
     (if (member head custom)
         (custom-order-list custom tail)
         (custom-order-list (append custom (list head))
                            tail))]))

(module+ test
  (define custom-order-list-tests
    (test-suite
     "custom-order-list"
     (test-case
         "can reorder"
       (check-equal? (custom-order-list '() '("x" "y"))
                     '("x" "y"))
       (check-equal? (custom-order-list '("y") '("x" "y"))
                     '("y" "x"))
       (check-equal? (custom-order-list '("z" "x" "y") '("w" "x" "y" "z" "a"))
                     '("z" "x" "y" "w" "a"))
       (check-equal? (custom-order-list '("x" "y") '("x" "y"))
                     '("x" "y"))))))

;; - - - - - - - - - - - - - -
;; map variable names to bit
;; - - - - - - - - - - - - - -

;; string at index 0 is mapped to the most significant bit
(: make-bit-mapping-reversed (-> (Listof String) Exact-Nonnegative-Integer
                                 (Immutable-HashTable String Boolean)))
(define (make-bit-mapping-reversed reversed bit)
  (: mapping (-> (Listof String) Exact-Nonnegative-Integer
                 (U (Listof (Pairof String Boolean)) Null)))
  (define (mapping lst i)
    (match lst
      [(list) '()]
      [(list head tail ...)
       (cons (cons head (bitwise-bit-set? bit i))
             (mapping tail (add1 i)))]))
  (make-immutable-hash (mapping reversed 0)))

;; maps each variable to corresponding bit
;; this curry reverses the list only once (is there a better way to do the same thing?)
(: make-bit-mapping (-> (Listof String)
                        (-> Exact-Nonnegative-Integer
                            (Immutable-HashTable String Boolean))))
(define (make-bit-mapping lst)
  (define reversed (reverse lst))
  (lambda ([bit : Exact-Nonnegative-Integer])
    (make-bit-mapping-reversed reversed bit)))

(module+ test
  (define make-bit-mapping-tests
    (test-suite
     "make-bit-mapping"
     (test-case
         "can create correct hashtable"
       (define xyzz (make-bit-mapping (list "x" "y" "z" "z'")))
       
       (check-equal? (xyzz #b0)
                     (make-immutable-hash '(("x" . #f) ("y" . #f) ("z" . #f) ("z'" . #f))))
       (check-equal? (xyzz #b1)
                     (make-immutable-hash '(("x" . #f) ("y" . #f) ("z" . #f) ("z'" . #t))))
       (check-equal? (xyzz #b100)
                     (make-immutable-hash '(("x" . #f) ("y" . #t) ("z" . #f) ("z'" . #f))))
       (check-equal? (xyzz #b1110)
                     (make-immutable-hash '(("x" . #t) ("y" . #t) ("z" . #t) ("z'" . #f))))
       (check-equal? (xyzz #b1000000)
                     (make-immutable-hash '(("x" . #f) ("y" . #f) ("z" . #f) ("z'" . #f)))))
     
     (test-case
         "no variable"
       (check-equal? ((make-bit-mapping '()) #b1)
                     (make-immutable-hash))))))

;; - - - - - - - - - - - - - - - - - - -
;; evaluate tree with variable mapping
;; - - - - - - - - - - - - - - - - - - -

;; DIY lazy boolean functions since I need types!
(: b-not (-> Boolean Boolean))
(define (b-not x) (if x #f #t))
(: b-or (-> Boolean (Promise Boolean) Boolean))
(define (b-or x y) (if x #t (force y)))
(: b-xor (-> Boolean Boolean Boolean))
(define (b-xor x y)
  (match (cons x y)
    [(cons #t #f) #t]
    [(cons #f #t) #t]
    [_ #f]))
(: b-and (-> Boolean (Promise Boolean) Boolean))
(define (b-and x y) (if x (force y) #f))

;; evaluate tree
;; hashtable must contain all variable names
(: evaluate (-> node (Immutable-HashTable String Boolean)
                Boolean))
(define (evaluate r hs)
  (define-values (tkn chs)
    (values (node-token r) (node-children r)))
  (define-values (type str)
    (values (token-type tkn) (token-str tkn)))
  #;(printf "evaluate ~a\n" str)        ; see shortcircuit!
  (case type
    [(one) #t]
    [(zero) #f]
    [(variable)
     (with-handlers
       ([exn:fail? (lambda (v) (error
                                'evaluate
                                "mapping for ~s is missing"
                                str))])
       (hash-ref hs str))]
    [(not) (b-not (evaluate (car chs) hs))]
    [(or)  (b-or  (evaluate (car chs) hs)
                  (delay (evaluate (cadr chs) hs)))]
    [(xor) (b-xor (evaluate (car chs) hs)
                  (evaluate (cadr chs) hs))]
    [(and) (b-and (evaluate (car chs) hs)
                  (delay (evaluate (cadr chs) hs)))]
    [else (error 'evaluate
                 "unknown operation type '~a'"
                 type)]))

(module+ test
  (define evaluate-tests
    (test-suite
     "evaluate"
     (test-case
         "can evaluate + *"
       (define tree (make-tree (tokenize "x+y*z")))
       (define vlst (variable-list tree)) ; x y z
       (define bmap (make-bit-mapping vlst))

       (check-equal? (evaluate tree (bmap #b100))
                     #t)
       (check-equal? (evaluate tree (bmap #b010))
                     #f)
       (check-equal? (evaluate tree (bmap #b011))
                     #t))

     (test-case
         "can evaluate ^ * 1"
       (define tree (make-tree (tokenize "1^x^x*y")))
       (define vlst (variable-list tree)) ; x y
       (define bmap (make-bit-mapping vlst))

       (check-equal? (evaluate tree (bmap #b00))
                     #t)
       (check-equal? (evaluate tree (bmap #b01))
                     #t)
       (check-equal? (evaluate tree (bmap #b10))
                     #f)
       (check-equal? (evaluate tree (bmap #b11))
                     #t))
     
     (test-case
         "can evaluate constant function"
       (define tree (make-tree (tokenize "0")))
       (define vlst (variable-list tree))
       (define bmap (make-bit-mapping vlst))
       
       (check-equal? (evaluate tree (bmap #b0))
                     #f)))))

(module+ test
  (void (run-tests
         (make-test-suite "all"
                          (list
                           variable-list-tests
                           custom-order-list-tests
                           make-bit-mapping-tests
                           evaluate-tests)))))
