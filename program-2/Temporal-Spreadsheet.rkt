#lang racket

;; Symbol -> Procedure | Symbol
;; Maps symbols to actual math operations
(define (oper s)
  (cond
    [(equal? '+ s) +]
    [(equal? '- s) -]
    [(equal? '* s) *]
    [(equal? '/ s) /]
    [(equal? + s) '+]
    [(equal? - s) '-]
    [(equal? * s) '*]
    [(equal? / s) '/]
    [else -1]))

;; Prints the hash in a nice format
(define (print-hash ht kys)
  (cond
    [(empty? kys) (newline)]
    [else 
        (printf "~a: ~a" (first kys) (hash-ref ht (first kys))) (newline)
        (print-hash ht (rest kys))]))

;; Address Address -> Number
;; Calculates the Manhattan distance (|x1-x2| + |y1-y2|) between two coordinates.
(define (manhattan-distance coord1 coord2) 
  (define coord1-p (if (list? coord1) 
                       (apply cons coord1) 
                       coord1))
  (define coord2-p (if (list? coord2) 
                       (apply cons coord2)
                       coord2))
  (+ (abs (- (car coord1-p) (car coord2-p)))
     (abs (- (cdr coord1-p) (cdr coord2-p)))))

;; Address -> Boolean
;; Returns true if the sum of a coordinate's x and y is even.
(define (even-cell? coord) 
  (define coord-p (if (list? coord)
                      (apply cons coord)
                      coord))
  (even? (+ (car coord-p) (cdr coord-p))))

;; Address Grid List -> Number | Symbol
;; Looks up a coordinate and tracks the visited path to prevent cycles.
(define (eval-cell sheet coord visited)
  (cond
    [(member coord visited) 'cycle-error]
    [else
     (let* ([cell (if (not (list? (hash-ref sheet coord 0)))  ; change this for the ai as it did not handel
                      (list (hash-ref sheet coord 0))         ; the cell value correctly
                      (hash-ref sheet coord 0))]              ;
            [cell-value (if (equal? '= (first cell))          ;  
                            (hash-ref sheet coord 0)          ; 
                            (first cell))])                   ;
       (cond
         [(or (number? cell-value) (symbol? cell-value)) cell-value] ; added an or to check for if a hash table already has a error in it and it keeps that error 
         [(list? cell-value) 
          (eval-formula sheet cell-value coord (cons coord visited))]
         [else 'invalid-data]))]))

;; Formula Address Grid List -> Number | Symbol
;; Evaluates a formula, applying the Parity Guard and Manhattan Distance Tax.
(define (eval-formula sheet formula base-coord visited)
  (match formula
    [`(= ,op1 ,operator ,op2)
     (define (resolve-operand op)
       (if (pair? op)
           (if (and (even-cell? base-coord) (not (even-cell? op)))
               'parity-error
               (let ([raw-value (eval-cell sheet op visited)])
                 (if (number? raw-value)
                     (- raw-value (manhattan-distance base-coord op))
                     raw-value)))
           op))
     (let ([val1 (resolve-operand op1)]
           [val2 (resolve-operand op2)])
       (cond
         [(symbol? val1) val1]
         [(symbol? val2) val2]
         [else
          ((oper operator) val1 val2)]))]
    
    [_ 'parse-error]))

(define (eval-temp-sheet sheet [sheet-keys (hash-keys sheet)])
  (cond 
    [(empty? sheet-keys) (print-hash sheet (hash-keys sheet))]
    [else
        (let* ([cell-cord (first sheet-keys)]
               [new-value (eval-cell sheet cell-cord '())])
          (eval-temp-sheet (hash-set sheet cell-cord new-value) (rest sheet-keys)))]))

(define sheet1 '(((1 1) 100) ((1 3) (= (1 1) + 0))))
(define sheet2 '(((2 3) 50) ((2 2) (= (2 3) + 0))))
(define sheet3 '(((1 1) (= (2 2) + 1)) ((2 2) (= (1 1) + 1)))) ; has cycle
(define sheet4 '(((1 1) 10) ((1 2) (= (1 1) + 0)) ((2 2) (= (1 2) * (1 1)))))
(define sheet5 '(((1 1) (= (9 9) + 0))))
(eval-temp-sheet (make-immutable-hash sheet1))
(eval-temp-sheet (make-immutable-hash sheet2))
(eval-temp-sheet (make-immutable-hash sheet3))
(eval-temp-sheet (make-immutable-hash sheet4))
(eval-temp-sheet (make-immutable-hash sheet5))

