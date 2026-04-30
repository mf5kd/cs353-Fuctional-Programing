#lang racket

(define TRIGGER-CHAR #\#)

(struct leaf (char weight) #:transparent)
(struct node (left right weight) #:transparent)

(define (get-weight item)
  (if (leaf? item) (leaf-weight item) (node-weight item)))

(define (insert-pq tree pq)
  (cond 
    [(empty? pq) (list tree)]
    [(<= (get-weight tree) (get-weight (first pq))) (cons tree pq)]
    [else (cons (first pq) (insert-pq tree (rest pq)))]))

(define (build-tree pq)
  (cond 
    [(empty? pq) empty]
    [(= (length pq) 1) (first pq)]
    [else
     (let* ([left (first pq)]
            [right (second pq)]
            [rest-pq (rest (rest pq))]
            [new-weight (+ (get-weight left) (get-weight right))]
            [new-node (node left right new-weight)])
       (build-tree (insert-pq new-node rest-pq)))]))

(define (build-dictionary tree [current-bits '()])
  (if (leaf? tree) (list (cons (leaf-char tree) (reverse current-bits)))
      (append (build-dictionary (node-left tree) (cons 0 current-bits))
              (build-dictionary (node-right tree) (cons 1 current-bits)))))

(define (lookup char dict)
  (cond
    [(empty? dict) (error "Character not found in dictionary" char)]
    [(equal? (first (first dict)) char) (rest (first dict))]
    [else (lookup char (rest dict))]))

(define (mirror-tree tree)
  (if (leaf? tree) tree
      (node 
          (mirror-tree (node-right tree))
          (mirror-tree (node-left tree))
          (get-weight tree))))

(define (encode chars dict-normal dict-mirrored mode)
  (if (empty? chars) empty
      (let* ([c (first chars)]
             [current-dict (if mode dict-normal dict-mirrored)]
             [bits (lookup c current-dict)])
        (if (equal? c TRIGGER-CHAR)
            (append bits (encode (rest chars) dict-normal dict-mirrored (not mode)))
            (append bits (encode (rest chars) dict-normal dict-mirrored mode))))))

(define (encode-string str tree)
  (let* ([chars (string->list str)]
         [dict-normal (build-dictionary tree)]
         [dict-mirrored (build-dictionary (mirror-tree tree))])
    (encode chars dict-normal dict-mirrored #t)))

(define (decode bits current-node root-normal root-mirrored mode)
  (cond
    [(leaf? current-node)
     (let ([c (leaf-char current-node)])
       (if (equal? c TRIGGER-CHAR)
           (cons c (if (empty? bits)
                       '()
                       (decode bits
                               (if (not mode) root-normal root-mirrored)
                               root-normal root-mirrored (not mode))))
           (cons c (if (empty? bits)
                       '()
                       (decode bits
                               (if mode root-normal root-mirrored)
                               root-normal root-mirrored mode)))))]
    [(empty? bits) '()]
    [else
     (let* ([b (first bits)]
            [next-node (if (= b 0) (node-left current-node) (node-right current-node))])
       (decode (rest bits) next-node root-normal root-mirrored mode))]))

(define (decode-bits bits tree)
  (let ([mirrored-tree (mirror-tree tree)])
    (list->string (decode bits tree tree mirrored-tree #t))))

(define (make-pq char-weights [pq '()])
  (cond 
    [(empty? char-weights) pq]
    [else
     (let* ([char (first char-weights)]
            [weight (second char-weights)]
            [next (rest (rest char-weights))])
       (make-pq next (append pq (list (leaf char weight)))))]))

(define (run-test pq s)
  (cond 
    [(empty? s) (newline)]
    [else
     (let* ([test-string (first s)]
            [tree (build-tree pq)]
            [encoded-bits (encode-string test-string tree)]
            [decoded-string (decode-bits encoded-bits tree)])
       (display "Encoding ") (displayln test-string)
       (displayln encoded-bits)
       (display "Decoding those bits: ")
       (displayln decoded-string)
       (run-test pq (rest s)))]))

;----------------------------------------------------------------------------------------------------------------------------------------------------------
(define test-pq (make-pq (list #\a 3 #\b 4 #\c 5 #\# 6)))
(define test-pq-2 (make-pq (list #\a 5 #\b 10 #\c 15 #\d 3 #\# 2)))
(define test-pq-3 (make-pq (list #\a 3 #\b 3 #\c 3 #\# 3 #\d 3 #\e 3 #\f 3)))
(define test-pq-4 (make-pq (list #\a 2 #\b 2 #\c 4 #\d 8 #\# 2 #\e 16 #\f 2 #\g 4)))

(run-test test-pq (list "abc#abc" "a#b#c##ac"))
(run-test test-pq-2 (list "abc#cba" "abd#ccb#ba" "#a##b##c###d"))
(run-test test-pq-3 (list "aa#bbc#cde#f"))
(run-test test-pq-4 (list "abg#e#feed#defa" "ab#cd#ef#g"))
