#lang racket

;; ==========================================
;; Data Structures & Definitions
;; ==========================================
;; The defined constant for the control character. [cite: 35]
(define TRIGGER-CHAR #\#) 

;; A Tree is either a Leaf or a Node.
;; A Leaf is represented as: (list 'leaf char weight)
;; A Node is represented as: (list 'node left right weight)

(define (leaf? t) (eq? (car t) 'leaf))
(define (node? t) (eq? (car t) 'node))
(define (leaf-char t) (cadr t))
(define (node-left t) (cadr t))
(define (node-right t) (caddr t))
(define (weight t)
  (if (leaf? t)
      (caddr t)
      (cadddr t)))

;; ==========================================
;; Phase 1: The Static Foundation [cite: 25]
;; ==========================================

;; Inserts a tree node into a sorted priority queue (sorted by weight).
;; This fulfills the functional constraint of using a sorted list. [cite: 29]
(define (insert-pq tree pq)
  (cond
    [(empty? pq) (list tree)]
    [(<= (weight tree) (weight (car pq))) (cons tree pq)]
    [else (cons (car pq) (insert-pq tree (cdr pq)))]))

;; Builds the Huffman tree from a priority queue of leaves by taking the two lowest-weight nodes and combining them. [cite: 95]
(define (build-tree pq)
  (cond
    [(empty? pq) '()]
    [(= (length pq) 1) (car pq)]
    [else
     (let* ([left (car pq)]
            [right (cadr pq)]
            [rest-pq (cddr pq)]
            [new-weight (+ (weight left) (weight right))]
            [new-node (list 'node left right new-weight)])
       (build-tree (insert-pq new-node rest-pq)))]))

;; Traverses the tree to create a dictionary mapping characters to their binary codes. [cite: 28]
(define (build-dictionary tree)
  (define (traverse t current-bits)
    (if (leaf? t)
        (list (cons (leaf-char t) (reverse current-bits)))
        (append (traverse (node-left t) (cons 0 current-bits))  ; Left -> adds a 0 [cite: 109]
                (traverse (node-right t) (cons 1 current-bits))))) ; Right -> adds a 1 [cite: 110]
  (traverse tree '()))

(define (lookup char dict)
  (cond
    [(empty? dict) (error "Character not found in dictionary" char)]
    [(equal? (caar dict) char) (cdar dict)]
    [else (lookup char (cdr dict))]))

;; ==========================================
;; Phase 2: The Entropic Shift [cite: 32]
;; ==========================================

;; Recursively traverses the tree and swaps left and right branches at all nested levels. [cite: 33]
;; This explicit recursion handles the "deep mirror" requirement to prevent the shallow-swap AI trap. [cite: 34]
(define (mirror-tree tree)
  (if (leaf? tree)
      tree
      (list 'node
            (mirror-tree (node-right tree))
            (mirror-tree (node-left tree))
            (weight tree))))

;; ==========================================
;; Phase 3: The State-Aware Coder [cite: 36]
;; ==========================================

;; Encodes a list of characters into a list of bits, carrying the mode boolean state
;; Purely functional: state is passed as the 'mode' argument. 
(define (encode chars dict-normal dict-mirrored mode)
  (if (empty? chars)
      '()
      (let* ([c (car chars)]
             [current-dict (if mode dict-normal dict-mirrored)]
             [bits (lookup c current-dict)])
        (if (equal? c TRIGGER-CHAR)
            ;; If trigger char, encode with CURRENT mode, then flip mode exactly after processing the bits for the recursive call. [cite: 16, 39, 43]
            (append bits (encode (cdr chars) dict-normal dict-mirrored (not mode)))
            ;; Otherwise, keep current mode for subsequent characters.
            (append bits (encode (cdr chars) dict-normal dict-mirrored mode))))))

;; Decodes a list of bits into a list of characters, tracking current boolean state. 
(define (decode bits current-node root-normal root-mirrored mode)
  (cond
    [(leaf? current-node)
     (let ([c (leaf-char current-node)])
       (if (equal? c TRIGGER-CHAR)
           ;; Trigger hit: Yield character, then recursively call decode with the opposite mode and opposite root. [cite: 39]
           (cons c (if (empty? bits)
                       '()
                       (decode bits
                               (if (not mode) root-normal root-mirrored)
                               root-normal root-mirrored (not mode))))
           ;; Normal character hit: Yield character, recurse starting from current mode's root.
           (cons c (if (empty? bits)
                       '()
                       (decode bits
                               (if mode root-normal root-mirrored)
                               root-normal root-mirrored mode)))))]
    [(empty? bits) '()]
    [else
     (let* ([b (car bits)]
            [next-node (if (= b 0) (node-left current-node) (node-right current-node))])
       ;; Walk down the tree, maintaining current mode until a leaf is hit.
       (decode (cdr bits) next-node root-normal root-mirrored mode))]))

;; ==========================================
;; Main Entry Wrappers 
;; ==========================================

;; Takes a string and returns a list of bits. [cite: 30, 42]
(define (encode-string str tree)
  (let* ([chars (string->list str)]
         [dict-normal (build-dictionary tree)]
         [dict-mirrored (build-dictionary (mirror-tree tree))])
    (encode chars dict-normal dict-mirrored #t))) ; Initiates in "normal" (#t) mode.

;; Takes a list of bits and returns a decoded string. [cite: 31, 42]
(define (decode-bits bits tree)
  (let ([mirrored-tree (mirror-tree tree)])
    (list->string (decode bits tree tree mirrored-tree #t))))

;; ---------------------------------------------------------
;; TESTING LINE 1 FROM YOUR INPUT FILE
;; Data: a(3) b(4) c(5) #(6)
;; Strings to test: "abc#abc" and "a#b#c##ac"
;; ---------------------------------------------------------

;; 1. Create a sorted priority queue (lowest frequency first)
(define test-pq (list (list 'leaf #\a 3)
                      (list 'leaf #\b 4)
                      (list 'leaf #\c 5)
                      (list 'leaf #\# 6)))

;; 2. Build the Huffman Tree from the queue
(define test-tree (build-tree test-pq))

;; 3. Test Encoding the first string: "abc#abc"
(display "Encoding 'abc#abc': ")
(define encoded-result (encode-string "abc#abc" test-tree))
(displayln encoded-result)

;; 4. Test Decoding it back to a string
(display "Decoding those bits: ")
(displayln (decode-bits encoded-result test-tree))

;; 5. Test the second string: "a#b#c##ac"
(display "Encoding 'a#b#c##ac': ")
(define encoded-result-2 (encode-string "a#b#c##ac" test-tree))
(displayln encoded-result-2)

(display "Decoding those bits: ")
(displayln (decode-bits encoded-result-2 test-tree))
