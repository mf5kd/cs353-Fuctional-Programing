#lang racket
; cost = total-file-size + (depth-level * 10)
; when compressed cost = (total-file-size / 2) + (depth-level * 10)
; file format '(root (file1 100) (file2 100) -> 0 
;               (folder1 (file1 100) (file2 100)) -> 1
;               (@compressed-folder1 (file1 100) (file2 100))) -> 2

(define test-map '(root
                    (file1 100) (file2 100)
                    (folder1
                      (file3 100) (file4 100))
                    (@compressed-folder1
                      (file5 100) (file6 100)))) ; -> 120 = 540

(define test-map-2 '(root
                      (file1 100) (file2 100) (file3 100)
                      (folder1
                        (file4 100)
                        (folder2
                          (file5 100) (file6 100))))) ; -> 650

(define test-map-3 '(root
                      (file1 100) (file2 100)
                      (folder1
                        (file3 100) (file4 100)
                        (@compressed-folder1
                          (file5 200)
                          (@folder2 (file6 200)))))) ; -> 230 = 770



(define (is-file? lst) ; -> boolean
  (and 
     (list? lst)
     (equal? (length lst) 2)
     (number? (second lst))))



(define (is-folder? lst) ; - boolean
  (and 
     (list? lst)
     (or 
       (symbol? (first lst))
       (string? (first lst)))))



(define (valid-disk? disk-map) ; -> integer
  (cond
    [(not (is-folder? disk-map)) #f]
    [(is-file? disk-map) #t]
    [else 
        (let ([next (rest disk-map)])
        (not (and (member #f (flatten (map valid-disk? next))) #t)))])) 



(define (count-files disk-map) ; -> integer
  (cond
    [(is-file? disk-map) 1]
    [else 
        (let ([next (rest disk-map)])
          (apply + (map count-files next)))]))



(define (find-file filename disk-map) ; -> boolean
  (let ([current (first disk-map)]
        [next (rest disk-map)])
    (cond 
      [(and (equal? current filename) (is-file? disk-map)) #t]
      [(is-file? disk-map) #f]
      [else (ormap (λ (x) (find-file filename x)) next)])))



(define (total-cost disk-map [depth -1] [is-compressed? #f]) ; -> integer
  (cond
    [(is-file? disk-map) (let ([cost (+ (second disk-map) (* depth 10))]
                               [compress-cost (+ (/ (second disk-map) 2) (* depth 10))])
                           (if is-compressed? compress-cost
                               cost))]
    [else 
        (let ([first-char (first (string->list (symbol->string (first disk-map))))]
              [next (rest disk-map)])
          (let ([compress (if (equal? first-char #\@) (not is-compressed?) is-compressed?)])

           (apply + (map (λ (x) (total-cost x (+ depth 1) compress)) next))))]))



(define (list-heavy-files threshold disk-map [depth -1] [is-compressed? #f]) ; -> list-of-names
  (cond
    [(is-file? disk-map) (let ([cost (+ (second disk-map) (* depth 10))]
                               [compress-cost (+ (/ (second disk-map) 2) (* depth 10))])
                           (if is-compressed? (if (> compress-cost threshold) (first disk-map) #f)
                               (if (> cost threshold) (first disk-map) #f)))]
    [else 
        (let ([first-char (first (string->list (symbol->string (first disk-map))))]
              [next (rest disk-map)])
          (let ([compress (if (equal? first-char #\@) (not is-compressed?) is-compressed?)])
           (remove* (list #f) (flatten (map (λ (x) (list-heavy-files threshold x (+ depth 1) compress)) next)))))]))



(define (display-tree disk-map [indent ""])
  (cond
    [(is-file? disk-map) (write disk-map)]
    [else (newline) (display indent)  (writeln (first disk-map)) (display indent) 
        (let ([next (rest disk-map)])
         (display "  ") (map (λ (x) (display-tree x (string-append indent "  "))) next)) (display "")]))


(display-tree test-map)
(newline)
(display-tree test-map-2)
(newline)
(display-tree test-map-3)

;; 1. The "Base Case" Disk: Only one file at the root.
(define disk-tiny
  '(root (readme.txt 50)))
;; Expected Total Cost: 50 (Depth 0)

;; 2. The "Linear" Disk: Tests depth accumulation without branching.
(define disk-deep
  '(level0
    (level1
     (level2
      (deep-file.txt 100)))))
;; Expected Total Cost: 120 (100 + 20 for depth 2)

;; 3. The "Compression" Disk: Tests if @ correctly triggers and stays active.
(define disk-compressed
  '(root
    (@archive
     (file-a.zip 1000)
     (subfolder
      (file-b.txt 200)))))
;; Expected Total Cost: 630
;; Logic: (1000/2 + 10) + (200/2 + 20) = 510 + 120

;; 4. The "Mixed" Disk: Tests that @ only affects its OWN branch.
(define disk-mixed
  '(root
    (@private
     (secret.vault 100))  ; Should be compressed
    (public
      (post.txt 100))))    ; Should NOT be compressed
;; Expected Total Cost: 170
;; Logic: (100/2 + 10) + (100 + 10) = 60 + 110

;; 5. The "Grand Audit" Disk
;; Rules tested:
;; - Nested Latency (Depth increases at every sub-folder)
;; - The Switch Rule: A folder name starting with @ toggles
;;   the current compression state (ON to OFF, or OFF to ON).

(define disk-complex
  '(root
    (system.sys 500)
    (@archive
     (photo.jpg 400)
     (@backups
      (notes.txt 600)
      (old-logs.zip 200)))
    (users
     (profile.png 100))))
; Expected total cost: 1660
; Heavy files (> 499): '(system.sys notes.txt)

(valid-disk? disk-tiny) (count-files disk-tiny)
(valid-disk? disk-deep)
(valid-disk? disk-compressed)
(valid-disk? disk-mixed)
(newline)
(valid-disk? disk-complex) (find-file 'old-logs.zip disk-complex) (list-heavy-files 200 disk-complex)
(display-tree disk-complex)
(newline)
(total-cost disk-complex)
