#lang racket

(struct packet (id playload port security-level history) #:transparent)
(struct success (val) #:transparent)
(struct failure (reason) #:transparent)

(define (firewall p)
  (cond 
    [(equal? (packet-port p) 80) (success p)]
    [(equal? (packet-port p) 443) (success p)]
    [else (failure 'port-blocked)]))

(define (proxy p) (displayln "got to proxy") (displayln (packet-history p)); NOTE DEBUG
  (cond
    [(member '(vault) (packet-history p)) (failure 'zero-trust-violation)]
    [else (success p)]))

(define (terminal p)
  (cond
    [(< (packet-security-level p) 2) (failure 'insufficient-clearance)]
    [else (success p)]))

(define (vpn p)
  (define pack (struct-copy packet p [playload (string-append "ENC-" (packet-playload p))] [security-level (+ 1 (packet-security-level p))]))
  (success pack))

(define (vault p) (displayln "got to vault") ; NOTE DEBUG
  (define pack (struct-copy packet p [security-level (+ 1 (packet-security-level p))] [history (flatten (append (packet-history p) 'vault))]))
  (success pack))

(define (get-node node-name)
  (match node-name
    ['firewall firewall]
    ['proxy proxy]
    ['terminal terminal]
    ['vpn vpn]
    ['vault vault]))

(define (chain-state node-name current-stateful-result)
  (let ([result (car current-stateful-result)]
         [logg (flatten (cons (cdr current-stateful-result) node-name))]
         [node-fuction (get-node node-name)])
    (displayln (flatten logg)) ; NOTE DEBUG
    (displayln node-fuction) ; NOTE DEBUG
    (cond
      [(success? result) (displayln "success!!!") (cons (node-fuction (success-val result)) (list logg))] ; NOTE DEBUG
      [else (displayln "failure :(") (cons result logg)]))) ; NOTE DEBUG

(define (navigate state path)
  (foldl chain-state state path))

(define packet-1 (packet 2 "data" 443 0 '(home)))
(define init-packet-1 (cons (success packet-1) 'home))
(define packet-2 (packet 2 "secrect" 80 0 '(home)))
(define init-packet-2 (cons (success packet-2) 'home))

(navigate init-packet-1 '(vpn vault terminal))
(displayln "----")
(navigate init-packet-2 '(vault proxy vault))


