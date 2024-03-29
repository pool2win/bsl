#lang racket/base

(provide (struct-out outpoint)
         (struct-out input)
         (struct-out output)
         (struct-out transaction)
         make-transaction
         make-input
         make-output)

;; transaction-hash is little endian
(struct outpoint (transaction-hash index) #:transparent)

(struct input (script [witness #:mutable] sequence prevout) #:transparent)
;; make-input defined to take keyword args and call the constructor
(define (make-input #:script script #:witness witness #:sequence sequence #:prevout prevout)
  (input script witness sequence prevout))

(struct output (script value) #:transparent)
;; make-output defined to take keyword args and call the constructor
(define (make-output #:script script #:value value)
  (output script value))

(struct transaction (version-number flag inputs outputs lock-time) #:transparent)

;; make-transaction defined to take keyword args and call the constructor
(define (make-transaction #:version-number version-number
                          #:flag flag
                          #:lock-time lock-time
                          #:inputs inputs
                          #:witnesses [witnesses '()]
                          #:outputs outputs)
  (for/list ([witness witnesses] [input inputs])
    (set-input-witness! input witness))
  (transaction version-number flag inputs outputs lock-time))
