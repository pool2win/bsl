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

(struct input (script witness sequence point) #:transparent)
;; make-input defined to take keyword args and call the constructor
(define (make-input #:script script #:witness witness #:sequence sequence #:point point)
  (input script witness sequence point))

(struct output (script value) #:transparent)
;; make-output defined to take keyword args and call the constructor
(define (make-output #:script script #:value value)
  (output script value))

(struct transaction (version-number flag inputs outputs lock-time) #:transparent)

;; make-transaction defined to take keyword args and call the constructor
(define (make-transaction #:version-number version-number #:flag flag
                          #:inputs inputs #:outputs outputs #:lock-time lock-time)
  (transaction version-number flag inputs outputs lock-time))
