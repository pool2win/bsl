#lang racket/base

(module+ test
  (require rackunit))

(require crypto
         "../crypto-utils.rkt"
         "serializer.rkt"
         "transaction.rkt")

(provide transaction-sign)

;; Ignore sighash for now, assume sighash all
(define (transaction-sign key tx index [sighash 'all])
  (let ([tx-digest (transaction-digest-for-sign tx sighash)])
    (pk-sign key tx-digest)))
