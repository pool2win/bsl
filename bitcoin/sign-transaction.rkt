#lang racket/base

(require crypto
         "../crypto-utils.rkt"
         "sighash.rkt"
         "transaction.rkt")

(provide transaction-sign)

(define (transaction-sign key tx index [sighash sighash-all])
  (let ([tx-digest (transaction-digest-for-signing tx sighash)])
    (pk-sign key tx-digest)))

;; skip running tests for this module
(module test racket/base)
