#lang racket/base

(require crypto
         "../crypto-utils.rkt"
         "sighash.rkt"
         "transaction.rkt")

(provide sign-transaction-for-input)

(define (sign-transaction-for-input #:key key #:transaction tx #:input-index index
                                    #:amount amount #:sighash [sighash sighash-all])
  (let ([tx-digest (transaction-digest-for-signing tx index amount sighash)])
    (pk-sign key tx-digest)))

;; skip running tests for this module
(module test racket/base)
