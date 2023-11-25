#lang racket/base

(require "sighash.rkt"
         "transaction.rkt"
         "endian-helper.rkt"
         "../crypto-utils.rkt")

(module+ test
  (require rackunit))

(module+ test
  (test-case "transaction serialization for signing"
    (let* ([test-prevout (outpoint "deadbeef" 0)]
           [test-inputs (list (make-input #:script '(script elements)
                                          #:witness '()
                                          #:sequence 1
                                          #:point test-prevout))]
           [test-outputs (list (output '(script elements) 100))]
           [test-transaction (make-transaction #:version-number 1
                                               #:flag 0
                                               #:inputs test-inputs
                                               #:outputs test-outputs
                                               #:witnesses '()
                                               #:lock-time 500001)])
      (check-equal? (serialize-version-number test-transaction) (to-little-endian-n-bytes 1 4))
      (check-equal? (serialize-prevouts test-transaction 0)
                    (double-sha256-hash (digest-prevouts test-transaction 0)))
      (check-equal? (serialize-sequences test-transaction)
                    (double-sha256-hash (digest-sequences test-transaction)))
      (check-equal? (serialize-outpoint test-transaction 0) (digest-outpoint test-prevout))
      ;; (check-equal? (serialize-script-code test-transaction 0)
      ;;               (digest-outpoint (input-script test-prevout))) ;; TODO correct this
      (check-equal? (serialize-amount 100) (to-little-endian-n-bytes 100 8))
      (check-equal? (serialize-input-sequence test-transaction 0) (to-little-endian-n-bytes 1 4))
      (check-equal? (serialize-transaction-lock-time test-transaction)
                    (to-little-endian-n-bytes 500001 4))
      (check-equal? (serialize-sighash-type sighash-all) (to-little-endian-n-bytes #x00000001 4)))))
