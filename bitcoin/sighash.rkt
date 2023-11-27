#lang errortrace racket/base

(provide (all-defined-out))

(module+ test
  (require rackunit))

(require racket/bytes
         crypto
         "../crypto-utils.rkt"
         "endian-helper.rkt"
         "transaction.rkt")

(define (digest-outpoint point)
  (let ([index-size 4] [signed #f] [big-endian #f])
    (bytes-append (hex->bytes (outpoint-transaction-hash point))
                  (integer->integer-bytes (outpoint-index point) index-size signed big-endian))))

(module+ test
  (test-case "digest outpoint"
    (check-equal? (digest-outpoint (outpoint "deadbeef" 1))
                  (bytes-append (hex->bytes "deadbeef") (integer->integer-bytes 1 4 #f #f)))))

(define (digest-prevouts tx index [sighash 'all])
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (digest-outpoint (input-prevout input)))))

(define (digest-sequences tx)
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (to-little-endian-n-bytes (input-sequence input) 4))))

(module+ test
  (test-case "bitcoin hash prevouts for transaction inputs"
    (let* ([input1 (input '() '() 1234 (outpoint "deadbeef" 1))]
           [input2 (input '() '() 5678 (outpoint "deadbeff" 2))]
           [test-tx (make-transaction #:version-number 1
                                      #:flag 1
                                      #:inputs (list input1 input2)
                                      #:witnesses '()
                                      #:outputs '()
                                      #:lock-time 1)])
      (check-equal? (digest-prevouts test-tx 1)
                    (bytes-append (hex->bytes "deadbeef")
                                  (integer->integer-bytes 1 4 #f #f)
                                  (hex->bytes "deadbeff")
                                  (integer->integer-bytes 2 4 #f #f)))
      (check-equal? (digest-sequences test-tx)
                    (bytes-append (integer->integer-bytes 1234 4 #f #f)
                                  (integer->integer-bytes 5678 4 #f #f))))))

(define (serialize-version-number tx)
  (to-little-endian-n-bytes (transaction-version-number tx) 4))

(define (serialize-prevouts tx index)
  (double-sha256-hash (digest-prevouts tx index)))

(define (serialize-sequences tx)
  (double-sha256-hash (digest-sequences tx)))

(define (serialize-outpoint tx index)
  ;; depends on sighash?
  (digest-outpoint (input-prevout (list-ref (transaction-inputs tx) index))))

(define (serialize-amount amount)
  (to-little-endian-n-bytes amount 8))

(define (serialize-input-sequence tx index)
  (to-little-endian-n-bytes (input-sequence (list-ref (transaction-inputs tx) index)) 4))

(define (serialize-transaction-lock-time tx)
  (to-little-endian-n-bytes (transaction-lock-time tx) 4))

(define (serialize-sighash-type sighash)
  (to-little-endian-n-bytes sighash 4))

;; TODO - use codeseparator to split script that needs to be serialized
(define (serialize-script-code tx index sighash)
  '())

(define sighash-all #x00000001)
(define sighash-none #x00000002)
(define sighash-single #x00000003)
(define sighash-anyonecanpay #x00000080)

;; Ignore sighash for now, assume sighash all
;; https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
(define (transaction-digest-for-signing tx index amount [sighash sighash-all])
  (bytes-append (serialize-version-number tx)
                (serialize-prevouts tx index)
                (serialize-sequences tx)
                (serialize-outpoint tx index)
                (serialize-script-code tx index sighash)
                (serialize-amount amount) ;; often not known, and is therefore provided separately
                (serialize-input-sequence tx index)
                (serialize-transaction-lock-time tx)
                (serialize-sighash-type tx)))

(module+ test
  (test-case "transaction serialization for signing"
    (let* ([test-prevout (outpoint "deadbeef" 0)]
           [test-inputs (list (make-input #:script '(script elements)
                                          #:witness '()
                                          #:sequence 1
                                          #:prevout test-prevout))]
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
