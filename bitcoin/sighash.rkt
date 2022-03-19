#lang racket/base

(provide transaction-digest-for-signing
         sighash-all sighash-none sighash-single sighash-anyonecanpay)

(module+ test
  (require rackunit))

(require racket/bytes
         crypto
         "../crypto-utils.rkt"
         "endian-helper.rkt"
         "transaction.rkt")

(define (digest-outpoint point)
  (let ([index-size 4]
        [signed #f]
        [big-endian #f])
    (bytes-append (hex->bytes (outpoint-transaction-hash point))
                  (integer->integer-bytes (outpoint-index point) index-size signed big-endian))))

(module+ test
  (test-case
      "digest outpoint"
    (check-equal? (digest-outpoint (outpoint "deadbeef" 1))
                  (bytes-append (hex->bytes "deadbeef")
                                (integer->integer-bytes 1 4 #f #f)))))

(define (digest-prevouts tx index [sighash 'all])
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (digest-outpoint (input-prevout input)))))

(define (digest-sequences tx)
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (to-little-endian-4-bytes (input-sequence input)))))

(module+ test
    (test-case
        "bitcoin hash prevouts for transaction inputs"
      (let* ([input1 (input '() '() 1234 (outpoint "deadbeef" 1))]
             [input2 (input '() '() 5678 (outpoint "deadbeff" 2))]
             [test-tx (transaction 1 1 (list input1 input2) '() '() 1)])
        (check-equal? (digest-prevouts test-tx 1)
                      (bytes-append (hex->bytes "deadbeef")
                                    (integer->integer-bytes 1 4 #f #f)
                                    (hex->bytes "deadbeff")
                                    (integer->integer-bytes 2 4 #f #f)))
        (check-equal? (digest-sequences test-tx)
                      (bytes-append (integer->integer-bytes 1234 4 #f #f)
                                    (integer->integer-bytes 5678 4 #f #f))))))

(define (serialize-version-number tx)
  (to-little-endian-4-bytes (transaction-version-number tx)))

(define (serialize-prevouts tx index)
  (double-sha256-hash (digest-prevouts tx index)))

(define (serialize-sequences tx)
  (double-sha256-hash (digest-sequences tx)))

(define (serialize-outpoint tx index)
   ;; depends on sighash?
  (digest-outpoint (input-prevout (list-ref (transaction-inputs tx) index))))

(define (serialize-script-code tx ix)
  '())

(define sighash-all  #x00000001)
(define sighash-none  #x00000002)
(define sighash-single  #x00000003)
(define sighash-anyonecanpay  #x00000080)

;; Ignore sighash for now, assume sighash all
;; https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
(define (transaction-digest-for-signing tx index [sighash sighash-all])
  (bytes-append (serialize-version-number tx)
                (serialize-prevouts tx index)
                (serialize-sequences tx)
                (serialize-outpoint tx index)
                (serialize-script-code tx)
                (transaction-lock-time tx) ;; TODO
                (transaction-inputs tx) ;; TODO
                (transaction-outputs tx) ;; TODO
                ))

(module+ test
(test-case
    "transaction serialization for signing"
  (let* ([test-prevout (outpoint "deadbeef" 0)]
         [test-inputs (list (make-input #:script '("script" "elements") #:witness '()
                                        #:sequence 1 #:prevout test-prevout))]
         [test-outputs (list (output '("script" "elements") 100))]
         [test-transaction (make-transaction #:version-number 1
                                             #:flag 0
                                             #:inputs test-inputs
                                             #:outputs test-outputs
                                             #:witnesses '()
                                             #:lock-time '())])
    (check-equal? (serialize-version-number test-transaction) (to-little-endian-4-bytes 1))
    (check-equal? (serialize-prevouts test-transaction 0) (double-sha256-hash (digest-prevouts test-transaction 0)))
    (check-equal? (serialize-sequences test-transaction) (double-sha256-hash (digest-sequences test-transaction)))
    (check-equal? (serialize-outpoint test-transaction 0) (digest-outpoint test-prevout))
    )))
