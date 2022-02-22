#lang racket/base

(provide digest-transaction-inputs transaction-digest-for-sign)

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


(define (digest-transaction-inputs tx index [sighash 'all])
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (digest-outpoint (input-prevout input)))))

(module+ test
    (test-case
        "bitcoin hash prevouts for transaction inputs"
      (let* ([input1 (input '() '() 1234 (outpoint "deadbeef" 1))]
             [input2 (input '() '() 5678 (outpoint "deadbeff" 2))]
             [test-tx (transaction 1 1 (list input1 input2) '() '() 1)])
        (check-equal? (digest-transaction-inputs test-tx 1)
                      (bytes-append (hex->bytes "deadbeef")
                                    (integer->integer-bytes 1 4 #f #f)
                                    (hex->bytes "deadbeff")
                                    (integer->integer-bytes 2 4 #f #f)))
        (check-equal? (digest-input-sequences test-tx)
                      (bytes-append (integer->integer-bytes 1234 4 #f #f)
                                    (integer->integer-bytes 5678 4 #f #f))))))

(define (digest-input-sequences tx)
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (to-little-endian-4-bytes (input-sequence input)))))

;; Ignore sighash for now, assume sighash all
;; https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
(define (transaction-digest-for-sign tx index [sighash 'all])
  (bytes-append (to-little-endian-4-bytes (transaction-version-number tx))
                (double-sha256-hash (digest-transaction-inputs tx index))
                (double-sha256-hash (digest-input-sequences tx))
                (digest-outpoint (list-ref transaction-inputs index))
                (transaction-lock-time tx)
                (transaction-inputs tx)
                (transaction-outputs tx)))

(module+ test
  (test-case
      "digest outpoint"
    (check-equal? (digest-outpoint (outpoint "deadbeef" 1))
                  (bytes-append (hex->bytes "deadbeef")
                                (integer->integer-bytes 1 4 #f #f))))
    (test-case
        "digest prevouts for transaction inputs"
      (let* ([input1 (input '() '() 1234 (outpoint "deadbeef" 1))]
             [input2 (input '() '() 1234 (outpoint "deadbeff" 2))]
             )
        (check-equal? (digest-transaction-inputs (transaction 1 1 (list input1 input2) '() '() 1) 1)
                      (bytes-append (hex->bytes "deadbeef")
                                    (integer->integer-bytes 1 4 #f #f)
                                    (hex->bytes "deadbeff")
                                    (integer->integer-bytes 2 4 #f #f))))))
