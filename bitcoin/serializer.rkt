#lang racket/base

(provide digest-transaction-inputs transaction-digest-for-sign)

(module+ test
  (require rackunit))

(require racket/bytes
         crypto
         "../crypto-utils.rkt"
         "transaction.rkt")

(define (digest-outpoint point)
  (let ([index-size 4]
        [signed #f]
        [big-endian #f])
    (bytes-append (hex->bytes (outpoint-transaction-hash point))
                  (integer->integer-bytes (outpoint-index point) index-size signed big-endian))))

(define (hash-digest-outpoint point)
  (double-sha256-hash (digest-outpoint point)))

(define (digest-transaction-inputs tx index [sighash 'all])
  (bytes-append* (for/list ([input (transaction-inputs tx)])
                   (digest-outpoint (input-prevout input)))))

(define (hash-transaction-inputs tx index [sighash 'all])
  (double-sha256-hash (digest-transaction-inputs tx index sighash)))

(module+ test
    (test-case
        "bitcoin hash prevouts for transaction inputs"
      (let* ([input1 (input '() '() 1234 (outpoint "deadbeef" 1))]
             [input2 (input '() '() 1234 (outpoint "deadbeff" 2))]
             )
        (check-equal? (hash-transaction-inputs (transaction 1 1 (list input1 input2) '() '() 1) 1)
                      (double-sha256-hash (bytes-append (hex->bytes "deadbeef")
                                                        (integer->integer-bytes 1 4 #f #f)
                                                        (hex->bytes "deadbeff")
                                                        (integer->integer-bytes 2 4 #f #f)))))))

;; Ignore sighash for now, assume sighash all
;; https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
(define (transaction-digest-for-sign tx index [sighash 'all])
  (let ([signed #t]
        [unsigned #f]
        [big-endian #t]
        [little-endian #f])
    (define (digest-transaction-version tx)
      (integer->integer-bytes (transaction-version-number tx) 4 unsigned little-endian))
    (bytes-append (digest-transaction-version tx)
                  (hash-transaction-inputs tx index)
                  (transaction-lock-time tx)
                  (transaction-inputs tx)
                  (transaction-outputs tx))))

(module+ test
  (test-case
      "digest outpoint"
    (check-equal? (digest-outpoint (outpoint "deadbeef" 1))
                  (bytes-append (hex->bytes "deadbeef")
                                (integer->integer-bytes 1 4 #f #f))))
    (test-case
      "bitcoin hash outpoint"
    (check-equal? (hash-digest-outpoint (outpoint "deadbeef" 1))
                  (double-sha256-hash (bytes-append (hex->bytes "deadbeef")
                                                    (integer->integer-bytes 1 4 #f #f)))))

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
