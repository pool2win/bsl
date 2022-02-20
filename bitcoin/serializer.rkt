#lang racket/base

(provide digest-transaction-inputs)

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
