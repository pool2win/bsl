#lang racket/base

(module+ test
  (require rackunit))

(require crypto
         "../crypto-utils.rkt"
         "serializer.rkt"
         "transaction.rkt")

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
