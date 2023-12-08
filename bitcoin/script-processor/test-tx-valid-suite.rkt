#lang errortrace racket

(require json
         "environment.rkt"
         "bitcoin-environment.rkt"
         "eval-script.rkt"
         "../transaction.rkt"
         "../transaction-reader.rkt"
         "../../crypto-utils.rkt")

(module+ test
  (require rackunit))


(module+ test
  (test-case
      "run tx valid cases"
    (let ([bitcoin-env (make-bitcoin-environment)]
          [tx (decode-transaction "010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000")])
      (check-equal? #"\0\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
                    (outpoint-transaction-hash (input-prevout (list-ref (transaction-inputs tx) 0))))
      (check-equal? 0 (outpoint-index (input-prevout (list-ref (transaction-inputs tx) 0))))
      (check-equal? '() (input-script (list-ref (transaction-inputs tx) 0)))
      )))
