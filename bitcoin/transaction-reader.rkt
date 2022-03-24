#lang racket/base

(require file/sha1
         "transaction.rkt"
         "endian-helper.rkt")

(provide decode-transaction)

(module+ test
  (require rackunit))

(define (parse-version tx-bytes)
    (read-little-endian-n-bytes (subbytes tx-bytes 0 4)))

(define (decode-transaction tx-data)
  ;; version
  ;; inputs
  ;; segregated
  ;; inputs (segrated or not)
  ;; outputs
  ;; locktime
  (make-transaction #:version-number 1 #:flag 2 #:inputs '() #:outputs '() #:witnesses '() #:lock-time '()))

(module+ test
  (test-case
      "parse transaction from bytes"
    (let* ([tx-data "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
           [tx-bytes (hex-string->bytes tx-data)])
      (check-equal? (parse-version tx-bytes) 3307961488))))
