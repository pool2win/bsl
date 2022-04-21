#lang errortrace racket/base

(require file/sha1
         "transaction.rkt"
         "endian-helper.rkt"
         "varint-reader.rkt")

(provide decode-transaction)

(module+ test
  (require rackunit))

(define (parse-version tx-port)
  (read-little-endian-bytes (read-bytes 4 tx-port)))

(define (parse-hash tx-port)
  (read-bytes 32 tx-port))

(define (parse-point tx-port)
  (let* ([tx-hash (parse-hash tx-port)]
         [index (read-little-endian-bytes (read-bytes 4 tx-port))])
    (outpoint tx-hash index)))

(define (parse-script tx-port)
  (read-bytes (read-varint-value tx-port) tx-port))

(define (parse-sequence tx-port)
  (read-little-endian-bytes (read-bytes 4 tx-port)))

(define (parse-input tx-port)
  (let* ([point (parse-point tx-port)]
         [script (parse-script tx-port)]
         [sequence (parse-sequence tx-port)])
    (make-input #:script script #:witness '() #:sequence sequence #:point point)))

(define (parse-inputs num-inputs inputs tx-port)
  (cond
    [(= 0 num-inputs) inputs]
    [else (let ([input (parse-input tx-port)])
            (parse-inputs (sub1 num-inputs) (append inputs (list input)) tx-port))]))

;; TODO - implement
(define (parse-outputs tx-port)
  '())

;; TODO - implement
(define (parse-locktime tx-port)
  '())

(define (decode-transaction tx-data)
  (let* ([tx-port (open-input-bytes tx-data)]
         [version-number (parse-version tx-port)]
         [num-inputs (read-varint-value tx-port)]
         [inputs (parse-inputs num-inputs '() tx-port)]
         [outputs (parse-outputs tx-port)]
         [locktime (parse-locktime tx-port)])
    (transaction version-number inputs outputs locktime)))

(module+ test
  (test-case
      "parse transaction from bytes"
    (let* ([tx-data "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
           [tx-bytes (hex-string->bytes tx-data)]
           [tx-port (open-input-bytes tx-bytes)]
           [version-number (parse-version tx-port)]
           [num-inputs (read-varint-value tx-port)]
           [inputs (parse-inputs num-inputs '() tx-port)])
      (check-equal? version-number 3307961488)
      (check-equal? num-inputs 3)
      (check-equal? inputs
                    (list
                     (input #"\253e\253\253" '() 3717931005
                            (outpoint
                             #"\255\341\34\303\260N\262\221\213oT{\0060\253V\222s\202GH\310~\241K\6\226Rlf\272t" 2))
                     (input #"j\253RS\253R\0\0R" '() 1596654765
                            (outpoint
                             #"N\360s\307\257\304\256\0\332\212f\364)\311\27\240\b\32\321\341\332\274\342\2157>\253\201\330b\215\350" 2))
                     (input #"\253SRe\0cjR\253" '() 4274166361
                            (outpoint #"%\357\263;\356\311\3636N\212\2219\350C\235\235~&R\234<0\266\303\375\211\370hL\375h\352" 2)))))))
