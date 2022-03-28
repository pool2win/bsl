#lang errortrace racket/base

(require file/sha1
         racket/stream
         "byte-reader.rkt"
         "transaction.rkt"
         "endian-helper.rkt"
         "varint-reader.rkt")

(provide decode-transaction)

(module+ test
  (require rackunit))

(define (parse-version tx-data-stream)
  (let-values ([(version-bytes rest) (next-n-bytes-and-rest tx-data-stream 4)])
    (values (read-little-endian-bytes version-bytes) rest)))

(define (parse-hash tx-data-stream)
  (let-values ([(hash-bytes rest) (next-n-bytes-and-rest tx-data-stream 32)])
    (values hash-bytes rest)))

(define (parse-point tx-data-stream)
  (let*-values ([(tx-hash rest) (parse-hash tx-data-stream)]
                [(index-bytes rest) (next-n-bytes-and-rest rest 4)]
                [(index) (read-little-endian-bytes index-bytes)])
    (values (outpoint tx-hash index) rest)))

(define (parse-script tx-data-stream)
  (let*-values ([(script-size tx-data-stream) (read-varint-value tx-data-stream)]
                [(script-bytes tx-data-stream) (next-n-bytes-and-rest tx-data-stream script-size)])
    (values script-bytes tx-data-stream)))

(define (parse-sequence tx-data-stream)
  (let*-values ([(sequence-bytes rest) (next-n-bytes-and-rest tx-data-stream 4)]
                [(sequence) (read-little-endian-bytes sequence-bytes)])
    (values sequence rest)))

(define (parse-input tx-data-stream)
  (let*-values ([(point rest) (parse-point tx-data-stream)]
                [(script rest) (parse-script rest)]
                [(sequence rest) (parse-sequence rest)])
    (values (make-input #:script script #:witness '() #:sequence sequence #:point point) rest)))

(define (parse-inputs num-inputs inputs tx-data-stream)
  (cond
    [(= 0 num-inputs) (values inputs tx-data-stream)]
    [else (let-values ([(input tx-data-stream) (parse-input tx-data-stream)])
            (parse-inputs (sub1 num-inputs) (append inputs (list input)) tx-data-stream))]))

(define (parse-outputs tx-data-stream)
  (values '() tx-data-stream))

(define (parse-locktime tx-data-stream)
  (values '() tx-data-stream))

(define (decode-transaction tx-data)
  ;; TODO: Use tx-data as a stream, so there is no need to track the 'byte locations' as we process the data
  ;; Explore using stream-take and for/stream etc functions
  (let*-values ([(tx-data-stream) (in-bytes tx-data)]
                [(version-number tx-data-stream) (parse-version tx-data-stream)]
                [(num-inputs tx-data-stream) (read-varint-value tx-data-stream)]
                [(inputs tx-data-stream) (parse-inputs num-inputs '() tx-data-stream)]
                [(outputs tx-data-stream) (parse-outputs tx-data-stream)]
                [(locktime tx-data-stream) (parse-locktime tx-data-stream)])
    (transaction version-number inputs outputs locktime)))

(module+ test
  (test-case
      "parse transaction from bytes"
    (let*-values ([(tx-data) "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
           [(tx-bytes) (hex-string->bytes tx-data)]
           [(tx-data-stream) (sequence->stream (in-bytes tx-bytes))]
           [(version-number tx-data-stream) (parse-version tx-data-stream)]
           [(num-inputs tx-data-stream) (read-varint-value tx-data-stream)]
           [(inputs tx-data-stream) (parse-inputs num-inputs '() tx-data-stream)])
      (check-equal? version-number 3307961488)
      (check-equal? num-inputs 3)
      (check-equal? inputs (list
                            (input '() '() 3717931005
                                   (outpoint
                                    #"\255\341\34\303\260N\262\221\213oT{\0060\253V\222s\202GH\310~\241K\6\226Rlf\272t" 2))
                            (input '() '() 1596654765
                                   (outpoint
                                    #"N\360s\307\257\304\256\0\332\212f\364)\311\27\240\b\32\321\341\332\274\342\2157>\253\201\330b\215\350" 2))
                            (input '() '() 4274166361
                                   (outpoint #"%\357\263;\356\311\3636N\212\2219\350C\235\235~&R\234<0\266\303\375\211\370hL\375h\352" 2)))))))
