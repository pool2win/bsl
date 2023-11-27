#lang errortrace racket/base

(require file/sha1
         "endian-helper.rkt"
         "script-reader.rkt"
         "transaction.rkt"
         "varint-reader.rkt")

(provide decode-transaction)

(module+ test
  (require rackunit))

(define (parse-version tx-port)
  (read-little-endian-bytes (read-bytes 4 tx-port)))

(define (parse-hash tx-port)
  (read-bytes 32 tx-port))

(define (parse-point tx-port)
  (let* ([tx-hash (parse-hash tx-port)] [index (read-little-endian-bytes (read-bytes 4 tx-port))])
    (outpoint tx-hash index)))

(define (parse-script tx-port)
  (parse-script-from-bytes (read-bytes (read-varint-value tx-port) tx-port)))

(define (parse-sequence tx-port)
  (read-little-endian-bytes (read-bytes 4 tx-port)))

(define (parse-input tx-port)
  (let* ([prevout (parse-point tx-port)]
         [script (parse-script tx-port)]
         [sequence (parse-sequence tx-port)])
    (make-input #:script script #:witness '() #:sequence sequence #:prevout prevout)))

(define (parse-inputs num-inputs inputs tx-port)
  (cond
    [(= 0 num-inputs) inputs]
    [else
     (let ([input (parse-input tx-port)])
       (parse-inputs (sub1 num-inputs) (append inputs (list input)) tx-port))]))

(define (parse-output tx-port)
  (let* ([value (read-little-endian-bytes (read-bytes 8 tx-port))] [script (parse-script tx-port)])
    (make-output #:script script #:value value)))

(define (parse-outputs num-outputs outputs tx-port)
  (cond
    [(= 0 num-outputs) outputs]
    [else
     (let ([output (parse-output tx-port)])
       (parse-outputs (sub1 num-outputs) (append outputs (list output)) tx-port))]))

(define (parse-lock-time tx-port)
  (read-little-endian-bytes (read-bytes 4 tx-port)))

(define (decode-transaction tx-data)
  (let* ([tx-bytes (hex-string->bytes tx-data)]
         [tx-port (open-input-bytes tx-bytes)]
         [version-number (parse-version tx-port)]
         [num-inputs-or-marker (read-varint-value tx-port)]
         [segwit-flag (peek-byte tx-port)])
    (cond
      [(and (equal? num-inputs-or-marker #x00) (equal? segwit-flag #x01))
       (decode-witness-transaction tx-port version-number num-inputs-or-marker)]
      [else (decode-legacy-transaction tx-port version-number num-inputs-or-marker)])))

(define (decode-legacy-transaction tx-port version-number num-inputs)
  (let* ([inputs (parse-inputs num-inputs '() tx-port)]
         [num-outputs (read-varint-value tx-port)]
         [outputs (parse-outputs num-outputs '() tx-port)]
         [lock-time (parse-lock-time tx-port)])
    (make-transaction #:version-number version-number
                      #:flag 0
                      #:inputs inputs
                      #:witnesses '()
                      #:outputs outputs
                      #:lock-time lock-time)))

(define (decode-witness-transaction tx-port version-number marker)
  (let* ([skipped-flag-byte (read-bytes 1 tx-port)]
         [num-inputs (read-varint-value tx-port)]
         [inputs (parse-inputs num-inputs '() tx-port)]
         [num-outputs (read-varint-value tx-port)]
         [outputs (parse-outputs num-outputs '() tx-port)]
         [witnesses (parse-witnesses num-inputs '() tx-port)]
         [lock-time (parse-lock-time tx-port)])
    (make-transaction #:version-number version-number
                      #:flag 0
                      #:inputs inputs
                      #:witnesses witnesses
                      #:outputs outputs
                      #:lock-time lock-time)))

(define (parse-witnesses num-witnesses witnesses tx-port)
  (cond
    [(= 0 num-witnesses) witnesses]
    [else
     (let ([witness (parse-witness tx-port)])
       (parse-witnesses (sub1 num-witnesses) (append witnesses (list witness)) tx-port))]))

(define (parse-witness tx-port)
  (let ([num-items (read-varint-value tx-port)]) (parse-witness-data num-items '() tx-port)))

(define (parse-witness-data num-items witness-data tx-port)
  (cond
    [(= 0 num-items) witness-data]
    [else
     (let ([next-script-element (read-bytes (read-varint-value tx-port) tx-port)])
       (parse-witness-data (sub1 num-items)
                           (append witness-data (list next-script-element))
                           tx-port))]))

(module+ test
  (test-case "parse transaction from bytes"
    (let* ([tx-data
            "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
           [tx-bytes (hex-string->bytes tx-data)]
           [tx-port (open-input-bytes tx-bytes)]
           [version-number (parse-version tx-port)]
           [num-inputs (read-varint-value tx-port)]
           [inputs (parse-inputs num-inputs '() tx-port)]
           [num-outputs (read-varint-value tx-port)]
           [outputs (parse-outputs num-outputs '() tx-port)]
           [lock-time (parse-lock-time tx-port)])
      (check-equal? version-number 3307961488)
      (check-equal? num-inputs 3)
      (check-equal?
       inputs
       (list
        (input #"\253e\253\253"
               '()
               3717931005
               (outpoint
                #"\255\341\34\303\260N\262\221\213oT{\0060\253V\222s\202GH\310~\241K\6\226Rlf\272t"
                2))
        (input
         #"j\253RS\253R\0\0R"
         '()
         1596654765
         (outpoint
          #"N\360s\307\257\304\256\0\332\212f\364)\311\27\240\b\32\321\341\332\274\342\2157>\253\201\330b\215\350"
          2))
        (input
         #"\253SRe\0cjR\253"
         '()
         4274166361
         (outpoint
          #"%\357\263;\356\311\3636N\212\2219\350C\235\235~&R\234<0\266\303\375\211\370hL\375h\352"
          2))))
      (check-equal? num-outputs 2)
      (check-equal? outputs (list (output #"SS\0QcRQQ" 82650789) (output #"\0c\0" 17708900)))
      (check-equal? lock-time 700591787))
    (let* ([tx-data
            "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
           [tx (decode-transaction tx-data)])
      (check-equal? (transaction-version-number tx) 3307961488)
      (check-equal?
       (transaction-inputs tx)
       (list
        (input #"\253e\253\253"
               '()
               3717931005
               (outpoint
                #"\255\341\34\303\260N\262\221\213oT{\0060\253V\222s\202GH\310~\241K\6\226Rlf\272t"
                2))
        (input
         #"j\253RS\253R\0\0R"
         '()
         1596654765
         (outpoint
          #"N\360s\307\257\304\256\0\332\212f\364)\311\27\240\b\32\321\341\332\274\342\2157>\253\201\330b\215\350"
          2))
        (input
         #"\253SRe\0cjR\253"
         '()
         4274166361
         (outpoint
          #"%\357\263;\356\311\3636N\212\2219\350C\235\235~&R\234<0\266\303\375\211\370hL\375h\352"
          2))))
      (check-equal? (transaction-outputs tx)
                    (list (output #"SS\0QcRQQ" 82650789) (output #"\0c\0" 17708900)))
      (check-equal? (transaction-lock-time tx) 700591787))))

(module+ test
  (test-case "parse segwit transaction from bytes Native P2WPKH from BIP 143"
    (let* ([tx-data
            "01000000000102fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f00000000494830450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac000247304402203609e17b84f6a7d30c80bfa610b5b4542f32a8a0d5447a12fb1366d7f01cc44a0220573a954c4518331561406f90300e8f3358f51928d43c212a8caed02de67eebee0121025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee635711000000"]
           [tx (decode-transaction tx-data)])
      (check-equal? 2 (length (transaction-inputs tx)))
      (check-equal? '() (input-witness (list-ref (transaction-inputs tx) 0)))
      (check-equal?
       "304402203609e17b84f6a7d30c80bfa610b5b4542f32a8a0d5447a12fb1366d7f01cc44a0220573a954c4518331561406f90300e8f3358f51928d43c212a8caed02de67eebee01"
       (bytes->hex-string (list-ref (input-witness (list-ref (transaction-inputs tx) 1)) 0)))
      (check-equal? "025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee6357"
                    (bytes->hex-string (list-ref (input-witness (list-ref (transaction-inputs tx) 1))
                                                 1))))))

(module+ test
  (test-case "parse segwit transaction from bytes P2SH-P2WPKH from BIP 143"
    (let* ([tx-data
            "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000"]
           [tx (decode-transaction tx-data)])
      (check-equal? 1 (length (transaction-inputs tx)))
      (check-equal?
       "3044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb01"
       (bytes->hex-string (list-ref (input-witness (list-ref (transaction-inputs tx) 0)) 0)))
      (check-equal? "03ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873"
                    (bytes->hex-string (list-ref (input-witness (list-ref (transaction-inputs tx) 0))
                                                 1))))))

(module+ test
  (test-case "parse legacy transaction from tx_valid data set"
    (let* ([tx-data
            "010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000"]
           [tx (decode-transaction tx-data)])
      (check-equal? 1 (length (transaction-inputs tx)))
      (check-equal? #"\0\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
                    (outpoint-transaction-hash (input-prevout (list-ref (transaction-inputs tx) 0))))
      (check-equal? 0 (outpoint-index (input-prevout (list-ref (transaction-inputs tx) 0)))))))
