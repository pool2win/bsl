#lang racket/base

(require rackunit
         file/sha1
         "transaction-reader.rkt"
         "transaction.rkt"
         "endian-helper.rkt")

(test-case "make a transaction struct from bytes"
  (let* ([tx-data
          "907c2bc503ade11cc3b04eb2918b6f547b0630ab569273824748c87ea14b0696526c66ba740200000004ab65ababfd1f9bdd4ef073c7afc4ae00da8a66f429c917a0081ad1e1dabce28d373eab81d8628de802000000096aab5253ab52000052ad042b5f25efb33beec9f3364e8a9139e8439d9d7e26529c3c30b6c3fd89f8684cfd68ea0200000009ab53526500636a52ab599ac2fe02a526ed040000000008535300516352515164370e010000000003006300ab2ec229"]
         [decoded (decode-transaction tx-data)])
    (check-equal? (transaction-version-number decoded) 3307961488)
    (check-equal?
     (list-ref (transaction-inputs decoded) 0)
     (input '(171 #"\253\253")
            '()
            3717931005
            (outpoint
             #"\255\341\34\303\260N\262\221\213oT{\0060\253V\222s\202GH\310~\241K\6\226Rlf\272t"
             2)))
    (check-equal?
     (list-ref (transaction-inputs decoded) 1)
     (input
      '(106 171 82 83 171 82 0 0 82)
      '()
      1596654765
      (outpoint
       #"N\360s\307\257\304\256\0\332\212f\364)\311\27\240\b\32\321\341\332\274\342\2157>\253\201\330b\215\350"
       2)))
    (check-equal?
     (list-ref (transaction-inputs decoded) 2)
     (input '(171 83 82 #"\0cjR\253")
            '()
            4274166361
            (outpoint
             #"%\357\263;\356\311\3636N\212\2219\350C\235\235~&R\234<0\266\303\375\211\370hL\375h\352"
             2)))
    (check-equal? (list-ref (transaction-outputs decoded) 0)
                  (output '(83 83 0 81 99 82 81 81) 82650789))
    (check-equal? (list-ref (transaction-outputs decoded) 1) (output '(0 99 0) 17708900))
    (check-equal? (transaction-lock-time decoded) 700591787)))

(test-case "Native P2WPKH - unsigned tx example from bip0143"
  (let* ([tx-data
          "0100000002fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f0000000000eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac11000000"]
         [decoded (decode-transaction tx-data)])
    (check-equal? (transaction-version-number decoded) 1) ;; 01000000 in little endiain is 1
    (check-equal? (length (transaction-inputs decoded)) 2)
    (let* ([inputs (transaction-inputs decoded)] [s (list-ref inputs 0)] [se (list-ref inputs 1)])
      (check-equal? (input-sequence s) (read-little-endian-bytes (hex-string->bytes "eeffffff")))
      (check-equal? (outpoint-transaction-hash (input-prevout s))
                    (hex-string->bytes
                     "fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f"))
      (check-equal? (outpoint-index (input-prevout s)) 0)
      (check-equal? (input-witness s) '())
      (check-equal? (input-script s) '())

      (check-equal? (input-sequence se) (read-little-endian-bytes (hex-string->bytes "ffffffff")))
      (check-equal? (outpoint-transaction-hash (input-prevout se))
                    (hex-string->bytes
                     "ef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a"))
      (check-equal? (outpoint-index (input-prevout se)) 1)
      (check-equal? (input-witness se) '())
      (check-equal? (input-script se) '()))
    (let* ([outputs (transaction-outputs decoded)] [s (list-ref outputs 0)] [se (list-ref outputs 1)])
      (check-equal? (output-script s)
                    `(#x76 #xa9
                           #x14
                           ,(hex-string->bytes "8280b37df378db99f66f85c95a783a76ac7a6d59")
                           #x88
                           #xac)) ;; drop varint prefix, 19
      (check-equal? (output-value s) (read-little-endian-hex-string "202cb20600000000"))

      (check-equal? (output-script se)
                    `(#x76 #xa9
                           #x14
                           ,(hex-string->bytes "3bde42dbee7e4dbe6a21b2d50ce2f0167faa8159")
                           #x88
                           #xac)) ;; drop varint prefix, 19
      (check-equal? (output-value se) (read-little-endian-hex-string "9093510d00000000")))))

;; (test-case
;;     "Native P2WPKH - unsigned tx example from bip0143 - using macro"
;;   (let* ([tx-data "0100000002fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f0000000000eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac11000000"]
;;          [decoded (decode-transaction tx-data)])
;;     (check-transaction-from-parts? decoded
;;                                    (version-number 1)
;;                                    (inputs (([sequence "eeffffff"]
;;                                              [prevout ("fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f" 0)]
;;                                              [witness '()]
;;                                              [script #""])
;;                                             ([sequence "ffffffff"]
;;                                              [prevout ("ef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a" 1)]
;;                                              [witness '()]
;;                                              [script #""])
;;                                             )))))
;;   ;; (outputs (([script "76a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac"]
;;   ;;            [value "202cb20600000000"])
;;   ;;           ([script "76a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac"]
;;   ;;            [value "9093510d00000000"]))))))

(test-case "Native P2WPKH - signed tx example from bip0143"
  (let* ([tx-data
          "01000000000102fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f00000000494830450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01eeffffffef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a0100000000ffffffff02202cb206000000001976a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac9093510d000000001976a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac000247304402203609e17b84f6a7d30c80bfa610b5b4542f32a8a0d5447a12fb1366d7f01cc44a0220573a954c4518331561406f90300e8f3358f51928d43c212a8caed02de67eebee0121025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee635711000000"]
         [decoded (decode-transaction tx-data)])
    (check-equal? (transaction-version-number decoded) 1) ;; 01000000 in little endiain is 1
    (check-equal? (length (transaction-inputs decoded)) 2)
    (let* ([inputs (transaction-inputs decoded)] [s (list-ref inputs 0)] [se (list-ref inputs 1)])
      (check-equal? (input-sequence s) (read-little-endian-bytes (hex-string->bytes "eeffffff")))
      (check-equal? (outpoint-transaction-hash (input-prevout s))
                    (hex-string->bytes
                     "fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f"))
      (check-equal? (outpoint-index (input-prevout s)) 0)
      (check-equal? (input-witness s) '())
      (check-equal?
       (input-script s)
       `(#x48
         ,(hex-string->bytes
           "30450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01")))

      (check-equal? (input-sequence se) (read-little-endian-bytes (hex-string->bytes "ffffffff")))
      (check-equal? (outpoint-transaction-hash (input-prevout se))
                    (hex-string->bytes
                     "ef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a"))
      (check-equal? (outpoint-index (input-prevout se)) 1)
      (check-equal? (length (input-witness se)) 2)
      (check-equal? (input-script se) '()))
    (let* ([outputs (transaction-outputs decoded)] [s (list-ref outputs 0)] [se (list-ref outputs 1)])
      (check-equal? (output-script s)
                    `(#x76 #xa9
                           #x14
                           ,(hex-string->bytes "8280b37df378db99f66f85c95a783a76ac7a6d59")
                           #x88
                           #xac)) ;; drop varint prefix, 19
      (check-equal? (output-value s) (read-little-endian-hex-string "202cb20600000000"))

      (check-equal? (output-script se)
                    `(#x76 #xa9
                           #x14
                           ,(hex-string->bytes "3bde42dbee7e4dbe6a21b2d50ce2f0167faa8159")
                           #x88
                           #xac)) ;; drop varint prefix, 19
      (check-equal? (output-value se) (read-little-endian-hex-string "9093510d00000000")))))
