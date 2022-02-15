;; Everything in this file/module can be converted into a macro and then into a DSL

#lang racket/base
(require racket/match crypto crypto/libcrypto)
(require ec)

(define ec-impl (get-pk 'ec libcrypto-factory))

;; Define alice private and public keys
(define alice:privkey (generate-private-key ec-impl '((curve secp256k1))))
(define alice:pubkey (pk-key->public-only-key alice:privkey))


;; Define a struct to represent UTXOs
(struct utxo (block_height status input-script output-script) #:mutable #:transparent)

;; A constant to define all coinbase outputs
(define coinbase null)

;; Define alice's coins earned in a coinbase output
(define alice-coins (utxo 0 #t coinbase `(p2pkh ,alice:pubkey)))


;; Validate that alice can spend alice-coins. This will be the beginning of a contracts language.
;; It all comes down to if a UTXO is spendable.
;; (define (validate-signed output-script pubkey)
;;   (match (car output-script)
;;   ['p2pkh (validate-p2pkh output-script pubkey)]))

;; Validate a p2pkh output is signed by the matching key
;; (define (validate-p2pkh output-script pubkey script-sig)
;;   (pk-verify pubkey output-script script-sig))


;; (validate-signed (utxo-output-script alice-coins) alice:pubkey)

