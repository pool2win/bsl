#lang racket/base

(provide hash160 keypair keypair-priv keypair-pub generate-keypair)

(require crypto crypto/libcrypto)

;; Resolve required MD implementations instead of leaving them to be auto resolved
(define ripemd160-impl (get-digest 'ripemd160 libcrypto-factory))

;; I have seen people implement this with contracts and types
;; This is a reminder that these functions are not for prodcution use :)
(define (hash160 msg)
  (digest ripemd160-impl msg))


(define ec-impl (get-pk 'ec libcrypto-factory))

(struct keypair (priv pub) #:transparent)

(define (generate-keypair)
  (let* ([privkey (generate-private-key ec-impl '((curve secp256k1)))]
         [pubkey (pk-key->public-only-key privkey)])
    (keypair privkey pubkey)))


