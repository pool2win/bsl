#lang racket/base

(provide double-sha256-hash generate-keypair hash160 keypair keypair-priv keypair-pub)

(require crypto crypto/libcrypto)

;; Resolve required MD implementations instead of leaving them to be auto resolved
(define ripemd160-impl (get-digest 'ripemd160 libcrypto-factory))
(define sha256-impl (get-digest 'sha256 libcrypto-factory))

;; I have seen people implement this with contracts and types
;; This is a reminder that these functions are not for prodcution use :)
(define (hash160 msg)
  (digest ripemd160-impl msg))


(define ec-impl (get-pk 'ec libcrypto-factory))
(get-digest 'sha256 libcrypto-factory)

(struct keypair (priv pub) #:transparent)

(define (generate-keypair)
  (let* ([privkey (generate-private-key ec-impl '((curve secp256k1)))]
         [pubkey (pk-key->public-only-key privkey)])
    (keypair privkey pubkey)))

;; bitcoin requires double sha256 hashes
(define (double-sha256-hash msg)
  (digest sha256-impl (digest sha256-impl msg)))
