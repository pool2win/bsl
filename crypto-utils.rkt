#lang racket/base

(module+ test
  (require rackunit))

(provide generate-keypair keypair keypair-priv keypair-pub
         ripemd160 hash160 sha1 sha256 double-sha256-hash)

(require crypto crypto/libcrypto)

;; Resolve required MD implementations instead of leaving them to be auto resolved
(define ripemd160-impl (get-digest 'ripemd160 libcrypto-factory))
(define sha1-impl (get-digest 'sha1 libcrypto-factory))
(define sha256-impl (get-digest 'sha256 libcrypto-factory))

;; I have seen people implement this with contracts and types
;; This is a reminder that these functions are not for prodcution use :)
(define (ripemd160 msg)
  (digest ripemd160-impl msg))

(define (hash160 msg)
  (ripemd160 (sha256 msg)))

(define (sha1 msg)
  (digest sha1-impl msg))

(define (sha256 msg)
  (digest sha256-impl msg))

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

(module+ test
  (test-case
      "Verify key generation and sign/verify"
    (let ([test-keypair (generate-keypair)]
          [test-message #"test message"])
      (check-not-false (keypair-priv test-keypair))
      (check-not-false (keypair-pub test-keypair))
      (check-true (pk-verify (keypair-priv test-keypair) test-message
                             (pk-sign (keypair-priv test-keypair) test-message)))))

  (test-case
      "Test bitcoin hash"
    (check-equal? (bytes->hex-string (double-sha256-hash #"Hello World!"))
                  "61f417374f4400b47dcae1a8f402d4f4dacf455a0442a06aa455a447b0d4e170")))
