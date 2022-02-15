#lang racket/base

(require crypto
         rackunit
         "crypto-utils.rkt")

(test-case
    "Verify key generation and sign/verify"
  (let ([test-keypair (generate-keypair)]
        [test-message #"test message"])
    (check-not-false (keypair-priv test-keypair))
    (check-not-false (keypair-pub test-keypair))
    (check-true (pk-verify (keypair-priv test-keypair) test-message
                           (pk-sign (keypair-priv test-keypair) test-message)))))
