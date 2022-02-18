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

(test-case
    "Test bitcoin hash"
  (check-equal? (bytes->hex-string (double-sha256-hash #"Hello World!"))
                 "61f417374f4400b47dcae1a8f402d4f4dacf455a0442a06aa455a447b0d4e170"))


