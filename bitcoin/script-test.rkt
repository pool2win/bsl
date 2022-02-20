#lang racket/base

(require rackunit
         "script.rkt"
         "../crypto-utils.rkt")


(test-case
    "Test p2pkh stack verfication"
  (let* ([alice:keypair (generate-keypair)]
         [alice:pubkey (keypair-pub alice:keypair)])
    (check-true (verify (p2pkh-pub-script alice:pubkey) (p2pkh-script-sig "sig" alice:pubkey)))))

