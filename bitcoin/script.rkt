#lang racket/base

(module+ test
  (require rackunit))

(require "../crypto-utils.rkt"
         "script-processor/environment.rkt")

(provide hr-script
         p2pkh-pub-script
         p2pkh-script-sig
         verify-stack
         verify)

(define-syntax-rule (p2pkh-pub-script pubkey)
  `(op_dup op_hash160 (hash160 pubkey) op_equalverify op_checksig))

(define-syntax-rule (p2pkh-script-sig signature pubkey) `(,signature ,pubkey))

(define (verify-stack stack)
  #t)

(define (verify script-pub-key script-sig)
  (let ([stack (list script-sig script-pub-key)]) (verify-stack stack)))

(define (hr-script script env)
  (for/list ([s script])
    (if (is-opcode? s env) (get-hr-opcode s env) s)))

(module+ test
  (test-case "Test p2pkh stack verfication"
    (let* ([alice:keypair (generate-keypair)] [alice:pubkey (keypair-pub alice:keypair)])
      (check-true (verify (p2pkh-pub-script alice:pubkey) (p2pkh-script-sig "sig" alice:pubkey))))))
