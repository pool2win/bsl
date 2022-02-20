#lang racket/base

(require "../crypto-utils.rkt")

(provide p2pkh-pub-script p2pkh-script-sig verify-stack verify)

(define-syntax-rule (p2pkh-pub-script pubkey)
  `(op_dup op_hash160 (hash160 pubkey) op_equalverify op_checksig))

(define-syntax-rule (p2pkh-script-sig signature pubkey)
  `(,signature ,pubkey))


(define (verify-stack stack)
  #t)

(define (verify script-pub-key script-sig)
 (let ([stack (list script-sig script-pub-key)])
    (verify-stack stack)
    ))
