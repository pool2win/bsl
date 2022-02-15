#lang racket/base

(require "crypto-utils.rkt")

;; There was a strong tempation to turn functions in this module into macros
;; But they are fine as functions for now. KISS.


(define (p2pkh-pub-script pubkey)
  `(op_dup op_hash160 (hash160 pubkey) op_equalverify op_checksig))

(define (p2pkh-script-sig signature pubkey)
  `(,signature ,pubkey))


(define (verify-stack stack)
  #t)

(define (verify script-pub-key script-sig)
 (let ([stack (list script-sig script-pub-key)])
    (verify-stack stack)
    ))

(let ([alice:pubkey "random"])
  (verify "script-pubkey" "script-sig"))

