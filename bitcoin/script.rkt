#lang racket/base

(module+ test
  (require rackunit))

(require file/sha1
         "script-processor/environment.rkt"
         "endian-helper.rkt")

(provide hr-script
         p2pkh-pub-script
         p2pkh-script-sig
         verify-stack
         verify
         script->hex-string)

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

(define (script->hex-string script)
  (map (lambda (s)
         (cond
           [(integer? s) (to-little-endian-hex-string s 1)]
           [(bytes? s) (bytes->hex-string (read-little-endian-bytes s))]))
       script))

(module+ test
  (require "../crypto-utils.rkt")
  (test-case "Test p2pkh stack verfication"
    (let* ([alice:keypair (generate-keypair)] [alice:pubkey (keypair-pub alice:keypair)])
      (check-true (verify (p2pkh-pub-script alice:pubkey) (p2pkh-script-sig "sig" alice:pubkey))))))
