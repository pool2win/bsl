#lang errortrace racket/base

(require "environment.rkt"
         "bitcoin-environment.rkt"
         "eval-script.rkt")

(module+ test
  (require rackunit))


(module+ test
  (test-case
      "test bitcoin opcodes in bitcoin-environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x00 '() bitcoin-env)) '(#""))
      ))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x01 '(#x01 #"A") bitcoin-env)) '(#"A")))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x02 '(#x02 #"BC") bitcoin-env)) '(#"BC")))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x4c '(#x4c #x02 #"BC") bitcoin-env)) '(#"BC")))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x4d '(#x4c #x0002 #"BC") bitcoin-env)) '(#"BC")))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x4e '(#x4c #x00000002 #"BC") bitcoin-env)) '(#"BC")))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x4f '(#x4f) bitcoin-env)) '(-1)))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x51 '(#x51) bitcoin-env)) '(1)))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x52 '(#x52) bitcoin-env)) '(2)))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x60 '(#x60) bitcoin-env)) '(16)))
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-equal? (environment-stack (apply-opcode #x61 '(#x61) bitcoin-env)) '())))
