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
      (check-equal? (environment-stack (apply-opcode #x00 '() bitcoin-env)) '(#"")))))
