#lang errortrace racket/base

(require "opcodes.rkt"
         "environment.rkt")

(provide make-bitcoin-environment)

(module+ test
  (require rackunit))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)])
    (add-opcode #x00 (make-opcode #:proc (lambda () #"") #:num-arguments 0
                                  #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 0) env)
    env
  ))


(module+ test
  (test-case
      "smoke test for making bitcoin environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-not-eq? bitcoin-env '())
      (check-true (is-opcode? #x00 bitcoin-env)))))