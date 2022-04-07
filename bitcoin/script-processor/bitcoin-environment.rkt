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
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode code
                  (make-opcode
                   #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2
                   #:proc (lambda (num-bytes bytes-to-push)
                            bytes-to-push)) env))
    (add-opcode #x4c
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 3
                 #:proc (lambda (code num-bytes bytes-to-push)
                          bytes-to-push)) env)
    (add-opcode #x4d
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 3
                 #:proc (lambda (code num-bytes bytes-to-push)
                          bytes-to-push)) env)
    (add-opcode #x4e
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 3
                 #:proc (lambda (code num-bytes bytes-to-push)
                          bytes-to-push)) env)
    (add-opcode #x4f
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1
                 #:proc (lambda (code) -1)) env)
    (add-opcode #x51
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1
                 #:proc (lambda (code) 1)) env)
    (for ([code (in-inclusive-range #x52 #x60)])
      (add-opcode code
                  (make-opcode
                   #:num-arguments 0 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1
                   #:proc (lambda (code) (- code 80))) env))
    (add-opcode #x61
                (make-opcode
                 #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 1
                 #:proc (lambda (code) '())) env)
    env
  ))


(module+ test
  (test-case
      "smoke test for making bitcoin environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-not-eq? bitcoin-env '())
      (check-true (is-opcode? #x00 bitcoin-env))
      (check-true (is-opcode? #x01 bitcoin-env))
      )))
