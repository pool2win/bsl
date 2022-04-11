#lang errortrace racket/base

(require racket/string
         racket/format
         racket/list
         "environment.rkt")

(provide make-bitcoin-environment)

(module+ test
  (require rackunit))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)])
    (add-opcode env '(op_0 op_false) #x00
                (lambda (script stack)
                  (values script (cons 0 stack))))
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode env `("op_",(~a code)) code
                  (lambda (script stack)
                    (values (list-tail script 1) (cons (first script) stack)))))
    (add-opcode env '(op_pushdata1) #x4c
                (lambda (script stack)
                  (values (list-tail script 2) (cons (second script) stack))))
    (add-opcode env '(op_pushdata2) #x4d
                (lambda (script stack)
                  (values (list-tail script 2) (cons (second script) stack))))
    (add-opcode env '(op_pushdata4) #x4e
                (lambda (script stack)
                  (values (list-tail script 2) (cons (second script) stack))))
    (add-opcode env '(op_1negate) #x4f
                (lambda (script stack)
                  (values script  (cons -1 stack))))
    (add-opcode env '(op_1 op_true) #x51
                (lambda (script stack)
                  (values script (cons 1 stack))))
    (for ([code (in-inclusive-range #x52 #x60)])
      (add-opcode env (string-join `("op_",(~a (- code 80))) "") code
                  (lambda (script stack)
                    (values script (cons (- code 80) stack)))))
    (add-opcode env '(op_nop) #x61
                (lambda (script stack)
                  (values script stack)))
    env))
                                  
    ;; (add-opcode '(op_if) #x63
    ;;             (make-opcode
    ;;              #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;              #:is_conditional #t
    ;;              #:proc (lambda (code) '())) env)
    ;; (add-opcode '(op_notif) #x64
    ;;             (make-opcode
    ;;              #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;              #:is_conditional #t
    ;;              #:proc (lambda (code) '())) env)
    ;; (add-opcode '(op_else) #x67
    ;;             (make-opcode
    ;;              #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;              #:is_conditional #t
    ;;              #:proc (lambda (code) '())) env)
    ;; (add-opcode '(op_endif) #x68
    ;;             (make-opcode
    ;;              #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;              #:is_conditional #t
    ;;              #:proc (lambda (code) '())) env)


(module+ test
  (test-case
      "smoke test for making bitcoin environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-not-eq? bitcoin-env '())
      (check-true (is-opcode? #x00 bitcoin-env))
      (check-true (is-opcode? #x01 bitcoin-env))
      )))
