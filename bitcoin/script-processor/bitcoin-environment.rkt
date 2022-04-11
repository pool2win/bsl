#lang errortrace racket/base

(require racket/string
         racket/format
         racket/list
         racket/match
         "environment.rkt"
         "eval-script.rkt")

(provide make-bitcoin-environment)

(module+ test
  (require rackunit))

(define (handle-conditional condition env script stack)
  (match script
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env new-stack) (eval-script ifexp env (rest stack))])
       (values (list-tail script 2) new-stack))]
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
       (values (list-tail script 2) (rest stack))]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env stack) (eval-script ifexp env (rest stack))])
       (values (list-tail script 4) stack))]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
     (let-values ([(rest-script new-env stack) (eval-script elseifexp env (rest stack))])
       (values (list-tail script 4) stack))]))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)])
    (add-opcode env '(op_0 op_false) #x00
                (lambda (script stack)
                  (values script (cons 0 stack))))
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode env (string-join `("op_" ,(~a code)) "") code
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
      (add-opcode env (string-join `("op_" ,(~a (- code 80))) "") code
                  (lambda (script stack)
                    (values script (cons (- code 80) stack)))))
    (add-opcode env '(op_nop) #x61
                (lambda (script stack)
                  (values script stack)))
    (add-opcode env '(op_if) #x63
                (lambda (script stack)
                  (handle-conditional 1 env script stack)))
    (add-opcode env '(op_notif) #x64
                (lambda (script stack)
                  (handle-conditional 0 env script stack)))
    (add-opcode env '(op_else) #x67
                (lambda (script stack)
                  (values script stack)))
    (add-opcode env '(op_endif) #x68
                (lambda (script stack)
                  (values script stack)))
    env))
                                  
(module+ test
  (test-case
      "smoke test for making bitcoin environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-not-eq? bitcoin-env '())
      (check-true (is-opcode? #x00 bitcoin-env))
      (check-true (is-opcode? #x01 bitcoin-env))
      )))
