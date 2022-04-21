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

(define (handle-conditional condition env script stack altstack)
  (match script
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env new-stack altstack) (eval-script ifexp env (rest stack) altstack)])
       (values (list-tail script 2) new-stack altstack #t))]
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
       (values (list-tail script 2) (rest stack) altstack #t)]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env stack altstack) (eval-script ifexp env (rest stack) altstack)])
       (values (list-tail script 4) stack altstack #t))]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
     (let-values ([(rest-script new-env stack altstack) (eval-script elseifexp env (rest stack) altstack)])
       (values (list-tail script 4) stack altstack #t))]))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)])
    (add-opcode env '(op_0 op_false) #x00
                (lambda (script stack altstack)
                  (values script (cons 0 stack) altstack #t)))
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode env (string-join `("op_" ,(~a code)) "") code
                  (lambda (script stack altstack)
                    (values (list-tail script 1) (cons (first script) stack) altstack #t))))
    (add-opcode env '(op_pushdata1) #x4c
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_pushdata2) #x4d
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_pushdata4) #x4e
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_1negate) #x4f
                (lambda (script stack altstack)
                  (values script  (cons -1 stack) altstack #t)))
    (add-opcode env '(op_1 op_true) #x51
                (lambda (script stack altstack)
                  (values script (cons 1 stack) altstack #t)))
    (for ([code (in-inclusive-range #x52 #x60)])
      (add-opcode env (string-join `("op_" ,(~a (- code 80))) "") code
                  (lambda (script stack altstack)
                    (values script (cons (- code 80) stack) altstack #t))))
    (add-opcode env '(op_nop) #x61
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_if) #x63
                (lambda (script stack altstack)
                  (handle-conditional 1 env script stack altstack )))
    (add-opcode env '(op_notif) #x64
                (lambda (script stack altstack)
                  (handle-conditional 0 env script stack altstack)))
    (add-opcode env '(op_else) #x67
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_endif) #x68
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_verify) #x69
                (lambda (script stack altstack)
                  (values script stack altstack (eq? (first stack) 1))))
    (add-opcode env '(op_return) #x6a
                (lambda (script stack altstack)
                  (values '() stack altstack #f)))
    ;; ;; stack operations
    ;; (add-opcode env '(op_toaltstack) #x6b
    ;;             (lambda (script stack altstack)
    ;;               (values '() stack () altstack #f)))
    env))
                                  
(module+ test
  (test-case
      "smoke test for making bitcoin environment"
    (let ([bitcoin-env (make-bitcoin-environment)])
      (check-not-eq? bitcoin-env '())
      (check-true (is-opcode? #x00 bitcoin-env))
      (check-true (is-opcode? #x01 bitcoin-env))
      )))
