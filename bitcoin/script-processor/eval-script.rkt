#lang errortrace racket/base

(require racket/list
         "environment.rkt"
         "../transaction.rkt")

(provide apply-opcode eval-script)

(module+ test
  (require rackunit))

(define (apply-opcode code script env stack altstack
                      [tx (make-transaction #:version-number 0 #:flag 0 #:inputs '() #:outputs '() #:lock-time 0)]
                      [input-index '()])
  (let-values ([(script stack altstack verified) (apply (get-opcode code env) (list script stack altstack tx input-index))])
    (values script stack altstack verified)))

(module+ test
  (test-case
      "Setup initial environment"
    (let ([env (make-initial-env)])
      (check-equal? (hash-keys (environment-opcodes env)) '())))
  (test-case
      "Add opcode, check and apply it"
    (let ([env (make-initial-env)])
      (add-opcode env '(op_1add) #x8b (lambda (script stack altstack tx input-index)
                                        (let*-values ([(args rest-of-script) (split-at script 1)]
                                                      [(stack) (cons (apply add1 args) stack)])
                                          (values rest-of-script stack altstack #t))))
      (add-opcode env '(op_add) #x93 (lambda (script stack altstack tx input-index)
                                       (let*-values ([(args rest-of-script) (split-at script 2)]
                                                     [(stack) (cons (apply + args) stack)])
                                         (values rest-of-script stack altstack #t))))
      (let-values ([(script stack altstack verified) (apply-opcode 'op_1add '(1) env '() '())])
        (check-equal? stack '(2)))
      (let-values ([(script stack altstack verified) (apply-opcode 'op_add '(1 2) env '(2) '())])
        (check-equal? stack '(3 2))))))


;; Excute a script in the context of a bitcoin-environment
;; Script is a script in list format, e.g. '(op_dup op_hash160 #"deadbeef" op_equalverify)
;; Return same as apply-opcode (values script stack altstack verified)
(define (eval-script script env stack altstack [verified #t])
  (cond
    [(empty? script) (values script stack altstack verified)]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a" script)]
    [else
     (let-values ([(script stack altstack verified) (apply-opcode (first script) (rest script) env stack altstack)])
       (eval-script script env stack altstack verified))]))
       

(module+ test
  (test-case
      "Evaluate simple script"
    (let*-values ([(env) (make-initial-env)])
      (add-opcode env '(op_add) #x93 (lambda (script stack altstack tx input-index)
                                       (let*-values ([(args rest-of-script) (split-at script 2)]
                                                     [(stack) (cons (apply + args) stack)])
                                         (values rest-of-script stack '() #t))))
      (add-opcode env '(op_2) #x02
                  (lambda (script stack altstack tx input-index)
                    (values (list-tail script 1) (cons (first script) stack) '() #t)))
      (add-opcode env '(op_3) #x03
                  (lambda (script stack altstack tx input-index)
                    (values (list-tail script 1) (cons (first script) stack) '() #t)))
      (let-values ([(script stack altstack verified) (eval-script '(op_add 1 2) env '() '())])
        (check-equal? script '())
        (check-equal?  stack '(3)))
      (let-values ([(script stack altstack verified) (eval-script '(op_add 10 20 op_add 100 200) env '() '())])
        (check-equal?  stack '(300 30)))
      (let-values ([(script stack altstack verified) (eval-script '(op_2 #"AA" op_3 #"AAA") env '() '())])
        (check-equal?  stack '(#"AAA" #"AA"))))))
