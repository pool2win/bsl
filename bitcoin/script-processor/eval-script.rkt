#lang errortrace racket/base

(require racket/list
         "environment.rkt"
         "../transaction.rkt")

(provide apply-opcode eval-script)

(module+ test
  (require rackunit))

;; Goal, execute script "OP_PUSHDATA1 1 10 OP_PUSHDATA1 1 100 OP_ADD" and result in 110 on the stack
;; OP_PUSHDATA1 	76 	0x4c 
;; OP_ADD               147     0x93


;; eval will convert a list of bytes into a series of opcode-procs
;; eval will have to consume data from an in-port based on opcode.
;; For example, op_pushdata1 consumes 1 + 1 bytes


;; What does eval do?
;; 1. Identify a byte as an opcode
;; 2. Consumes operands from in-port if opcode specifies anyone
;; 3. Calls apply for the procedure that opcode directs to
;; 4. Apply updates the env with the result

;; What does apply do?
;; 1. Checks the procedure is a primitive
;; 1.1 If primitive: execute it
;; 1.1 If not, then call eval on the procedure with the arguments (I don't think this should happen in Script)

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
(define (eval-script script env stack altstack)
  (cond
    [(empty? script) (values script env stack altstack)]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a" script)]
    [else
     (let-values ([(script stack altstack verified) (apply-opcode (first script) (rest script) env stack altstack)])
       (eval-script script env stack altstack))]))
       

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
      (let-values ([(script env stack altstack) (eval-script '(op_add 1 2) env '() '())])
        (check-equal? script '())
        (check-equal?  stack '(3)))
      (let-values ([(script env stack altstack) (eval-script '(op_add 10 20 op_add 100 200) env '() '())])
        (check-equal?  stack '(300 30)))
      (let-values ([(script env stack altstack) (eval-script '(op_2 #"AA" op_3 #"AAA") env '() '())])
        (check-equal?  stack '(#"AAA" #"AA"))))))
