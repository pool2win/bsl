#lang errortrace racket/base

(require racket/list
         "environment.rkt")

(provide apply-opcode)

(module+ test
  (require rackunit))

;; Goal, execute script "OP_PUSHDATA1 1 10 OP_PUSHDATA1 1 100 OP_ADD" and result in 110 on the stack
;; OP_PUSHDATA1 	76 	0x4c 
;; OP_ADD               147     0x93


;; eval will convert a list of bytes into a series of opcode-procs
;; eval will have to consume data off the stream based on opcode.
;; For example, op_pushdata1 consumes 1 + 1 bytes


;; What does eval do?
;; 1. Identify a byte as an opcode
;; 2. Consumes operands from stream if opcode specifies anyone
;; 3. Calls apply for the procedure that opcode directs to
;; 4. Apply updates the env with the result

;; What does apply do?
;; 1. Checks the procedure is a primitive
;; 1.1 If primitive: execute it
;; 1.1 If not, then call eval on the procedure with the arguments (I don't think this should happen in Script)

(define (apply-opcode code script env stack)
  (apply (get-opcode code env) (list script stack)))


(module+ test
  (test-case
      "Setup initial environment"
    (let ([env (make-initial-env)])
      (check-equal? (hash-keys (environment-opcodes env)) '())
      (check-equal? (environment-stack env) '())))
  (test-case
      "Add opcode, check and apply it"
    (let ([env (make-initial-env)])
      (add-opcode env '(op_1add) #x8b (lambda (script stack)
                                        (let*-values ([(args rest-of-script) (split-at script 1)]
                                                      [(stack) (cons (apply add1 args) stack)])
                                          (values rest-of-script env stack))))
      (add-opcode env '(op_add) #x93 (lambda (script stack)
                                       (let*-values ([(args rest-of-script) (split-at script 2)]
                                                     [(stack) (cons (apply + args) stack)])
                                         (values rest-of-script env stack))))
      (let-values ([(script env stack) (apply-opcode 'op_1add '(1) env '())])
        (check-equal? stack '(2)))
      (let-values ([(script env stack) (apply-opcode 'op_add '(1 2) env '(2))])
        (check-equal? stack '(3 2))))))


;; Excute a script in the context of a bitcoin-environment
;; Script is a script in list format, e.g. '(op_dup op_hash160 #"deadbeef" op_equalverify)
(define (eval-script script env stack)
  (cond
    [(empty? script) (values script env stack)]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a" script)]
    [else
     (let-values ([(script env stack) (apply-opcode (first script) (rest script) env stack)])
       (eval-script script env stack))]))
       


(module+ test
  (test-case
      "Evaluate simple script"
    (let*-values ([(env) (make-initial-env)])
      (add-opcode env '(op_add) #x93 (lambda (script stack)
                                       (let*-values ([(args rest-of-script) (split-at script 2)]
                                                     [(stack) (cons (apply + args) stack)])
                                         (values rest-of-script env stack))))
      (let-values ([(script env stack) (eval-script '(op_add 1 2) env '())])
        (check-equal? script '())
        (check-equal?  stack '(3)))
      (let-values ([(script env stack) (eval-script '(op_add 10 20 op_add 100 200) env '())])
        (check-equal?  stack '(300 30))))))
    ;; (test-case
    ;;     "Evaluate script with single conditional branch"
    ;;   (let*-values ([(env) (make-initial-env)])
    ;;     (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
    ;;                                             #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
    ;;     (add-opcode '(op_if) #x63
    ;;                 (make-opcode
    ;;                  #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;                  #:is_conditional #t
    ;;                  #:proc (lambda (code) '())) env)
    ;;     (add-opcode '(op_notif) #x64
    ;;                 (make-opcode
    ;;                  #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;                  #:is_conditional #t
    ;;                  #:proc (lambda (code) '())) env)
    ;;     (add-opcode '(op_else) #x67
    ;;             (make-opcode
    ;;              #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;              #:is_conditional #t
    ;;              #:proc (lambda (code) '())) env)
    ;;     (add-opcode '(op_endif) #x68
    ;;                 (make-opcode
    ;;                  #:num-arguments 0 #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 0
    ;;                  #:is_conditional #t
    ;;                  #:proc (lambda (code) '())) env)
    ;;     (eval-script '(op_true op_if op_add '(1 2) op_endif) env)
    ;;     (check-equal?  (environment-stack env) '(3))
    ;;     (eval-script '(op_false op_if '(op_add 1 2) op_else '(#x93 100 200) op_endif) env)
    ;;     (check-equal?  (environment-stack env) '(3)))))
