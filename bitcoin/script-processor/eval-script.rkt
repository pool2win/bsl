#lang errortrace racket/base

(require racket/list
         "opcodes.rkt"
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


;; apply-opcode is implemented as a data driven procedure.
;; It reads conditions from opcode and acts accordingly
(define (apply-opcode code script env)
  (let*-values ([(opcode) (get-opcode code env)]
                [(push-to-stack?) (opcode-push-to-stack opcode)]
                [(pop-from-stack) (opcode-pop-from-stack opcode)]
                [(args left-on-stack) (split-at (environment-stack env) pop-from-stack)]
                [(num-read-ahead-from-script) (opcode-read-ahead-from-script opcode)]
                [(args-read-ahead script) (split-at script num-read-ahead-from-script)]
                [(args) (append args-read-ahead args)]
                [(result) (apply (get-opcode-proc code env) args)])
    (cond
      [push-to-stack?
       (set-environment-stack! env (cons result left-on-stack))]
      [else (set-environment-stack! env left-on-stack)])
    env))

(module+ test
  (test-case
      "Setup initial environment"
    (let ([env (make-initial-env)])
      (check-equal? (hash-keys (environment-opcodes env)) '())
      (check-equal? (environment-stack env) '())))
  (test-case
      "Add opcode, check and apply it"
    (let ([env (make-initial-env)])
      (add-opcode '(op_1add) #x8b (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1) env)
      (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (check-equal? (environment-stack (apply-opcode #x8b '(1) env)) '(2))
      (check-equal? (environment-stack (apply-opcode #x93 '(1 2) env)) '(3 2))))
  (test-case
      "opcode without push to stack"
    (let ([env (make-initial-env)])
      (add-opcode '(op_1add) #x8b (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                               #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 1) env)
      (check-equal? (environment-stack (apply-opcode #x8b '(1) env)) '())))
  (test-case
      "opcode with pop from stack"
    (let ([env (make-initial-env)])
      (set-environment-stack! env '(100))
      (add-opcode '(op_1add) #x8b (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                               #:push-to-stack #t #:pop-from-stack 1 #:read-ahead-from-script 0) env)
      (check-equal? (environment-stack (apply-opcode #x8b '() env)) '(101)))))


;; Excute a script in the context of a bitcoin-environment
;; Script is a script in list format, e.g. '(op_dup op_hash160 #"deadbeef" op_equalverify)
(define (eval-script script env)
  (cond
    [(empty? script) '()]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a" script)]
    [else
     (let-values ([(args rest-of-script) (get-opcode-args (first script) (rest script) env)])
       (apply-opcode (first script) args env)
       (eval-script rest-of-script env))]))
       


(module+ test
  (test-case
      "Evaluate simple script"
    (let*-values ([(env) (make-initial-env)])
      (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                              #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (eval-script '(op_add 1 2) env)
      (check-equal?  (environment-stack env) '(3))
      (eval-script '(op_add 10 20 op_add 100 200) env)
      (check-equal?  (environment-stack env) '(300 30 3)))))
    ;; (test-case
    ;;   "Evaluate script with single conditional branch"
    ;; (let*-values ([(env) (make-initial-env)])
    ;;   (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
    ;;                                 #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
    ;;   (eval-script '(op_1 op_if op_add '(1 2) op_endif) env)
    ;;   (check-equal?  (environment-stack env) '(3))
      ;; (eval-script '(#x00 #x63 '(#x93 1 2) #x67 '(#x93 100 200) #x68) env)
      ;; (check-equal?  (environment-stack env) '(3))
