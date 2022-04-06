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
      (add-opcode #xab (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1) env)
      (add-opcode #x65 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (check-equal? (environment-stack (apply-opcode #xab '(1) env)) '(2))
      (check-equal? (environment-stack (apply-opcode #x65 '(1 2) env)) '(3 2))))
  (test-case
      "opcode without push to stack"
    (let ([env (make-initial-env)])
      (add-opcode #xab (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                    #:push-to-stack #f #:pop-from-stack 0 #:read-ahead-from-script 1) env)
      (check-equal? (environment-stack (apply-opcode #xab '(1) env)) '())))
  (test-case
      "opcode with pop from stack"
    (let ([env (make-initial-env)])
      (set-environment-stack! env '(100))
      (add-opcode #xab (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                    #:push-to-stack #t #:pop-from-stack 1 #:read-ahead-from-script 0) env)
      (check-equal? (environment-stack (apply-opcode #xab '() env)) '(101)))))
;; (define (eval-script script env)
;;   (cond
;;     [(is-opcode? (first script) env)
;;      (let-values ([(args rest) (get-opcode-args (first script) (rest script) env)])
;;        (values (apply-opcode (first script) args env) rest))]))


;; (module+ test
;;   (test-case
;;       "Evaluate a dummy script"
;;     (let*-values ([(env) (make-initial-env)]
;;                   [(unused) (add-opcode #x65 (opcode (lambda (x y) (+ x y)) 2) env)]
;;                   [(result rest) (eval-script '(#x65 1 2 3 4) env)])
;;       (check-equal? result 3)
;;       (check-equal? rest '(3 4)))))
