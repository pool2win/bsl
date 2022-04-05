#lang errortrace racket/base

(require racket/list)

(module+ test
  (require rackunit))

;; Goal, execute script "OP_PUSHDATA1 1 10 OP_PUSHDATA1 1 100 OP_ADD" and result in 110 on the stack
;; OP_PUSHDATA1 	76 	0x4c 
;; OP_ADD               147     0x93


;; Support only the two opcodes

;; Provide an env (as a stack)
;; Does env have the opcode-procs hash as well?

;; eval will convert a list of bytes into a series of opcode-procs
;; eval will have to consume data off the stream based on opcode.
;; For example, op_pushdata1 consumes 1 + 1 bytes


;; Don't start from operators, start from eval and apply

;; What does eval do?
;; 1. Identify a byte as an opcode
;; 2. Consumes operands from stream if opcode specifies anyone
;; 3. Calls apply for the procedure that opcode directs to
;; 4. Apply updates the env with the result

;; What does apply do?
;; 1. Checks the procedure is a primitive
;; 1.1 If primitive: execute it
;; 1.1 If not, then call eval on the procedure with the arguments (I don't think this should happen in Script)

;; struct opcode has proc and the number of arguments.
;; What format (hex or byte) the opcode is stored in is not relevant right now.
;; push-to-stack is a boolean to specify if result should be pushed to stack
;; pop-from-stack is a number specifying how many args to pop from stack
;; read-ahead-from-script is the number of elements to read from script following the current opcode
(struct opcode (proc num-arguments push-to-stack pop-from-stack read-ahead-from-script))
(define (make-opcode #:proc proc #:num-arguments num-arguments #:push-to-stack push-to-stack
                     #:pop-from-stack pop-from-stack #:read-ahead-from-script read-ahead-from-script)
  (opcode proc num-arguments push-to-stack pop-from-stack read-ahead-from-script))

;; An env with a hash of opcodes indexed by key and the stack for script computation
(struct environment (opcodes stack) #:mutable)

(define (add-opcode opcode proc env)
  (hash-set! (environment-opcodes env) opcode proc))

(define (get-opcode opcode env)
  (hash-ref (environment-opcodes env) opcode))

(define (get-opcode-proc opcode env)
  (opcode-proc (hash-ref (environment-opcodes env) opcode)))

(define (get-opcode-num-args opcode env)
  (opcode-num-arguments (hash-ref (environment-opcodes env) opcode)))

(define (is-opcode? value env)
  (hash-has-key? (environment-opcodes env) value))

(define (make-initial-env)
  (let ([env (environment (make-hash) '())])
    env))


;; apply-opcode is implemented as a data driven procedure.
;; It reads conditions from opcode and acts accordingly
(define (apply-opcode code script env)
  (let*-values ([(opcode) (get-opcode code env)]
                [(push-to-stack?) (opcode-push-to-stack opcode)]
                [(pop-from-stack) (opcode-pop-from-stack opcode)]
                [(args) (take (environment-stack env) pop-from-stack)]
                [(num-read-ahead-from-script) (opcode-read-ahead-from-script opcode)]
                [(args-read-ahead script) (split-at script num-read-ahead-from-script)]
                [(args) (append args-read-ahead args)]
                [(result) (apply (get-opcode-proc code env) args)])
    (cond
      [push-to-stack?
       (set-environment-stack! env (cons result (environment-stack env)))])
    env))

(define (get-opcode-args opcode script env)
  (let ([num-arguments (get-opcode-num-args opcode env)])
    (split-at script num-arguments)))

(module+ test
  (test-case
      "Setup initial environment"
    (let ([env (make-initial-env)])
      (check-equal? (hash-keys (environment-opcodes env)) '())
      (check-equal? (environment-stack env) '())))
  (test-case
      "Add opcode, check and apply it"
    ;; (struct opcode (proc num-arguments push-to-stack pop-from-stack read-ahead-from-script))
    (let ([env (make-initial-env)])
      (add-opcode #xab (make-opcode #:proc (lambda (x) (add1 x)) #:num-arguments 1
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 1) env)
      (add-opcode #x65 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (check-true (is-opcode? #xab env))
      (check-equal? (get-opcode-num-args #xab env) 1)
      (check-equal? (environment-stack (apply-opcode #xab '(1) env)) '(2))
      (check-true (is-opcode? #x65 env))
      (check-equal? (get-opcode-num-args #x65 env) 2)
      (check-equal? (environment-stack (apply-opcode #x65 '(1 2) env)) '(3 2))))
  (test-case
      "Get opcode args from script"
    (let ([env (make-initial-env)])
      (add-opcode #x65 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                    #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (let-values ([(args rest) (get-opcode-args #x65 '(1 2 3 4) env)])
        (check-equal?  args '(1 2))
        (check-equal? rest '(3 4))))))
        
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
