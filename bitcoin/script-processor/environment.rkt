#lang racket/base

(provide (struct-out environment)
         add-opcode get-opcode get-opcode-proc get-opcode-num-args is-opcode? make-initial-env get-opcode-args)

(require racket/list
         "opcodes.rkt")

(module+ test
  (require rackunit))

;; An env with a hash of opcodes indexed by key and the stack for script computation
(struct environment (opcodes stack altstack) #:mutable)

(define (add-opcode opcodes opcode-hex proc env)
  (hash-set! (environment-opcodes env) opcode-hex proc)
  (for ([code opcodes])
    (hash-set! (environment-opcodes env) code proc)))

(define (get-opcode opcode env)
  (hash-ref (environment-opcodes env) opcode))

(define (get-opcode-proc opcode env)
  (opcode-proc (hash-ref (environment-opcodes env) opcode)))

(define (get-opcode-num-args opcode env)
  (opcode-num-arguments (hash-ref (environment-opcodes env) opcode)))

(define (is-opcode? value env)
  (hash-has-key? (environment-opcodes env) value))

;; returns '(<args> <rest-of-script>)
(define (get-opcode-args opcode script env)
  (let ([num-arguments (get-opcode-num-args opcode env)])
    (split-at script num-arguments)))

(define (make-initial-env)
  (let ([env (environment (make-hash) '() '())])
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
      (check-true (is-opcode? #x8b env))
      (check-equal? (get-opcode-num-args #x8b env) 1)
      (check-true (is-opcode? #x93 env))
      (check-equal? (get-opcode-num-args #x93 env) 2)))
  (test-case
      "Get opcode args from script"
    (let ([env (make-initial-env)])
      (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                              #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (let-values ([(args rest) (get-opcode-args #x93 '(1 2 3 4) env)])
        (check-equal?  args '(1 2))
        (check-equal? rest '(3 4)))
      (let-values ([(args rest) (get-opcode-args 'op_add '(1 2 3 4) env)])
        (check-equal?  args '(1 2))
        (check-equal? rest '(3 4)))))
  (test-case
      "Get opcode args from script using op code hex"
    (let ([env (make-initial-env)])
      (add-opcode '(op_add) #x93 (make-opcode #:proc (lambda (x y) (+ x y)) #:num-arguments 2
                                              #:push-to-stack #t #:pop-from-stack 0 #:read-ahead-from-script 2) env)
      (let-values ([(args rest) (get-opcode-args #x93 '(1 2 3 4) env)])
        (check-equal?  args '(1 2))
        (check-equal? rest '(3 4)))
      (let-values ([(args rest) (get-opcode-args 'op_add '(1 2 3 4) env)])
        (check-equal?  args '(1 2))
        (check-equal? rest '(3 4))))))
