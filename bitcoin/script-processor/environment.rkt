#lang racket/base

(provide (struct-out environment)
         add-opcode get-opcode is-opcode? make-initial-env)

(require racket/list racket/symbol)

(module+ test
  (require rackunit))

;; An env with a hash of opcodes indexed by key and the stack for script computation
(struct environment (opcodes) #:mutable)

(define (remove-op-prefix opcode)
  (string->symbol (substring (symbol->immutable-string opcode)
                             3
                             (string-length (symbol->immutable-string opcode)))))

(define (add-opcode env opcodes opcode-hex proc)
  (hash-set! (environment-opcodes env) opcode-hex proc)
  (for ([code opcodes])
    (hash-set! (environment-opcodes env) code proc)
    (hash-set! (environment-opcodes env) (remove-op-prefix code) proc)))

(define (get-opcode opcode env)
  (hash-ref (environment-opcodes env) opcode))

(define (is-opcode? value env)
  (hash-has-key? (environment-opcodes env) value))

(define (make-initial-env)
  (let ([env (environment (make-hash))])
    env))

(module+ test
  (test-case
      "Setup initial environment"
    (let ([env (make-initial-env)])
      (check-equal? (hash-keys (environment-opcodes env)) '())))
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
      (check-true (is-opcode? #x8b env))
      (check-true (is-opcode? 'op_1add env))
      (check-true (is-opcode? '1add env))
      (check-true (is-opcode? #x93 env))
      (check-true (is-opcode? 'op_add env))
      (check-true (is-opcode? 'add env)))))
