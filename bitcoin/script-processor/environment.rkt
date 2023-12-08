#lang racket

(provide (struct-out environment)
         add-opcode
         get-opcode
         is-opcode?
         make-initial-env
         get-hr-opcode
         get-serialized-opcode
         get-symbol-to-hex)

(require racket/list
         racket/symbol)

(module+ test
  (require rackunit))

;; An env with a hash of opcodes indexed by key and the stack for script computation
(struct environment (opcodes hr serialized symbol-to-hex) #:mutable)

(define (remove-op-prefix opcode)
  (let ([opcode-string (symbol->immutable-string opcode)])
    (string->symbol (substring opcode-string 3 (string-length opcode-string)))))

(define (add-opcode env opcodes opcode-hex proc)
  (hash-set! (environment-opcodes env) opcode-hex proc)
  (for ([code opcodes])
    (hash-set! (environment-serialized env) code (integer->integer-bytes opcode-hex 1 #f))
    (hash-set! (environment-opcodes env) (symbol->string code) proc)
    (hash-set! (environment-opcodes env) code proc)
    (hash-set! (environment-opcodes env) (remove-op-prefix code) proc)
    (hash-set! (environment-hr env) opcode-hex code)
    (hash-set! (environment-symbol-to-hex env) code opcode-hex)))

(define (get-symbol-to-hex opcode env)
  (hash-ref (environment-symbol-to-hex env) opcode))

(define (get-serialized-opcode opcode env)
  (hash-ref (environment-serialized env) opcode))

(define (get-hr-opcode opcode-hex env)
  (hash-ref (environment-hr env) opcode-hex))

(define (get-opcode opcode env)
  (hash-ref (environment-opcodes env) opcode))

(define (is-opcode? value env)
  (hash-has-key? (environment-opcodes env) value))

(define (make-initial-env)
  (let ([env (environment (make-hash) (make-hash) (make-hash) (make-hash))]) env))

(module+ test
  (test-case "Setup initial environment"
    (let ([env (make-initial-env)]) (check-equal? (hash-keys (environment-opcodes env)) '())))
  (test-case "Add opcode, check and apply it"
    (let ([env (make-initial-env)])
      (add-opcode env
                  '(op_1add)
                  #x8b
                  (lambda (script stack)
                    (let*-values ([(args rest-of-script) (split-at script 1)]
                                  [(stack) (cons (apply add1 args) stack)])
                      (values rest-of-script env stack))))
      (add-opcode env
                  '(op_add)
                  #x93
                  (lambda (script stack)
                    (let*-values ([(args rest-of-script) (split-at script 2)]
                                  [(stack) (cons (apply + args) stack)])
                      (values rest-of-script env stack))))
      (check-true (is-opcode? #x8b env))
      (check-true (is-opcode? 'op_1add env))
      (check-true (is-opcode? '1add env))
      (check-true (is-opcode? #x93 env))
      (check-true (is-opcode? 'op_add env))
      (check-true (is-opcode? 'add env))
      (check-equal? (get-serialized-opcode 'op_add env) #"\223")
      (check-equal? (get-symbol-to-hex 'op_add env) #x93))))
