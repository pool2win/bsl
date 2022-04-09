#lang racket/base

(provide (struct-out opcode)
         make-opcode)

;; struct opcode has proc and the number of arguments.
;; What format (hex or byte) the opcode is stored in is not relevant right now.
;; push-to-stack is a boolean to specify if result should be pushed to stack
;; pop-from-stack is a number specifying how many args to pop from stack
;; read-ahead-from-script is the number of elements to read from script following the current opcode
(struct opcode (proc num-arguments push-to-stack pop-from-stack read-ahead-from-script is_conditional))
(define (make-opcode #:proc proc #:num-arguments num-arguments #:push-to-stack push-to-stack
                     #:pop-from-stack pop-from-stack #:read-ahead-from-script read-ahead-from-script
                     #:is_conditional [is_conditional #f])
  (opcode proc num-arguments push-to-stack pop-from-stack read-ahead-from-script is_conditional))

