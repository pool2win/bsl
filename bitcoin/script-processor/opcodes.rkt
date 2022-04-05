#lang racket/base

;; (provide opcodes)

;; opcode procs get args from stack and then args read ahead all in a single list
;; '(stack-arg1 stack-arg0 read-ahead0 read0-ahead1)
