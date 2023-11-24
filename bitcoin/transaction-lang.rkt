#lang racket

(require (for-syntax syntax/parse))
(require "transaction.rkt"
         "endian-helper.rkt")

(provide (all-defined-out))

(define default-version-number 2) ;; we only work with version 2 transactions and up
(define default-flag 1)
(define default-lock-time '())
(define default-witness '())
(define default-input-sequence "FFFFFFFF") ;; By default we don't use CSV

(begin-for-syntax
  (define-splicing-syntax-class inputs
    #:description "transaction inputs syntax class"
    (pattern ((
               (~alt
                (~optional (~seq #:sequence sequence:expr))
                (~optional (~seq #:prevout (hash:expr vout:expr)))
                (~optional (~seq #:witness witness:expr))
                (~optional (~seq #:script script:expr))) ...) ...)
      #:attr ip #'(for/list([s (syntax->datum #'(script ...))]
                            [w (syntax->datum #'((~? witness '()) ...))]
                            [sq (syntax->datum #'((~? sequence "FFFFFFFF") ...))]
                            [h (syntax->datum #'(hash ...))]
                            [sc (syntax->datum #'(vout ...))])
                  (input s w (read-little-endian-hex-string sq) (outpoint h sc)))))
  
  (define-splicing-syntax-class outputs
    #:description "transaction outputs syntax class"
    (pattern ((
               (~seq #:script os:expr)
               (~seq #:amount a:expr)) ...)
      #:attr op #'(for/list ([s (syntax->datum #'(os ...))]
                             [v (syntax->datum #'(a ...))])
                    (output s v)))))

(define-syntax (tx stx)    
  (syntax-parse stx
    [(_
      (~alt
       (~optional (~seq #:version-number vn:expr))
       (~optional (~seq #:flag flg:expr))
       (~optional (~seq #:lock-time lt:expr))
       (~optional (~seq #:inputs ins:inputs))
       (~optional (~seq #:outputs outs:outputs))) ...)
     #'(let 
           ((~? (v vn) (v default-version-number))
            (~? (f flg) (f default-flag))
            (~? (l  lt) (l default-lock-time))
            (~? (i ins.ip) (i '()))
            (~? (o outs.op) (o '())))
           (make-transaction #:version-number v
                             #:flag f
                             #:lock-time l
                             #:inputs i
                             #:outputs o))]))
  
