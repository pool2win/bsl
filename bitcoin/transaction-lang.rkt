#lang racket

(require (for-syntax syntax/parse))
(require "transaction.rkt")

(define default-version-number 1)
(define default-flag 1)
(define default-lock-time '())

(define-syntax (transaction stx)  
  (define-splicing-syntax-class inputs
    #:description "transaction inputs"
    (pattern ((
               (~alt
                (~optional (~seq #:sequence sq:expr))
                (~optional (~seq #:prevout po:expr))
                (~optional (~seq #:witness w:expr))
                (~optional (~seq #:script s:expr))) ...) ...)))
  (define-splicing-syntax-class outputs
    #:description "transaction outputs"
    (pattern ((
               (~alt
                (~optional (~seq #:script os:expr))
                (~optional (~seq #:amount a:expr))) ...) ...)))
  
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
            (~? (i `ins) (i '()))
            (~? (o `outs) (o '())))
           (make-transaction #:version-number v
                             #:flag f
                             #:lock-time l
                             #:inputs i
                             #:outputs o))]))
  

(transaction #:version-number 100)
(transaction)
(transaction #:flag "f")
(transaction #:version-number 10 #:flag "g")
(transaction #:version-number 10 #:flag "g" #:lock-time 10)
(transaction #:lock-time 10 #:version-number 10 #:flag "g")
(transaction #:lock-time 10
             #:version-number 10
             #:flag "g"
             #:inputs ((#:sequence 1)))

(transaction #:lock-time 10
             #:version-number 10
             #:flag "g"
             #:inputs ((#:sequence 1 #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                       (#:sequence 1 #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
             )
(transaction #:lock-time 10
             #:version-number 10
             #:flag "g"
             #:inputs ((#:sequence 1 #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                       (#:sequence 1 #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
             #:outputs ((#:script "aa" #:amount 100) (#:script "bb" #:amount 200))
             )
(transaction #:lock-time 10
             #:version-number 10
             #:flag "g"
             #:inputs ((#:sequence 1 #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                       (#:sequence 1 #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
             #:outputs ((#:script "aa") (#:amount 200))
             )
(transaction #:outputs ((#:script "zz") (#:amount 200.20)))
