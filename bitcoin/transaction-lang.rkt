#lang racket

(require (for-syntax syntax/parse))
(require "transaction.rkt")

(module+ test
  (require rackunit))

(define default-version-number 1)
(define default-flag 1)
(define default-lock-time '())

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
                          [w (syntax->datum #'(witness ...))]
                          [sq (syntax->datum #'(sequence ...))]
                          [h (syntax->datum #'(hash ...))]
                          [sc (syntax->datum #'(vout ...))])
                  (input s w sq (outpoint h sc)))))
  
  (define-splicing-syntax-class outputs
    #:description "transaction outputs syntax class"
    (pattern ((
               (~alt
                (~optional (~seq #:script os:expr))
                (~optional (~seq #:amount a:expr))) ...) ...))))

(define-syntax (transaction stx)    
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
            (~? (o `outs) (o '())))
           (make-transaction #:version-number v
                             #:flag f
                             #:lock-time l
                             #:inputs i
                             #:outputs o))]))
  
(module+ test
  (test-case "parse transactions from the new lang")
  (check-equal? (transaction-version-number (transaction)) default-version-number)
  (check-equal? (transaction-flag (transaction)) default-flag)
  (check-equal? (transaction-lock-time (transaction)) default-lock-time)
  (check-equal? (transaction-version-number (transaction #:version-number 100)) 100)

  (check-equal? (transaction-flag (transaction #:flag "f")) "f")
  (check-equal? (transaction-version-number (transaction #:version-number 10 #:flag "g")) 10)
  (check-equal? (transaction-flag (transaction #:version-number 10 #:flag "g")) "g")

  (let ([tx (transaction #:lock-time 10
                         #:version-number 10
                         #:flag "g"
                         #:inputs ((#:sequence 1 #:prevout ("abcd" 1) #:witness "w1" #:script "pkh(alice)")
                                   (#:sequence 2 #:witness "w2" #:prevout ("hash2" 2) #:script "op2")))]) 
    (check-equal? (transaction-flag tx) "g")
    (check-equal? (transaction-lock-time tx) 10)
    (check-equal? (transaction-version-number  tx) 10)
    (check-equal? (length (transaction-inputs tx)) 2)
    (check-equal? (input-script (list-ref (transaction-inputs tx) 0)) "pkh(alice)")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 0)) "w1")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 0)) (outpoint "abcd" 1))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 0)) 1)
    
    (check-equal? (input-script (list-ref (transaction-inputs tx) 1)) "op2")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 1)) "w2")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 1)) (outpoint "hash2" 2))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 1)) 2)
    ))

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
