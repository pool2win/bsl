#lang racket

(require (for-syntax syntax/parse))
(require "transaction.rkt"
         "endian-helper.rkt")

(module+ test
  (require rackunit))

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
            (~? (o outs.op) (o '())))
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
                         #:inputs ((#:sequence "FFFFFFFF" #:prevout ("abcd" 1) #:witness "w1" #:script "pkh(alice)")
                                   (#:sequence "FFFFFFFF" #:witness "w2" #:prevout ("hash2" 2) #:script "op2")))]) 
    (check-equal? (transaction-flag tx) "g")
    (check-equal? (transaction-lock-time tx) 10)
    (check-equal? (transaction-version-number  tx) 10)
    (check-equal? (length (transaction-inputs tx)) 2)
    (check-equal? (input-script (list-ref (transaction-inputs tx) 0)) "pkh(alice)")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 0)) "w1")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 0)) (outpoint "abcd" 1))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 0)) (read-little-endian-hex-string "FFFFFFFF"))
    
    (check-equal? (input-script (list-ref (transaction-inputs tx) 1)) "op2")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 1)) "w2")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 1)) (outpoint "hash2" 2))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 1)) (read-little-endian-hex-string "FFFFFFFF"))
    ))

(module+ test
  (test-case
   "parse transactions with optional input fields not provided"
   (let ([tx (transaction #:lock-time 10
                          #:version-number 10
                          #:flag "g"
                          #:inputs ((#:witness "w1" #:prevout ("hash1" 1) #:script "op1")
                                    (#:sequence "EFFFFFFF" #:witness "w2" #:prevout ("hash2" 2) #:script "op2")))])
     (check-equal? (input-witness (list-ref (transaction-inputs tx) 0)) "w1")
     (check-equal? (input-sequence (list-ref (transaction-inputs tx) 1)) (read-little-endian-hex-string "EFFFFFFF"))
     )))

(module+ test
  (test-case
   "parse transactions with outputs"
   (let ([tx (transaction #:lock-time 10
             #:version-number 10
             #:flag "g"
             #:inputs ((#:sequence "FFFFFFFF" #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                       (#:sequence "FFFFFFFF" #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
             #:outputs ((#:script "aa" #:amount 100) (#:script "bb" #:amount 200)))])
     (check-equal? (length (transaction-outputs tx)) 2)
     (check-equal? (output-script (list-ref (transaction-outputs tx) 0)) "aa")
     (check-equal? (output-value (list-ref (transaction-outputs tx) 0)) 100)
     (check-equal? (output-script (list-ref (transaction-outputs tx) 1)) "bb")
     (check-equal? (output-value (list-ref (transaction-outputs tx) 1)) 200))))

(module+ test
  (test-case
   "transaction with nothing but an output"
   (let ([tx (transaction #:outputs ((#:script "zz" #:amount 200.20)))])
     (check-equal? (length (transaction-outputs tx)) 1)
     (check-equal? (output-script (list-ref (transaction-outputs tx) 0)) "zz")
     (check-equal? (output-value (list-ref (transaction-outputs tx) 0)) 200.20))))
     
