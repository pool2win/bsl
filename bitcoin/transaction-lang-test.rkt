#lang racket

(require "transaction-lang.rkt"
         "transaction.rkt"
         "endian-helper.rkt")

(module+ test
  (require rackunit))

(module+ test
  (test-case "parse transactions from the new lang"
    )
  (check-equal? (transaction-version-number (tx)) default-version-number)
  (check-equal? (transaction-flag (tx)) default-flag)
  (check-equal? (transaction-lock-time (tx)) default-lock-time)
  (check-equal? (transaction-version-number (tx #:version-number 100)) 100)

  (check-equal? (transaction-flag (tx #:flag "f")) "f")
  (check-equal? (transaction-version-number (tx #:version-number 10 #:flag "g")) 10)
  (check-equal? (transaction-flag (tx #:version-number 10 #:flag "g")) "g")

  (let ([tx (tx #:lock-time 10
                #:version-number 10
                #:flag "g"
                #:inputs
                ((#:sequence "FFFFFFFF" #:prevout ("abcd" 1) #:witness "w1" #:script "pkh(alice)")
                 (#:sequence "FFFFFFFF" #:witness "w2" #:prevout ("hash2" 2) #:script "op2")))])
    (check-equal? (transaction-flag tx) "g")
    (check-equal? (transaction-lock-time tx) 10)
    (check-equal? (transaction-version-number tx) 10)
    (check-equal? (length (transaction-inputs tx)) 2)
    (check-equal? (input-script (list-ref (transaction-inputs tx) 0)) "pkh(alice)")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 0)) "w1")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 0)) (outpoint "abcd" 1))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 0))
                  (read-little-endian-hex-string "FFFFFFFF"))

    (check-equal? (input-script (list-ref (transaction-inputs tx) 1)) "op2")
    (check-equal? (input-witness (list-ref (transaction-inputs tx) 1)) "w2")
    (check-equal? (input-prevout (list-ref (transaction-inputs tx) 1)) (outpoint "hash2" 2))
    (check-equal? (input-sequence (list-ref (transaction-inputs tx) 1))
                  (read-little-endian-hex-string "FFFFFFFF"))))

(module+ test
  (test-case "parse transactions with optional input fields not provided"
    (let ([tx (tx #:lock-time 10
                  #:version-number 10
                  #:flag "g"
                  #:inputs
                  ((#:witness "w1" #:prevout ("hash1" 1) #:script "op1")
                   (#:sequence "EFFFFFFF" #:witness "w2" #:prevout ("hash2" 2) #:script "op2")))])
      (check-equal? (input-witness (list-ref (transaction-inputs tx) 0)) "w1")
      (check-equal? (input-sequence (list-ref (transaction-inputs tx) 1))
                    (read-little-endian-hex-string "EFFFFFFF")))))

(module+ test
  (test-case "parse transactions with outputs"
    (let ([tx (tx #:lock-time 10
                  #:version-number 10
                  #:flag "g"
                  #:inputs
                  ((#:sequence "FFFFFFFF" #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                   (#:sequence "FFFFFFFF" #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
                  #:outputs ((#:script "aa" #:amount 100) (#:script "bb" #:amount 200)))])
      (check-equal? (length (transaction-outputs tx)) 2)
      (check-equal? (output-script (list-ref (transaction-outputs tx) 0)) "aa")
      (check-equal? (output-value (list-ref (transaction-outputs tx) 0)) 100)
      (check-equal? (output-script (list-ref (transaction-outputs tx) 1)) "bb")
      (check-equal? (output-value (list-ref (transaction-outputs tx) 1)) 200))))

(module+ test
  (test-case "transaction with nothing but an output"
    (let ([tx (tx #:outputs ((#:script "zz" #:amount 200.20)))])
      (check-equal? (length (transaction-outputs tx)) 1)
      (check-equal? (output-script (list-ref (transaction-outputs tx) 0)) "zz")
      (check-equal? (output-value (list-ref (transaction-outputs tx) 0)) 200.20))))

(module+ test
  (test-case "parse transactions with CSV in sequence"
    (let ([tx (tx #:lock-time 10
                  #:version-number 10
                  #:flag "g"
                  #:inputs
                  ((#:sequence "csv 10" #:witness 'w1 #:prevout ("hash1" 1) #:script "op1")
                   (#:sequence "FFFFFFFF" #:witness 'w2 #:prevout ("hash2" 2) #:script "op2"))
                  #:outputs ((#:script "aa" #:amount 100) (#:script "bb" #:amount 200)))])
      (check-equal? (length (transaction-outputs tx)) 2)
      (check-equal? (output-script (list-ref (transaction-outputs tx) 0)) "aa")
      (check-equal? (output-value (list-ref (transaction-outputs tx) 0)) 100)
      (check-equal? (output-script (list-ref (transaction-outputs tx) 1)) "bb")
      (check-equal? (output-value (list-ref (transaction-outputs tx) 1)) 200))))
