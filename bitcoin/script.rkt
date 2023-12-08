#lang racket

(module+ test
  (require rackunit))

(require file/sha1
         "script-processor/environment.rkt"
         "endian-helper.rkt")

(provide hr-script
         p2pkh-pub-script
         p2pkh-script-sig
         verify-stack
         verify
         script->hex-string
         serialize-number
         serialize-data)

(define-syntax-rule (p2pkh-pub-script pubkey)
  `(op_dup op_hash160 (hash160 pubkey) op_equalverify op_checksig))

(define-syntax-rule (p2pkh-script-sig signature pubkey) `(,signature ,pubkey))

(define (verify-stack stack)
  #t)

(define (verify script-pub-key script-sig)
  (let ([stack (list script-sig script-pub-key)]) (verify-stack stack)))

(define (hr-script script env)
  (for/list ([s script])
    (if (is-opcode? s env) (get-hr-opcode s env) s)))

(define (script->hex-string script)
  (map (lambda (s)
         (cond
           [(integer? s) (to-little-endian-hex-string s 1)]
           [(bytes? s) (bytes->hex-string (read-little-endian-bytes s))]))
       script))

(define (serialize-number s env)
  (let ([num (string->number s)])
    (cond
      [(= num -1) (get-serialized-opcode 'op_1negate env)]
      [(and (>= num 0) (<= num 16))
       (get-serialized-opcode (string->symbol (string-append "op_" s)) env)]
      [(and (> num 16) (<= num 75)) (bytes-append* (list #"\1" (to-n-bytes num 1)))]
      [(and (> num 75) (<= num #xff))
       (bytes-append* (list (get-serialized-opcode 'op_pushdata1 env) #"\1" (to-n-bytes num 1)))]
      [(and (> num #xff) (<= num #xffff))
       (bytes-append* (list (get-serialized-opcode 'op_pushdata2 env) #"\2" (to-n-bytes num 2)))]
      [else
       (bytes-append* (list (get-serialized-opcode 'op_pushdata4 env) #"\4" (to-n-bytes num 4)))])))

;; For non numerical data, we use the length of the data to
(define (serialize-data d env)
  (let ([num-bytes (bytes-length d)])
    (cond
      [(or (= num-bytes 0) (equal? d #"\0")) #"\0"]
      [(and (= num-bytes 1) (< (integer-bytes->integer d 1) 16))
       (get-serialized-opcode
        (string->symbol (string-append "op_" (number->string (integer-bytes->integer d 1 #f))))
        env)]
      [(and (= num-bytes 1) (= (integer-bytes->integer d 1 #f) #x4f))
       (get-serialized-opcode 'op_1negate env)]
      [(< num-bytes #x4c) (list (to-little-endian-n-bytes num-bytes 1) d)]
      [(and (>= num-bytes #x4c) (< num-bytes #xff))
       (list (get-serialized-opcode 'op_pushdata1 env) (to-little-endian-n-bytes num-bytes 1) d)]
      [(and (>= num-bytes #xff) (< num-bytes #xffff))
       (list (get-serialized-opcode 'op_pushdata2 env) (to-little-endian-n-bytes num-bytes 2) d)]
      [(>= num-bytes #xffff)
       (list (get-serialized-opcode 'op_pushdata4 env) (to-little-endian-n-bytes num-bytes 4) d)])))

(module+ test
  (require "../crypto-utils.rkt"
           "script-processor/bitcoin-environment.rkt")
  (test-case "Test p2pkh stack verfication"
    (let* ([alice:keypair (generate-keypair)] [alice:pubkey (keypair-pub alice:keypair)])
      (check-true (verify (p2pkh-pub-script alice:pubkey) (p2pkh-script-sig "sig" alice:pubkey)))))
  (test-case "serialize numbers"
    (let ([env (make-bitcoin-environment)])
      (check-equal? (serialize-number "-1" env) #"O")
      (check-equal? (serialize-number "0" env) #"\0")
      (check-equal? (serialize-number "1" env) #"Q")
      (check-equal? (serialize-number "16" env) #"`")
      (check-equal? (serialize-number "33" env) #"\1!")
      (check-equal? (serialize-number "85" env) #"L\1U")
      (check-equal? (serialize-number "511" env) #"M\2\377\1")
      (check-equal? (serialize-number "66047" env) #"N\4\377\1\1\0")))
  (test-case "serialize data"
    (let ([env (make-bitcoin-environment)]
          [medium-bytes (bytes-join (for/list ([i (in-range 0 512)])
                                      #"A")
                                    #"")]
          [large-bytes (bytes-join (for/list ([i (in-range 0 70000)])
                                     #"A")
                                   #"")])
      (check-equal? (serialize-data #"" env) #"\0")
      (check-equal? (serialize-data #"\0" env) #"\0")
      (check-equal? (serialize-data #"\n" env) (get-serialized-opcode 'op_10 env))
      (check-equal? (serialize-data #"O" env) (get-serialized-opcode 'op_1negate env))
      (check-equal?
       (serialize-data #"ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABC"
                       env)
       (list #"K" #"ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABC"))
      (check-equal?
       (serialize-data
        #"ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD"
        env)
       (list #"L"
             #"P"
             #"ABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCDABCD"))
      (check-equal? (serialize-data medium-bytes env)
                    (list #"M" (integer->integer-bytes 512 2 #f) medium-bytes))
      (check-equal? (serialize-data large-bytes env)
                    (list #"N" (integer->integer-bytes 70000 4 #f) large-bytes)))))
