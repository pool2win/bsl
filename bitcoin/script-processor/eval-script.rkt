#lang errortrace racket

(require racket/list
         "environment.rkt"
         "../script-reader.rkt"
         "../transaction.rkt")

(provide apply-opcode
         eval-script
         eval-script-from-bytes)

(define (apply-opcode
         code
         script
         env
         stack
         altstack
         [tx (make-transaction #:version-number 0 #:flag 0 #:inputs '() #:outputs '() #:lock-time 0)]
         [input-index '()])
  (let-values ([(script stack altstack verified) (apply (get-opcode code env)
                                                        (list script stack altstack tx input-index))])
    (values script stack altstack verified)))

;; Evaluate script from given serialized bytes
(define (eval-script-from-bytes b env stack altstack)
  (println b)
  (eval-script (parse-script-from-bytes env b) env stack altstack))

;; Excute a script in the context of a bitcoin-environment
;; Script is a script in list format, e.g. '(op_dup op_hash160 #"deadbeef" op_equalverify)
;; Return same as apply-opcode (values script stack altstack verified)
(define (eval-script script env stack altstack [verified #t])
  (printf "in eval-script ~a ~a ~a\n" script stack verified)
  (cond
    [(empty? script)
     (values script
             stack
             altstack
             (and verified (not (empty? stack)) (not (equal? (first stack) 0))))]
    [(and (string? (first script))
          (string-prefix? (first script) "'")
          (string-suffix? (first script) "'"))
     (values (list-tail script 1)
             (cons (string->bytes/latin-1 (first script)) stack)
             altstack
             verified)]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a ~a ~a ~a" (first script) (is-opcode? (first script) env) script stack)]
    [else
     (printf "before: ~a ~a\n" script stack)
     (let-values ([(script stack altstack verified)
                   (apply-opcode (first script) (rest script) env stack altstack)])
       (printf "result: ~a ~a\n" script stack)
       (eval-script script env stack altstack verified))]))
