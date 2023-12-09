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
         condstack
         [tx (make-transaction #:version-number 0 #:flag 0 #:inputs '() #:outputs '() #:lock-time 0)]
         [input-index '()])
  (let-values ([(script stack altstack condstack verified)
                (apply (get-opcode code env) (list script stack altstack condstack tx input-index))])
    (values script stack altstack condstack verified)))

;; Evaluate script from given serialized bytes
(define (eval-script-from-bytes b env stack [altstack '()] [condstack '()])
  (println b)
  (eval-script (parse-script-from-bytes env b) env stack altstack condstack))

;; Excute a script in the context of a bitcoin-environment
;; Script is a script in list format, e.g. '(op_dup op_hash160 #"deadbeef" op_equalverify)
;; Return same as apply-opcode (values script stack altstack verified)
(define (eval-script script env [stack '()] [altstack '()] [condstack '()] [verified #t])
  (printf "in eval-script ~a ~a ~a ~a\n" script stack condstack verified)
  (cond
    [(empty? script)
     (values script
             stack
             altstack
             condstack
             (and verified (not (empty? stack)) (not (equal? (first stack) 0))))]
    [(and (string? (first script))
          (string-prefix? (first script) "'")
          (string-suffix? (first script) "'"))
     (values (list-tail script 1)
             (cons (string->bytes/latin-1 (first script)) stack)
             altstack
             condstack
             verified)]
    [(not (is-opcode? (first script) env))
     (error "Bad script ~a ~a ~a ~a\n" (first script) (is-opcode? (first script) env) script stack)]
    [else
     (let-values ([(script stack altstack condstack verified)
                   (apply-opcode (first script) (rest script) env stack altstack condstack)])
       (printf "result: ~a ~a ~a\n" script stack condstack)
       (eval-script script env stack altstack condstack verified))]))
