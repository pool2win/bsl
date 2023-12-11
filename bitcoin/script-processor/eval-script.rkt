#lang errortrace racket

(require racket/list
         "environment.rkt"
         "../script-reader.rkt"
         "../transaction.rkt")

(provide apply-opcode
         skip-apply?
         eval-script
         eval-script-from-bytes)

;; apply an opcode only if it is a conditional opcode or there are no 0s in condstack
(define (apply? opcode condstack env)
  (or (opcode-equal? opcode 'op_if env)
      (opcode-equal? opcode 'op_notif env)
      (opcode-equal? opcode 'op_else env)
      (opcode-equal? opcode 'op_endif env)
      (not (skip-apply? condstack))))

(define (skip-apply? condstack)
  (member 0 condstack))

(define (apply-opcode
         code
         script
         env
         stack
         altstack
         condstack
         [tx (make-transaction #:version-number 0 #:flag 0 #:inputs '() #:outputs '() #:lock-time 0)]
         [input-index '()])
  (if (apply? code condstack env)
      (apply (get-opcode code env) (list script stack altstack condstack tx input-index))
      (values script stack altstack condstack #t)))

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
             (and verified (or (empty? stack) (not (equal? (first stack) 0)))))]
    [(and (string? (first script))
          (string-prefix? (first script) "'")
          (string-suffix? (first script) "'"))
     (values (list-tail script 1)
             (cons (string->bytes/latin-1 (first script)) stack)
             altstack
             condstack
             verified)]
    [(and (apply? (first script) condstack env)
          (not (is-opcode? (first script) env)))
     (error "Bad script ~a ~a ~a ~a\n" (first script) (is-opcode? (first script) env) script stack)]
    [else
     (let-values ([(script stack altstack condstack verified)
                   (apply-opcode (first script) (rest script) env stack altstack condstack)])
       (printf "result: ~a ~a ~a\n" script stack condstack)
       (eval-script script env stack altstack condstack verified))]))
