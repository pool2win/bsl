#lang errortrace racket/base

(require racket/string
         racket/format
         racket/list
         racket/match
         "environment.rkt"
         "eval-script.rkt"
         "../../crypto-utils.rkt")

(provide make-bitcoin-environment)

(define (handle-conditional condition env script stack altstack)
  (match script
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env new-stack altstack) (eval-script ifexp env (rest stack) altstack)])
       (values (list-tail script 2) new-stack altstack #t))]
    [(list ifexp endifexp)
     #:when (and (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
       (values (list-tail script 2) (rest stack) altstack #t)]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (eq? (first stack) condition))
     (let-values ([(rest-script new-env stack altstack) (eval-script ifexp env (rest stack) altstack)])
       (values (list-tail script 4) stack altstack #t))]
    [(list ifexp elseexp elseifexp endifexp)
     #:when (and (eq? elseexp 'op_else) (eq? endifexp 'op_endif) (not (eq? (first stack) condition)))
     (let-values ([(rest-script new-env stack altstack) (eval-script elseifexp env (rest stack) altstack)])
       (values (list-tail script 4) stack altstack #t))]))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)])
    (add-opcode env '(op_0 op_false) #x00
                (lambda (script stack altstack)
                  (values script (cons 0 stack) altstack #t)))
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode env (string-join `("op_" ,(~a code)) "") code
                  (lambda (script stack altstack)
                    (values (list-tail script 1) (cons (first script) stack) altstack #t))))
    (add-opcode env '(op_pushdata1) #x4c
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_pushdata2) #x4d
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_pushdata4) #x4e
                (lambda (script stack altstack)
                  (values (list-tail script 2) (cons (second script) stack) altstack #t)))
    (add-opcode env '(op_1negate) #x4f
                (lambda (script stack altstack)
                  (values script  (cons -1 stack) altstack #t)))
    (add-opcode env '(op_1 op_true) #x51
                (lambda (script stack altstack)
                  (values script (cons 1 stack) altstack #t)))
    (for ([code (in-inclusive-range #x52 #x60)])
      (add-opcode env (string-join `("op_" ,(~a (- code 80))) "") code
                  (lambda (script stack altstack)
                    (values script (cons (- code 80) stack) altstack #t))))
    (add-opcode env '(op_nop) #x61
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_if) #x63
                (lambda (script stack altstack)
                  (handle-conditional 1 env script stack altstack )))
    (add-opcode env '(op_notif) #x64
                (lambda (script stack altstack)
                  (handle-conditional 0 env script stack altstack)))
    (add-opcode env '(op_else) #x67
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_endif) #x68
                (lambda (script stack altstack)
                  (values script stack altstack #t)))
    (add-opcode env '(op_verify) #x69
                (lambda (script stack altstack)
                  (values script stack altstack (eq? (first stack) 1))))
    (add-opcode env '(op_return) #x6a
                (lambda (script stack altstack)
                  (values script stack altstack #f)))
    ;; stack operations
    (add-opcode env '(op_toaltstack) #x6b
                (lambda (script stack altstack)
                  (values script (rest stack) (cons (first stack) altstack) #t)))
    (add-opcode env '(op_fromaltstack) #x6c
                (lambda (script stack altstack)
                  (values script (cons (first altstack) stack) (rest altstack) #t)))
    (add-opcode env '(op_ifdup) #x73
                (lambda (script stack altstack)
                  (cond
                    [(not (eq? (first stack) 0))
                     (values script (cons (first stack) stack) altstack #t)]
                    [else
                     (values script stack altstack #t)])))
    (add-opcode env '(op_depth) #x74
                (lambda (script stack altstack)
                  (values script (cons (length stack) stack) altstack #t)))
    (add-opcode env '(op_drop) #x75
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else (values script (rest stack) altstack #t)])))
    (add-opcode env '(op_dup) #x76
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else (values script (cons (first stack) stack) altstack #t)])))
    (add-opcode env '(op_nip) #x77
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else (values script (cons (first stack) (list-tail stack 2)) altstack #t)])))    
    (add-opcode env '(op_over) #x78
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else (values script (append (list (second stack) (first stack)) (list-tail stack 2)) altstack #t)])))
    (add-opcode env '(op_pick) #x79
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script (cons (list-ref stack (add1 (first stack))) (rest stack)) altstack #t)]
                    )))
    (add-opcode env '(op_roll) #x7a
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (let* ((posn (first stack))
                            (stack (rest stack)))
                       (values script
                               (append (cons (list-ref stack posn) (take stack posn)) (list-tail stack (add1 posn)))
                               altstack #t))]
                    )))
    (add-opcode env '(op_rot) #x7b
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 3) (values script stack altstack #t)]
                    [else
                     (let ((posn 3))
                       (values script
                               (append (cons (list-ref stack posn) (take stack posn)) (list-tail stack (add1 posn)))
                               altstack #t))]
                    )))
    (add-opcode env '(op_swap) #x7c
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script
                             (append (list (list-ref stack 1) (list-ref stack 0)) (list-tail stack 2))
                             altstack #t)]
                    )))
    (add-opcode env '(op_tuck) #x7d
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script
                             (append (take stack 2) (list (first stack)) (list-tail stack 2))
                             altstack #t)]
                    )))
    (add-opcode env '(op_2drop) #x6d
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script (list-tail stack 2) altstack #t)]
                    )))
    (add-opcode env '(op_2dup) #x6e
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script (append (take stack 2) stack) altstack #t)]
                    )))
    (add-opcode env '(op_3dup) #x6f
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 3) (values script stack altstack #t)]
                    [else
                     (values script (append (take stack 3) stack) altstack #t)]
                    )))
    (add-opcode env '(op_2over) #x6f
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 4) (values script stack altstack #t)]
                    [else
                     (values script (append (list (list-ref stack 2) (list-ref stack 3)) stack) altstack #t)]
                    )))    
    (add-opcode env '(op_2rot) #x71
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 6) (values script stack altstack #t)]
                    [else
                     (values script
                             (append (list (list-ref stack 4) (list-ref stack 5)) (take stack 4) (list-tail stack 6))
                             altstack #t)]
                    )))    
    (add-opcode env '(op_2swap) #x72
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 4) (values script stack altstack #t)]
                    [else
                     (values script
                             (append (take (list-tail stack 2) 2) (take stack 2) (list-tail stack 4))
                             altstack #t)]
                    )))    
    (add-opcode env '(op_size) #x82
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (string-length (first stack)) stack)
                             altstack #t)])
                ))
    (add-opcode env '(op_equal) #x87
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (equal? (first stack) (second stack)) (list-tail stack 2))
                             altstack #t)])
                ))
    (add-opcode env '(op_equalverify) #x88
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #f)]
                    [else
                     (values script
                             (list-tail stack 2)
                             altstack (equal? (first stack) (second stack)))])
                ))
    (add-opcode env '(op_1add) #x8b
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (add1 (first stack)) (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_1sub) #x8c
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (sub1 (first stack)) (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_negate) #x8f
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (* -1 (first stack)) (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_abs) #x90
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (abs (first stack)) (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_not) #x91
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [(equal? 0 (first stack))
                     (values script
                             (cons 1 (rest stack))
                             altstack #t)]
                    [else
                     (values script
                             (cons 0 (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_0notequal) #x92
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [(equal? 0 (first stack))
                     (values script
                             (cons 0 (rest stack))
                             altstack #t)]
                    [else
                     (values script
                             (cons 1 (rest stack))
                             altstack #t)])
                ))
    (add-opcode env '(op_add) #x93
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (+ (first stack) (second stack)) (list-tail stack 2))
                             altstack #t)])
                ))
    (add-opcode env '(op_sub) #x94
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [else
                     (values script
                             (cons (- (first stack) (second stack)) (list-tail stack 2))
                             altstack #t)])
                ))
    (add-opcode env '(op_booland) #x9a
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (not (equal? (first stack) 0)) (not (equal? (second stack) 0)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_boolor) #x9b
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(or (not (equal? (first stack) 0)) (not (equal? (second stack) 0)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_numequal) #x9c
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (equal? (first stack) (second stack)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_numequalverify) #x9d
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (equal? (first stack) (second stack)))
                     (values script (list-tail stack 2) altstack #t)]
                    [else
                     (values script (list-tail stack 2) altstack #f)])
                  ))
    (add-opcode env '(op_numnotequal) #x9e
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(or (not (number? (first stack))) (not (number? (second stack))) (not (equal? (first stack) (second stack))))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_lessthan) #x9f
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (< (second stack) (first stack)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_greaterthan) #xa0
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (> (second stack) (first stack)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_lessthanorequal) #xa1
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_greaterthanorequal) #xa2
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (>= (second stack) (first stack)))
                     (values script (cons 1 (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_min) #xa3
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
                     (values script (cons (second stack) (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons (first stack) (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_max) #xa4
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 2) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
                     (values script (cons (first stack) (list-tail stack 2)) altstack #t)]
                    [else
                     (values script (cons (second stack) (list-tail stack 2)) altstack #t)])
                  ))
    (add-opcode env '(op_within) #xa4
                (lambda (script stack altstack)
                  (cond
                    [(< (length stack) 3) (values script stack altstack #t)]
                    [(and (number? (first stack)) (number? (second stack)) (number? (third stack))
                          (<= (third stack) (first stack))
                          (>= (third stack) (second stack)))
                     (values script (cons 1 (list-tail stack 3)) altstack #t)]
                    [else
                     (values script (cons 0 (list-tail stack 3)) altstack #t)])
                  ))

    ;; crypto opcodes
    (add-opcode env '(op_ripemd160) #xa6
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script (cons (hash160 (first stack)) (rest stack)) altstack #t)])
                  ))
    (add-opcode env '(op_sha1) #xa7
                (lambda (script stack altstack)
                  (cond
                    [(empty? stack) (values script stack altstack #t)]
                    [else
                     (values script (cons (sha1 (first stack)) (rest stack)) altstack #t)])
                  ))
    env))  
