#lang errortrace racket

(require racket/string
         racket/format
         racket/list
         racket/match
         "environment.rkt"
         "eval-script.rkt"
         "../../crypto-utils.rkt"
         "../transaction.rkt"
         "../endian-helper.rkt")

(provide make-bitcoin-environment zero)

(module+ test
  (require rackunit))

(define zero #"0")

(define (top-truthy? s)
  (and (not (empty? s))
       (not (equal? (first s) zero))))

(define (as-number b)
  (cond [(equal? b zero) 0]
        [(number? b) b]
        [else (read-little-endian-bytes b)]))

;; Determine if the branch should be executed and tracks that in the condstack
;; Later the apply? function in eval-script will use condstack to decide to execute an opcode or not.
(define (handle-conditional opcode script stack altstack condstack)
  (printf "In handle condition ~v ~v ~v\n" script stack condstack)
  (cond
    [(or (and (equal? opcode 'op_if) (top-truthy? stack))
         (and (equal? opcode 'op_notif) (not (top-truthy? stack))))
     (if (skip-apply? condstack)
         (values script stack altstack (cons 0 condstack) true)
         (values script (rest stack) altstack (cons 1 condstack) true))]
    [(or (and (equal? opcode 'op_if) (not (top-truthy? stack)))
         (and (equal? opcode 'op_notif) (top-truthy? stack)))
     (if (skip-apply? condstack)
         (values script stack altstack (cons 0 condstack) true)
         (values script (rest stack) altstack (cons 0 condstack) true))]
    ;; return values of rest script after end, stack, altstack, verified as is the current status
    [(equal? opcode 'op_else)
     (values script stack altstack (cons (- 1 (first condstack)) (rest condstack)) true)]
    ;; return values of rest script, stack, altstack, verified as is the current status
    [(equal? opcode 'op_endif) (values script stack altstack (rest condstack) true)]))

(define (make-bitcoin-environment)
  (let ([env (make-initial-env)]
        [locktime-threshold 500000000]
        [safe-tail (lambda (s tail) (if (>= (length s) tail) (list-tail s tail) '()))]
        [safe-head (lambda (s head) (if (>= (length s) head) (list-ref s (sub1 head)) '()))])
    (add-opcode env
                '(op_0 op_false)
                #x00
                (lambda (script stack altstack condstack tx input-index)
                  (values script (cons zero stack) altstack condstack #t)))
    (for ([code (in-inclusive-range 1 75)])
      (add-opcode
       env
       (list (string->symbol (string-join `("op_" ,(~v code)) "")))
       code
       (lambda (script stack altstack condstack tx input-index)
         (values (safe-tail script 1) (cons (safe-head script 1) stack) altstack condstack #t))))
    (add-opcode env
                '(op_pushdata1)
                #x4c
                (lambda (script stack altstack condstack tx input-index)
                  (values (safe-tail script 2) (cons (second script) stack) altstack condstack #t)))
    (add-opcode env
                '(op_pushdata2)
                #x4d
                (lambda (script stack altstack condstack tx input-index)
                  (values (safe-tail script 2) (cons (second script) stack) altstack condstack #t)))
    (add-opcode env
                '(op_pushdata4)
                #x4e
                (lambda (script stack altstack condstack tx input-index)
                  (values (safe-tail script 2) (cons (second script) stack) altstack condstack #t)))
    (add-opcode env
                '(op_1negate)
                #x4f
                (lambda (script stack altstack condstack tx input-index)
                  (values script (cons -1 stack) altstack condstack #t)))
    (add-opcode env
                '(op_1 op_true)
                #x51
                (lambda (script stack altstack condstack tx input-index) ;
                  (values script (cons 1 stack) altstack condstack #t)))
    (for ([code (in-inclusive-range #x52 #x60)])
      (add-opcode env
                  (list (string->symbol (string-join `("op_" ,(~v (- code 80))) "")))
                  code
                  (lambda (script stack altstack condstack tx input-index)
                    (values script (cons (- code 80) stack) altstack condstack #t))))
    (add-opcode env
                '(op_nop)
                #x61
                (lambda (script stack altstack condstack tx input-index)
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_if)
                #x63
                (lambda (script stack altstack condstack tx input-index)
                  (handle-conditional 'op_if script stack altstack condstack)))
    (add-opcode env
                '(op_notif)
                #x64
                (lambda (script stack altstack condstack tx input-index)
                  (handle-conditional 'op_notif script stack altstack condstack)))
    (add-opcode env
                '(op_else)
                #x67
                (lambda (script stack altstack condstack tx input-index)
                  (handle-conditional 'op_else
                                      script
                                      stack
                                      altstack
                                      condstack))) ;; (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_endif)
                #x68
                (lambda (script stack altstack condstack tx input-index)
                  (handle-conditional 'op_endif
                                      script
                                      stack
                                      altstack
                                      condstack))) ;; (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_verify)
                #x69
                (lambda (script stack altstack condstack tx input-index)
                  (values script stack altstack condstack (top-truthy? stack))))
    (add-opcode env
                '(op_return)
                #x6a
                (lambda (script stack altstack condstack tx input-index)
                  (values script stack altstack condstack #f)))
    ;; stack operations
    (add-opcode env
                '(op_toaltstack)
                #x6b
                (lambda (script stack altstack condstack tx input-index)
                  (values script (rest stack) (cons (first stack) altstack) condstack #t)))
    (add-opcode env
                '(op_fromaltstack)
                #x6c
                (lambda (script stack altstack condstack tx input-index)
                  (values script (cons (first altstack) stack) (rest altstack) condstack #t)))
    (add-opcode env
                '(op_ifdup)
                #x73
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(not (eq? (first stack) zero))
                     (values script (cons (first stack) stack) altstack condstack #t)]
                    [else (values script stack altstack condstack #t)])))
    (add-opcode env
                '(op_depth)
                #x74
                (lambda (script stack altstack condstack tx input-index)
                  (values script (cons (length stack) stack) altstack condstack #t)))
    (add-opcode env
                '(op_drop)
                #x75
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [else (values script (rest stack) altstack condstack #t)])))
    (add-opcode env
                '(op_dup)
                #x76
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [else (values script (cons (first stack) stack) altstack condstack #t)])))
    (add-opcode
     env
     '(op_nip)
     #x77
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (first stack) (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode env
                '(op_over)
                #x78
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (second stack) stack)
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_pick)
                #x79
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (list-ref stack (add1 (as-number (first stack)))) (rest stack))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_roll)
                #x7a
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [(equal? (first stack) zero) (values script (rest stack) altstack condstack #t)]
                    [else
                     (let* ([posn (first stack)] [stack (rest stack)])
                       (values script
                               (append (cons (list-ref stack posn) (take stack posn))
                                       (safe-tail stack (add1 posn)))
                               altstack
                               condstack
                               #t))])))
    (add-opcode env
                '(op_rot)
                #x7b
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 3) (values script stack altstack condstack #t)]
                    [else
                     (let ([posn 2])
                       (values script
                               (append (cons (list-ref stack posn) (take stack posn))
                                       (safe-tail stack (add1 posn)))
                               altstack
                               condstack
                               #t))])))
    (add-opcode env
                '(op_swap)
                #x7c
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (append (list (list-ref stack 1) (list-ref stack 0)) (safe-tail stack 2))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_tuck)
                #x7d
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (append (take stack 2) (list (first stack)) (safe-tail stack 2))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_2drop)
                #x6d
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else (values script (safe-tail stack 2) altstack condstack #t)])))
    (add-opcode env
                '(op_2dup)
                #x6e
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else (values script (append (take stack 2) stack) altstack condstack #t)])))
    (add-opcode env
                '(op_3dup)
                #x6f
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 3) (values script stack altstack condstack #t)]
                    [else (values script (append (take stack 3) stack) altstack condstack #t)])))
    (add-opcode env
                '(op_2over)
                #x6f
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 4) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (append (list (list-ref stack 2) (list-ref stack 3)) stack)
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_2rot)
                #x71
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 6) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (append (list (list-ref stack 4) (list-ref stack 5))
                                     (take stack 4)
                                     (safe-tail stack 6))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_2swap)
                #x72
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 4) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (append (take (safe-tail stack 2) 2) (take stack 2) (safe-tail stack 4))
                             altstack
                             condstack
                             #t)])))
    (add-opcode
     env
     '(op_size)
     #x82
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (string-length (first stack)) stack) altstack condstack #t)])))
    (add-opcode env
                '(op_equal)
                #x87
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (equal? (first stack) (second stack)) (safe-tail stack 2))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_equalverify)
                #x88
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #f)]
                    [else
                     (values script
                             (safe-tail stack 2)
                             altstack
                             condstack
                             (equal? (first stack) (second stack)))])))
    (add-opcode
     env
     '(op_1add)
     #x8b
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (add1 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_1sub)
     #x8c
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (sub1 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_negate)
     #x8f
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (* -1 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_abs)
     #x90
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (abs (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_not)
     #x91
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [(equal? zero (first stack)) (values script (cons 1 (rest stack)) altstack condstack #t)]
         [else (values script (cons zero (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_0notequal)
     #x92
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [(equal? zero (first stack)) (values script (cons zero (rest stack)) altstack condstack #t)]
         [else (values script (cons 1 (rest stack)) altstack condstack #t)])))
    (add-opcode env
                '(op_add)
                #x93
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (+ (as-number (first stack)) (as-number (second stack))) (safe-tail stack 2))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_sub)
                #x94
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (- (first stack) (second stack)) (safe-tail stack 2))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_booland)
                #x9a
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [(and (not (equal? (first stack) zero)) (not (equal? (second stack) zero)))
                     (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
                    [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode env
                '(op_boolor)
                #x9b
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [(or (not (equal? (first stack) zero)) (not (equal? (second stack) zero)))
                     (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
                    [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_numequal)
     #x9c
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (equal? (first stack) (second stack)))
          (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_numequalverify)
     #x9d
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (equal? (first stack) (second stack)))
          (values script (safe-tail stack 2) altstack condstack #t)]
         [else (values script (safe-tail stack 2) altstack condstack #f)])))
    (add-opcode env
                '(op_numnotequal)
                #x9e
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 2) (values script stack altstack condstack #t)]
                    [(or (not (number? (first stack)))
                         (not (number? (second stack)))
                         (not (equal? (first stack) (second stack))))
                     (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
                    [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_lessthan)
     #x9f
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (< (second stack) (first stack)))
          (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_greaterthan)
     #xa0
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (> (second stack) (first stack)))
          (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_lessthanorequal)
     #xa1
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
          (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_greaterthanorequal)
     #xa2
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (>= (second stack) (first stack)))
          (values script (cons 1 (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons zero (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_min)
     #xa3
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
          (values script (cons (second stack) (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons (first stack) (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_max)
     #xa4
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(< (length stack) 2) (values script stack altstack condstack #t)]
         [(and (number? (first stack)) (number? (second stack)) (<= (second stack) (first stack)))
          (values script (cons (first stack) (safe-tail stack 2)) altstack condstack #t)]
         [else (values script (cons (second stack) (safe-tail stack 2)) altstack condstack #t)])))
    (add-opcode env
                '(op_within)
                #xa4
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(< (length stack) 3) (values script stack altstack condstack #t)]
                    [(and (number? (first stack))
                          (number? (second stack))
                          (number? (third stack))
                          (<= (third stack) (first stack))
                          (>= (third stack) (second stack)))
                     (values script (cons 1 (safe-tail stack 3)) altstack condstack #t)]
                    [else (values script (cons zero (safe-tail stack 3)) altstack condstack #t)])))

    ;; crypto opcodes
    (add-opcode
     env
     '(op_ripemd160)
     #xa6
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (ripemd160 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_sha1)
     #xa7
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (sha1 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_sha256)
     #xa8
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (sha256 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode
     env
     '(op_hash160)
     #xa9
     (lambda (script stack altstack condstack tx input-index)
       (cond
         [(empty? stack) (values script stack altstack condstack #t)]
         [else (values script (cons (hash160 (first stack)) (rest stack)) altstack condstack #t)])))
    (add-opcode env
                '(op_hash256)
                #xaa
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(empty? stack) (values script stack altstack condstack #t)]
                    [else
                     (values script
                             (cons (double-sha256-hash (first stack)) (rest stack))
                             altstack
                             condstack
                             #t)])))
    (add-opcode env
                '(op_codeseparator)
                #xab
                (lambda (script stack altstack condstack tx input-index)
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_checksig)
                #xac
                (lambda (script stack altstack condstack tx input-index)
                  ;; TODO - use checksig implementation
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_checksig)
                #xad
                (lambda (script stack altstack condstack tx input-index)
                  ;; TODO - use checksig implementation
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_checkmultisig)
                #xae
                (lambda (script stack altstack condstack tx input-index)
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_checkmultisigverify)
                #xaf
                (lambda (script stack altstack condstack tx input-index)
                  ;; TODO - use checksig implementation
                  (values script stack altstack condstack #t)))
    (add-opcode env
                '(op_checklocktimeverify)
                #xb1
                (lambda (script stack altstack condstack tx input-index)
                  (cond
                    [(or (empty? stack)
                         (equal? (first stack) zero)
                         (negative? (first stack))
                         (not (or (and (< (first stack) locktime-threshold)
                                       (< (transaction-lock-time tx) locktime-threshold))
                                  (and (>= (first stack) locktime-threshold)
                                       (>= (transaction-lock-time tx) locktime-threshold))))
                         (equal? #xffffffff
                                 (input-sequence (list-ref (transaction-inputs tx) input-index)))
                         (and (number? (first stack)) (> (first stack) (transaction-lock-time tx))))
                     (values script stack altstack condstack #f)]
                    [else (values script stack altstack condstack #t)])))
    (add-opcode env
                '(op_checksequenceverify)
                #xb2
                (lambda (script stack altstack condstack tx input-index)
                  (let ([disabled-flag (arithmetic-shift 1 31)]
                        [sequence-type-flag (arithmetic-shift 1 22)]
                        [sequence-locktime-mask #x0000ffff]
                        [sequence (input-sequence (list-ref (transaction-inputs tx) input-index))])
                    (cond
                      [(or (empty? stack)
                           (negative? (first stack))
                           (and (= (bitwise-and (first stack) disabled-flag) 0)
                                (or (< (transaction-version-number tx) 2)
                                    (= (bitwise-and sequence disabled-flag) 1)
                                    (not (= (bitwise-and (first stack) sequence-type-flag)
                                            (bitwise-and sequence sequence-type-flag)))
                                    (<= (first stack) sequence))))
                       (values script stack altstack condstack #f)]
                      [else (values script stack altstack condstack #t)]))))
    (for ([opcode-hex '(#x50 #x62 #x65 #x66 #x89 #x8a #x0ba #xb3 #xb4 #xb5 #xb6 #xb7 #xb8 #xb9)]
          [opcode '(op_reserved op_ver
                                op_verif
                                op_vernoti
                                op_reserved1
                                op_reserved2
                                op_nop1
                                op_nop4
                                op_nop5
                                op_nop6
                                op_nop7
                                op_nop8
                                op_nop9
                                op_nop10)]
          [validity '(true true true true true true true true true true true true true true)])
      (add-opcode env
                  (list opcode)
                  opcode-hex
                  (lambda (script stack altstack condstack tx input-index)
                    ;; The reserved words simply set the transaction validity
                    (values script stack altstack condstack validity))))
    env))
