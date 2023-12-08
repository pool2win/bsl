#lang errortrace racket

(require json
         file/sha1
         "../script.rkt"
         "environment.rkt"
         "bitcoin-environment.rkt"
         "../endian-helper.rkt"
         "eval-script.rkt")

(module+ test
  (require rackunit))

(define (normalise e env)
  (cond
    [(regexp-match #rx"^[0-9]+$" e) (serialize-number e env)]
    [(and (string-prefix? e "0x") (regexp-match #rx"^[0-9A-Fa-f]+$" (substring e 2)))
     (hex-string->bytes (substring e 2))]
    [(and (string-prefix? e "'") (string-suffix? e "'"))
     (serialize-data (string->bytes/latin-1 (substring e 1 (sub1 (string-length e)))) env)]
    [else (get-serialized-opcode (string->symbol (string-downcase (string-append "op_" e))) env)]))

(define (serialized-script row env)
  (bytes-append* (flatten (map (lambda (s) (normalise s env)) (string-split row)))))

(module+ test
  (test-case "script_tests.json from bitcoin script test suite"
    (let* ([cases (call-with-input-file "./test_data/script_tests.json" read-json)]
           [env (make-bitcoin-environment)]
           [run-test (lambda (c)
                       (println c)
                       (let-values ([(script stack altstack verified)
                                     (eval-script-from-bytes
                                      (serialized-script (string-append (first c) " " (second c)) env)
                                      env
                                      '()
                                      '())])
                         (check-true verified)))])
      (for ([c cases])
        (if (> (length c) 1) (run-test c) '())))))
