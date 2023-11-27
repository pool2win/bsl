#lang errortrace racket/base

(require file/sha1
         "./endian-helper.rkt"
         "script.rkt"
         "./script-processor/environment.rkt"
         "./script-processor/bitcoin-environment.rkt"
         "varint-reader.rkt")

(provide parse-script-from-bytes)

(module+ test
  (require rackunit))

(define env (make-bitcoin-environment))

(define (parse-script-args in-port opcode)
  '())

(define (parse-script-from-bytes in-bytes)
  (parse-script (open-input-bytes in-bytes)))

(define (parse-script in-port [script '()])
  (let ([next-byte (read-bytes 1 in-port)])
    ;;(printf "~s\n" (hr-script script env))
    (println script)
    (cond
      [(eof-object? next-byte) script]
      [(not (is-opcode? (read-little-endian-bytes next-byte) env))
       (parse-script in-port
                     (append script
                             (list (read-bytes (read-little-endian-bytes next-byte) in-port))))]
      [else
       (let* ([opcode (read-little-endian-bytes next-byte)]
              [script-args (parse-script-args in-port opcode)])
         (parse-script
          in-port
          (append script (if (equal? script-args '()) (list opcode) (list opcode script-args)))))])))

(module+ test
  (test-case "parse script from bytes"
    (let* ([script (parse-script (open-input-bytes
                                  (hex-string->bytes
                                   "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC")))])
      (printf "~s\n" (hr-script script env))
      (check-equal? (list-ref script 0) #x76))))
