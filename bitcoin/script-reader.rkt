#lang errortrace racket/base

(require "./endian-helper.rkt"
         "script.rkt"
         "./script-processor/environment.rkt"
         "./script-processor/bitcoin-environment.rkt"
         "varint-reader.rkt")

(provide parse-script-from-bytes)

(define env (make-bitcoin-environment))

(define (parse-script-args in-port opcode)
  (let ([reader (lambda (num-bytes) (list num-bytes (read-bytes num-bytes in-port)))])
    (cond
      [(and (>= opcode #x01) (<= opcode #x4b)) (list (read-bytes opcode in-port))]
      [(= opcode #x4c) (reader (read-little-endian-bytes (read-bytes 1 in-port)))]
      [(= opcode #x4d) (reader (read-little-endian-bytes (read-bytes 2 in-port)))]
      [(= opcode #x4e) (reader (read-little-endian-bytes (read-bytes 4 in-port)))]
      [else '()])))

(define (parse-script-from-bytes in-bytes)
  (parse-script (open-input-bytes in-bytes)))

(define (parse-script in-port [script '()])
  (let ([next-byte (read-bytes 1 in-port)])
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
          (append script (if (equal? script-args '()) (list opcode) (cons opcode script-args)))))])))
