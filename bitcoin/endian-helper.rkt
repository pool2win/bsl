#lang racket/base

(provide to-little-endian-n-bytes
         read-little-endian-n-bytes)

(module+ test
  (require rackunit))


(define (to-little-endian-n-bytes value num_bytes)
  (let ([signed #f]
        [big-endian #f])
    (integer->integer-bytes value num_bytes signed big-endian)))

(define (read-little-endian-n-bytes data)
  (let ([signed #f]
        [big-endian #f])
    (integer-bytes->integer data signed big-endian)))

(module+ test
    (test-case
        "to little endian 4 bytes"
      (check-equal? (to-little-endian-n-bytes 100 4) (integer->integer-bytes 100 4 #f #f)))
    (test-case
        "read little endian 4 bytes"
      (check-equal? (read-little-endian-n-bytes #"\1\0\0\0") 1)))
