#lang racket/base

(provide to-little-endian-4-bytes)

(module+ test
  (require rackunit))


(define (to-little-endian-4-bytes n)
  (let ([unsigned #f]
        [little-endian #f])
    (integer->integer-bytes n 4 unsigned little-endian)))

(module+ test
    (test-case
        "to little endian 4 bytes"
      (check-equal? (to-little-endian-4-bytes 100) (integer->integer-bytes 100 4 #f #f))))
