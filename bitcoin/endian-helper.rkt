#lang racket/base

(provide to-little-endian-n-bytes)

(module+ test
  (require rackunit))


(define (to-little-endian-n-bytes value n)
  (let ([unsigned #f]
        [little-endian #f])
    (integer->integer-bytes value n unsigned little-endian)))

(module+ test
    (test-case
        "to little endian 4 bytes"
      (check-equal? (to-little-endian-n-bytes 100 4) (integer->integer-bytes 100 4 #f #f))))
