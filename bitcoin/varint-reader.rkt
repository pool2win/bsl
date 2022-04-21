#lang errortrace racket/base

(require file/sha1
         "endian-helper.rkt")

(provide read-varint-value)

(module+ test
  (require rackunit))


(define (read-varint-value in)
  (let* ([varint-2-bytes-flag (integer->integer-bytes #xfd 1 #f)]
         [varint-4-bytes-flag (integer->integer-bytes #xfe 1 #f)]
         [varint-8-bytes-flag (integer->integer-bytes #xff 1 #f)]
         [first-byte (read-bytes 1 in)]
         [value (cond
                  [(equal? first-byte varint-8-bytes-flag)
                   (read-bytes 8 in)]
                  [(equal? first-byte varint-4-bytes-flag)
                   (read-bytes 4 in)]
                  [(equal? first-byte varint-2-bytes-flag)
                   (read-bytes 2 in)]
                  [else first-byte])])
    (read-little-endian-bytes value)))


(module+ test
  (test-case
      "read a varint from given hex encoded bytes"
    (let ([value (read-varint-value (open-input-bytes (hex-string->bytes "bb")))]) 
      (check-equal?  value 187))
    (let ([value (read-varint-value (open-input-bytes (hex-string->bytes "fd0100")))])
      (check-equal?  value 1))
    (let ([value (read-varint-value (open-input-bytes (hex-string->bytes "fe01000000")))])
      (check-equal?  value 1))
    (let ([value (read-varint-value (open-input-bytes (hex-string->bytes "ff0100000000000000")))])
      (check-equal?  value 1))
    (let ([value (read-varint-value (open-input-bytes (hex-string->bytes "fd0100abab")))])
      (check-equal?  value 1))))
