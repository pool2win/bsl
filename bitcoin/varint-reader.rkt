#lang errortrace racket/base

(require file/sha1
         racket/stream
         "byte-reader.rkt"
         "endian-helper.rkt")

(provide read-varint-value)

(module+ test
  (require rackunit))


(define (read-varint-value from-stream)
  (let*-values ([(varint-2-bytes-flag) (integer->integer-bytes #xfd 1 #f)]
                [(varint-4-bytes-flag) (integer->integer-bytes #xfe 1 #f)]
                [(varint-8-bytes-flag) (integer->integer-bytes #xff 1 #f)]
                [(first-byte rest) (next-n-bytes-and-rest from-stream 1)]
                [(value rest) (cond
                                [(equal? first-byte varint-8-bytes-flag)
                                 (next-n-bytes-and-rest rest 8)]
                                [(equal? first-byte varint-4-bytes-flag)
                                 (next-n-bytes-and-rest rest 4)]
                                [(equal? first-byte varint-2-bytes-flag)
                                 (next-n-bytes-and-rest rest 2)]
                                [else
                                 (values first-byte rest)])])
    (values (read-little-endian-bytes value) rest)))


(module+ test
  (test-case
      "read a varint from given hex encoded bytes"
    (let-values ([(value rest) (read-varint-value (sequence->stream (in-bytes (hex-string->bytes "bb"))))]) 
      (check-equal?  value 187))
    (let-values ([(value rest) (read-varint-value (sequence->stream (in-bytes (hex-string->bytes "fd0100"))))]) 
      (check-equal?  value 1))
    (let-values ([(value rest) (read-varint-value (sequence->stream (in-bytes (hex-string->bytes "fe01000000"))))]) 
      (check-equal?  value 1))
    (let-values ([(value rest) (read-varint-value
                                (sequence->stream (in-bytes (hex-string->bytes "ff0100000000000000"))))])
      (check-equal?  value 1))
    (let-values ([(value rest) (read-varint-value (sequence->stream (in-bytes (hex-string->bytes "fd0100abab"))))])
      (check-equal?  value 1)
      (check-equal?  (stream-length rest) 2)
      (check-equal?  (bytes->hex-string (list->bytes (stream->list rest))) "abab"))))
