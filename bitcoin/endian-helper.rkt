#lang racket/base

(require file/sha1)

(provide to-little-endian-n-bytes
         read-little-endian-bytes
         read-little-endian-hex-string
         to-little-endian-hex-string
         to-n-bytes)

(module+ test
  (require rackunit))

;; Return number as big endian bytes
(define (to-n-bytes value num_bytes)
  (integer->integer-bytes value num_bytes #f))

(define (to-little-endian-n-bytes value num_bytes)
  (let ([signed #f] [big-endian #f]) (integer->integer-bytes value num_bytes signed big-endian)))

;; reads bytes in little endian into an integer
(define (read-little-endian-bytes data)
  (let ([signed #f] [big-endian #f]) (integer-bytes->integer data signed big-endian)))

;; converts an integer into a little endian hex string
(define (to-little-endian-hex-string value num_bytes)
  (bytes->hex-string (to-little-endian-n-bytes value num_bytes)))

;; read a hex string in little endian into an integer
(define (read-little-endian-hex-string value)
  (read-little-endian-bytes (hex-string->bytes value)))

(module+ test
  (test-case "reading hex strings in little endian"
    (check-equal? (read-little-endian-hex-string "ff00") 255)
    (check-equal? (read-little-endian-hex-string "00ff") 65280))
  (test-case "to little endian 4 bytes"
    (check-equal? (to-little-endian-n-bytes 100 4) (integer->integer-bytes 100 4 #f #f)))
  (test-case "read little endian 4 bytes"
    (check-equal? (read-little-endian-bytes #"\1\0\0\0") 1))
  (test-case "convert integer to hex string"
    (check-equal? (to-little-endian-hex-string 255 4) "ff000000"))
  (test-case "convert integer to big endian bytes"
    (check-equal? (to-n-bytes 255 1) #"\377"))
  (test-case "convert integer to big endian bytes"
    (check-equal? (to-n-bytes 255 4) #"\377\0\0\0")))
