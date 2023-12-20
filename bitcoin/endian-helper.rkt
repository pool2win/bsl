#lang racket/base

(require file/sha1 binaryio/integer)

(provide to-little-endian-n-bytes
         read-little-endian-bytes
         read-little-endian-hex-string
         to-little-endian-hex-string
         serialize-for-script)

(module+ test
  (require rackunit))

(define (to-little-endian-n-bytes value num-bytes)
  (let ([signed #f] [big-endian #f]) (integer->integer-bytes value num-bytes signed big-endian)))

(define (serialize-for-script value)
  (let ([num-bytes (integer-bytes-length (abs value) #t)])
    (cond
      [(> value 0) (to-little-endian-n-bytes value num-bytes)]
      [else (integer->integer-bytes (bitwise-ior (integer-bytes->integer (to-little-endian-n-bytes (abs value) num-bytes) #f #t) #x80) num-bytes #f #t)])))

;; reads bytes in little endian into an integer
(define (read-little-endian-bytes data)
  (let ([signed #f] [big-endian #f]) (integer-bytes->integer data signed big-endian)))

;; converts an integer into a little endian hex string
(define (to-little-endian-hex-string value num-bytes)
  (bytes->hex-string (to-little-endian-n-bytes value num-bytes)))

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
  (test-case "serialize integers to bitcoin script serialization"
    (check-equal? (bytes->hex-string (serialize-for-script 1000)) "e803")
    (check-equal? (bytes->hex-string (serialize-for-script 100)) "64")
    (check-equal? (bytes->hex-string (serialize-for-script -1000)) "e883")
    (check-equal? (bytes->hex-string (serialize-for-script 127)) "7f")
    (check-equal? (bytes->hex-string (serialize-for-script 128)) "8000")
    (check-equal? (bytes->hex-string (serialize-for-script 129)) "8100")
    (check-equal? (bytes->hex-string (serialize-for-script -127)) "ff")
    (check-equal? (bytes->hex-string (serialize-for-script -128)) "8080")
    (check-equal? (bytes->hex-string (serialize-for-script -129)) "8180")))

