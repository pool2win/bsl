#lang racket/base

(require file/sha1
         "script-reader.rkt")

(module+ test
  (require rackunit))

(module+ test
  (test-case "parse script from bytes"
    (let* ([script (parse-script-from-bytes
                    (hex-string->bytes "76A91489ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA88AC"))])
      (check-equal? (list-ref script 0) #x76)
      (check-equal? (list-ref script 1) #xA9)
      (check-equal? (list-ref script 2) #x14)
      (check-equal? (list-ref script 3)
                    (hex-string->bytes "89ABCDEFABBAABBAABBAABBAABBAABBAABBAABBA"))
      (check-equal? (list-ref script 4) #x88)
      (check-equal? (list-ref script 5) #xAC))
    (let* ([script (parse-script-from-bytes (hex-string->bytes "76A94c0289AB88AC"))])
      (check-equal? (list-ref script 0) #x76)
      (check-equal? (list-ref script 1) #xA9)
      (check-equal? (list-ref script 2) #x4c)
      (check-equal? (list-ref script 3) 2)
      (check-equal? (list-ref script 4) (hex-string->bytes "89AB"))
      (check-equal? (list-ref script 5) #x88)
      (check-equal? (list-ref script 6) #xAC))
    (let* ([script (parse-script-from-bytes (hex-string->bytes "76A94d020089AB88AC"))])
      (check-equal? (list-ref script 0) #x76)
      (check-equal? (list-ref script 1) #xA9)
      (check-equal? (list-ref script 2) #x4d)
      (check-equal? (list-ref script 3) #x02)
      (check-equal? (list-ref script 4) (hex-string->bytes "89AB"))
      (check-equal? (list-ref script 5) #x88)
      (check-equal? (list-ref script 6) #xAC))
    (let* ([script (parse-script-from-bytes (hex-string->bytes "76A94e0200000089AB88AC"))])
      (check-equal? (list-ref script 0) #x76)
      (check-equal? (list-ref script 1) #xA9)
      (check-equal? (list-ref script 2) #x4e)
      (check-equal? (list-ref script 3) 2)
      (check-equal? (list-ref script 4) (hex-string->bytes "89AB"))
      (check-equal? (list-ref script 5) #x88)
      (check-equal? (list-ref script 6) #xAC))
    (let* ([script
            (parse-script-from-bytes
             (hex-string->bytes
              "4830450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01"))])
      (check-equal? (list-ref script 0) #x48)
      (check-equal?
       (list-ref script 1)
       (hex-string->bytes
        "30450221008b9d1dc26ba6a9cb62127b02742fa9d754cd3bebf337f7a55d114c8e5cdd30be022040529b194ba3f9281a99f2b1c0a19c0489bc22ede944ccf4ecbab4cc618ef3ed01")))))
