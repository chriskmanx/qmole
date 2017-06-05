(require 'lunit)
(require 'hmac-md5)

(luna-define-class test-hmac-md5 (lunit-test-case))

(luna-define-method test-hmac-md5-1 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-md5 "Hi There" (make-string 16 ?\x0b)))
    "9294727a3638bb1c13f48ef8158bfc9d")))

(luna-define-method test-hmac-md5-2 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-md5 "what do ya want for nothing?" "Jefe"))
    "750c783e6ab0b503eaa86e310a5db738")))

(luna-define-method test-hmac-md5-3 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-md5 (make-string 50 ?\xdd) (make-string 16 ?\xaa)))
    "56be34521d144c88dbb8c733f0e8b3f6")))

(luna-define-method test-hmac-md5-4 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-md5
      (make-string 50 ?\xcd)
      (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
    "697eaf0aca3a3aea3a75164746ffaa79")))

(luna-define-method test-hmac-md5-5 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-md5 "Test With Truncation" (make-string 16 ?\x0c)))
    "56461ef2342edc00f9bab995690efd4c")))

(luna-define-method test-hmac-md5-6 ((case test-hmac-md5))
  (lunit-assert
   (string=
     (encode-hex-string
      (hmac-md5-96 "Test With Truncation" (make-string 16 ?\x0c)))
     "56461ef2342edc00f9bab995")))

(luna-define-method test-hmac-md5-7 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-md5
      "Test Using Larger Than Block-Size Key - Hash Key First"
      (make-string 80 ?\xaa)))
    "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd")))

(luna-define-method test-hmac-md5-8 ((case test-hmac-md5))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-md5
      "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
      (make-string 80 ?\xaa)))
    "6f630fad67cda0ee1fb1f562db3aa53e")))
