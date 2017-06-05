(require 'lunit)
(require 'hmac-sha1)

(luna-define-class test-hmac-sha1 (lunit-test-case))

(luna-define-method test-hmac-sha1-1 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-sha1 "Hi There" (make-string 20 ?\x0b)))
    "b617318655057264e28bc0b6fb378c8ef146be00")))

(luna-define-method test-hmac-sha1-2 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-sha1 "what do ya want for nothing?" "Jefe"))
    "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")))

(luna-define-method test-hmac-sha1-3 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string (hmac-sha1 (make-string 50 ?\xdd) (make-string 20 ?\xaa)))
    "125d7342b9ac11cd91a39af48aa17b4f63f175d3")))

(luna-define-method test-hmac-sha1-4 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-sha1
      (make-string 50 ?\xcd)
      (decode-hex-string "0102030405060708090a0b0c0d0e0f10111213141516171819")))
    "4c9007f4026250c6bc8414f9bf50c86c2d7235da")))

(luna-define-method test-hmac-sha1-5 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-sha1 "Test With Truncation" (make-string 20 ?\x0c)))
    "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04")))

(luna-define-method test-hmac-sha1-6 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-sha1-96 "Test With Truncation" (make-string 20 ?\x0c)))
    "4c1a03424b55e07fe7f27be1")))

(luna-define-method test-hmac-sha1-7 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-sha1
      "Test Using Larger Than Block-Size Key - Hash Key First"
      (make-string 80 ?\xaa)))
    "aa4ae5e15272d00e95705637ce8a3b55ed402112")))

(luna-define-method test-hmac-sha1-8 ((case test-hmac-sha1))
  (lunit-assert
   (string=
    (encode-hex-string
     (hmac-sha1
      "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"
      (make-string 80 ?\xaa)))
    "e8e99d0f45237d786d6bbaa7965c7808bbff1a91")))
