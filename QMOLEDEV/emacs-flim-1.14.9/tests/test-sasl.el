(require 'lunit)
(require 'sasl)

(luna-define-class test-sasl (lunit-test-case))

(luna-define-method test-sasl-find-mechanism ((case test-sasl))
  (let ((mechanisms sasl-mechanisms))
    (while mechanisms
      (let* ((sasl-mechanisms (list (car mechanisms))))
	(lunit-assert
	 (sasl-find-mechanism (list (car mechanisms)))))
      (setq mechanisms (cdr mechanisms)))))

(luna-define-method test-sasl-digest-md5-imap ((case test-sasl))
  (let* ((sasl-mechanisms '("DIGEST-MD5"))
	 (mechanism
	  (sasl-find-mechanism '("DIGEST-MD5")))
	 (client
	  (sasl-make-client mechanism "chris" "imap" "elwood.innosoft.com"))
	 (sasl-read-passphrase
	  #'(lambda (prompt)
	      "secret"))
	 step
	 response)
    (sasl-client-set-property client 'realm "elwood.innosoft.com")
    (sasl-client-set-property client 'cnonce "OA6MHXh6VqTrRk")
    (setq step (sasl-next-step client nil))
    (sasl-step-set-data
     step "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",\
qop=\"auth\",algorithm=md5-sess,charset=utf-8")
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    (setq response (sasl-digest-md5-parse-string (sasl-step-data step)))
    (lunit-assert
     (string=
      (plist-get response 'response) "d388dad90d4bbd760a152321f2143af7"))))

(luna-define-method test-sasl-digest-md5-acap ((case test-sasl))
  (let* ((sasl-mechanisms '("DIGEST-MD5"))
	 (mechanism
	  (sasl-find-mechanism '("DIGEST-MD5")))
	 (client
	  (sasl-make-client mechanism "chris" "acap" "elwood.innosoft.com"))
	 (sasl-read-passphrase
	  #'(lambda (prompt)
	      "secret"))
	 step
	 response)
    (sasl-client-set-property client 'realm "elwood.innosoft.com")
    (sasl-client-set-property client 'cnonce "OA9BSuZWMSpW8m")
    (setq step (sasl-next-step client nil))
    (sasl-step-set-data
     step "realm=\"elwood.innosoft.com\",nonce=\"OA9BSXrbuRhWay\",qop=\"auth\",\
algorithm=md5-sess,charset=utf-8")
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    (setq response (sasl-digest-md5-parse-string (sasl-step-data step)))
    (lunit-assert
     (string=
      (plist-get response 'response) "6084c6db3fede7352c551284490fd0fc"))))

(luna-define-method test-sasl-scram-md5-imap ((case test-sasl))
  (let* ((sasl-mechanisms '("SCRAM-MD5"))
	 (mechanism
	  (sasl-find-mechanism '("SCRAM-MD5")))
	 (client
	  (sasl-make-client mechanism "chris" "imap" "eleanor.innosoft.com"))
	 (sasl-read-passphrase
	  #'(lambda (prompt)
	      "secret stuff"))
	 step
	 response)
    (sasl-client-set-property client 'nonce
			      "<t4n4Pab9HB0Am/QLXB72eg@eleanor.innosoft.com>")
    (setq step (sasl-next-step client nil))
    (sasl-step-set-data step "")
    (setq step (sasl-next-step client step))
    (sasl-step-set-data
     step
     (base64-decode-string
      "dGVzdHNhbHQBAAAAaW1hcEBlbGVhbm9yLmlubm9zb2Z0LmNvbQBqaGNOWmxSdVBiemlGcCt2TFYrTkN3"))
    (setq step (sasl-next-step client step))
    (lunit-assert
     (string= (sasl-step-data step)
           (base64-decode-string "AQAAAMg9jU8CeB4KOfk7sUhSQPs=")))))

(luna-define-method test-sasl-ntlm-imap ((case test-sasl))
  (let* ((sasl-mechanisms '("NTLM"))
	 (mechanism
	  (sasl-find-mechanism '("NTLM")))
	 (client
	  (sasl-make-client mechanism "kawagish@nokiaseap" "imap" "xxx.yyy.com"))
	 (sasl-read-passphrase
	  #'(lambda (passphrase)
	      "!\"#456secret"))
	 step
	 response)
    ;; init
    (setq step (sasl-next-step client nil))
    ;; generate authentication request
    (sasl-step-set-data step "")
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    ;; (base64-encode-string (sasl-step-data step) t) is sent to server
    ;; generate response to challenge
    (sasl-step-set-data
     step
     (string-as-unibyte
      (base64-decode-string
       "TlRMTVNTUAACAAAADAAMADAAAAAFggEApmEjGvh9M8YAAAAAAAAAAAAAAAA8AAAATgBPAEsARQBYAEMA")))
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    (setq response (base64-encode-string (sasl-step-data step) t))
    (lunit-assert
     (string=
      response "TlRMTVNTUAADAAAAGAAYAEAAAAAYABgAWAAAABIAEgBwAAAAEAAQAIIAAAAQABAAkgAAAAAAAABiAAAABYIBAIwN9i7qK/9Y31dIDR6JQTaBbjcLJm8Sc6VogMe7fnHP96+eQ5Yf3ys2nIY4rx+iQG4AbwBrAGkAYQBzAGUAYQBwAGsAYQB3AGEAZwBpAHMAaABrAGEAdwBhAGcAaQBzAGgA"))
;;response
))
