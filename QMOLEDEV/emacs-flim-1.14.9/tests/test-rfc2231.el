(require 'lunit)
(require 'mime)

(luna-define-class test-rfc2231 (lunit-test-case))

;;;
;;; Parameter Value Continuations
;;;

;; The content-type field
;;
;;	Content-Type: message/external-body; access-type=URL;
;;	 URL*0="ftp://";
;;	 URL*1="cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar"
;;
;; is semantically identical to
;;
;;	Content-Type: message/external-body; access-type=URL;
;;	 URL="ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar"
;;
(luna-define-method test-rfc2231-continuation-1 ((case test-rfc2231))
  (lunit-assert
   (eq
    (mime-content-type-primary-type
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))
    (mime-content-type-primary-type
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")))))

(luna-define-method test-rfc2231-continuation-2 ((case test-rfc2231))
  (lunit-assert
   (eq
    (mime-content-type-subtype
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))
    (mime-content-type-subtype
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")))))

(luna-define-method test-rfc2231-continuation-3 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "access-type")
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "access-type"))))

(luna-define-method test-rfc2231-continuation-4 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "url")
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "url"))))

;;;
;;; Parameter Value Character Set and Language Information
;;;

;;	Content-Type: application/x-stuff;
;;	 title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A
(luna-define-method test-rfc2231-charset-language-1 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")
     "title")
    "This is ***fun***")))

(luna-define-method test-rfc2231-charset-language-2 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*=''This%20is%20%2A%2A%2Afun%2A%2A%2A")
     "title")
    "This is ***fun***")))

;;;
;;; Combining Character Set, Language, and Parameter Continuations
;;;

;;	Content-Type: application/x-stuff;
;;	 title*0*=us-ascii'en'This%20is%20even%20more%20;
;;	 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
;;	 title*2="isn't it!"
(luna-define-method test-rfc2231-charset-language-continuation-1 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn't it!\"")
     "title")
    "This is even more ***fun*** isn't it!")))

;; MIME states that parameters are not order sensitive.
(luna-define-method test-rfc2231-charset-language-continuation-2 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*2=\"isn't it!\";
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*0*=us-ascii'en'This%20is%20even%20more%20")
     "title")
    "This is even more ***fun*** isn't it!")))

;; ABNF states that `ext-octet' is case-insensitive.
(luna-define-method test-rfc2231-charset-language-continuation-3 ((case test-rfc2231))
  (lunit-assert
   (let ((case-fold-search nil))
     (string=
      (mime-content-type-parameter
       (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2a%2a%2afun%2a%2a%2a")
       "title")
      "This is ***fun***"))))

;; unencoded segments MUST NOT be decoded.
(luna-define-method test-rfc2231-charset-language-continuation-4 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn%27t%20it!\"")
     "title")
    "This is even more ***fun*** isn%27t%20it!")))

;;;
;;; Language specification in Encoded Words
;;;

(luna-define-method test-rfc2231-encoded-word-1 ((case test-rfc2231))
  (lunit-assert
   (string=
    (eword-decode-encoded-word "=?US-ASCII?Q?Keith_Moore?=")
    "Keith Moore")))

(luna-define-method test-rfc2231-encoded-word-2 ((case test-rfc2231))
  (lunit-assert
   (string=
    (eword-decode-encoded-word "=?US-ASCII*EN?Q?Keith_Moore?=")
    "Keith Moore")))

(luna-define-method test-rfc2231-encoded-word-3 ((case test-rfc2231))
  (lunit-assert
   (eq
    (get-text-property
     0 'mime-language
     (eword-decode-encoded-word "=?US-ASCII*EN?Q?Keith_Moore?="))
    'en)))

;;;
;;; Language specification in FLIM
;;;

;; both flim-1_13-rfc2231 and flim-1_14-rfc2231 choose to put language
;; info to the `mime-language' text-property of the parameter value.

(luna-define-method test-rfc2231-mime-language-1 ((case test-rfc2231))
  (lunit-assert
   (eq
    (get-text-property
     0 'mime-language
     (mime-content-type-parameter
      (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")
      "title"))
    'en-us)))

(luna-define-method test-rfc2231-mime-language-2 ((case test-rfc2231))
  (lunit-assert
   (eq
    (get-text-property
     0 'mime-language
     (mime-content-type-parameter
      (mime-parse-Content-Type "application/x-stuff;
 title*=US-ASCII'EN-US'This%20is%20%2A%2A%2Afun%2A%2A%2A")
      "title"))
    'en-us)))

(luna-define-method test-rfc2231-mime-language-3 ((case test-rfc2231))
  (lunit-assert
   (null
    (get-text-property
     0 'mime-language
     (mime-content-type-parameter
      (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii''This%20is%20%2A%2A%2Afun%2A%2A%2A")
      "title")))))

(luna-define-method test-rfc2231-mime-language-4 ((case test-rfc2231))
  (lunit-assert
   (null
    (get-text-property
     0 'mime-language
     (mime-content-type-parameter
      (mime-parse-Content-Type "application/x-stuff;
 title*=''This%20is%20%2A%2A%2Afun%2A%2A%2A")
      "title")))))
