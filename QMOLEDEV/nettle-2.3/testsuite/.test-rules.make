aes-test$(EXEEXT): aes-test.$(OBJEXT)
	$(LINK) aes-test.$(OBJEXT) $(TEST_OBJS) -o aes-test$(EXEEXT)

arcfour-test$(EXEEXT): arcfour-test.$(OBJEXT)
	$(LINK) arcfour-test.$(OBJEXT) $(TEST_OBJS) -o arcfour-test$(EXEEXT)

arctwo-test$(EXEEXT): arctwo-test.$(OBJEXT)
	$(LINK) arctwo-test.$(OBJEXT) $(TEST_OBJS) -o arctwo-test$(EXEEXT)

blowfish-test$(EXEEXT): blowfish-test.$(OBJEXT)
	$(LINK) blowfish-test.$(OBJEXT) $(TEST_OBJS) -o blowfish-test$(EXEEXT)

cast128-test$(EXEEXT): cast128-test.$(OBJEXT)
	$(LINK) cast128-test.$(OBJEXT) $(TEST_OBJS) -o cast128-test$(EXEEXT)

base16-test$(EXEEXT): base16-test.$(OBJEXT)
	$(LINK) base16-test.$(OBJEXT) $(TEST_OBJS) -o base16-test$(EXEEXT)

base64-test$(EXEEXT): base64-test.$(OBJEXT)
	$(LINK) base64-test.$(OBJEXT) $(TEST_OBJS) -o base64-test$(EXEEXT)

camellia-test$(EXEEXT): camellia-test.$(OBJEXT)
	$(LINK) camellia-test.$(OBJEXT) $(TEST_OBJS) -o camellia-test$(EXEEXT)

des-test$(EXEEXT): des-test.$(OBJEXT)
	$(LINK) des-test.$(OBJEXT) $(TEST_OBJS) -o des-test$(EXEEXT)

des3-test$(EXEEXT): des3-test.$(OBJEXT)
	$(LINK) des3-test.$(OBJEXT) $(TEST_OBJS) -o des3-test$(EXEEXT)

des-compat-test$(EXEEXT): des-compat-test.$(OBJEXT)
	$(LINK) des-compat-test.$(OBJEXT) $(TEST_OBJS) -o des-compat-test$(EXEEXT)

md2-test$(EXEEXT): md2-test.$(OBJEXT)
	$(LINK) md2-test.$(OBJEXT) $(TEST_OBJS) -o md2-test$(EXEEXT)

md4-test$(EXEEXT): md4-test.$(OBJEXT)
	$(LINK) md4-test.$(OBJEXT) $(TEST_OBJS) -o md4-test$(EXEEXT)

md5-test$(EXEEXT): md5-test.$(OBJEXT)
	$(LINK) md5-test.$(OBJEXT) $(TEST_OBJS) -o md5-test$(EXEEXT)

md5-compat-test$(EXEEXT): md5-compat-test.$(OBJEXT)
	$(LINK) md5-compat-test.$(OBJEXT) $(TEST_OBJS) -o md5-compat-test$(EXEEXT)

memxor-test$(EXEEXT): memxor-test.$(OBJEXT)
	$(LINK) memxor-test.$(OBJEXT) $(TEST_OBJS) -o memxor-test$(EXEEXT)

ripemd160-test$(EXEEXT): ripemd160-test.$(OBJEXT)
	$(LINK) ripemd160-test.$(OBJEXT) $(TEST_OBJS) -o ripemd160-test$(EXEEXT)

sha1-test$(EXEEXT): sha1-test.$(OBJEXT)
	$(LINK) sha1-test.$(OBJEXT) $(TEST_OBJS) -o sha1-test$(EXEEXT)

sha224-test$(EXEEXT): sha224-test.$(OBJEXT)
	$(LINK) sha224-test.$(OBJEXT) $(TEST_OBJS) -o sha224-test$(EXEEXT)

sha256-test$(EXEEXT): sha256-test.$(OBJEXT)
	$(LINK) sha256-test.$(OBJEXT) $(TEST_OBJS) -o sha256-test$(EXEEXT)

sha384-test$(EXEEXT): sha384-test.$(OBJEXT)
	$(LINK) sha384-test.$(OBJEXT) $(TEST_OBJS) -o sha384-test$(EXEEXT)

sha512-test$(EXEEXT): sha512-test.$(OBJEXT)
	$(LINK) sha512-test.$(OBJEXT) $(TEST_OBJS) -o sha512-test$(EXEEXT)

serpent-test$(EXEEXT): serpent-test.$(OBJEXT)
	$(LINK) serpent-test.$(OBJEXT) $(TEST_OBJS) -o serpent-test$(EXEEXT)

twofish-test$(EXEEXT): twofish-test.$(OBJEXT)
	$(LINK) twofish-test.$(OBJEXT) $(TEST_OBJS) -o twofish-test$(EXEEXT)

knuth-lfib-test$(EXEEXT): knuth-lfib-test.$(OBJEXT)
	$(LINK) knuth-lfib-test.$(OBJEXT) $(TEST_OBJS) -o knuth-lfib-test$(EXEEXT)

cbc-test$(EXEEXT): cbc-test.$(OBJEXT)
	$(LINK) cbc-test.$(OBJEXT) $(TEST_OBJS) -o cbc-test$(EXEEXT)

ctr-test$(EXEEXT): ctr-test.$(OBJEXT)
	$(LINK) ctr-test.$(OBJEXT) $(TEST_OBJS) -o ctr-test$(EXEEXT)

gcm-test$(EXEEXT): gcm-test.$(OBJEXT)
	$(LINK) gcm-test.$(OBJEXT) $(TEST_OBJS) -o gcm-test$(EXEEXT)

hmac-test$(EXEEXT): hmac-test.$(OBJEXT)
	$(LINK) hmac-test.$(OBJEXT) $(TEST_OBJS) -o hmac-test$(EXEEXT)

meta-hash-test$(EXEEXT): meta-hash-test.$(OBJEXT)
	$(LINK) meta-hash-test.$(OBJEXT) $(TEST_OBJS) -o meta-hash-test$(EXEEXT)

meta-cipher-test$(EXEEXT): meta-cipher-test.$(OBJEXT)
	$(LINK) meta-cipher-test.$(OBJEXT) $(TEST_OBJS) -o meta-cipher-test$(EXEEXT)

meta-armor-test$(EXEEXT): meta-armor-test.$(OBJEXT)
	$(LINK) meta-armor-test.$(OBJEXT) $(TEST_OBJS) -o meta-armor-test$(EXEEXT)

buffer-test$(EXEEXT): buffer-test.$(OBJEXT)
	$(LINK) buffer-test.$(OBJEXT) $(TEST_OBJS) -o buffer-test$(EXEEXT)

yarrow-test$(EXEEXT): yarrow-test.$(OBJEXT)
	$(LINK) yarrow-test.$(OBJEXT) $(TEST_OBJS) -o yarrow-test$(EXEEXT)

sexp-test$(EXEEXT): sexp-test.$(OBJEXT)
	$(LINK) sexp-test.$(OBJEXT) $(TEST_OBJS) -o sexp-test$(EXEEXT)

sexp-format-test$(EXEEXT): sexp-format-test.$(OBJEXT)
	$(LINK) sexp-format-test.$(OBJEXT) $(TEST_OBJS) -o sexp-format-test$(EXEEXT)

rsa2sexp-test$(EXEEXT): rsa2sexp-test.$(OBJEXT)
	$(LINK) rsa2sexp-test.$(OBJEXT) $(TEST_OBJS) -o rsa2sexp-test$(EXEEXT)

sexp2rsa-test$(EXEEXT): sexp2rsa-test.$(OBJEXT)
	$(LINK) sexp2rsa-test.$(OBJEXT) $(TEST_OBJS) -o sexp2rsa-test$(EXEEXT)

bignum-test$(EXEEXT): bignum-test.$(OBJEXT)
	$(LINK) bignum-test.$(OBJEXT) $(TEST_OBJS) -o bignum-test$(EXEEXT)

random-prime-test$(EXEEXT): random-prime-test.$(OBJEXT)
	$(LINK) random-prime-test.$(OBJEXT) $(TEST_OBJS) -o random-prime-test$(EXEEXT)

pkcs1-test$(EXEEXT): pkcs1-test.$(OBJEXT)
	$(LINK) pkcs1-test.$(OBJEXT) $(TEST_OBJS) -o pkcs1-test$(EXEEXT)

rsa-test$(EXEEXT): rsa-test.$(OBJEXT)
	$(LINK) rsa-test.$(OBJEXT) $(TEST_OBJS) -o rsa-test$(EXEEXT)

rsa-encrypt-test$(EXEEXT): rsa-encrypt-test.$(OBJEXT)
	$(LINK) rsa-encrypt-test.$(OBJEXT) $(TEST_OBJS) -o rsa-encrypt-test$(EXEEXT)

rsa-keygen-test$(EXEEXT): rsa-keygen-test.$(OBJEXT)
	$(LINK) rsa-keygen-test.$(OBJEXT) $(TEST_OBJS) -o rsa-keygen-test$(EXEEXT)

dsa-test$(EXEEXT): dsa-test.$(OBJEXT)
	$(LINK) dsa-test.$(OBJEXT) $(TEST_OBJS) -o dsa-test$(EXEEXT)

dsa-keygen-test$(EXEEXT): dsa-keygen-test.$(OBJEXT)
	$(LINK) dsa-keygen-test.$(OBJEXT) $(TEST_OBJS) -o dsa-keygen-test$(EXEEXT)

sha1-huge-test$(EXEEXT): sha1-huge-test.$(OBJEXT)
	$(LINK) sha1-huge-test.$(OBJEXT) $(TEST_OBJS) -o sha1-huge-test$(EXEEXT)

cxx-test$(EXEEXT): cxx-test.$(OBJEXT)
	$(LINK_CXX) cxx-test.$(OBJEXT) $(TEST_OBJS) -o cxx-test$(EXEEXT)

