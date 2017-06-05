typedef enum
{
  TEST_SUCCEED, TEST_FAILED, TEST_UNSURE, TEST_IGNORE
} test_code_t;

test_code_t test_server (gnutls_session_t state);
test_code_t test_record_padding (gnutls_session_t state);
test_code_t test_export (gnutls_session_t state);
test_code_t test_export_info (gnutls_session_t state);
test_code_t test_hello_extension (gnutls_session_t state);
test_code_t test_dhe (gnutls_session_t state);
test_code_t test_dhe_group (gnutls_session_t state);
test_code_t test_ssl3 (gnutls_session_t state);
test_code_t test_aes (gnutls_session_t state);
#ifdef	ENABLE_CAMELLIA
test_code_t test_camellia (gnutls_session_t state);
#endif
test_code_t test_md5 (gnutls_session_t state);
test_code_t test_sha (gnutls_session_t state);
test_code_t test_3des (gnutls_session_t state);
test_code_t test_arcfour (gnutls_session_t state);
test_code_t test_arcfour_40 (gnutls_session_t state);
test_code_t test_tls1 (gnutls_session_t state);
test_code_t test_safe_renegotiation (gnutls_session_t state);
test_code_t test_safe_renegotiation_scsv (gnutls_session_t state);
test_code_t test_tls1_1 (gnutls_session_t state);
test_code_t test_tls1_2 (gnutls_session_t state);
test_code_t test_tls1_1_fallback (gnutls_session_t state);
test_code_t test_tls_disable (gnutls_session_t state);
test_code_t test_rsa_pms (gnutls_session_t state);
test_code_t test_max_record_size (gnutls_session_t state);
test_code_t test_version_rollback (gnutls_session_t state);
test_code_t test_anonymous (gnutls_session_t state);
test_code_t test_unknown_ciphersuites (gnutls_session_t state);
test_code_t test_openpgp1 (gnutls_session_t state);
test_code_t test_bye (gnutls_session_t state);
test_code_t test_certificate (gnutls_session_t state);
test_code_t test_server_cas (gnutls_session_t state);
test_code_t test_session_resume2 (gnutls_session_t state);
test_code_t test_rsa_pms_version_check (gnutls_session_t session);
test_code_t test_version_oob (gnutls_session_t session);
test_code_t test_zlib (gnutls_session_t session);
int _test_srp_username_callback (gnutls_session_t session,
                                 char **username, char **password);
