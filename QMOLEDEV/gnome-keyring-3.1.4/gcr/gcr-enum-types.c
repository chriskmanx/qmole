
/* Generated data (by glib-mkenums) */

#include "gcr.h"

/* enumerations from "gcr-certificate-chain.h" */
GType
gcr_certificate_chain_status_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GCR_CERTIFICATE_CHAIN_UNKNOWN, "GCR_CERTIFICATE_CHAIN_UNKNOWN", "unknown" },
            { GCR_CERTIFICATE_CHAIN_INCOMPLETE, "GCR_CERTIFICATE_CHAIN_INCOMPLETE", "incomplete" },
            { GCR_CERTIFICATE_CHAIN_DISTRUSTED, "GCR_CERTIFICATE_CHAIN_DISTRUSTED", "distrusted" },
            { GCR_CERTIFICATE_CHAIN_SELFSIGNED, "GCR_CERTIFICATE_CHAIN_SELFSIGNED", "selfsigned" },
            { GCR_CERTIFICATE_CHAIN_PINNED, "GCR_CERTIFICATE_CHAIN_PINNED", "pinned" },
            { GCR_CERTIFICATE_CHAIN_ANCHORED, "GCR_CERTIFICATE_CHAIN_ANCHORED", "anchored" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GcrCertificateChainStatus"), values);
    }
    return etype;
}

GType
gcr_certificate_chain_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GFlagsValue values[] = {
            { GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS, "GCR_CERTIFICATE_CHAIN_FLAG_NO_LOOKUPS", "lookups" },
            { 0, NULL, NULL }
        };
        etype = g_flags_register_static (g_intern_static_string ("GcrCertificateChainFlags"), values);
    }
    return etype;
}

/* enumerations from "gcr-column.h" */
GType
gcr_column_flags_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GCR_COLUMN_HIDDEN, "GCR_COLUMN_HIDDEN", "hidden" },
            { GCR_COLUMN_SORTABLE, "GCR_COLUMN_SORTABLE", "sortable" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GcrColumnFlags"), values);
    }
    return etype;
}

/* enumerations from "gcr-importer.h" */
GType
gcr_importer_prompt_behavior_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GCR_IMPORTER_PROMPT_NEEDED, "GCR_IMPORTER_PROMPT_NEEDED", "needed" },
            { GCR_IMPORTER_PROMPT_ALWAYS, "GCR_IMPORTER_PROMPT_ALWAYS", "always" },
            { GCR_IMPORTER_PROMPT_NEVER, "GCR_IMPORTER_PROMPT_NEVER", "never" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GcrImporterPromptBehavior"), values);
    }
    return etype;
}

/* enumerations from "gcr-types.h" */
GType
gcr_data_error_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GCR_ERROR_FAILURE, "GCR_ERROR_FAILURE", "failure" },
            { GCR_ERROR_UNRECOGNIZED, "GCR_ERROR_UNRECOGNIZED", "unrecognized" },
            { GCR_ERROR_CANCELLED, "GCR_ERROR_CANCELLED", "cancelled" },
            { GCR_ERROR_LOCKED, "GCR_ERROR_LOCKED", "locked" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GcrDataError"), values);
    }
    return etype;
}

GType
gcr_data_format_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { GCR_FORMAT_INVALID, "GCR_FORMAT_INVALID", "invalid" },
            { GCR_FORMAT_DER_PRIVATE_KEY, "GCR_FORMAT_DER_PRIVATE_KEY", "der-private-key" },
            { GCR_FORMAT_DER_PRIVATE_KEY_RSA, "GCR_FORMAT_DER_PRIVATE_KEY_RSA", "der-private-key-rsa" },
            { GCR_FORMAT_DER_PRIVATE_KEY_DSA, "GCR_FORMAT_DER_PRIVATE_KEY_DSA", "der-private-key-dsa" },
            { GCR_FORMAT_DER_CERTIFICATE_X509, "GCR_FORMAT_DER_CERTIFICATE_X509", "der-certificate-x509" },
            { GCR_FORMAT_DER_PKCS7, "GCR_FORMAT_DER_PKCS7", "der-pkcs7" },
            { GCR_FORMAT_DER_PKCS8, "GCR_FORMAT_DER_PKCS8", "der-pkcs8" },
            { GCR_FORMAT_DER_PKCS8_PLAIN, "GCR_FORMAT_DER_PKCS8_PLAIN", "der-pkcs8-plain" },
            { GCR_FORMAT_DER_PKCS8_ENCRYPTED, "GCR_FORMAT_DER_PKCS8_ENCRYPTED", "der-pkcs8-encrypted" },
            { GCR_FORMAT_DER_PKCS12, "GCR_FORMAT_DER_PKCS12", "der-pkcs12" },
            { GCR_FORMAT_PEM, "GCR_FORMAT_PEM", "pem" },
            { GCR_FORMAT_PEM_PRIVATE_KEY_RSA, "GCR_FORMAT_PEM_PRIVATE_KEY_RSA", "pem-private-key-rsa" },
            { GCR_FORMAT_PEM_PRIVATE_KEY_DSA, "GCR_FORMAT_PEM_PRIVATE_KEY_DSA", "pem-private-key-dsa" },
            { GCR_FORMAT_PEM_CERTIFICATE_X509, "GCR_FORMAT_PEM_CERTIFICATE_X509", "pem-certificate-x509" },
            { GCR_FORMAT_PEM_PKCS7, "GCR_FORMAT_PEM_PKCS7", "pem-pkcs7" },
            { GCR_FORMAT_PEM_PKCS8_PLAIN, "GCR_FORMAT_PEM_PKCS8_PLAIN", "pem-pkcs8-plain" },
            { GCR_FORMAT_PEM_PKCS8_ENCRYPTED, "GCR_FORMAT_PEM_PKCS8_ENCRYPTED", "pem-pkcs8-encrypted" },
            { GCR_FORMAT_PEM_PKCS12, "GCR_FORMAT_PEM_PKCS12", "pem-pkcs12" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("GcrDataFormat"), values);
    }
    return etype;
}

 /**/

/* Generated data ends here */

