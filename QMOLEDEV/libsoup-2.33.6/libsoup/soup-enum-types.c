
/* Generated data (by glib-mkenums) */

#include "soup.h"
#define LIBSOUP_USE_UNSTABLE_REQUEST_API
#include "soup-cache.h"
#include "soup-requester.h"
/* enumerations from "soup-address.h" */
GType
soup_address_family_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_ADDRESS_FAMILY_INVALID, "SOUP_ADDRESS_FAMILY_INVALID", "invalid" },
			{ SOUP_ADDRESS_FAMILY_IPV4, "SOUP_ADDRESS_FAMILY_IPV4", "ipv4" },
			{ SOUP_ADDRESS_FAMILY_IPV6, "SOUP_ADDRESS_FAMILY_IPV6", "ipv6" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupAddressFamily"), values);
	}
	return etype;
}

/* enumerations from "soup-cache.h" */
GType
soup_cacheability_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GFlagsValue values[] = {
			{ SOUP_CACHE_CACHEABLE, "SOUP_CACHE_CACHEABLE", "cacheable" },
			{ SOUP_CACHE_UNCACHEABLE, "SOUP_CACHE_UNCACHEABLE", "uncacheable" },
			{ SOUP_CACHE_INVALIDATES, "SOUP_CACHE_INVALIDATES", "invalidates" },
			{ SOUP_CACHE_VALIDATES, "SOUP_CACHE_VALIDATES", "validates" },
			{ 0, NULL, NULL }
		};
		etype = g_flags_register_static (g_intern_static_string ("SoupCacheability"), values);
	}
	return etype;
}

GType
soup_cache_response_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_CACHE_RESPONSE_FRESH, "SOUP_CACHE_RESPONSE_FRESH", "fresh" },
			{ SOUP_CACHE_RESPONSE_NEEDS_VALIDATION, "SOUP_CACHE_RESPONSE_NEEDS_VALIDATION", "needs-validation" },
			{ SOUP_CACHE_RESPONSE_STALE, "SOUP_CACHE_RESPONSE_STALE", "stale" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupCacheResponse"), values);
	}
	return etype;
}

GType
soup_cache_type_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_CACHE_SINGLE_USER, "SOUP_CACHE_SINGLE_USER", "single-user" },
			{ SOUP_CACHE_SHARED, "SOUP_CACHE_SHARED", "shared" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupCacheType"), values);
	}
	return etype;
}

/* enumerations from "soup-cookie-jar.h" */
GType
soup_cookie_jar_accept_policy_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_COOKIE_JAR_ACCEPT_ALWAYS, "SOUP_COOKIE_JAR_ACCEPT_ALWAYS", "always" },
			{ SOUP_COOKIE_JAR_ACCEPT_NEVER, "SOUP_COOKIE_JAR_ACCEPT_NEVER", "never" },
			{ SOUP_COOKIE_JAR_ACCEPT_NO_THIRD_PARTY, "SOUP_COOKIE_JAR_ACCEPT_NO_THIRD_PARTY", "no-third-party" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupCookieJarAcceptPolicy"), values);
	}
	return etype;
}

/* enumerations from "soup-date.h" */
GType
soup_date_format_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_DATE_HTTP, "SOUP_DATE_HTTP", "http" },
			{ SOUP_DATE_COOKIE, "SOUP_DATE_COOKIE", "cookie" },
			{ SOUP_DATE_RFC2822, "SOUP_DATE_RFC2822", "rfc2822" },
			{ SOUP_DATE_ISO8601_COMPACT, "SOUP_DATE_ISO8601_COMPACT", "iso8601-compact" },
			{ SOUP_DATE_ISO8601_FULL, "SOUP_DATE_ISO8601_FULL", "iso8601-full" },
			{ SOUP_DATE_ISO8601, "SOUP_DATE_ISO8601", "iso8601" },
			{ SOUP_DATE_ISO8601_XMLRPC, "SOUP_DATE_ISO8601_XMLRPC", "iso8601-xmlrpc" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupDateFormat"), values);
	}
	return etype;
}

/* enumerations from "soup-logger.h" */
GType
soup_logger_log_level_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_LOGGER_LOG_NONE, "SOUP_LOGGER_LOG_NONE", "none" },
			{ SOUP_LOGGER_LOG_MINIMAL, "SOUP_LOGGER_LOG_MINIMAL", "minimal" },
			{ SOUP_LOGGER_LOG_HEADERS, "SOUP_LOGGER_LOG_HEADERS", "headers" },
			{ SOUP_LOGGER_LOG_BODY, "SOUP_LOGGER_LOG_BODY", "body" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupLoggerLogLevel"), values);
	}
	return etype;
}

/* enumerations from "soup-message.h" */
GType
soup_http_version_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_HTTP_1_0, "SOUP_HTTP_1_0", "http-1-0" },
			{ SOUP_HTTP_1_1, "SOUP_HTTP_1_1", "http-1-1" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupHTTPVersion"), values);
	}
	return etype;
}

GType
soup_message_flags_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GFlagsValue values[] = {
			{ SOUP_MESSAGE_NO_REDIRECT, "SOUP_MESSAGE_NO_REDIRECT", "no-redirect" },
			{ SOUP_MESSAGE_OVERWRITE_CHUNKS, "SOUP_MESSAGE_OVERWRITE_CHUNKS", "overwrite-chunks" },
			{ SOUP_MESSAGE_CONTENT_DECODED, "SOUP_MESSAGE_CONTENT_DECODED", "content-decoded" },
			{ SOUP_MESSAGE_CERTIFICATE_TRUSTED, "SOUP_MESSAGE_CERTIFICATE_TRUSTED", "certificate-trusted" },
			{ 0, NULL, NULL }
		};
		etype = g_flags_register_static (g_intern_static_string ("SoupMessageFlags"), values);
	}
	return etype;
}

/* enumerations from "soup-message-body.h" */
GType
soup_memory_use_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_MEMORY_STATIC, "SOUP_MEMORY_STATIC", "static" },
			{ SOUP_MEMORY_TAKE, "SOUP_MEMORY_TAKE", "take" },
			{ SOUP_MEMORY_COPY, "SOUP_MEMORY_COPY", "copy" },
			{ SOUP_MEMORY_TEMPORARY, "SOUP_MEMORY_TEMPORARY", "temporary" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupMemoryUse"), values);
	}
	return etype;
}

/* enumerations from "soup-message-headers.h" */
GType
soup_message_headers_type_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_MESSAGE_HEADERS_REQUEST, "SOUP_MESSAGE_HEADERS_REQUEST", "request" },
			{ SOUP_MESSAGE_HEADERS_RESPONSE, "SOUP_MESSAGE_HEADERS_RESPONSE", "response" },
			{ SOUP_MESSAGE_HEADERS_MULTIPART, "SOUP_MESSAGE_HEADERS_MULTIPART", "multipart" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupMessageHeadersType"), values);
	}
	return etype;
}

GType
soup_encoding_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_ENCODING_UNRECOGNIZED, "SOUP_ENCODING_UNRECOGNIZED", "unrecognized" },
			{ SOUP_ENCODING_NONE, "SOUP_ENCODING_NONE", "none" },
			{ SOUP_ENCODING_CONTENT_LENGTH, "SOUP_ENCODING_CONTENT_LENGTH", "content-length" },
			{ SOUP_ENCODING_EOF, "SOUP_ENCODING_EOF", "eof" },
			{ SOUP_ENCODING_CHUNKED, "SOUP_ENCODING_CHUNKED", "chunked" },
			{ SOUP_ENCODING_BYTERANGES, "SOUP_ENCODING_BYTERANGES", "byteranges" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupEncoding"), values);
	}
	return etype;
}

GType
soup_expectation_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GFlagsValue values[] = {
			{ SOUP_EXPECTATION_UNRECOGNIZED, "SOUP_EXPECTATION_UNRECOGNIZED", "unrecognized" },
			{ SOUP_EXPECTATION_CONTINUE, "SOUP_EXPECTATION_CONTINUE", "continue" },
			{ 0, NULL, NULL }
		};
		etype = g_flags_register_static (g_intern_static_string ("SoupExpectation"), values);
	}
	return etype;
}

/* enumerations from "soup-misc.h" */
GType
soup_ssl_error_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_SSL_ERROR_HANDSHAKE_NEEDS_READ, "SOUP_SSL_ERROR_HANDSHAKE_NEEDS_READ", "handshake-needs-read" },
			{ SOUP_SSL_ERROR_HANDSHAKE_NEEDS_WRITE, "SOUP_SSL_ERROR_HANDSHAKE_NEEDS_WRITE", "handshake-needs-write" },
			{ SOUP_SSL_ERROR_CERTIFICATE, "SOUP_SSL_ERROR_CERTIFICATE", "certificate" },
			{ SOUP_SSL_ERROR_HANDSHAKE_FAILED, "SOUP_SSL_ERROR_HANDSHAKE_FAILED", "handshake-failed" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupSSLError"), values);
	}
	return etype;
}

GType
soup_connection_state_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_CONNECTION_NEW, "SOUP_CONNECTION_NEW", "new" },
			{ SOUP_CONNECTION_CONNECTING, "SOUP_CONNECTION_CONNECTING", "connecting" },
			{ SOUP_CONNECTION_IDLE, "SOUP_CONNECTION_IDLE", "idle" },
			{ SOUP_CONNECTION_IN_USE, "SOUP_CONNECTION_IN_USE", "in-use" },
			{ SOUP_CONNECTION_REMOTE_DISCONNECTED, "SOUP_CONNECTION_REMOTE_DISCONNECTED", "remote-disconnected" },
			{ SOUP_CONNECTION_DISCONNECTED, "SOUP_CONNECTION_DISCONNECTED", "disconnected" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupConnectionState"), values);
	}
	return etype;
}

/* enumerations from "soup-requester.h" */
GType
soup_requester_error_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_REQUESTER_ERROR_BAD_URI, "SOUP_REQUESTER_ERROR_BAD_URI", "bad-uri" },
			{ SOUP_REQUESTER_ERROR_UNSUPPORTED_URI_SCHEME, "SOUP_REQUESTER_ERROR_UNSUPPORTED_URI_SCHEME", "unsupported-uri-scheme" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupRequesterError"), values);
	}
	return etype;
}

/* enumerations from "soup-socket.h" */
GType
soup_socket_io_status_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_SOCKET_OK, "SOUP_SOCKET_OK", "ok" },
			{ SOUP_SOCKET_WOULD_BLOCK, "SOUP_SOCKET_WOULD_BLOCK", "would-block" },
			{ SOUP_SOCKET_EOF, "SOUP_SOCKET_EOF", "eof" },
			{ SOUP_SOCKET_ERROR, "SOUP_SOCKET_ERROR", "error" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupSocketIOStatus"), values);
	}
	return etype;
}

/* enumerations from "soup-status.h" */
GType
soup_known_status_code_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_STATUS_NONE, "SOUP_STATUS_NONE", "none" },
			{ SOUP_STATUS_CANCELLED, "SOUP_STATUS_CANCELLED", "cancelled" },
			{ SOUP_STATUS_CANT_RESOLVE, "SOUP_STATUS_CANT_RESOLVE", "cant-resolve" },
			{ SOUP_STATUS_CANT_RESOLVE_PROXY, "SOUP_STATUS_CANT_RESOLVE_PROXY", "cant-resolve-proxy" },
			{ SOUP_STATUS_CANT_CONNECT, "SOUP_STATUS_CANT_CONNECT", "cant-connect" },
			{ SOUP_STATUS_CANT_CONNECT_PROXY, "SOUP_STATUS_CANT_CONNECT_PROXY", "cant-connect-proxy" },
			{ SOUP_STATUS_SSL_FAILED, "SOUP_STATUS_SSL_FAILED", "ssl-failed" },
			{ SOUP_STATUS_IO_ERROR, "SOUP_STATUS_IO_ERROR", "io-error" },
			{ SOUP_STATUS_MALFORMED, "SOUP_STATUS_MALFORMED", "malformed" },
			{ SOUP_STATUS_TRY_AGAIN, "SOUP_STATUS_TRY_AGAIN", "try-again" },
			{ SOUP_STATUS_TOO_MANY_REDIRECTS, "SOUP_STATUS_TOO_MANY_REDIRECTS", "too-many-redirects" },
			{ SOUP_STATUS_CONTINUE, "SOUP_STATUS_CONTINUE", "continue" },
			{ SOUP_STATUS_SWITCHING_PROTOCOLS, "SOUP_STATUS_SWITCHING_PROTOCOLS", "switching-protocols" },
			{ SOUP_STATUS_PROCESSING, "SOUP_STATUS_PROCESSING", "processing" },
			{ SOUP_STATUS_OK, "SOUP_STATUS_OK", "ok" },
			{ SOUP_STATUS_CREATED, "SOUP_STATUS_CREATED", "created" },
			{ SOUP_STATUS_ACCEPTED, "SOUP_STATUS_ACCEPTED", "accepted" },
			{ SOUP_STATUS_NON_AUTHORITATIVE, "SOUP_STATUS_NON_AUTHORITATIVE", "non-authoritative" },
			{ SOUP_STATUS_NO_CONTENT, "SOUP_STATUS_NO_CONTENT", "no-content" },
			{ SOUP_STATUS_RESET_CONTENT, "SOUP_STATUS_RESET_CONTENT", "reset-content" },
			{ SOUP_STATUS_PARTIAL_CONTENT, "SOUP_STATUS_PARTIAL_CONTENT", "partial-content" },
			{ SOUP_STATUS_MULTI_STATUS, "SOUP_STATUS_MULTI_STATUS", "multi-status" },
			{ SOUP_STATUS_MULTIPLE_CHOICES, "SOUP_STATUS_MULTIPLE_CHOICES", "multiple-choices" },
			{ SOUP_STATUS_MOVED_PERMANENTLY, "SOUP_STATUS_MOVED_PERMANENTLY", "moved-permanently" },
			{ SOUP_STATUS_FOUND, "SOUP_STATUS_FOUND", "found" },
			{ SOUP_STATUS_MOVED_TEMPORARILY, "SOUP_STATUS_MOVED_TEMPORARILY", "moved-temporarily" },
			{ SOUP_STATUS_SEE_OTHER, "SOUP_STATUS_SEE_OTHER", "see-other" },
			{ SOUP_STATUS_NOT_MODIFIED, "SOUP_STATUS_NOT_MODIFIED", "not-modified" },
			{ SOUP_STATUS_USE_PROXY, "SOUP_STATUS_USE_PROXY", "use-proxy" },
			{ SOUP_STATUS_NOT_APPEARING_IN_THIS_PROTOCOL, "SOUP_STATUS_NOT_APPEARING_IN_THIS_PROTOCOL", "not-appearing-in-this-protocol" },
			{ SOUP_STATUS_TEMPORARY_REDIRECT, "SOUP_STATUS_TEMPORARY_REDIRECT", "temporary-redirect" },
			{ SOUP_STATUS_BAD_REQUEST, "SOUP_STATUS_BAD_REQUEST", "bad-request" },
			{ SOUP_STATUS_UNAUTHORIZED, "SOUP_STATUS_UNAUTHORIZED", "unauthorized" },
			{ SOUP_STATUS_PAYMENT_REQUIRED, "SOUP_STATUS_PAYMENT_REQUIRED", "payment-required" },
			{ SOUP_STATUS_FORBIDDEN, "SOUP_STATUS_FORBIDDEN", "forbidden" },
			{ SOUP_STATUS_NOT_FOUND, "SOUP_STATUS_NOT_FOUND", "not-found" },
			{ SOUP_STATUS_METHOD_NOT_ALLOWED, "SOUP_STATUS_METHOD_NOT_ALLOWED", "method-not-allowed" },
			{ SOUP_STATUS_NOT_ACCEPTABLE, "SOUP_STATUS_NOT_ACCEPTABLE", "not-acceptable" },
			{ SOUP_STATUS_PROXY_AUTHENTICATION_REQUIRED, "SOUP_STATUS_PROXY_AUTHENTICATION_REQUIRED", "proxy-authentication-required" },
			{ SOUP_STATUS_PROXY_UNAUTHORIZED, "SOUP_STATUS_PROXY_UNAUTHORIZED", "proxy-unauthorized" },
			{ SOUP_STATUS_REQUEST_TIMEOUT, "SOUP_STATUS_REQUEST_TIMEOUT", "request-timeout" },
			{ SOUP_STATUS_CONFLICT, "SOUP_STATUS_CONFLICT", "conflict" },
			{ SOUP_STATUS_GONE, "SOUP_STATUS_GONE", "gone" },
			{ SOUP_STATUS_LENGTH_REQUIRED, "SOUP_STATUS_LENGTH_REQUIRED", "length-required" },
			{ SOUP_STATUS_PRECONDITION_FAILED, "SOUP_STATUS_PRECONDITION_FAILED", "precondition-failed" },
			{ SOUP_STATUS_REQUEST_ENTITY_TOO_LARGE, "SOUP_STATUS_REQUEST_ENTITY_TOO_LARGE", "request-entity-too-large" },
			{ SOUP_STATUS_REQUEST_URI_TOO_LONG, "SOUP_STATUS_REQUEST_URI_TOO_LONG", "request-uri-too-long" },
			{ SOUP_STATUS_UNSUPPORTED_MEDIA_TYPE, "SOUP_STATUS_UNSUPPORTED_MEDIA_TYPE", "unsupported-media-type" },
			{ SOUP_STATUS_REQUESTED_RANGE_NOT_SATISFIABLE, "SOUP_STATUS_REQUESTED_RANGE_NOT_SATISFIABLE", "requested-range-not-satisfiable" },
			{ SOUP_STATUS_INVALID_RANGE, "SOUP_STATUS_INVALID_RANGE", "invalid-range" },
			{ SOUP_STATUS_EXPECTATION_FAILED, "SOUP_STATUS_EXPECTATION_FAILED", "expectation-failed" },
			{ SOUP_STATUS_UNPROCESSABLE_ENTITY, "SOUP_STATUS_UNPROCESSABLE_ENTITY", "unprocessable-entity" },
			{ SOUP_STATUS_LOCKED, "SOUP_STATUS_LOCKED", "locked" },
			{ SOUP_STATUS_FAILED_DEPENDENCY, "SOUP_STATUS_FAILED_DEPENDENCY", "failed-dependency" },
			{ SOUP_STATUS_INTERNAL_SERVER_ERROR, "SOUP_STATUS_INTERNAL_SERVER_ERROR", "internal-server-error" },
			{ SOUP_STATUS_NOT_IMPLEMENTED, "SOUP_STATUS_NOT_IMPLEMENTED", "not-implemented" },
			{ SOUP_STATUS_BAD_GATEWAY, "SOUP_STATUS_BAD_GATEWAY", "bad-gateway" },
			{ SOUP_STATUS_SERVICE_UNAVAILABLE, "SOUP_STATUS_SERVICE_UNAVAILABLE", "service-unavailable" },
			{ SOUP_STATUS_GATEWAY_TIMEOUT, "SOUP_STATUS_GATEWAY_TIMEOUT", "gateway-timeout" },
			{ SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED, "SOUP_STATUS_HTTP_VERSION_NOT_SUPPORTED", "http-version-not-supported" },
			{ SOUP_STATUS_INSUFFICIENT_STORAGE, "SOUP_STATUS_INSUFFICIENT_STORAGE", "insufficient-storage" },
			{ SOUP_STATUS_NOT_EXTENDED, "SOUP_STATUS_NOT_EXTENDED", "not-extended" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupKnownStatusCode"), values);
	}
	return etype;
}

/* enumerations from "soup-xmlrpc.h" */
GType
soup_xmlrpc_error_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_XMLRPC_ERROR_ARGUMENTS, "SOUP_XMLRPC_ERROR_ARGUMENTS", "arguments" },
			{ SOUP_XMLRPC_ERROR_RETVAL, "SOUP_XMLRPC_ERROR_RETVAL", "retval" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupXMLRPCError"), values);
	}
	return etype;
}

GType
soup_xmlrpc_fault_get_type (void)
{
	static GType etype = 0;
	if (G_UNLIKELY (etype == 0)) {
		static const GEnumValue values[] = {
			{ SOUP_XMLRPC_FAULT_PARSE_ERROR_NOT_WELL_FORMED, "SOUP_XMLRPC_FAULT_PARSE_ERROR_NOT_WELL_FORMED", "parse-error-not-well-formed" },
			{ SOUP_XMLRPC_FAULT_PARSE_ERROR_UNSUPPORTED_ENCODING, "SOUP_XMLRPC_FAULT_PARSE_ERROR_UNSUPPORTED_ENCODING", "parse-error-unsupported-encoding" },
			{ SOUP_XMLRPC_FAULT_PARSE_ERROR_INVALID_CHARACTER_FOR_ENCODING, "SOUP_XMLRPC_FAULT_PARSE_ERROR_INVALID_CHARACTER_FOR_ENCODING", "parse-error-invalid-character-for-encoding" },
			{ SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_XML_RPC, "SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_XML_RPC", "server-error-invalid-xml-rpc" },
			{ SOUP_XMLRPC_FAULT_SERVER_ERROR_REQUESTED_METHOD_NOT_FOUND, "SOUP_XMLRPC_FAULT_SERVER_ERROR_REQUESTED_METHOD_NOT_FOUND", "server-error-requested-method-not-found" },
			{ SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_METHOD_PARAMETERS, "SOUP_XMLRPC_FAULT_SERVER_ERROR_INVALID_METHOD_PARAMETERS", "server-error-invalid-method-parameters" },
			{ SOUP_XMLRPC_FAULT_SERVER_ERROR_INTERNAL_XML_RPC_ERROR, "SOUP_XMLRPC_FAULT_SERVER_ERROR_INTERNAL_XML_RPC_ERROR", "server-error-internal-xml-rpc-error" },
			{ SOUP_XMLRPC_FAULT_APPLICATION_ERROR, "SOUP_XMLRPC_FAULT_APPLICATION_ERROR", "application-error" },
			{ SOUP_XMLRPC_FAULT_SYSTEM_ERROR, "SOUP_XMLRPC_FAULT_SYSTEM_ERROR", "system-error" },
			{ SOUP_XMLRPC_FAULT_TRANSPORT_ERROR, "SOUP_XMLRPC_FAULT_TRANSPORT_ERROR", "transport-error" },
			{ 0, NULL, NULL }
		};
		etype = g_enum_register_static (g_intern_static_string ("SoupXMLRPCFault"), values);
	}
	return etype;
}


/* Generated data ends here */

