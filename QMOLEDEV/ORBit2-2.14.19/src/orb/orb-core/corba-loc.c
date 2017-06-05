#include "config.h"
#include <orbit/orbit.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "corba-ops.h"
#include "orb-core-private.h"
#include "orb-core-export.h"
#include "orbit-debug.h"
#include "../util/orbit-purify.h"

#include "iop-profiles.h"

#define OMG_LOCATOR "2809"

/* does not preserve @profile string, inserts '\0' as seperator,
 * @return FALSE on failure and TRUE otherwise, on success restults
 * will be assigned to @ver and @host and @port, do not _free_
 * @host  */ 
static gboolean
corbaloc_profile_iiop_parse (gchar  *profile,
			     GIOPVersion     *ver,
			     gchar          **host,
			     unsigned short  *port,
			     gboolean        *ssl)
{
	gchar *_prof  = NULL; 
	gchar *_token = NULL; 

	gchar *_ver   = NULL;
	gchar *_host  = NULL;
	gchar *_port  = NULL;

	/*  [iiop]:[1.2@]host-name[:port] */ 

	_prof = profile;

	if (!strncmp (_prof, ":", strlen (":")) ||
	    !strncmp (_prof, "iiop:", strlen ("iiop:")))
		*ssl = FALSE;
	else if (!strncmp (_prof, "iiops:", strlen ("iiops:")) ||
		 !strncmp (_prof, "ssliop:", strlen ("ssliop:")))
		*ssl = TRUE;
	else 
		goto ret_error;
	
	_token = strchr (_prof, ':');
	_token++;           /* point right of ':' */
	
	/* skip leading '/' */ 
	while (*_token == '/') 
		++_token; 
 
	if (strchr (_token, '@')) {
		_ver  = _token;
		
		_token = strchr (_token, '@');
		*_token = '\0';
		++_token;
	}
	else {
		_ver = *ssl ? "1.1" : "1.0";
	}
	
	_host = _token;
	
	if (strchr (_token, ':')) {
		_port = strchr (_token, ':');
		*_port = '\0';
		++_port;
	}
	else {
		_port = OMG_LOCATOR;
	}
	
	/* asure strings are not empty */ 
	if (!strlen (_ver))  goto ret_error;
	if (!strlen (_host)) goto ret_error;
	if (!strlen (_port)) goto ret_error;
	
	/* verify @_port is ushort */ 
	if (atoi(_port) < 0 || atoi (_port) > USHRT_MAX)
		goto ret_error;
	
	*port = (unsigned short) atoi (_port);

	while (*_port) 
		if (!isdigit (*_port++))
			goto ret_error;
	
	/* verify @_ver is one of GIOP 1.0, 1.1. or 1.2 */
	if (!strncmp (_ver, "1.0", strlen ("1.0")))      *ver =  GIOP_1_0;
	else if (!strncmp (_ver, "1.1", strlen ("1.1"))) *ver =  GIOP_1_1;
	else if (!strncmp (_ver, "1.2", strlen ("1.2"))) *ver =  GIOP_1_2;
	else goto ret_error;

	*host = _host;

	return TRUE;

 ret_error:
	return FALSE; 
}

static gboolean
corbaloc_profile_uiop_parse (gchar           *profile,
			     gchar          **socket_path,
			     gushort         *ipv6_port )
{
	g_assert (profile && strlen (profile) > 0 );
	
	/*  [uiop]: ['//'] socket-path ':' [ port ] */ 

	if (strncmp (profile, "uiop:", strlen ("uiop:"))) 
	    goto ret_error;
	
	profile = strchr (profile, ':');
	profile++;           /* point behind ':' */
	
	/* skip leading '/', step back to last */ 
	while (*profile == '/') ++profile;
	--profile;     
	
	*socket_path = profile;

	if (!(profile = strrchr (profile, ':')))
		goto ret_error;
	*profile++ = '\0'; 
	
	if (strlen (profile)) {
		if (atoi(profile) < 0 || atoi (profile) > USHRT_MAX)
			goto ret_error;
		*ipv6_port = atoi (profile);

		while (*profile) 
			if (!isdigit (*profile++))
				goto ret_error;
	} 
	else {
		*ipv6_port = 0;
	}

	if (!strlen (*socket_path))
		goto ret_error;
		
	return TRUE;

 ret_error:
	return FALSE; 
}

static IOP_TAG_INTERNET_IOP_info*
corbaloc_profile_iiop (gchar           *token, 
		       ORBit_ObjectKey *objkey)
{	
	IOP_TAG_INTERNET_IOP_info* iiop = NULL;
		
	GIOPVersion     ver  = GIOP_1_0; 
	gchar          *host = NULL;
	unsigned short  port = 0; /* invalid port number */ 
	gboolean        ssl = FALSE;

	/* will insert '\0' into token [i] */ 
	if (!corbaloc_profile_iiop_parse (token, &ver, &host, &port, &ssl))
		return NULL;

	if (ssl && ver < GIOP_1_1) 
		return NULL;

	iiop = g_new0 (IOP_TAG_INTERNET_IOP_info, 1);
	iiop->parent.profile_type = IOP_TAG_INTERNET_IOP;
	iiop->iiop_version = ver;
	iiop->host         = g_strdup (host);
	iiop->port         = ssl ? 0 : port;
	
	iiop->object_key   = IOP_ObjectKey_copy (objkey);
	iiop->components   = NULL; /* FIXME, set CodeSet utf8,utf16 etc.*/ 
	
	if (ssl) {
		IOP_TAG_SSL_SEC_TRANS_info *sslsec;
		
		sslsec = g_new0 (IOP_TAG_SSL_SEC_TRANS_info, 1);     
		sslsec->parent.component_type = IOP_TAG_SSL_SEC_TRANS;
		sslsec->target_supports =  /* CSIIOP_NoProtection          | */
			                    CSIIOP_Integrity               |
			                   CSIIOP_Confidentiality          |
                                           CSIIOP_DetectReplay             |
			                   CSIIOP_DetectMisordering        |
                                           CSIIOP_EstablishTrustInTarget   |
			                   CSIIOP_EstablishTrustInClient;
		sslsec->target_requires =  CSIIOP_Integrity                |
			                   CSIIOP_Confidentiality;
		sslsec->port = port;

		iiop->components = g_slist_append (
			iiop->components, sslsec);
	}	

	return iiop;
}

static IOP_TAG_ORBIT_SPECIFIC_info*
corbaloc_profile_uiop (gchar           *token, 
		       ORBit_ObjectKey *objkey)
{	
	IOP_TAG_ORBIT_SPECIFIC_info* osi = NULL;
	gchar   *socket_path = NULL;
	gushort ipv6_port    = 0;

	/* will insert '\0' into token [i] */ 
	if (!corbaloc_profile_uiop_parse (token, &socket_path, &ipv6_port))
		return NULL;

	osi = g_new0 (IOP_TAG_ORBIT_SPECIFIC_info, 1);
	osi->parent.profile_type = IOP_TAG_ORBIT_SPECIFIC;
	osi->unix_sock_path      = g_strdup (socket_path);
	osi->ipv6_port           = ipv6_port;
	osi->object_key          = IOP_ObjectKey_copy (objkey);

	return osi;
}

/* taken from Mico ORB */ 
static gchar
orbit_from_xdigit(gchar c)
{
	c = tolower (c);
	g_assert (isxdigit (c));
	return isdigit (c) ? c - '0' : c - 'a' + 10;
}

/* taken from Mico ORB */ 
static ORBit_ObjectKey*
orbit_url_decode (const char * ptr)
{
	ORBit_ObjectKey *retval  = NULL;
	guchar * buf = NULL; 
	
        retval = CORBA_sequence_CORBA_octet__alloc ();
	retval->_length  = 0;
	retval->_maximum = strlen (ptr)+1;
	retval->_buffer  = CORBA_sequence_CORBA_octet_allocbuf (retval->_maximum);
	retval->_release = CORBA_TRUE;

	buf = retval->_buffer;

	while (*ptr) {
		if (*ptr == '%') {
			if (!isxdigit((unsigned char)ptr[1]) ||
			    !isxdigit((unsigned char)ptr[2])) {
				CORBA_free (retval);
				return NULL;
			}
			*buf = (orbit_from_xdigit(ptr[1])<<4 |
				       orbit_from_xdigit(ptr[2]));
			ptr += 3;
		}
		else {
			*buf = *ptr++;
		}
		buf++;
		(retval->_length)++;
	}
	
	/*
	 * Null-terminate the result so that it can be used as a
	 * string. The null is deliberately not added to the length,
	 * because it isn't part of the string.
	 */
	
	*buf = 0;

	return retval;
}

GSList*
ORBit_corbaloc_parse (const gchar       *corbaloc)
{
	GSList            *profiles = NULL;
	ORBit_ObjectKey   *objkey   = NULL;

	gchar   *loc      = NULL;
	gchar  **token    = NULL;
	gchar   *okey     = NULL;
	glong    i        = 0;
		
	g_return_val_if_fail (corbaloc,        NULL);

	if (!strchr (corbaloc, '/'))   /* any object key ? */
		goto ret_error;  

	if (!strncmp (corbaloc, "corbaloc:", strlen("corbaloc:"))) 
		corbaloc += strlen("corbaloc:");
	
	/* dup. multi profile + object key */ 
	loc = g_strdup (corbaloc); 

	/* split at last '/' */ 
	okey = strrchr (loc, '/');
	if (!okey || strlen (okey)==0)
		goto ret_error;
	*okey = '\0'; 
	++okey;       

	if (!strlen(okey)) 
		goto ret_error;
	
	if (!(objkey = orbit_url_decode (okey)))
		goto ret_error;
	
	if (!(token = g_strsplit (loc, ",", G_MAXINT))) 
		goto ret_error;

	/* [ 'iiop' ]   ':' [ '//' ] [ version '@' ] host [ ':' port ] */
	/* [ 'iiops' ]  ':' [ '//' ] [ version '@' ] host [ ':' port ] */
	/* [ 'ssliop' ] ':' [ '//' ] [ version '@' ] host [ ':' port ] */
	/* [ 'uiop' ]   ':' [ '//' ] socket ':' [ port ]               */
	for (i=0; token[i]; ++i) {
		switch (token [i][0]) {
		case ':':
		case 'i':
		case 's': {
			IOP_TAG_INTERNET_IOP_info *iiop_profile 
				= corbaloc_profile_iiop (token[i], objkey);
			if (!iiop_profile) 
				goto ret_error;
			profiles = g_slist_append (profiles, iiop_profile);
			break;
		}
		case 'u': {
			IOP_TAG_ORBIT_SPECIFIC_info *osi_profile
				= corbaloc_profile_uiop (token[i], objkey);
			if (!osi_profile) 
				goto ret_error;
			profiles = g_slist_append (profiles,  osi_profile);
			break;
		}
		default:
			goto ret_error;
			
		}
	}
	goto ret_ok;

ret_error:
	if (profiles) {
		IOP_delete_profiles (NULL, &profiles);
		profiles = NULL;
	}

ret_ok:
	if (loc)
		g_free (loc);        /* also free's okey */
	if (token)
		g_strfreev (token);
	if (objkey)
		CORBA_free (objkey);

	return profiles;
}

static gboolean
as_corbaloc (GSList *profile_list)
{
	gboolean   valid  = FALSE;
	GSList    *cur    = NULL;

        for (cur = profile_list; cur; cur = cur->next) {
		IOP_Profile_info *info = cur->data;
		switch (info->profile_type) {
		case IOP_TAG_INTERNET_IOP:
		case IOP_TAG_ORBIT_SPECIFIC:
			valid = TRUE;
			break;
		default:
			break;
		}
	}

	if (!valid) {
		dprintf (OBJECTS, "none IIOP, SSLIOP or UIOP profile found\n");
	}

	return valid;
}

static IOP_TAG_SSL_SEC_TRANS_info*
get_ssl_component (GSList *components)
{
	GSList          *cur    = NULL;
	
	for (cur = components; cur; cur = cur->next) {
		IOP_Component_info *comp = cur->data;
		
		switch (comp->component_type) {
		case IOP_TAG_SSL_SEC_TRANS:
			return (IOP_TAG_SSL_SEC_TRANS_info*) comp;
			break;
		default:
			break;
		}
	}

	return NULL;
}
	
static gchar* 
giop_version_str (GIOPVersion ver) {
	static gchar  *str[] = {"1.0", "1.1", "1.2"};

	g_return_val_if_fail (ver == GIOP_1_0 || 
			      ver == GIOP_1_1 || 
			      ver == GIOP_1_2,
			      str [2]);

	return str [ver];
}
 
CORBA_char*
ORBit_corbaloc_from (GSList *profile_list, ORBit_ObjectKey *object_key)
{
	CORBA_char      *retval;
	GString         *str;
	GSList          *cur;
	gboolean         first_profile;

	if (!as_corbaloc (profile_list))
		return NULL;

	str = g_string_sized_new (256);

	g_string_printf (str, "corbaloc:");

	first_profile = TRUE;
        for (cur = profile_list; cur; cur = cur->next) {
		IOP_Profile_info *info = cur->data;
		
		switch (info->profile_type) {
		case IOP_TAG_INTERNET_IOP: { 
			IOP_TAG_INTERNET_IOP_info  *iiop     = cur->data;
			IOP_TAG_SSL_SEC_TRANS_info *ssl_info = NULL;
			gint       i = 0;
			
			if (!first_profile)
				g_string_append_printf (str, ",");
			first_profile = FALSE;

			if ((ssl_info = get_ssl_component (iiop->components))) {
				g_assert (ssl_info->port != 0);

				g_string_append_printf (str, "ssliop:%s@%s:%d/",
						        giop_version_str (iiop->iiop_version),
							iiop->host, 
							ssl_info->port);

			} else
				g_string_append_printf (str, "iiop:%s@%s:%d/",
						        giop_version_str (iiop->iiop_version),
							iiop->host, 
							iiop->port);

			/* url encoding */ 
			for (i = 0; i < object_key->_length; i++)
				g_string_append_printf (str, "%%%02x", 
							object_key->_buffer [i]);
			break;
		}	

		case IOP_TAG_ORBIT_SPECIFIC: {
			IOP_TAG_ORBIT_SPECIFIC_info *os = cur->data;
			gint i = 0;
			
			if (!first_profile)
				g_string_append_printf (str, ",");
			first_profile = FALSE;

			/* suppress ipv6_port if possible to be
			 * compliant to uiop syntax known by
			 * 'openorb' */ 
			if (os->ipv6_port) 
			        g_string_append_printf (str, "uiop:%s:%d/",
							      os->unix_sock_path, 
							      os->ipv6_port);
			else
				g_string_append_printf (str, "uiop:%s:/",
							os->unix_sock_path);
				
			/* url encoding */ 
			for (i = 0; i < object_key->_length; i++)
				g_string_append_printf (str, "%%%02x", 
							object_key->_buffer [i]);			
			break;
		}
		case  IOP_TAG_GENERIC_IOP: 
		default:
			/* ignore */ 
			break;
		}
        }
	
	retval = CORBA_string_dup (str->str);

	g_string_free (str, TRUE);
	
	return retval;
}

