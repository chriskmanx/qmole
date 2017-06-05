
#include <config.h>
#include <ctype.h>
#include <string.h>

#include <orbit/orbit.h>
#include <orbit/orb-core/orb-types.h>

#include "../orbit-init.h"
#include "../poa/orbit-poa-export.h"
#include "../util/orbit-options.h"
#include "../util/orbit-purify.h"
#include "iop-profiles.h"
#include "orb-core-private.h"
#if defined ENABLE_HTTP
#include "orbhttp.h"
#endif
#include "orbit-debug.h"

extern const ORBit_option orbit_supported_options[];

#ifdef G_ENABLE_DEBUG
OrbitDebugFlags _orbit_debug_flags = ORBIT_DEBUG_NONE;
#endif

/*
 * Command line option handling.
 */
#ifdef G_OS_WIN32
static gboolean     orbit_use_ipv4           = TRUE;
#else
static gboolean     orbit_use_ipv4           = FALSE;
#endif
static gboolean     orbit_use_ipv6           = FALSE; 
#ifdef G_OS_WIN32
static gboolean     orbit_use_usocks         = FALSE;
#else
static gboolean     orbit_use_usocks         = TRUE;
#endif
static gint         orbit_initial_recv_limit = -1;
static gboolean     orbit_use_irda           = FALSE;
static gboolean     orbit_use_ssl            = FALSE;
static gboolean     orbit_use_genuid_simple  = FALSE;
#ifdef G_OS_WIN32
static gboolean     orbit_local_only         = TRUE;
#else
static gboolean     orbit_local_only         = FALSE;
#endif
static char        *orbit_net_id             = NULL;
static gboolean     orbit_use_http_iors      = FALSE;
static char        *orbit_ipsock             = NULL;
static const char  *orbit_ipname             = NULL;
static char        *orbit_debug_options      = NULL;
static char        *orbit_naming_ref         = NULL;
static GSList      *orbit_initref_list       = NULL; 
static gboolean     orbit_use_corbaloc       = FALSE;
static guint        orbit_timeout_msec       = 60000; /* 60 seconds - 0 will disable timeouts altogether */

void
ORBit_ORB_start_servers (CORBA_ORB orb)
{
	LinkProtocolInfo     *info;
	LinkConnectionOptions create_options = 0;

	LINK_MUTEX_LOCK (orb->lock);
	
	if (orb->servers) { /* beaten to it */
		LINK_MUTEX_UNLOCK (orb->lock);
		return;
	}

	if (orbit_local_only)
		create_options |= LINK_CONNECTION_LOCAL_ONLY;

	if (orbit_local_only || (orbit_use_usocks && !(orbit_use_ipv4 || orbit_use_ipv6 || orbit_use_irda || orbit_use_ssl)))
			link_use_local_hostname (LINK_NET_ID_IS_LOCAL);
	else {
		do {
			if (!orbit_net_id)
				break;

			if (!strcmp(orbit_net_id, "local")) {
				link_use_local_hostname (LINK_NET_ID_IS_LOCAL);
				break;
			}
			if (!strcmp(orbit_net_id, "short")) {
				link_use_local_hostname (LINK_NET_ID_IS_SHORT_HOSTNAME);
				break;
			}
			if (!strcmp(orbit_net_id, "fqdn")) {
				link_use_local_hostname (LINK_NET_ID_IS_FQDN);
				break;
			}
			if (!strcmp(orbit_net_id, "ipaddr")) {
				link_use_local_hostname (LINK_NET_ID_IS_IPADDR);
				break;
			}
			link_set_local_hostname(orbit_net_id);
		} while (0);
	}
		    
	if (!orbit_ipname)
		orbit_ipname = link_get_local_hostname();
	else
		link_set_local_hostname(orbit_ipname);

	for (info = link_protocol_all (); info->name; info++) {
		GIOPServer           *server;

		if (!ORBit_proto_use (info->name))
			continue;

		server = giop_server_new (
			orb->default_giop_version, info->name,
			orbit_ipname, orbit_ipsock,
			create_options, orb);

		if (server) {
			orb->servers = g_slist_prepend (orb->servers, server);

			if (!(info->flags & LINK_PROTOCOL_SECURE)) {
				if (!ORBit_proto_use ("SSL"))
					continue;

				server = giop_server_new (
					orb->default_giop_version, info->name,
					NULL, NULL, LINK_CONNECTION_SSL | create_options,
					orb);

				if (server)
					orb->servers = g_slist_prepend (orb->servers, server);
			}
#ifdef DEBUG
			fprintf (stderr, "ORB created giop server '%s'\n", info->name);
#endif
		}
#ifdef DEBUG
		else
			fprintf (stderr, "ORB failed to create giop server '%s'\n", info->name);
#endif
	}

	orb->profiles = IOP_start_profiles (orb);

	LINK_MUTEX_UNLOCK (orb->lock);
}

static void
strip_object_profiles (gpointer o, gpointer b, gpointer c)
{
	CORBA_Object obj = o;

	IOP_delete_profiles (obj->orb, &obj->profile_list);
	IOP_delete_profiles (obj->orb, &obj->forward_locations);

	obj->orb = NULL;
}

static void
ORBit_ORB_shutdown_servers (CORBA_ORB orb)
{
	LINK_MUTEX_LOCK (orb->lock);

	if (orb->objrefs) {
		g_hash_table_foreach (orb->objrefs,
				      strip_object_profiles, NULL);
		g_hash_table_destroy (orb->objrefs);
		orb->objrefs = NULL;
	}

	IOP_shutdown_profiles (orb->profiles);
	orb->profiles = NULL;

	g_slist_foreach (orb->servers, (GFunc) g_object_unref, NULL);
	g_slist_free (orb->servers); 
	orb->servers = NULL;

	LINK_MUTEX_UNLOCK (orb->lock);
}

static ORBitGenUidType
ORBit_genuid_type (void)
{
	ORBitGenUidType retval = ORBIT_GENUID_STRONG;;

	if (orbit_use_genuid_simple)
		retval = ORBIT_GENUID_SIMPLE;

	else if (orbit_use_usocks && !orbit_use_ipv4 &&
		 !orbit_use_ipv6 && !orbit_use_irda)
		retval = ORBIT_GENUID_SIMPLE;

	return retval;
}

static void
genuid_init (void)
{
	/* We treat the 'local_only' mode as a very special case */
	if (orbit_local_only &&
	    orbit_use_genuid_simple)
		g_error  ("It is impossible to isolate one user from another "
			  "with only simple cookie generation, you cannot "
			  "explicitely enable this option and LocalOnly mode "
			  "at the same time");

	else if (!ORBit_genuid_init (ORBit_genuid_type ())) {

		if (orbit_local_only)
			g_error ("Failed to find a source of randomness good "
				 "enough to insulate local users from each "
				 "other. If you use Solaris you need /dev/random "
				 "from the SUNWski package");
	}
}


static void
ORBit_service_list_free_ref (gpointer         key,
			     ORBit_RootObject objref,
			     gpointer         dummy)
{
	ORBit_RootObject_release (objref);
}

static void
CORBA_ORB_release_fn (ORBit_RootObject robj)
{
	CORBA_ORB orb = (CORBA_ORB)robj;

	g_ptr_array_free (orb->adaptors, TRUE);

	g_hash_table_destroy (orb->initial_refs);

	p_free (orb, struct CORBA_ORB_type);
}

GMutex *ORBit_RootObject_lifecycle_lock = NULL;

static void
ORBit_locks_initialize (void)
{
	ORBit_RootObject_lifecycle_lock = link_mutex_new ();
}

#ifdef G_ENABLE_DEBUG
static void
ORBit_setup_debug_flags (void)
{
	static GDebugKey debug_keys[] = {
		{ "traces",         ORBIT_DEBUG_TRACES },
		{ "inproc_traces",  ORBIT_DEBUG_INPROC_TRACES },
		{ "timings",        ORBIT_DEBUG_TIMINGS },
		{ "types",          ORBIT_DEBUG_TYPES },
		{ "messages",       ORBIT_DEBUG_MESSAGES },
		{ "errors",         ORBIT_DEBUG_ERRORS },
		{ "objects",        ORBIT_DEBUG_OBJECTS },
		{ "giop",           ORBIT_DEBUG_GIOP },
		{ "refs",           ORBIT_DEBUG_REFS },
		{ "force_threaded", ORBIT_DEBUG_FORCE_THREADED }
	};
	const char *env_string;

	env_string = g_getenv ("ORBIT2_DEBUG");

	if (env_string)
		_orbit_debug_flags |=
			g_parse_debug_string (env_string,
					      debug_keys,
					      G_N_ELEMENTS (debug_keys));

	if (orbit_debug_options)
		_orbit_debug_flags |=
			g_parse_debug_string (orbit_debug_options,
					      debug_keys,
					      G_N_ELEMENTS (debug_keys));

	if (_orbit_debug_flags & ORBIT_DEBUG_INPROC_TRACES)
		ORBit_small_flags |= ORBIT_SMALL_FORCE_GENERIC_MARSHAL;
}
#endif /* G_ENABLE_DEBUG */

static CORBA_ORB _ORBit_orb = NULL;
static gulong    init_level = 0;
static gboolean  atexit_shutdown = FALSE;

#ifndef G_OS_WIN32 /* See comment at g_atexit() call below */
/*
 *   This is neccessary to clean up any remaining UDS
 * and to flush any remaining oneway traffic in buffers.
 */
static void
shutdown_orb (void)
{
	CORBA_ORB orb;
	CORBA_Environment ev;

	if (!(orb = _ORBit_orb))
		return;

	init_level = 1; /* clobber it */
	atexit_shutdown = TRUE;
	
	CORBA_exception_init (&ev);

	CORBA_ORB_destroy (orb, &ev);
	ORBit_RootObject_release (orb);

	CORBA_exception_free (&ev);

	atexit_shutdown = FALSE;
}
#endif

static
gboolean
ORBit_initial_reference_protected_id (gchar* id)
{
        return (!strncmp (id, "RootPOA", strlen("RootPOA")) ||
                !strncmp (id, "POACurrent", strlen("POACurrent")));
}
  
static void 
ORBit_initial_references_by_user (CORBA_ORB          orb, 
				  gchar             *naming_ref,
				  GSList            *initref_list,
				  CORBA_Environment *ev)
{
	GSList *l;
	CORBA_Object objref;

	if (ev->_major != CORBA_NO_EXCEPTION)
		return;

	if (naming_ref) {
		objref = CORBA_ORB_string_to_object (orb, naming_ref, ev);
		
		/* FIXME, should abort if invalid option, don't forget
		 * to free resources allocated by ORB */ 
		if (ev->_major != CORBA_NO_EXCEPTION) {
			g_warning ("Option ORBNamingIOR has invalid object reference: %s", 
				   naming_ref);
			CORBA_exception_free (ev);
		} else {
			/* FIXME, test type of object for
			 * IDL:omg.org/CosNaming/NamingContext using _is_a()
			 * operation */ 
			ORBit_set_initial_reference (orb, "NameService", objref);
			ORBit_RootObject_release (objref);
		}
	}

	for (l = initref_list; l; l = l->next) {
		ORBit_OptionKeyValue *tuple = l->data;

		g_assert (tuple != NULL);
		g_assert (tuple->key   != (gchar*)NULL);
		g_assert (tuple->value != (gchar*)NULL);
			
		objref = CORBA_ORB_string_to_object (orb, tuple->value, ev);

		/* FIXME, should abort if invalid option,
		 * don't forget to free resources allocated by
		 * ORB */ 
		if (ev->_major != CORBA_NO_EXCEPTION) {
			g_warning ("Option ORBInitRef has invalid object reference: %s=%s",  
				   tuple->key, tuple->value);
			CORBA_exception_free (ev);
		} else {
			if (ORBit_initial_reference_protected_id(tuple->key)) {
				g_warning ("Option ORBInitRef permission denied: %s=%s",
					   tuple->key, tuple->value);
			} else {
				ORBit_set_initial_reference (orb, tuple->key, objref);
			}
			
			ORBit_RootObject_release (objref);
		}
	}

}

CORBA_ORB
CORBA_ORB_init (int *argc, char **argv,
		CORBA_ORBid orb_identifier,
		CORBA_Environment *ev)
{
	gboolean thread_safe;
	CORBA_ORB retval;
	static ORBit_RootObject_Interface orb_if = {
		ORBIT_ROT_ORB,
		CORBA_ORB_release_fn
	};

	init_level++;

	if ((retval = _ORBit_orb))
		return ORBit_RootObject_duplicate (retval);

	/* the allocation code uses the bottom bit of any pointer */
	g_assert (ORBIT_ALIGNOF_CORBA_DOUBLE > 2);

	if (orb_identifier &&
	    strstr (orb_identifier, "orbit-local-non-threaded-orb") != NULL)
		thread_safe = FALSE;
	else
		thread_safe = TRUE;

	ORBit_option_parse (argc, argv, orbit_supported_options);
	
#ifdef G_ENABLE_DEBUG
	ORBit_setup_debug_flags ();

	if (_orbit_debug_flags & ORBIT_DEBUG_FORCE_THREADED) {
		g_warning ("-- Forced orb into threaded mode --");
		thread_safe |= TRUE;
	}
#endif /* G_ENABLE_DEBUG */

	giop_recv_set_limit (orbit_initial_recv_limit);
	giop_set_timeout (orbit_timeout_msec);
	giop_init (thread_safe,
		   orbit_use_ipv4 || orbit_use_ipv6 ||
		   orbit_use_irda || orbit_use_ssl);

	if (orb_identifier && thread_safe &&
	    strstr (orb_identifier, "orbit-io-thread") != NULL)
		link_set_io_thread (TRUE);

	genuid_init ();
	_ORBit_object_init ();
	ORBit_poa_init ();

	ORBit_locks_initialize ();

	retval = g_new0 (struct CORBA_ORB_type, 1);

	ORBit_RootObject_init (&retval->root_object, &orb_if);
	/* released by CORBA_ORB_destroy */
	_ORBit_orb = ORBit_RootObject_duplicate (retval);
	_ORBit_orb->lock = link_mutex_new ();
#ifndef G_OS_WIN32
	/* atexit(), which g_atexit() is just a #define for on Win32,
	 * often causes breakage when invoked from DLLs. It causes the
	 * registered function to be called when the calling DLL is
	 * being unloaded. At that time, however, random other DLLs
	 * might also have already been unloaded. There is no
	 * guarantee WinSock even works any longer. Etc. Best to avoid
	 * atexit() completely on Win32, and hope that just exiting
	 * the process and thus severing all connections will be
	 * noticed by all peers the process was connected to and acted
	 * upon properly.
	 *
	 * In the evolution-exchange-storage process's case, the
	 * shutdown_orb() function caused the process to hang and not
	 * exit, leaving the sockets it was listening on still in a
	 * LISTEN state. bonobo-activation-server thought the bonobo
	 * servers in evolution-exchange-storage were still OK and
	 * tried to contact them when Evolution was started the next
	 * time, causing it to hang, too.
	 */
	g_atexit (shutdown_orb);
#endif

	retval->default_giop_version = GIOP_LATEST;

	retval->adaptors = g_ptr_array_new ();

	/* init the forward bind hashtable*/
	retval->forw_binds = g_hash_table_new_full (
		g_str_hash, g_str_equal,
		g_free,
		NULL); 

	ORBit_init_internals (retval, ev);
	/* FIXME, handle exceptions */ 

	ORBit_initial_references_by_user (retval, 
					  orbit_naming_ref,
					  orbit_initref_list,
					  ev);
	/* FIXME, handle exceptions */ 

	return ORBit_RootObject_duplicate (retval);
}

CORBA_char *
CORBA_ORB_object_to_string (CORBA_ORB          orb,
			    const CORBA_Object obj,
			    CORBA_Environment *ev)
{
	GIOPSendBuffer *buf;
	CORBA_octet     endianness = GIOP_FLAG_ENDIANNESS;
	CORBA_char     *out;
	int             i, j, k;

	g_return_val_if_fail (ev != NULL, NULL);

	if(!orb || !obj || ORBIT_ROOT_OBJECT_TYPE (obj) != ORBIT_ROT_OBJREF) {
		CORBA_exception_set_system (
				ev, ex_CORBA_BAD_PARAM, CORBA_COMPLETED_NO);

		return NULL;
	}

	if (orbit_use_corbaloc) {
		out = ORBit_object_to_corbaloc (obj, ev);
		if (ev->_major == CORBA_NO_EXCEPTION)
			return out;

		CORBA_exception_free (ev);	
		/* fall thru, common marshalling */ 
	}
	
	buf = giop_send_buffer_use (orb->default_giop_version);

	g_assert (buf->num_used == 1);

	buf->header_size             = 0;
	buf->lastptr                 = NULL;
	buf->num_used                = 0; /* we don't want the header in there */
	buf->msg.header.message_size = 0;

	giop_send_buffer_append (buf, &endianness, 1);

	ORBit_marshal_object (buf, obj);
	out = CORBA_string_alloc (4 + (buf->msg.header.message_size * 2) + 1);

	strcpy (out, "IOR:");

	for (i = 0, k = 4; i < buf->num_used; i++) {
		struct iovec *curvec;
		guchar       *ptr;

		curvec = &buf->iovecs [i];
		ptr    = curvec->iov_base;

		for (j = 0; j < curvec->iov_len; j++, ptr++) {
			int n1, n2;

			n1 = (*ptr & 0xF0) >> 4;
			n2 = (*ptr & 0xF);

			out [k++] = num2hexdigit (n1);
			out [k++] = num2hexdigit (n2);
		}
	}

	out [k++] = '\0';
  
	giop_send_buffer_unuse (buf);

	return out;
}

CORBA_Object
CORBA_ORB_string_to_object (CORBA_ORB          orb,
			    const CORBA_char  *string,
			    CORBA_Environment *ev)
{
	CORBA_Object         retval = CORBA_OBJECT_NIL;
	CORBA_unsigned_long  len;
	GIOPRecvBuffer      *buf;
#if defined ENABLE_HTTP
	gchar               *ior = NULL;
#endif
	guchar              *tmpbuf;
	int                  i;

	if (strncmp (string, "IOR:", strlen("IOR:"))            &&
	    strncmp (string, "corbaloc:", strlen ("corbaloc:")) &&
	    strncmp (string, "iiop:", strlen ("iiop:"))         &&
	    strncmp (string, "iiops:", strlen ("iiops:"))       &&
	    strncmp (string, "ssliop:", strlen ("ssliop:"))     &&
	    strncmp (string, "uiop:", strlen ("uiop:"))) {

#if defined ENABLE_HTTP
		if (orbit_use_http_iors &&
		    strstr (string, "://")) {
			/* FIXME: this code is a security hazard */
			ior = orb_http_resolve (string);
			if (!ior) {
				/* FIXME, set error minor code
				 * (vendor's error code) to tell user
				 * initial location of error, ie my
				 * local ORB, proxy's ORB, server's
				 * ORB, etc. */
				CORBA_exception_set_system (
					ev,
					ex_CORBA_BAD_PARAM,
					CORBA_COMPLETED_NO);

				return CORBA_OBJECT_NIL;
			}
			string = ior;
		} else 
#endif
		{
			CORBA_exception_set_system (
					ev,
					ex_CORBA_BAD_PARAM,
					CORBA_COMPLETED_NO);

			return CORBA_OBJECT_NIL;
		}
	}

	if (!strncmp (string, "IOR:", strlen ("IOR:")))
	{
		string += 4;
		len = strlen (string);
		while (len > 0 && !g_ascii_isxdigit (string [len - 1]))
			len--;
		
		if (len % 2) {
#if defined ENABLE_HTTP
			g_free (ior);
#endif
			return CORBA_OBJECT_NIL;
		}
		
		tmpbuf = g_alloca (len / 2);
		
		for (i = 0; i < len; i += 2)
			tmpbuf [i/2] = (g_ascii_xdigit_value (string [i]) << 4) |
				g_ascii_xdigit_value (string [i + 1]);
		
		buf = giop_recv_buffer_use_encaps (tmpbuf, len / 2);
		
		if (ORBit_demarshal_object (&retval, buf, orb)) {
			CORBA_exception_set_system (
				ev,
				ex_CORBA_MARSHAL,
				CORBA_COMPLETED_NO);
			
			retval = CORBA_OBJECT_NIL;
		}
		
		giop_recv_buffer_unuse (buf);
#if defined ENABLE_HTTP		
		g_free (ior);
#endif
		return retval;
	} else {
		return ORBit_object_by_corbaloc (orb, string, ev);
	} 
}

void
CORBA_ORB_create_list (CORBA_ORB          obj,
		       const CORBA_long   count,
		       CORBA_NVList      *new_list,
		       CORBA_Environment *ev)
{
	CORBA_NVList nvlist;

	nvlist = g_new0 (struct CORBA_NVList_type, 1);

	nvlist->list = g_array_new (FALSE, TRUE, sizeof (CORBA_NamedValue));

	*new_list = nvlist;
}

void
CORBA_ORB_create_operation_list (CORBA_ORB                 orb,
				 const CORBA_OperationDef  oper,
				 CORBA_NVList             *new_list,
				 CORBA_Environment        *ev)
{
}

void
CORBA_ORB_send_multiple_requests_oneway (CORBA_ORB               orb,
					 const CORBA_RequestSeq *req,
					 CORBA_Environment      *ev)
{
}

void
CORBA_ORB_send_multiple_requests_deferred (CORBA_ORB               orb,
					   const CORBA_RequestSeq *req,
					   CORBA_Environment      *ev)
{
}

CORBA_boolean
CORBA_ORB_poll_next_response (CORBA_ORB          orb,
			      CORBA_Environment *ev)
{
	return CORBA_FALSE;
}

void
CORBA_ORB_get_next_response (CORBA_ORB          orb,
			     CORBA_Request     *req,
			     CORBA_Environment *ev)
{
	*req = NULL;
}

CORBA_boolean
CORBA_ORB_get_service_information (CORBA_ORB                  orb,
				   const CORBA_ServiceType    service_type,
				   CORBA_ServiceInformation **service_information,
				   CORBA_Environment         *ev)
{
	/* FIXME: 
         * see http://mail.gnome.org/archives/orbit-list/2003-May/msg00093.html

	 * Assigning NULL to parameter service_information is not
	 * compliant to CORBA spec. This operation is part of pseudo
	 * interface and must react like operation of true remote
	 * interface. The question is what value it should point to in
	 * case CORBA_FALSE is returned to caller.

	 4.2.2 Getting Service Information
	 4.2.2.1 get_service_information

	 boolean get_service_information (in ServiceType service_type;
	                                  out ServiceInformation service_information;);

         * The get_service_information operation is used to obtain
         * information about CORBA facilities and services that are
         * supported by this ORB. The service type for which
         * information is being requested is passed in as the in
         * parameter service_type, the values defined by constants in
         * the CORBA module. If service information is available for
         * that type, that is returned in the out parameter
         * service_information, and the operation returns the value
         * TRUE. If no information for the requested services type is
         * available, the operation returns FALSE (i.e., the service
         * is not supported by this ORB).
	 */	
	*service_information = NULL;

	return CORBA_FALSE;
}

struct ORBit_service_list_info {
	CORBA_ORB_ObjectIdList *list;
	CORBA_long              index;
};

static void
ORBit_service_list_add_id (CORBA_string                    key,
			   gpointer                        value,
			   struct ORBit_service_list_info *info)
{
	info->list->_buffer [info->index++] = CORBA_string_dup (key);
}

CORBA_ORB_ObjectIdList *
CORBA_ORB_list_initial_services (CORBA_ORB          orb,
				 CORBA_Environment *ev)
{
	CORBA_ORB_ObjectIdList         *retval;

	retval = CORBA_ORB_ObjectIdList__alloc();

	if (orb->initial_refs) {
		struct ORBit_service_list_info *info;

		info = g_alloca (sizeof (struct ORBit_service_list_info));

		info->index = 0;
		info->list  = retval;

		retval->_length  = g_hash_table_size (orb->initial_refs);
		retval->_maximum = retval->_length;
		retval->_buffer  = CORBA_sequence_CORBA_ORB_ObjectId_allocbuf (
					retval->_length);

		g_hash_table_foreach (orb->initial_refs,
				      (GHFunc)ORBit_service_list_add_id,
				      info);

		retval->_release = CORBA_TRUE;

		g_assert (info->index == retval->_length);
	} else {
		retval->_length = 0;
		retval->_buffer = NULL;
	}

	return retval;
}

/** The InvalidName exception is raised at @ev when
 *  ORB.resolve_initial_references is passed an @identifier for which
 *  there is no initial reference.
 */
CORBA_Object
CORBA_ORB_resolve_initial_references (CORBA_ORB          orb,
				      const CORBA_char  *identifier,
				      CORBA_Environment *ev)
{
	CORBA_Object objref;

	/* FIXME, verify identifier and raise exception for invalid
	 * service names, valid names might be: NameService, RootPOA,
	 * SecurityCurrent, PolicyCurrent, etc. */
 
	if (!orb->initial_refs ||
	    !(objref = g_hash_table_lookup (orb->initial_refs, identifier)))
		return CORBA_OBJECT_NIL;
	
	return ORBit_RootObject_duplicate (objref);
#if 0
 raise_invalid_name:
	CORBA_exception_set (ev,
			     CORBA_USER_EXCEPTION,
			     ex_CORBA_ORB_InvalidName,
			     NULL);
	return CORBA_OBJECT_NIL;
#endif
}

static CORBA_TypeCode
ORBit_TypeCode_allocate (void)
{
	CORBA_TypeCode tc = g_new0 (struct CORBA_TypeCode_struct, 1);

	ORBit_RootObject_init (&tc->parent, &ORBit_TypeCode_epv);

	return ORBit_RootObject_duplicate (tc);
}

CORBA_TypeCode
CORBA_ORB_create_struct_tc (CORBA_ORB                    orb,
			    const CORBA_char            *id,
			    const CORBA_char            *name,
			    const CORBA_StructMemberSeq *members,
			    CORBA_Environment           *ev)
{
	CORBA_TypeCode retval;
	int            i;

	retval = ORBit_TypeCode_allocate ();

	retval->subtypes = g_new0 (CORBA_TypeCode, members->_length);
	retval->subnames = g_new0 (char *, members->_length);

	retval->kind      = CORBA_tk_struct;
	retval->name      = g_strdup (name);
	retval->repo_id   = g_strdup (id);
	retval->sub_parts = members->_length;
	retval->length    = members->_length;

	for(i = 0; i < members->_length; i++) {
		CORBA_StructMember *member = &members->_buffer [i];

		g_assert (&member->type != CORBA_OBJECT_NIL);

		retval->subtypes [i] = ORBit_RootObject_duplicate (member->type);
		retval->subnames [i] = g_strdup (member->name);
	}

	return retval;
}

static void
copy_case_value (CORBA_long *dest,
		 CORBA_any  *src)
{
	CORBA_TypeCode tc = src->_type;

	if (tc->kind == CORBA_tk_alias)
		tc = tc->subtypes [0];

        switch (tc->kind) {
        case CORBA_tk_ulong:
        case CORBA_tk_long:
        case CORBA_tk_enum:
                *dest = *(CORBA_long *) src->_value;
                break;
        case CORBA_tk_ushort:
        case CORBA_tk_short:
                *dest = *(CORBA_short *) src->_value;
                break;
        case CORBA_tk_char:
        case CORBA_tk_boolean:
        case CORBA_tk_octet:
                *dest = *(CORBA_octet *) src->_value;
                break;
        default:
		g_assert_not_reached ();
		break;
        }
}

CORBA_TypeCode
CORBA_ORB_create_union_tc (CORBA_ORB                   orb,
			   const CORBA_char           *id,
			   const CORBA_char           *name,
			   const CORBA_TypeCode        discriminator_type,
			   const CORBA_UnionMemberSeq *members,
			   CORBA_Environment          *ev)
{
	CORBA_TypeCode retval;
	int            i;

	retval = ORBit_TypeCode_allocate ();

	retval->discriminator = ORBit_RootObject_duplicate (discriminator_type);
		
	retval->subtypes  = g_new0 (CORBA_TypeCode, members->_length);
	retval->subnames  = g_new0 (char *, members->_length);
	retval->sublabels = g_new0 (CORBA_long, members->_length);

	retval->kind          = CORBA_tk_union;
	retval->name          = g_strdup (name);
	retval->repo_id       = g_strdup (id);
	retval->sub_parts     = members->_length;
	retval->length        = members->_length;
	retval->default_index = -1;

	for (i = 0; i < members->_length; i++) {
		CORBA_UnionMember *member = &members->_buffer [i];

		g_assert (member->type != CORBA_OBJECT_NIL);

		copy_case_value (&retval->sublabels [i], &member->label);

		retval->subtypes [i] = ORBit_RootObject_duplicate (member->type);
		retval->subnames [i] = g_strdup (member->name);

		if (member->label._type->kind == CORBA_tk_octet)
			retval->default_index = i;
	}

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_enum_tc (CORBA_ORB                  orb,
			  const CORBA_char          *id,
			  const CORBA_char          *name,
			  const CORBA_EnumMemberSeq *members,
			  CORBA_Environment         *ev)
{
	CORBA_TypeCode retval;
	int            i;

	retval = ORBit_TypeCode_allocate ();

	retval->subnames=g_new0 (char *, members->_length);

	retval->kind      = CORBA_tk_enum;
	retval->name      = g_strdup (name);
	retval->repo_id   = g_strdup (id);
	retval->sub_parts = members->_length;
	retval->length    = members->_length;

	for (i = 0; i < members->_length; i++)
		retval->subnames [i] = g_strdup (members->_buffer [i]);

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_alias_tc (CORBA_ORB             orb,
			   const CORBA_char     *id,
			   const CORBA_char     *name,
			   const CORBA_TypeCode  original_type,
			   CORBA_Environment    *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();
	
	retval->subtypes = g_new0 (CORBA_TypeCode, 1);

	retval->kind      = CORBA_tk_alias;
	retval->name      = g_strdup (name);
	retval->repo_id   = g_strdup (id);
	retval->sub_parts = 1;
	retval->length    = 1;

	retval->subtypes [0] = ORBit_RootObject_duplicate (original_type);

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_exception_tc (CORBA_ORB                    orb,
			       const CORBA_char            *id,
			       const CORBA_char            *name,
			       const CORBA_StructMemberSeq *members,
			       CORBA_Environment           *ev)
{
	CORBA_TypeCode retval;
	int            i;

	retval = ORBit_TypeCode_allocate ();

	if (members->_length) {
		retval->subtypes = g_new0 (CORBA_TypeCode, members->_length);
		retval->subnames = g_new0 (char *, members->_length);
	}

	retval->kind      = CORBA_tk_except;
	retval->name      = g_strdup (name);
	retval->repo_id   = g_strdup (id);
	retval->sub_parts = members->_length;
	retval->length    = members->_length;

	for (i = 0; i < members->_length; i++) {
		CORBA_StructMember *member = &members->_buffer [i];

		g_assert (member->type != CORBA_OBJECT_NIL);

		retval->subtypes [i] = ORBit_RootObject_duplicate (member->type);
		retval->subnames [i] = g_strdup (member->name);
	}

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_interface_tc (CORBA_ORB                 orb,
			       const CORBA_char         *id,
			       const CORBA_char         *name,
			       CORBA_Environment        *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();

	retval->kind    = CORBA_tk_objref;
	retval->name    = g_strdup (name);
	retval->repo_id = g_strdup (id);

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_string_tc (CORBA_ORB                  orb,
			    const CORBA_unsigned_long  bound,
			    CORBA_Environment         *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();

	retval->kind   = CORBA_tk_string;
	retval->length = bound;

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_wstring_tc (CORBA_ORB                  orb,
			     const CORBA_unsigned_long  bound,
			     CORBA_Environment         *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();

	retval->kind   = CORBA_tk_wstring;
	retval->length = bound;

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_fixed_tc (CORBA_ORB                   orb,
			   const CORBA_unsigned_short  digits,
			   const CORBA_short           scale,
			   CORBA_Environment          *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();

	retval->kind   = CORBA_tk_fixed;
	retval->digits = digits;
	retval->scale  = scale;

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_sequence_tc (CORBA_ORB                  orb,
			      const CORBA_unsigned_long  bound,
			      const CORBA_TypeCode       element_type,
			      CORBA_Environment         *ev)
{
	CORBA_TypeCode retval;

	retval = ORBit_TypeCode_allocate ();

	retval->subtypes = g_new0 (CORBA_TypeCode, 1);

	retval->kind      = CORBA_tk_sequence;
	retval->sub_parts = 1;
	retval->length    = bound;

	retval->subtypes [0] = ORBit_RootObject_duplicate (element_type);

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_recursive_sequence_tc (CORBA_ORB                  orb,
					const CORBA_unsigned_long  bound,
					const CORBA_unsigned_long  offset,
					CORBA_Environment         *ev)
{
	CORBA_TypeCode retval;

	retval=ORBit_TypeCode_allocate ();

	retval->subtypes = g_new0 (CORBA_TypeCode, 1);

	retval->kind      = CORBA_tk_sequence;
	retval->sub_parts = 1;
	retval->length    = bound;

	retval->subtypes [0] = ORBit_TypeCode_allocate ();

	retval->subtypes [0]->kind          = CORBA_tk_recursive;
	retval->subtypes [0]->recurse_depth = offset;

	return retval;
}

CORBA_TypeCode
CORBA_ORB_create_array_tc (CORBA_ORB                  orb,
			   const CORBA_unsigned_long  length,
			   const CORBA_TypeCode       element_type,
			   CORBA_Environment         *ev)
{
	CORBA_TypeCode tc;

	tc = ORBit_TypeCode_allocate ();

	tc->subtypes = g_new0 (CORBA_TypeCode, 1);

	tc->kind      = CORBA_tk_array;
	tc->sub_parts = 1;
	tc->length    = length;

	tc->subtypes [0] = ORBit_RootObject_duplicate (element_type);

	return (tc);
}

CORBA_TypeCode
CORBA_ORB_create_value_tc (CORBA_ORB                   orb,
			   const CORBA_char           *id,
			   const CORBA_char           *name,
			   const CORBA_ValueModifier   type_modifier,
			   const CORBA_TypeCode        concrete_base,
			   const CORBA_ValueMemberSeq *members,
			   CORBA_Environment          *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_TypeCode
CORBA_ORB_create_value_box_tc (CORBA_ORB             orb,
			       const CORBA_char     *id,
			       const CORBA_char     *name,
			       const CORBA_TypeCode  boxed_type,
			       CORBA_Environment    *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_TypeCode
CORBA_ORB_create_native_tc (CORBA_ORB          orb,
			    const CORBA_char  *id,
			    const CORBA_char  *name,
			    CORBA_Environment *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_TypeCode
CORBA_ORB_create_recursive_tc (CORBA_ORB          orb,
			       const CORBA_char  *id,
			       CORBA_Environment *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_TypeCode
CORBA_ORB_create_abstract_interface_tc (CORBA_ORB          orb,
				        const CORBA_char  *id,
				        const CORBA_char  *name,
				        CORBA_Environment *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_boolean
CORBA_ORB_work_pending (CORBA_ORB          orb,
			CORBA_Environment *ev)
{
	return link_main_pending ();
}

void
CORBA_ORB_perform_work (CORBA_ORB          orb,
			CORBA_Environment *ev)
{
	link_main_iteration (FALSE);
}

void
CORBA_ORB_run (CORBA_ORB          orb,
	       CORBA_Environment *ev)
{
	giop_main_run ();
}

void
CORBA_ORB_shutdown (CORBA_ORB           orb,
		    const CORBA_boolean wait_for_completion,
		    CORBA_Environment  *ev)
{
	PortableServer_POA root_poa;

	root_poa = g_ptr_array_index (orb->adaptors, 0);
	if (root_poa) {
		PortableServer_POA_destroy (
			root_poa, TRUE, wait_for_completion, ev);
		if (ev->_major) {
			if (wait_for_completion)
				g_warning ("FIXME: wait for "
					   "completion unimplemented");
			else
				return;
		}
	}

	giop_shutdown ();

	ORBit_ORB_shutdown_servers (orb);
}

void
CORBA_ORB_destroy (CORBA_ORB          orb,
		   CORBA_Environment *ev)
{
	PortableServer_POA root_poa;

	if (orb->life_flags & ORBit_LifeF_Destroyed)
		return;

	init_level--;

	if (init_level > 0)
		return;

	CORBA_ORB_shutdown (orb, TRUE, ev);

	g_assert (_ORBit_orb == orb);
	_ORBit_orb = NULL;

	if (ev->_major)
		return;

	root_poa = g_ptr_array_index (orb->adaptors, 0);
	if (root_poa &&
	    ((ORBit_RootObject) root_poa)->refs != 1) {
#ifdef G_ENABLE_DEBUG
		if (!atexit_shutdown)
			g_warning ("CORBA_ORB_destroy: Application still has %d "
				   "refs to RootPOA.",
				   ((ORBit_RootObject)root_poa)->refs - 1);
#endif
		CORBA_exception_set_system (
			ev, ex_CORBA_FREE_MEM, CORBA_COMPLETED_NO);
	}

	g_hash_table_foreach (orb->initial_refs,
			      (GHFunc)ORBit_service_list_free_ref,
			      NULL);

	ORBit_RootObject_release (orb->default_ctx);
	orb->default_ctx = CORBA_OBJECT_NIL;

	{
		int i;
		int leaked_adaptors = 0;

		/* Each poa has a ref on the ORB */
		for (i = 0; i < orb->adaptors->len; i++) {
			ORBit_ObjectAdaptor adaptor;

			adaptor = g_ptr_array_index (orb->adaptors, i);

			if (adaptor)
				leaked_adaptors++;
		}

		if (leaked_adaptors) {
#ifdef G_ENABLE_DEBUG
			if (!atexit_shutdown)
				g_warning ("CORBA_ORB_destroy: leaked '%d' Object Adaptors",
					   leaked_adaptors);
#endif
			CORBA_exception_set_system (
				ev, ex_CORBA_FREE_MEM, CORBA_COMPLETED_NO);
		}

		if (((ORBit_RootObject)orb)->refs != 2 + leaked_adaptors) {
#ifdef G_ENABLE_DEBUG
			if (!atexit_shutdown) {
				if (((ORBit_RootObject)orb)->refs == 1 + leaked_adaptors)
					g_warning ("CORBA_ORB_destroy: ORB unreffed but not _destroy'd");
				else
					g_warning ("CORBA_ORB_destroy: ORB still has %d refs.",
						   ((ORBit_RootObject)orb)->refs - 1 - leaked_adaptors);
			}
#endif
			CORBA_exception_set_system (
				ev, ex_CORBA_FREE_MEM, CORBA_COMPLETED_NO);
		}
	}

	/* destroy the forward bind hashtable*/
	g_hash_table_destroy (orb->forw_binds);
	orb->forw_binds = NULL;
	
	orb->life_flags |= ORBit_LifeF_Destroyed;

	if (orb->lock) {
		g_mutex_free (orb->lock);
		orb->lock = NULL;
	}

	ORBit_RootObject_release (orb);

	/* At this stage there should be 1 ref left in the system -
	 * on the ORB */
	if (ORBit_RootObject_shutdown (!atexit_shutdown))
		CORBA_exception_set_system (
			ev, ex_CORBA_FREE_MEM, CORBA_COMPLETED_NO);
}

CORBA_Policy
CORBA_ORB_create_policy (CORBA_ORB               orb,
			 const CORBA_PolicyType  type,
			 const CORBA_any        *val,
			 CORBA_Environment      *ev)
{
	return CORBA_OBJECT_NIL;
}

CORBA_ValueFactory
CORBA_ORB_register_value_factory (CORBA_ORB                 orb,
				  const CORBA_char         *id,
				  const CORBA_ValueFactory  factory,
				  CORBA_Environment        *ev)
{
	return CORBA_OBJECT_NIL;
}

void
CORBA_ORB_unregister_value_factory (CORBA_ORB          orb,
				    const CORBA_char  *id,
				    CORBA_Environment *ev)
{
}

CORBA_ValueFactory
CORBA_ORB_lookup_value_factory (CORBA_ORB          orb,
			        const CORBA_char  *id,
			        CORBA_Environment *ev)
{
	return CORBA_OBJECT_NIL;
}

void
ORBit_set_initial_reference (CORBA_ORB  orb,
			     gchar     *identifier,
			     gpointer   objref)
{
	CORBA_Object old_objref;

	if (!orb->initial_refs)
		orb->initial_refs = g_hash_table_new (g_str_hash, g_str_equal);

	if ((old_objref = g_hash_table_lookup (orb->initial_refs, identifier))) {
		ORBit_RootObject_release (old_objref);
		g_hash_table_remove (orb->initial_refs, identifier);
	}

	g_hash_table_insert (orb->initial_refs,
			     identifier,
			     ORBit_RootObject_duplicate (objref));
}

void
ORBit_ORB_forw_bind (CORBA_ORB                   orb,
		     CORBA_sequence_CORBA_octet *objkey,
		     CORBA_Object                obj,
		     CORBA_Environment          *ev)
{
	
	if (obj)
		g_hash_table_insert (orb->forw_binds, objkey->_buffer, obj);
	else {
		g_hash_table_remove(orb->forw_binds, objkey->_buffer);
	}
	
}

gboolean
ORBit_proto_use (const char *name)
{

	if ((orbit_use_ipv4   && !strcmp ("IPv4", name)) ||
	    (orbit_use_ipv6   && !strcmp ("IPv6", name)) || 
	    (orbit_use_usocks && !strcmp ("UNIX", name)) || 
	    (orbit_use_irda   && !strcmp ("IrDA", name)) || 
	    (orbit_use_ssl    && !strcmp ("SSL",  name)))
		return TRUE;

	return FALSE;
}

/**
 * ORBit_get_giop_recv_limit:
 * 
 * This function will return the GIOP receive limit. The
 * GIOP receive limit is the maximum number of bytes that
 * are allowed be received in any one ingoing GIOP 
 * communication. This function is essential if an application
 * is about to receive a big amount of data. Knowing the GIOP 
 * receive limit will enable the application to poll to data
 * in chunks that are below the receive limit.
 *
 * Since: 2.14.1
 */
glong
ORBit_get_giop_recv_limit (void)
{
	return giop_recv_get_limit ();
}

const ORBit_option orbit_supported_options[] = {
	{ "ORBid",              ORBIT_OPTION_STRING,  NULL }, /* FIXME: unimplemented */
	{ "ORBImplRepoIOR",     ORBIT_OPTION_STRING,  NULL }, /* FIXME: unimplemented */
	{ "ORBIfaceRepoIOR",    ORBIT_OPTION_STRING,  NULL }, /* FIXME: unimplemented */
	{ "ORBNamingIOR",       ORBIT_OPTION_STRING,  &orbit_naming_ref},

	{ "ORBRootPOAIOR",      ORBIT_OPTION_STRING,  NULL }, /* FIXME: huh?          */
 	{ "ORBIIOPIPName",      ORBIT_OPTION_STRING,  &orbit_ipname }, /* Will always take precedence over ORBNetID */
 	{ "ORBIIOPIPSock",      ORBIT_OPTION_STRING,  &orbit_ipsock },
	{ "ORBInitialMsgLimit", ORBIT_OPTION_INT,     &orbit_initial_recv_limit },
	{ "ORBLocalOnly",       ORBIT_OPTION_BOOLEAN, &orbit_local_only },
 	{ "ORBNetID",           ORBIT_OPTION_STRING,  &orbit_net_id },
	/* warning: this option is a security risk unless used with LocalOnly */
	{ "ORBIIOPIPv4",        ORBIT_OPTION_BOOLEAN, &orbit_use_ipv4 },
	{ "ORBIIOPIPv6",        ORBIT_OPTION_BOOLEAN, &orbit_use_ipv6 },
	{ "ORBIIOPUSock",       ORBIT_OPTION_BOOLEAN, &orbit_use_usocks },
	{ "ORBIIOPUNIX",        ORBIT_OPTION_BOOLEAN, &orbit_use_usocks },
	{ "ORBIIOPIrDA",        ORBIT_OPTION_BOOLEAN, &orbit_use_irda },
	{ "ORBIIOPSSL",         ORBIT_OPTION_BOOLEAN, &orbit_use_ssl },
	/* warning: this option is a security risk */
	{ "ORBHTTPIORs",        ORBIT_OPTION_BOOLEAN, &orbit_use_http_iors },
	/* warning: this option is a security risk */
	{ "ORBSimpleUIDs",      ORBIT_OPTION_BOOLEAN, &orbit_use_genuid_simple },
	{ "ORBDebugFlags",      ORBIT_OPTION_STRING,  &orbit_debug_options },
	{ "ORBInitRef",         ORBIT_OPTION_KEY_VALUE,  &orbit_initref_list},
	{ "ORBCorbaloc",        ORBIT_OPTION_BOOLEAN, &orbit_use_corbaloc},
	{ "GIOPTimeoutMSEC",    ORBIT_OPTION_ULONG,   &orbit_timeout_msec },
	{ NULL,                 0,                    NULL }
};

#ifdef G_OS_WIN32

/* DllMain function used to store the DLL handle */
static HMODULE hmodule;
G_LOCK_DEFINE_STATIC (mutex);

/* Silence gcc warnings about no prototype */
BOOL WINAPI DllMain (HINSTANCE hinstDLL,
		     DWORD     fdwReason,
		     LPVOID    lpvReserved);
const gchar *ORBit_win32_get_typelib_dir (void);
const gchar *ORBit_win32_get_system_rcfile (void);

BOOL WINAPI
DllMain (HINSTANCE hinstDLL,
	 DWORD     fdwReason,
	 LPVOID    lpvReserved)
{
        switch (fdwReason) {
        case DLL_PROCESS_ATTACH:
                hmodule = hinstDLL;
                break;
        }
        return TRUE;
}

static const char *typelib_dir = NULL;
static const char *system_rcfile = NULL;

static void
setup (void)
{
	gchar *prefix;

	G_LOCK (mutex);
	if (typelib_dir != NULL) {
		G_UNLOCK (mutex);
		return;
	}

	prefix = g_win32_get_package_installation_directory_of_module (hmodule);

	if (prefix == NULL) {
		/* Just to not crash... */
		prefix = g_strdup ("");
	}

	typelib_dir = g_strconcat (prefix,
				   "\\lib\\orbit-2.0",
				   NULL);

	system_rcfile = g_strconcat (prefix,
				     "\\etc\\orbitrc",
				     NULL);
	G_UNLOCK (mutex);

	g_free (prefix);
}

const gchar *
ORBit_win32_get_typelib_dir (void)
{
	setup ();
	return typelib_dir;
}

const gchar *
ORBit_win32_get_system_rcfile (void)
{
	setup ();
	return system_rcfile;
}

#endif
