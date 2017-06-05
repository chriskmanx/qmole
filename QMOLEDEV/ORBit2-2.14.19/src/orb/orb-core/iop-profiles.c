#include <config.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <orbit/orbit.h>

#include "iop-profiles.h"
#include "orb-core-private.h"
#include "../poa/orbit-poa-export.h"
#include "orbit-debug.h"

#undef DEBUG

/* FIXME: this whole module is horribly unneccesary - if we just
   used the wire structures more intelligently we could store
   everything far more sensibly */

static void IOP_profile_free (IOP_Profile_info *p);

#ifdef LINK_SSL_SUPPORT
static IOP_Component_info *
IOP_component_find (GSList *list, IOP_ComponentId type, GSList **pos)
{
  for (; list; list = list->next)
    {
      IOP_Component_info *pi = list->data;
      if(pi->component_type == type)
	return pi;
    }

  return NULL;
}
#endif /* LINK_SSL_SUPPORT */

static gchar *
IOP_ObjectKey_dump (ORBit_ObjectKey *objkey)
{
	int i;
	GString *str = g_string_sized_new (objkey->_length * 2 + 4);

	for (i = 0; i < objkey->_length; i++)
		g_string_append_printf (str, "%02x", objkey->_buffer [i]);

	return g_string_free (str, FALSE);
}

G_GNUC_UNUSED gchar * 
IOP_profile_dump (CORBA_Object obj, gpointer p)
{
	IOP_ProfileId t;
	char         *key = NULL;
	GString      *str = g_string_sized_new (64);

	t = ((IOP_Profile_info *)p)->profile_type;

	switch (t) {
	case IOP_TAG_INTERNET_IOP: {
		IOP_TAG_INTERNET_IOP_info  *iiop = p;

		g_assert (!iiop->object_key);
		
		key = IOP_ObjectKey_dump (obj->object_key);
		g_string_printf (str, "P-IIOP %s:0x%x '%s'",
				 iiop->host, iiop->port, key);
		g_free (key);
 		break;
	}
	
	case IOP_TAG_GENERIC_IOP: {
		IOP_TAG_GENERIC_IOP_info *giop = p;
		
		g_string_printf (str, "P-GIOP %s:%s:%s",
				 giop->proto, giop->service,
				 giop->host);
		break;
	}
	
	case IOP_TAG_ORBIT_SPECIFIC: {
		IOP_TAG_ORBIT_SPECIFIC_info *os = p;
		
		g_assert (!os->object_key);

		key = IOP_ObjectKey_dump (obj->object_key);
		g_string_printf (str, "P-OS %s:0x%x '%s'",
				 os->unix_sock_path, os->ipv6_port,
				 key);

		g_free (key);
		break;
	}
	case IOP_TAG_MULTIPLE_COMPONENTS:
	default:
		g_string_printf (str, "P-<None>");
		break;
	}

	return g_string_free (str, FALSE);
}

/*
 * IOP_profiles_sync_objkey:
 * @profiles: newly demarshalled profile list.
 *
 * This method scans @profiles for object keys and returns one
 * of this object keys. All other keys are freed.
 *
 * This is based on the (potentially) dangerous assumption that
 * no ORB would have two different object keys representing the
 * same object. For this reason the object keys are compared and
 * if they do not match an error is displayed.
 *
 * Return Value: An #ORBit_ObjectKey pointer.
 */
ORBit_ObjectKey*
IOP_profiles_sync_objkey (GSList *profiles)
{
	ORBit_ObjectKey  *objkey;
	IOP_Profile_info *pi;
	GSList           *l;
	gboolean          match;

	objkey = NULL;
	match = TRUE;

	for(l = profiles; l; l = l->next) {
		pi = l->data;

		switch (pi->profile_type) {
		case IOP_TAG_INTERNET_IOP: {
			IOP_TAG_INTERNET_IOP_info *iiopi = 
					(IOP_TAG_INTERNET_IOP_info *)pi;

			if (!objkey)
				objkey = iiopi->object_key;
			else {
				match = IOP_ObjectKey_equal (objkey,
							     iiopi->object_key);
				ORBit_free (iiopi->object_key);
			}

			iiopi->object_key = NULL;
			}
			break;
		case IOP_TAG_ORBIT_SPECIFIC: {
			IOP_TAG_ORBIT_SPECIFIC_info *osi = 
					(IOP_TAG_ORBIT_SPECIFIC_info *)pi;

			if (!objkey)
				objkey = osi->object_key;
			else {
				match = IOP_ObjectKey_equal (objkey,
							     osi->object_key);
				ORBit_free (osi->object_key);
			}

			osi->object_key = NULL;
			}
			break;
		case IOP_TAG_MULTIPLE_COMPONENTS: {
			IOP_TAG_MULTIPLE_COMPONENTS_info *mci = 
					(IOP_TAG_MULTIPLE_COMPONENTS_info *)pi;
			GSList                           *mcl = mci->components;

			for(; mcl; mcl = mcl->next)
				if (((IOP_Component_info *)mcl->data)->component_type == 
						IOP_TAG_COMPLETE_OBJECT_KEY) {
					IOP_TAG_COMPLETE_OBJECT_KEY_info *coki = 
						(IOP_TAG_COMPLETE_OBJECT_KEY_info *)mcl->data;

					if (!objkey)
						objkey = coki->object_key;
					else {
						match = IOP_ObjectKey_equal (
								objkey, coki->object_key);
						ORBit_free (coki->object_key);
					}

					coki->object_key = NULL;
				}
			}
			break;
		default:
			break;
		}

		/*
		 * FIXME:
		 *    We might want to remove this check after a while.
		 */
		if (!match)
			g_warning ("Object Keys in different profiles don't match.\n"
				   "Scream and Shout on orbit-list@gnome\n."
				   "You might want to mention what ORB you're using\n");
	}
	
	return objkey;
}

gboolean
IOP_profile_get_info (CORBA_Object  obj,
		      gpointer     *pinfo,
		      GIOPVersion  *iiop_version,
		      char         **proto,
		      char         **host,
		      char         **service,
		      gboolean      *ssl,
		      char          *tmpbuf)
{
  IOP_TAG_ORBIT_SPECIFIC_info *osi;
  IOP_TAG_INTERNET_IOP_info *iiop;
  IOP_TAG_GENERIC_IOP_info *giop;
  IOP_Profile_info         *pi = (IOP_Profile_info *)pinfo;

  *ssl = FALSE;

#ifdef DEBUG
  {
    char *str;
    fprintf (stderr, "profile for object '%p' '%s'\n",
	     obj, (str = IOP_profile_dump (obj, pi)));
    g_free (str);
  }
#endif

  switch(pi->profile_type)
    {
    case IOP_TAG_INTERNET_IOP:
      iiop = (IOP_TAG_INTERNET_IOP_info *)pi;
      *iiop_version = iiop->iiop_version;
      *proto = "IPv4";
      *host = iiop->host;
      *service = tmpbuf;
      g_snprintf(tmpbuf, 8, "%d", iiop->port);
#ifdef LINK_SSL_SUPPORT
      {
	IOP_TAG_SSL_SEC_TRANS_info *ssli;
	ssli = (IOP_TAG_SSL_SEC_TRANS_info *)
	  IOP_component_find(iiop->components, IOP_TAG_SSL_SEC_TRANS, NULL);
	if(ssli)
	  {
	    g_snprintf(tmpbuf, 8, "%d", ssli->port);
	    *ssl = TRUE;
	  }
      }
#endif
      return TRUE;
      break;
    case IOP_TAG_GENERIC_IOP:
      giop = (IOP_TAG_GENERIC_IOP_info *)pi;
      *iiop_version = giop->iiop_version;
      *proto = giop->proto;
      *host = giop->host;
      *service = giop->service;
#ifdef LINK_SSL_SUPPORT
      {
	IOP_TAG_GENERIC_SSL_SEC_TRANS_info *ssli;
	ssli = (IOP_TAG_GENERIC_SSL_SEC_TRANS_info *)
	  IOP_component_find(giop->components, IOP_TAG_GENERIC_SSL_SEC_TRANS,
			     NULL);
	if(ssli)
	  {
	    *service = ssli->service;
	    *ssl = TRUE;
	  }
      }
#endif
      return TRUE;
      break;
    case IOP_TAG_ORBIT_SPECIFIC:
      /* Due to (a) my brain deadness in putting multiple protocols in
	 one profile in ORBit 0.[3-5].x (b) the inability of this current
	 code to support multiple protocols per profile,
	 this only works to pull out the UNIX socket path OR IPv6. It's not
	 like anyone used IPv6 with old ORBit, anyways - most people
	 just want UNIX sockets. */
      osi = (IOP_TAG_ORBIT_SPECIFIC_info *)pi;
      if(osi->unix_sock_path && *osi->unix_sock_path)
	{
	  *iiop_version = GIOP_1_0;
	  *proto = "UNIX";
	  *host = "";
	  *service = osi->unix_sock_path;
	  return TRUE;
	}
      break;
    default:
      break;
    }

  return FALSE;
}

static IOP_TAG_MULTIPLE_COMPONENTS_info *
IOP_get_mci (GSList *p)
{
  for (; p; p = p->next) {
    if (((IOP_Profile_info *)p->data)->profile_type ==
	IOP_TAG_MULTIPLE_COMPONENTS)
	    return p->data;
  }
  return NULL;
}

gboolean
IOP_ObjectKey_equal (ORBit_ObjectKey *a,
		     ORBit_ObjectKey *b)
{
	if (a->_length != b->_length)
		return FALSE;

	if (memcmp (a->_buffer, b->_buffer, a->_length))
		return FALSE;

	return TRUE;
}

static guint
IOP_mem_hash (gconstpointer key, gulong len)
{
	guint h = 0;
	const char *p, *pend;

	for (p = key, pend = p + len; p < pend; p++)
		h = (h << 5) - h + *p;

	return h;
}

guint
IOP_ObjectKey_hash (ORBit_ObjectKey *k)
{
	return IOP_mem_hash (k->_buffer, k->_length);
}

gboolean
IOP_profile_equal (CORBA_Object obj1, CORBA_Object obj2,
		   gpointer d1, gpointer d2)
{
	IOP_TAG_MULTIPLE_COMPONENTS_info *mci1, *mci2;
	IOP_ProfileId                    t1, t2;

	mci1 = IOP_get_mci(obj1->profile_list);
	mci2 = IOP_get_mci(obj2->profile_list);

	t1 = ((IOP_Profile_info *)d1)->profile_type;
	t2 = ((IOP_Profile_info *)d2)->profile_type;

	if(t1 != t2)
		return FALSE;

	switch (t1) {
	case IOP_TAG_INTERNET_IOP: {
		IOP_TAG_INTERNET_IOP_info  *iiop1 = d1;
		IOP_TAG_INTERNET_IOP_info  *iiop2 = d2;

		g_assert (!iiop1->object_key && !iiop2->object_key);

		if (iiop1->port != iiop2->port)
			return FALSE;

		if (strcmp (iiop1->host, iiop2->host))
			return FALSE;

		/* FIXME, also compare ssl ports */ 

		break;
	}

	case IOP_TAG_GENERIC_IOP: {
		IOP_TAG_GENERIC_IOP_info *giop1 = d1;
		IOP_TAG_GENERIC_IOP_info *giop2 = d2;

		if (!(mci1 || mci2))
			return FALSE;

		if (strcmp (giop1->service, giop2->service))
			return FALSE;
		if (strcmp (giop1->host, giop2->host))
			return FALSE;
		if (strcmp (giop1->proto, giop2->proto))
			return FALSE;

		break;
	}

	case IOP_TAG_ORBIT_SPECIFIC: {
		IOP_TAG_ORBIT_SPECIFIC_info *os1 = d1;
		IOP_TAG_ORBIT_SPECIFIC_info *os2 = d2;

		g_assert (!os1->object_key && !os2->object_key);

		if (os1->ipv6_port != os2->ipv6_port)
			return FALSE;

		if (strcmp (os1->unix_sock_path, os2->unix_sock_path))
			return FALSE;
		break;
	}
	case IOP_TAG_MULTIPLE_COMPONENTS: {
		static int warned = 0;
		if (!(warned++)) /* FIXME: */
			g_warning ("IOP_profile_equal: no multiple "
				   "components support");
		return FALSE;
		break;
	}
	default:
		g_warning ("No IOP_Profile_match for component");
		return FALSE;
		break;
	}

	return TRUE;
}

/*
 * This performs really badly, and is pretty useless.
 */
void
IOP_profile_hash (gpointer item, gpointer data)
{
	IOP_Profile_info *p = item;
	guint *h = data;
	IOP_TAG_INTERNET_IOP_info *iiop;
	IOP_TAG_GENERIC_IOP_info *giop;
	IOP_TAG_ORBIT_SPECIFIC_info *osi;
	IOP_TAG_MULTIPLE_COMPONENTS_info *mci;
	IOP_UnknownProfile_info *upi;

	*h ^= p->profile_type;
	switch (p->profile_type) {
	case IOP_TAG_ORBIT_SPECIFIC:
		osi = item;
		*h ^= g_str_hash(osi->unix_sock_path);
		break;
	case IOP_TAG_INTERNET_IOP:
		iiop = item;
		*h ^= g_str_hash(iiop->host);
		*h ^= iiop->port;
		break;
	case IOP_TAG_GENERIC_IOP:
		giop = item;
		*h ^= g_str_hash(giop->proto);
		*h ^= g_str_hash(giop->host);
		*h ^= g_str_hash(giop->service);
		break;
	case IOP_TAG_MULTIPLE_COMPONENTS:
		mci = item;
		*h ^= g_slist_length(mci->components);
		break;
	default:
		upi = item;
		*h ^= IOP_mem_hash(upi->data._buffer, upi->data._length);
		break;
	}
}

void
IOP_delete_profiles (CORBA_ORB orb,
		     GSList  **profiles)
{
	if (!profiles || !*profiles)
		return;

	if (orb && *profiles == orb->profiles)
		*profiles = NULL;
	else {
		g_slist_foreach (*profiles,
				 (GFunc)IOP_profile_free, NULL);
		g_slist_free (*profiles);
		*profiles = NULL;
	}
}

GSList *
IOP_start_profiles (CORBA_ORB orb)
{
	GSList  *l;
	GSList  *common_profiles = NULL;
	gboolean need_objkey_component = FALSE;
	IOP_TAG_MULTIPLE_COMPONENTS_info *mci = NULL;
	IOP_TAG_ORBIT_SPECIFIC_info      *osi = NULL;
	IOP_TAG_INTERNET_IOP_info        *iiop = NULL;

	for (l = orb->servers ; l != NULL ; l = l->next) {
		LinkServer *serv = l->data;
		gboolean   ipv4, uds, ssl;

		ipv4 = !strcmp (serv->proto->name, "IPv4");
		uds  = !strcmp (serv->proto->name, "UNIX");

		ssl  = (serv->create_options & LINK_CONNECTION_SSL);

		if (!osi && uds) {
			osi = g_new0 (IOP_TAG_ORBIT_SPECIFIC_info, 1);
			osi->parent.profile_type = IOP_TAG_ORBIT_SPECIFIC;
		}

		if (uds && !osi->unix_sock_path)
			osi->unix_sock_path = g_strdup (serv->local_serv_info);

		if (ipv4) {
			if (!iiop) {
				iiop = g_new0 (IOP_TAG_INTERNET_IOP_info, 1);
				iiop->host = g_strdup (serv->local_host_info);
				common_profiles = g_slist_append (common_profiles, iiop);
			}

			if (ssl) {
				IOP_TAG_SSL_SEC_TRANS_info *sslsec;

				sslsec = g_new0 (IOP_TAG_SSL_SEC_TRANS_info, 1);
				sslsec->parent.component_type = IOP_TAG_SSL_SEC_TRANS;
				/* integrity & confidentiality */
				sslsec->target_supports = sslsec->target_requires = 2|4;
				sslsec->port = atoi (serv->local_serv_info);
				iiop->components = g_slist_append (
					iiop->components, sslsec);
			} else {
				g_assert (!iiop->port);
				iiop->port = atoi (serv->local_serv_info);
				iiop->iiop_version = orb->default_giop_version;
			}
		} else {
			GSList *l2;
			IOP_TAG_GENERIC_IOP_info *giop;

			for (giop = NULL, l2 = common_profiles; l2; l2 = l2->next) {
				IOP_TAG_GENERIC_IOP_info *giopt;

				giopt = l2->data;
				if (giopt->parent.profile_type == IOP_TAG_GENERIC_IOP &&
				   !strcmp (giopt->proto, serv->proto->name)) {
					giop = giopt;
					break;
				}
			}

			if (!giop) {
				giop = g_new0 (IOP_TAG_GENERIC_IOP_info, 1);
				giop->parent.profile_type = IOP_TAG_GENERIC_IOP;
				giop->iiop_version = orb->default_giop_version;
				giop->proto = g_strdup (serv->proto->name);
				giop->host = g_strdup (serv->local_host_info);
				common_profiles = g_slist_append (common_profiles, giop);
			}

			if (ssl) {
				IOP_TAG_GENERIC_SSL_SEC_TRANS_info *sslsec;

				sslsec = g_new0 (IOP_TAG_GENERIC_SSL_SEC_TRANS_info, 1);
				sslsec->parent.component_type = IOP_TAG_GENERIC_SSL_SEC_TRANS;
				sslsec->service = g_strdup (serv->local_serv_info);
				giop->components = g_slist_append (giop->components, sslsec);
			} else {
				g_assert (!giop->service);
				giop->service = g_strdup (serv->local_serv_info);
			}
		}
		need_objkey_component = TRUE;
	}

	if (osi)
		common_profiles = g_slist_append (common_profiles, osi);

	/* We always create this to marshal the TAG_CODE_SET component */
	mci = g_new0 (IOP_TAG_MULTIPLE_COMPONENTS_info, 1);
	mci->parent.profile_type = IOP_TAG_MULTIPLE_COMPONENTS;

	if (need_objkey_component) {
		IOP_TAG_COMPLETE_OBJECT_KEY_info *coki;

		coki = g_new0 (IOP_TAG_COMPLETE_OBJECT_KEY_info, 1);
		coki->parent.component_type = IOP_TAG_COMPLETE_OBJECT_KEY;
		mci->components = g_slist_append (mci->components, coki);
	}

	{
		IOP_TAG_CODE_SETS_info *csets;

		csets = g_new0 (IOP_TAG_CODE_SETS_info, 1);
		csets->parent.component_type = IOP_TAG_CODE_SETS;

		csets->data.ForCharData.native_code_set  = IOP_PROFILES_CODE_SET_UTF8;
		csets->data.ForWcharData.native_code_set = IOP_PROFILES_CODE_SET_UTF16;

		mci->components = g_slist_append (mci->components, csets);
	}

	return g_slist_append (common_profiles, mci);
}

void
IOP_shutdown_profiles (GSList *profiles)
{
	g_slist_foreach (profiles, (GFunc)IOP_profile_free, NULL);
	g_slist_free (profiles);
}

void
IOP_generate_profiles (CORBA_Object obj)
{
	CORBA_ORB orb;
	ORBit_OAObject adaptor_obj;

	g_assert (obj && (obj->profile_list == NULL) && obj->orb);

	orb = obj->orb;
	adaptor_obj = obj->adaptor_obj;

	/*
	 * no need to have any listening sockets until now.
	 * if the ORB has been shutdown and restarted,
	 * the profiles must be regenerated.
	 */
	if (!orb->servers)
		ORBit_ORB_start_servers (orb);

	if (!obj->object_key && adaptor_obj)
		obj->object_key = ORBit_OAObject_object_to_objkey (adaptor_obj);

	obj->profile_list = orb->profiles;
}

static void
IOP_component_free (IOP_Component_info *c)
{
	switch (c->component_type) {
	case IOP_TAG_GENERIC_SSL_SEC_TRANS:
		g_free (((IOP_TAG_GENERIC_SSL_SEC_TRANS_info *)c)->service);
		break;
	case IOP_TAG_COMPLETE_OBJECT_KEY: {
		IOP_TAG_COMPLETE_OBJECT_KEY_info *coki = 
			(IOP_TAG_COMPLETE_OBJECT_KEY_info *)c;

		if (coki->object_key)
			ORBit_free_T (coki->object_key);
		coki->object_key = NULL;

		break;
		}
	case IOP_TAG_SSL_SEC_TRANS:
		break;
	case IOP_TAG_CODE_SETS: {
		IOP_TAG_CODE_SETS_info  *csets = (IOP_TAG_CODE_SETS_info*)c;
		CORBA_sequence_CONV_FRAME_CodeSetId *conv_codesets;
		
		conv_codesets 
			= &(csets->data.ForCharData.conversion_code_sets);
		if (conv_codesets->_buffer) 
			ORBit_free_T (conv_codesets->_buffer);

		conv_codesets 
			= &(csets->data.ForWcharData.conversion_code_sets);
		if (conv_codesets->_buffer) 
			ORBit_free_T (conv_codesets->_buffer);
                break;
	        }

	default:
		g_free (((IOP_UnknownProfile_info*)c)->data._buffer);
		break;
	}

	g_free(c);
}

static void
IOP_components_free (GSList **components)
{
	g_slist_foreach (*components, (GFunc)IOP_component_free, NULL);
	g_slist_free (*components);
	*components = NULL;
}

static void
IOP_TAG_MULTIPLE_COMPONENTS_free (IOP_Profile_info *p)
{
	IOP_TAG_MULTIPLE_COMPONENTS_info *info = (IOP_TAG_MULTIPLE_COMPONENTS_info *)p;

	IOP_components_free (&info->components);
}

static void
IOP_TAG_INTERNET_IOP_free (IOP_Profile_info *p)
{
	IOP_TAG_INTERNET_IOP_info *info = (IOP_TAG_INTERNET_IOP_info *)p;

	IOP_components_free (&info->components);
	g_free (info->host);

	if (info->object_key)
		ORBit_free_T (info->object_key);
	info->object_key = NULL;
}

static void
IOP_TAG_GENERIC_IOP_free (IOP_Profile_info *p)
{
	IOP_TAG_GENERIC_IOP_info *info = (IOP_TAG_GENERIC_IOP_info *)p;

	IOP_components_free (&info->components);
	g_free (info->proto);
	g_free (info->host);
	g_free (info->service);
}

static void
IOP_TAG_ORBIT_SPECIFIC_free (IOP_Profile_info *p)
{
	IOP_TAG_ORBIT_SPECIFIC_info *info = (IOP_TAG_ORBIT_SPECIFIC_info *)p;

	g_free (info->unix_sock_path);

	if (info->object_key)
		ORBit_free_T (info->object_key);
	info->object_key = NULL;
}

static void
IOP_UnknownProfile_free (IOP_Profile_info *p)
{
	IOP_UnknownProfile_info *info = (IOP_UnknownProfile_info *)p;

	g_free (info->data._buffer);
}

static void
IOP_profile_free (IOP_Profile_info *p)
{
	switch (p->profile_type) {
	case IOP_TAG_INTERNET_IOP:
		IOP_TAG_INTERNET_IOP_free (p);
		break;
	case IOP_TAG_MULTIPLE_COMPONENTS:
		IOP_TAG_MULTIPLE_COMPONENTS_free (p);
		break;
	case IOP_TAG_GENERIC_IOP:
		IOP_TAG_GENERIC_IOP_free (p);
		break;
	case IOP_TAG_ORBIT_SPECIFIC:
		IOP_TAG_ORBIT_SPECIFIC_free (p);
		break;
	default:
		IOP_UnknownProfile_free (p);
		break;
	}

	g_free (p);
}

static void
IOP_ObjectKey_marshal (CORBA_Object                obj,
		       GIOPSendBuffer             *buf,
		       ORBit_ObjectKey            *objkey)
{
	giop_send_buffer_append_aligned (buf, &objkey->_length, 4);

	giop_send_buffer_append (buf, objkey->_buffer, objkey->_length);
}

static void
IOP_TAG_GENERIC_SSL_SEC_TRANS_marshal (CORBA_Object        obj,
				       GIOPSendBuffer     *buf,
				       IOP_Component_info *ci)
{
	IOP_TAG_GENERIC_SSL_SEC_TRANS_info *ssli;
	
	ssli = (IOP_TAG_GENERIC_SSL_SEC_TRANS_info *)ci;

	giop_send_buffer_append_string (buf, ssli->service);
}

static void
IOP_TAG_SSL_SEC_TRANS_marshal(CORBA_Object        obj,
			      GIOPSendBuffer     *buf,
			      IOP_Component_info *ci)
{
	IOP_TAG_SSL_SEC_TRANS_info *ssli = (IOP_TAG_SSL_SEC_TRANS_info *) ci;

	giop_send_buffer_align (buf, 4);
	giop_send_buffer_append (buf, &ssli->target_supports, 10);
}

static void
IOP_TAG_COMPLETE_OBJECT_KEY_marshal (CORBA_Object       obj,
				     GIOPSendBuffer    *buf,
				    IOP_Component_info *ci)
{
	IOP_ObjectKey_marshal (obj, buf, obj->object_key);
}

static void
CodeSetComponent_marshal (GIOPSendBuffer *buf,
			  CORBA_unsigned_long native_code_set,
			  CORBA_sequence_CORBA_unsigned_long *opt_conversion_code_sets)
{
	/* native_code_set */
	giop_send_buffer_append_aligned (buf, &native_code_set, 4);

	if (opt_conversion_code_sets && opt_conversion_code_sets->_buffer) {
		CORBA_unsigned_long length = opt_conversion_code_sets->_length;
		giop_send_buffer_append_aligned (buf, &length, 4);
		giop_send_buffer_append (buf,
					 opt_conversion_code_sets->_buffer,
					 length * sizeof(CORBA_unsigned_long));
	} else {
		CORBA_unsigned_long length = 0;
		giop_send_buffer_append_aligned (buf, &length, 4);
	}	
}

/* we always marshal the same thing: see 13.7.2.4 */
static void
IOP_TAG_CODE_SETS_marshal(CORBA_Object obj, GIOPSendBuffer *buf,
			  IOP_Component_info *ci)
{
	/* To get these magic numbers see the 'OSF Character
	   and Codeset Registry'; ftp.opengroup.org/pub/code_set_registry */
	/* CORBA_unsigned_long utf8_key  = 0x05010001; */
	/* CORBA_unsigned_long utf16_key = 0x00010109; */
	IOP_TAG_CODE_SETS_info *csets = (IOP_TAG_CODE_SETS_info*) ci;

	/* Marshal a CodeSetComponentInfo structure */
	CodeSetComponent_marshal (buf,
				  csets->data.ForCharData.native_code_set,
				  &(csets->data.ForCharData.conversion_code_sets));
	CodeSetComponent_marshal (buf,
				  csets->data.ForWcharData.native_code_set,
				  &(csets->data.ForWcharData.conversion_code_sets));
}

static void
IOP_UnknownComponent_marshal (CORBA_Object        obj,
			      GIOPSendBuffer     *buf, 
			      IOP_Component_info *ci)
{
	IOP_UnknownComponent_info *uci = (IOP_UnknownComponent_info *) ci;

	giop_send_buffer_append (buf, &uci->data._length, 4);
	giop_send_buffer_append (buf, uci->data._buffer, uci->data._length);
}

static void
IOP_components_marshal (CORBA_Object    obj, 
			GIOPSendBuffer *buf,
			GSList         *components)
{
	CORBA_unsigned_long  len;
	GSList              *cur;
	guchar              *marker;

	len = g_slist_length (components);
	giop_send_buffer_append_aligned (buf, &len, 4);

	for (cur = components; cur; cur = cur->next) {
		IOP_Component_info *ci = cur->data;

		giop_send_buffer_align (buf, 4);
		giop_send_buffer_append (buf, &ci->component_type, 4);

		switch (ci->component_type) {
		case IOP_TAG_GENERIC_SSL_SEC_TRANS:
		case IOP_TAG_SSL_SEC_TRANS:
		case IOP_TAG_CODE_SETS:
			marker = giop_send_buffer_append_aligned (buf, &len, 4);
			len = buf->msg.header.message_size;
			giop_send_buffer_append (buf, &buf->msg.header.flags, 1);
			break;
		default:
			marker = NULL;
			break;
		}

		switch (ci->component_type) {
		case IOP_TAG_GENERIC_SSL_SEC_TRANS:
			IOP_TAG_GENERIC_SSL_SEC_TRANS_marshal (obj, buf, ci);
			break;
		case IOP_TAG_SSL_SEC_TRANS:
			IOP_TAG_SSL_SEC_TRANS_marshal (obj, buf, ci);
			break;
		case IOP_TAG_COMPLETE_OBJECT_KEY:
			IOP_TAG_COMPLETE_OBJECT_KEY_marshal (obj, buf, ci);
			break;
		case IOP_TAG_CODE_SETS:
			IOP_TAG_CODE_SETS_marshal (obj, buf, ci);
			break;
		default:
			IOP_UnknownComponent_marshal (obj, buf, ci);
			break;
		}

		if (marker) {
			len = buf->msg.header.message_size - len;
			memcpy (marker, &len, 4);
		}
	}
}

static void
IOP_TAG_INTERNET_IOP_marshal (CORBA_Object      obj,
			      GIOPSendBuffer   *buf, 
			      IOP_Profile_info *profile)
{
	IOP_TAG_INTERNET_IOP_info *iiop;

	iiop = (IOP_TAG_INTERNET_IOP_info *) profile;

	giop_send_buffer_append (
		buf, giop_version_ids [iiop->iiop_version], 2);

	giop_send_buffer_append_string (buf, iiop->host);

	giop_send_buffer_align (buf, 2);
	giop_send_buffer_append (buf, &iiop->port, 2);

	IOP_ObjectKey_marshal (obj, buf, obj->object_key);

	IOP_components_marshal (obj, buf, iiop->components);
}

static void
IOP_TAG_GENERIC_IOP_marshal (CORBA_Object     obj,
			     GIOPSendBuffer   *buf, 
			     IOP_Profile_info *profile)
{
	IOP_TAG_GENERIC_IOP_info *giop = (IOP_TAG_GENERIC_IOP_info *) profile;

	giop_send_buffer_append (buf, giop_version_ids [giop->iiop_version], 2);

	giop_send_buffer_append_string (buf, giop->proto);
	giop_send_buffer_append_string (buf, giop->host);
	giop_send_buffer_append_string (buf, giop->service);

	IOP_components_marshal (obj, buf, giop->components);
}

static void
IOP_TAG_MULTIPLE_COMPONENTS_marshal (CORBA_Object      obj,
				     GIOPSendBuffer   *buf,
				     IOP_Profile_info *profile)
{
	IOP_TAG_MULTIPLE_COMPONENTS_info *mci;

	mci = (IOP_TAG_MULTIPLE_COMPONENTS_info*) profile;

	IOP_components_marshal(obj, buf, mci->components);
}

static void
IOP_TAG_ORBIT_SPECIFIC_marshal (CORBA_Object      obj,
				GIOPSendBuffer   *buf,
				IOP_Profile_info *profile)
{
	IOP_TAG_ORBIT_SPECIFIC_info *osi;
	guchar compat_version[2] = { 1, 2 }; /* for ORBit1 */

	osi = (IOP_TAG_ORBIT_SPECIFIC_info*) profile;

	giop_send_buffer_append (buf, compat_version, 2);

	giop_send_buffer_append_string (buf, osi->unix_sock_path);

	giop_send_buffer_align (buf, 2);
	giop_send_buffer_append (buf, &osi->ipv6_port, 2);

	IOP_ObjectKey_marshal (obj, buf, obj->object_key);
}

static void
IOP_UnknownProfile_marshal(CORBA_Object obj, GIOPSendBuffer *buf, 
			   IOP_Profile_info *pi)
{
	IOP_UnknownProfile_info *upi = (IOP_UnknownProfile_info *) pi;

	giop_send_buffer_append (
		buf, upi->data._buffer, upi->data._length);
}

void
IOP_profile_marshal (CORBA_Object obj, GIOPSendBuffer *buf, gpointer *p)
{
	IOP_Profile_info    *profile = (IOP_Profile_info *)p;
	CORBA_unsigned_long  seqlen, msgsz;
	guchar              *marker;

	giop_send_buffer_append_aligned (buf, &profile->profile_type, 4);
	marker = giop_send_buffer_append_aligned (buf, NULL, 4);

	msgsz = buf->msg.header.message_size;

	switch (profile->profile_type) {
	case IOP_TAG_INTERNET_IOP:
		giop_send_buffer_append (buf, &buf->msg.header.flags, 1);
		IOP_TAG_INTERNET_IOP_marshal (obj, buf, profile);
		break;
	case IOP_TAG_ORBIT_SPECIFIC:
		giop_send_buffer_append (buf, &buf->msg.header.flags, 1);
		IOP_TAG_ORBIT_SPECIFIC_marshal (obj, buf, profile);
		break;
	case IOP_TAG_GENERIC_IOP:
		giop_send_buffer_append (buf, &buf->msg.header.flags, 1);
		IOP_TAG_GENERIC_IOP_marshal (obj, buf, profile);
		break;
	case IOP_TAG_MULTIPLE_COMPONENTS:
		giop_send_buffer_append (buf, &buf->msg.header.flags, 1);
		IOP_TAG_MULTIPLE_COMPONENTS_marshal (obj, buf, profile);
		break;
	default:
		IOP_UnknownProfile_marshal (obj, buf, profile);
		break;
	}

	seqlen = buf->msg.header.message_size - msgsz;
	memcpy (marker, &seqlen, 4);
}

/*
 * demarshalling routines.
 */

static ORBit_ObjectKey*
IOP_ObjectKey_demarshal (GIOPRecvBuffer *buf)
{
	ORBit_ObjectKey     *objkey;
	CORBA_unsigned_long  len;

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		return NULL;

	len = *(CORBA_unsigned_long *)buf->cur;

	if (giop_msg_conversion_needed (buf))
		len = GUINT32_SWAP_LE_BE (len);

	buf->cur += 4;

	if ((buf->cur + len) > buf->end ||
	    (buf->cur + len) < buf->cur)
		return NULL;

	objkey           = CORBA_sequence_CORBA_octet__alloc ();
	objkey->_length  = objkey->_maximum = len;
	objkey->_buffer  = CORBA_sequence_CORBA_octet_allocbuf (objkey->_length);
	objkey->_release = CORBA_TRUE;

	memcpy (objkey->_buffer, buf->cur, len);

	buf->cur += len;

	return objkey;
}

ORBit_ObjectKey *
IOP_ObjectKey_copy (ORBit_ObjectKey *src)
{
	ORBit_ObjectKey *objkey;

	if (!src)
		return NULL;

	objkey           = CORBA_sequence_CORBA_octet__alloc ();
	objkey->_length  = objkey->_maximum = src->_length;
	objkey->_buffer  = CORBA_sequence_CORBA_octet_allocbuf (src->_length);
	objkey->_release = CORBA_TRUE;

	memcpy (objkey->_buffer, src->_buffer, src->_length);

	return objkey;
}

static IOP_Component_info *
IOP_TAG_SSL_SEC_TRANS_demarshal(IOP_ComponentId id, GIOPRecvBuffer *buf)
{
  IOP_TAG_SSL_SEC_TRANS_info *retval = NULL;
  GIOPRecvBuffer *sub;

  sub = giop_recv_buffer_use_encaps_buf(buf);
  if(!sub)
    return NULL;

  sub->cur = ALIGN_ADDRESS(sub->cur, 4);

  if((sub->cur + 10) > sub->end)
    {
      giop_recv_buffer_unuse(sub);
      return NULL;
    }
  retval = g_new(IOP_TAG_SSL_SEC_TRANS_info, 1);
  retval->parent.component_type = id;

  retval->target_supports = *(CORBA_unsigned_long *)sub->cur;
  if(giop_msg_conversion_needed(buf))
    retval->target_supports = GUINT32_SWAP_LE_BE(retval->target_supports);
  sub->cur += 4;
  retval->target_requires = *(CORBA_unsigned_long *)sub->cur;
  if(giop_msg_conversion_needed(buf))
    retval->target_requires = GUINT32_SWAP_LE_BE(retval->target_requires);
  sub->cur += 4;
  retval->port = *(CORBA_unsigned_short *)sub->cur;
  if(giop_msg_conversion_needed(buf))
    retval->port = GUINT16_SWAP_LE_BE(retval->port);
  sub->cur += 2;

  giop_recv_buffer_unuse(sub);

  return (IOP_Component_info *)retval;
}

static IOP_Component_info *
IOP_TAG_GENERIC_SSL_SEC_TRANS_demarshal(IOP_ComponentId id, GIOPRecvBuffer *buf)
{
  IOP_TAG_GENERIC_SSL_SEC_TRANS_info *retval = NULL;
  GIOPRecvBuffer *sub;
  CORBA_unsigned_long len;

  sub = giop_recv_buffer_use_encaps_buf(buf);
  if(!sub)
    return NULL;

  sub->cur = ALIGN_ADDRESS(sub->cur, 4);
  if((sub->cur + 4) > sub->end)
    goto errout;
  len = *(CORBA_unsigned_long *)sub->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  sub->cur += 4;
  if((sub->cur + len) > sub->end
     || (sub->cur + len) < sub->cur)
    goto errout;
  retval = g_new(IOP_TAG_GENERIC_SSL_SEC_TRANS_info, 1);
  retval->parent.component_type = id;
  retval->service = g_memdup(sub->cur, len);

  giop_recv_buffer_unuse(sub);
  return (IOP_Component_info *)retval;

 errout:
  g_free(retval);
  giop_recv_buffer_unuse(sub);
  return NULL;
}

static IOP_Component_info *
IOP_TAG_COMPLETE_OBJECT_KEY_demarshal (IOP_ComponentId id,
				       GIOPRecvBuffer *buf)
{
	IOP_TAG_COMPLETE_OBJECT_KEY_info *retval;
	ORBit_ObjectKey                  *objkey;
  
	objkey = IOP_ObjectKey_demarshal (buf);
	if (!objkey)
		return NULL;

	retval = g_new (IOP_TAG_COMPLETE_OBJECT_KEY_info, 1);
	retval->parent.component_type = id;
	retval->object_key            = objkey;

	return (IOP_Component_info *)retval;
}

static gboolean
CodeSetComponent_demarshal (GIOPRecvBuffer *buf,
			    CONV_FRAME_CodeSetComponent *component)
{
	CORBA_unsigned_long sequence_length;

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);

	if (buf->cur + 8 > buf->end)
		return FALSE;

	component->native_code_set = *(CORBA_unsigned_long *)buf->cur;
	if (giop_msg_conversion_needed (buf))
		component->native_code_set 
			= GUINT32_SWAP_LE_BE (component->native_code_set);
	buf->cur += 4;

	sequence_length = *(CORBA_unsigned_long *)buf->cur;
	if (giop_msg_conversion_needed (buf))
		sequence_length = GUINT32_SWAP_LE_BE (sequence_length);
	buf->cur += 4;

       if (buf->cur + sequence_length * 4 > buf->end)
               return FALSE;

	if (sequence_length > 0) {
		int i;
		dprintf (OBJECTS, "Ignoring incoming code_sets component");
		component->conversion_code_sets._maximum = sequence_length;
		component->conversion_code_sets._length  = sequence_length;
		component->conversion_code_sets._release = TRUE;
		component->conversion_code_sets._buffer  =
			CORBA_sequence_CORBA_unsigned_long_allocbuf (sequence_length);
		for (i=0; i<sequence_length; ++i) {
			component->conversion_code_sets._buffer [i] = 
				*(CORBA_unsigned_long *)buf->cur;
			if (giop_msg_conversion_needed (buf))
				component->conversion_code_sets._buffer [i] 
					= GUINT32_SWAP_LE_BE (component->conversion_code_sets._buffer [i]);
			buf->cur += 4;
		}
	}
	
	return TRUE;
}

static IOP_Component_info *
IOP_TAG_CODE_SETS_demarshal (IOP_ComponentId  id,
			     GIOPRecvBuffer  *buf)
{
	IOP_TAG_CODE_SETS_info *retval;
	GIOPRecvBuffer         *encaps;

	if (!(encaps = giop_recv_buffer_use_encaps_buf (buf)))
		return NULL;
  
	retval = g_new0 (IOP_TAG_CODE_SETS_info, 1);
	retval->parent.component_type = id;

	/* We don't care about the data much */
	if (!CodeSetComponent_demarshal (encaps, &(retval->data.ForCharData)) ||
	    !CodeSetComponent_demarshal (encaps, &(retval->data.ForWcharData))) {
		giop_recv_buffer_unuse (encaps);
		if (retval->data.ForCharData.conversion_code_sets._buffer) 
			ORBit_free_T (retval->data.ForCharData.conversion_code_sets._buffer);
		if (retval->data.ForWcharData.conversion_code_sets._buffer) 
			ORBit_free_T (retval->data.ForWcharData.conversion_code_sets._buffer);
		g_free (retval);
		return NULL;
	}

	giop_recv_buffer_unuse (encaps);

	return (IOP_Component_info *) retval;
}

static IOP_Component_info *
IOP_UnknownComponent_demarshal(IOP_ComponentId p, GIOPRecvBuffer *buf)
{
  IOP_UnknownComponent_info *retval;
  CORBA_unsigned_long len;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    return NULL;
  len = *(CORBA_unsigned_long *)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    return NULL;
  retval = g_new(IOP_UnknownComponent_info, 1);
  retval->parent.component_type = p;
  retval->data._length = len;
  retval->data._buffer = g_memdup(buf->cur, len);
  retval->data._release = CORBA_FALSE; /* We free this manually */
  buf->cur += len;
  
  return (IOP_Component_info *)retval;
}

static gboolean
IOP_components_demarshal (GIOPRecvBuffer *buf, GSList **components)
{
	GSList             *retval;
	IOP_ComponentId     cid;
	CORBA_unsigned_long len, i;

	*components = retval = NULL;
	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		return TRUE;
	len = *(CORBA_unsigned_long*) buf->cur;
	if (giop_msg_conversion_needed (buf))
		len = GUINT32_SWAP_LE_BE (len);
	buf->cur += 4;

	for (i = 0; i < len; i++) {
		IOP_Component_info *c;

		buf->cur = ALIGN_ADDRESS (buf->cur, 4);
		if ((buf->cur + 4) > buf->end)
			goto errout;
		cid = *(CORBA_unsigned_long *) buf->cur;
		if (giop_msg_conversion_needed (buf))
			cid = GUINT32_SWAP_LE_BE (cid);
		buf->cur += 4;

		switch (cid) {
		case IOP_TAG_COMPLETE_OBJECT_KEY:
			c = IOP_TAG_COMPLETE_OBJECT_KEY_demarshal (cid, buf);
			break;
		case IOP_TAG_GENERIC_SSL_SEC_TRANS:
			c = IOP_TAG_GENERIC_SSL_SEC_TRANS_demarshal (cid, buf);
			break;
		case IOP_TAG_SSL_SEC_TRANS:
			c = IOP_TAG_SSL_SEC_TRANS_demarshal (cid, buf);
			break;
		case IOP_TAG_CODE_SETS:
			c = IOP_TAG_CODE_SETS_demarshal (cid, buf);
			break;
		default:
			c = IOP_UnknownComponent_demarshal (cid, buf);
			break;
		}
		if (c)
			retval = g_slist_append (retval, c);
		else
			goto errout;
	}

	*components = retval;
	return FALSE;

 errout:
	IOP_components_free (&retval);
	return TRUE;
}

static IOP_Profile_info *
IOP_UnknownProfile_demarshal(IOP_ProfileId p, GIOPRecvBuffer *buf,
			     CORBA_ORB orb)
{
  IOP_UnknownProfile_info *retval;
  CORBA_unsigned_long len;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    goto errout;
  len = *(CORBA_unsigned_long*)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    goto errout;
  retval = g_new(IOP_UnknownProfile_info, 1);
  retval->parent.profile_type = p;
  retval->data._length = len;
  retval->data._buffer = g_memdup(buf->cur, len);
  retval->data._release = CORBA_FALSE; /* We free this manually */
  buf->cur += len;
  
  return (IOP_Profile_info *)retval;

 errout:
  return NULL;
}

static IOP_Profile_info *
IOP_TAG_ORBIT_SPECIFIC_demarshal (IOP_ProfileId   p,
				  GIOPRecvBuffer *pbuf,
				  CORBA_ORB       orb)
{
	IOP_TAG_ORBIT_SPECIFIC_info *retval = NULL;
	CORBA_unsigned_long          len;
	GIOPRecvBuffer              *buf;
	gboolean                     msg_conversion;

	buf = giop_recv_buffer_use_encaps_buf (pbuf);
	if (!buf)
		goto errout;

	/* Ignore the version info */
	if ((buf->cur + 2) > buf->end)
		return NULL;
	buf->cur += 2;

	msg_conversion = giop_msg_conversion_needed (buf);

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		return NULL;
	len = *(CORBA_unsigned_long *) buf->cur;
	if (msg_conversion)
		len = GUINT32_SWAP_LE_BE (len);
	buf->cur += 4;
	if ((buf->cur + len) > buf->end || (buf->cur + len) < buf->cur)
		goto errout;

	retval = g_new (IOP_TAG_ORBIT_SPECIFIC_info, 1);
	retval->parent.profile_type = p;
	retval->unix_sock_path = g_malloc (len);
	memcpy (retval->unix_sock_path, buf->cur, len);
	buf->cur += len;

	buf->cur = ALIGN_ADDRESS (buf->cur, 2);
	if ((buf->cur + 2) > buf->end)
		goto errout;

	retval->ipv6_port = *(CORBA_unsigned_short *) buf->cur;
	if (msg_conversion)
		retval->ipv6_port = GUINT16_SWAP_LE_BE (retval->ipv6_port);
	buf->cur += 2;

	retval->object_key = IOP_ObjectKey_demarshal (buf);
	if (!retval->object_key)
		goto errout;

	giop_recv_buffer_unuse (buf);

	return (IOP_Profile_info *) retval;

 errout:
	if (retval) {
		ORBit_free (retval->object_key);
		g_free (retval->unix_sock_path);
		g_free (retval);
	}
	giop_recv_buffer_unuse (buf);

	return NULL;
}

static IOP_Profile_info *
IOP_TAG_MULTIPLE_COMPONENTS_demarshal (IOP_ProfileId   p,
				       GIOPRecvBuffer *pbuf,
				       CORBA_ORB       orb)
{
	IOP_TAG_MULTIPLE_COMPONENTS_info *retval = NULL;
	GIOPRecvBuffer *buf;
	GSList         *components;

	buf = giop_recv_buffer_use_encaps_buf (pbuf);

	if (buf && !IOP_components_demarshal (buf, &components)) {
		retval = g_new (IOP_TAG_MULTIPLE_COMPONENTS_info, 1);
		retval->parent.profile_type = p;
		retval->components = components;
	}
	giop_recv_buffer_unuse (buf);

	return (IOP_Profile_info*) retval;
}

static IOP_Profile_info *
IOP_TAG_GENERIC_IOP_demarshal(IOP_ProfileId p, GIOPRecvBuffer *pbuf,
			      CORBA_ORB orb)
{
  IOP_TAG_GENERIC_IOP_info *retval;
  CORBA_octet v1, v2;
  GIOPVersion version;
  CORBA_unsigned_long len;
  GIOPRecvBuffer *buf;

  buf = giop_recv_buffer_use_encaps_buf(pbuf);
  if(!buf)
    goto eo2;

  if((buf->cur + 2) > buf->end)
    goto eo2;
  v1 = *(buf->cur++);
  v2 = *(buf->cur++);
  switch(v1)
    {
    case 1:
      switch(v2)
	{
	case 0:
	  version = GIOP_1_0;
	  break;
	case 1:
	  version = GIOP_1_1;
	  break;
	case 2:
	  version = GIOP_1_2;
	  break;
	default:
	  goto eo2;
	  break;
	}
      break;
    default:
      goto eo2;
      break;
    }
  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    goto eo2;
  len = *(CORBA_unsigned_long *)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;

  retval = g_new0(IOP_TAG_GENERIC_IOP_info, 1);
  retval->parent.profile_type = IOP_TAG_GENERIC_IOP;
  retval->iiop_version = version;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    goto errout;
  retval->proto = g_memdup(buf->cur, len);
  buf->cur += len;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    goto errout;
  len = *(CORBA_unsigned_long *)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    goto errout;
  retval->host = g_memdup(buf->cur, len);
  buf->cur += len;

  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    goto errout;
  len = *(CORBA_unsigned_long *)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    goto errout;
  retval->service = g_memdup(buf->cur, len);
  buf->cur += len;

  if(IOP_components_demarshal(buf, &retval->components))
    goto errout;

  giop_recv_buffer_unuse(buf);
  return (IOP_Profile_info *)retval;

 errout:
  if(retval)
    {
      IOP_components_free (&retval->components);
      g_free(retval->proto);
      g_free(retval->host);
      g_free(retval->service);
    }

  g_free(retval);
 eo2:
  giop_recv_buffer_unuse(buf);
  return NULL;
}

static IOP_Profile_info *
IOP_TAG_INTERNET_IOP_demarshal(IOP_ProfileId p, GIOPRecvBuffer *pbuf,
			       CORBA_ORB orb)
{
  IOP_TAG_INTERNET_IOP_info *retval;
  CORBA_octet v1, v2;
  GIOPVersion version;
  CORBA_unsigned_long len;
  GIOPRecvBuffer *buf;

  buf = giop_recv_buffer_use_encaps_buf(pbuf);
  if(!buf)
    goto eo2;

  if((buf->cur + 2) > buf->end)
    goto eo2;
  v1 = *(buf->cur++);
  v2 = *(buf->cur++);
  switch(v1)
    {
    case 1:
      switch(v2)
	{
	case 0:
	  version = GIOP_1_0;
	  break;
	case 1:
	  version = GIOP_1_1;
	  break;
	case 2:
	  version = GIOP_1_2;
	  break;
	default:
	  goto eo2;
	  break;
	}
      break;
    default:
      goto eo2;
      break;
    }
  buf->cur = ALIGN_ADDRESS(buf->cur, 4);
  if((buf->cur + 4) > buf->end)
    goto eo2;
  len = *(CORBA_unsigned_long *)buf->cur;
  if(giop_msg_conversion_needed(buf))
    len = GUINT32_SWAP_LE_BE(len);
  buf->cur += 4;

  retval = g_new0(IOP_TAG_INTERNET_IOP_info, 1);
  retval->parent.profile_type = IOP_TAG_INTERNET_IOP;
  retval->iiop_version = version;
  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    goto errout;
  retval->host = g_memdup(buf->cur, len);
  buf->cur += len;
  buf->cur = ALIGN_ADDRESS(buf->cur, 2);
  if((buf->cur + 2) > buf->end)
    goto errout;
  if(giop_msg_conversion_needed(buf))
    retval->port = GUINT16_SWAP_LE_BE(*(CORBA_unsigned_short *)buf->cur);
  else
    retval->port = *(CORBA_unsigned_short *)buf->cur;
  buf->cur += 2;
  retval->object_key = IOP_ObjectKey_demarshal(buf);
  if(!retval->object_key)
    goto errout;
  if(version > GIOP_1_0)
    {
      if(IOP_components_demarshal(buf, &retval->components))
	goto errout;
    }
  else
    retval->components = NULL;

  giop_recv_buffer_unuse(buf);
  return (IOP_Profile_info *)retval;

 errout:
  if(retval)
    {
      IOP_components_free (&retval->components);
      g_free (retval->host);
      ORBit_free (retval->object_key);

      g_free (retval);
    }

  eo2:
  giop_recv_buffer_unuse(buf);
  return NULL;
}

static IOP_Profile_info *
IOP_profile_demarshal (GIOPRecvBuffer *buf, CORBA_ORB orb)
{
	IOP_ProfileId p;
	IOP_Profile_info *retval;

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);

	if ((buf->cur + 4) > buf->end)
		return NULL;

	p = *(CORBA_unsigned_long *) buf->cur;
	if (giop_msg_conversion_needed (buf))
		p = GUINT32_SWAP_LE_BE (p);
	buf->cur += 4;

	switch (p) {
	case IOP_TAG_INTERNET_IOP:
		retval = IOP_TAG_INTERNET_IOP_demarshal (p, buf, orb);
		break;
	case IOP_TAG_MULTIPLE_COMPONENTS:
		retval = IOP_TAG_MULTIPLE_COMPONENTS_demarshal (p, buf, orb);
		break;
	case IOP_TAG_GENERIC_IOP:
		retval = IOP_TAG_GENERIC_IOP_demarshal (p, buf, orb);
		break;
	case IOP_TAG_ORBIT_SPECIFIC:
		retval = IOP_TAG_ORBIT_SPECIFIC_demarshal (p, buf, orb);
		break;
	default:
		retval = IOP_UnknownProfile_demarshal (p, buf, orb);
		break;
	}

	return retval;
}

gboolean
ORBit_demarshal_IOR (CORBA_ORB        orb,
		     GIOPRecvBuffer  *buf,
		     gchar          **ret_type_id,
		     GSList         **ret_profiles)
{
	CORBA_unsigned_long  num_profiles;
	CORBA_unsigned_long  len;
	GSList              *profiles = NULL;
	gchar               *type_id;
	int                  i;

	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		return TRUE;

	len = *(CORBA_unsigned_long *)buf->cur;
	if (giop_msg_conversion_needed (buf))
		len = GUINT32_SWAP_LE_BE (len);
	buf->cur += 4;

	if ((buf->cur + len) > buf->end)
		return TRUE;

	type_id = buf->cur;

	buf->cur = ALIGN_ADDRESS (buf->cur + len, 4);
	if ((buf->cur + 4) > buf->end)
		return TRUE;

	num_profiles = *(CORBA_unsigned_long *)buf->cur;
	if (giop_msg_conversion_needed (buf))
		num_profiles = GUINT32_SWAP_LE_BE (num_profiles);
	buf->cur += 4;

	if (!type_id [0] && num_profiles == 0)
		return FALSE;

	for (i = 0; i < num_profiles; i++) {
		IOP_Profile_info *profile;

		profile = IOP_profile_demarshal (buf, orb);
		if (profile)
			profiles = g_slist_append (profiles, profile);
		else {
			IOP_delete_profiles (orb, &profiles);
			return TRUE;
		}
	}

	if (ret_profiles)
		*ret_profiles = profiles;

	if (ret_type_id)
		*ret_type_id = type_id;

	return FALSE;
}

static void
IOP_TAG_COMPLETE_OBJECT_KEY_copy (IOP_TAG_COMPLETE_OBJECT_KEY_info *dest,
				  const IOP_TAG_COMPLETE_OBJECT_KEY_info *src)
{
	dest->object_key = IOP_ObjectKey_copy (src->object_key);
}

static void
IOP_TAG_GENERIC_SSL_SEC_TRANS_copy (IOP_TAG_GENERIC_SSL_SEC_TRANS_info *dest,
				    const IOP_TAG_GENERIC_SSL_SEC_TRANS_info *src)
{
	dest->service = g_strdup (src->service);
}

static void
IOP_TAG_SSL_SEC_TRANS_copy (IOP_TAG_SSL_SEC_TRANS_info *dest,
			    const IOP_TAG_SSL_SEC_TRANS_info *src)
{
	dest->port = src->port;
	dest->target_supports = src->target_supports;
	dest->target_requires = src->target_requires;
}

static void
IOP_TAG_CODE_SETS_copy (IOP_TAG_CODE_SETS_info *dest,
			const IOP_TAG_CODE_SETS_info *src)
{
	/* no-op */
}

static void
IOP_UnknownComponent_copy (IOP_UnknownComponent_info *dest,
			   const IOP_UnknownComponent_info *src)
{
	gconstpointer src_data = &src->data;
	gpointer      dest_data = &dest->data;

	ORBit_copy_value_core (
		&src_data, &dest_data, TC_CORBA_sequence_CORBA_octet);
}

static IOP_Component_info *
IOP_component_copy (IOP_Component_info *src)
{
#define CPY(src,type) \
	G_STMT_START {  \
	        type##_info *ret = g_new0 (type##_info, 1); \
		ret->parent.component_type = src->component_type; \
		type##_copy (ret, (type##_info *) src);	    \
							    \
	       	return (IOP_Component_info *) ret;	    \
	} G_STMT_END

	switch (src->component_type) {
	case IOP_TAG_COMPLETE_OBJECT_KEY:
		CPY (src, IOP_TAG_COMPLETE_OBJECT_KEY);
		break;
	case IOP_TAG_GENERIC_SSL_SEC_TRANS:
		CPY (src, IOP_TAG_GENERIC_SSL_SEC_TRANS);
		break;
	case IOP_TAG_SSL_SEC_TRANS:
		CPY (src, IOP_TAG_SSL_SEC_TRANS);
		break;
	case IOP_TAG_CODE_SETS:
		CPY (src, IOP_TAG_CODE_SETS);
		break;
	default:
		CPY (src, IOP_UnknownComponent);
		break;
	}
#undef CPY
}

static GSList *
IOP_components_copy (GSList *components)
{
	GSList *ret = NULL, *l;

	for (l = components; l; l = l->next)
		ret = g_slist_prepend (ret, IOP_component_copy (l->data));

	return ret;
}

static void
IOP_TAG_INTERNET_IOP_copy (IOP_TAG_INTERNET_IOP_info       *dest,
			   const IOP_TAG_INTERNET_IOP_info *src)
{
	dest->iiop_version = src->iiop_version;
	dest->host = g_strdup (src->host);
	dest->port = src->port;
	dest->object_key = IOP_ObjectKey_copy (src->object_key);
	dest->components = IOP_components_copy (src->components);
}

static void
IOP_TAG_GENERIC_IOP_copy (IOP_TAG_GENERIC_IOP_info       *dest,
			  const IOP_TAG_GENERIC_IOP_info *src)
{
	dest->iiop_version = src->iiop_version;
	dest->proto = g_strdup (src->proto);
	dest->host = g_strdup (src->host);
	dest->service = g_strdup (src->service);
	dest->components = IOP_components_copy (src->components);
}

static void
IOP_TAG_ORBIT_SPECIFIC_copy (IOP_TAG_ORBIT_SPECIFIC_info       *dest,
			     const IOP_TAG_ORBIT_SPECIFIC_info *src)
{
	dest->unix_sock_path = g_strdup (src->unix_sock_path);
	dest->ipv6_port = src->ipv6_port;
	dest->object_key = IOP_ObjectKey_copy (src->object_key);
}

static void
IOP_TAG_MULTIPLE_COMPONENTS_copy (IOP_TAG_MULTIPLE_COMPONENTS_info       *dest,
				  const IOP_TAG_MULTIPLE_COMPONENTS_info *src)
{
	dest->components = IOP_components_copy (src->components);
}

static void
IOP_UnknownProfile_copy (IOP_UnknownProfile_info *dest,
			 const IOP_UnknownProfile_info *src)
{
	gconstpointer src_data = &src->data;
	gpointer      dest_data = &dest->data;

	ORBit_copy_value_core (
		&src_data, &dest_data, TC_CORBA_sequence_CORBA_octet);
}

static IOP_Profile_info *
IOP_profile_copy (IOP_Profile_info *src)
{
#define CPY(src,type) \
	G_STMT_START {  \
	        type##_info *ret = g_new0 (type##_info, 1); \
		ret->parent.profile_type = src->profile_type; \
		type##_copy (ret, (type##_info *) src);	    \
							    \
	       	return (IOP_Profile_info *) ret;	    \
	} G_STMT_END

	switch (src->profile_type) {
	case IOP_TAG_INTERNET_IOP:
		CPY (src, IOP_TAG_INTERNET_IOP);
		break;

	case IOP_TAG_MULTIPLE_COMPONENTS:
		CPY (src, IOP_TAG_MULTIPLE_COMPONENTS);
		break;

	case IOP_TAG_GENERIC_IOP:
		CPY (src, IOP_TAG_GENERIC_IOP);
		break;

	case IOP_TAG_ORBIT_SPECIFIC:
		CPY (src, IOP_TAG_ORBIT_SPECIFIC);
		break;

	default:
		CPY (src, IOP_UnknownProfile);
		break;
	}
#undef CPY
}

GSList *
IOP_profiles_copy (GSList *profile_list)
{
	GSList *ret = NULL;
	GSList *l;

	for (l = profile_list; l; l = l->next)
		ret = g_slist_prepend (ret, IOP_profile_copy (l->data));

	return ret;
}
