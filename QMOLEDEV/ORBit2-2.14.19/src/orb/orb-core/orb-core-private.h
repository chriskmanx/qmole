#ifndef ORB_CORE_PRIVATE_H
#define ORB_CORE_PRIVATE_H 1

#include <orbit/orbit.h>

CORBA_TypeCode ORBit_get_union_tag     (CORBA_TypeCode union_tc,
					gconstpointer *val,
					gboolean       update);
size_t         ORBit_gather_alloc_info (CORBA_TypeCode tc);
void           ORBit_copy_value_core   (gconstpointer *val,
					gpointer       *newval,
					CORBA_TypeCode tc);

void         ORBit_register_objref  (CORBA_Object obj);
CORBA_Object ORBit_objref_get_proxy (CORBA_Object obj);
void         ORBit_start_servers    (CORBA_ORB    orb);

void         ORBit_set_initial_reference (CORBA_ORB    orb,
					  gchar       *identifier,
					  gpointer     objref);

CORBA_Object ORBit_object_by_corbaloc (CORBA_ORB          orb,
				       const gchar       *corbaloc,
				       CORBA_Environment *ev);

CORBA_char*  ORBit_object_to_corbaloc (CORBA_Object       obj,
				       CORBA_Environment *ev);

CORBA_char* ORBit_corbaloc_from       (GSList            *profile_list, 
				       ORBit_ObjectKey   *object_key);

GSList*     ORBit_corbaloc_parse       (const gchar      *corbaloc);

/* profile methods. */
GSList          *IOP_start_profiles       (CORBA_ORB        orb);
void             IOP_shutdown_profiles    (GSList          *profiles);
void             IOP_delete_profiles      (CORBA_ORB        orb,
					   GSList         **profiles);
void             IOP_generate_profiles    (CORBA_Object     obj);
void             IOP_register_profiles    (CORBA_Object     obj,
					   GSList          *profiles);
ORBit_ObjectKey *IOP_profiles_sync_objkey (GSList          *profiles);
ORBit_ObjectKey *IOP_ObjectKey_copy       (ORBit_ObjectKey *src);
gboolean         IOP_ObjectKey_equal      (ORBit_ObjectKey *a,
					   ORBit_ObjectKey *b);
guint            IOP_ObjectKey_hash       (ORBit_ObjectKey *k);
gboolean         IOP_profile_get_info     (CORBA_Object     obj,
					   gpointer        *pinfo,
					   GIOPVersion     *iiop_version,
					   char           **proto,
					   char           **host,
					   char           **service,
					   gboolean        *ssl,
					   char           *tmpbuf);
void             IOP_profile_hash         (gpointer        item,
					   gpointer        data);
gchar           *IOP_profile_dump         (CORBA_Object    obj,
					   gpointer        p);
gboolean         IOP_profile_equal        (CORBA_Object    obj1,
					   CORBA_Object    obj2,
					   gpointer        d1,
					   gpointer        d2);
void             IOP_profile_marshal      (CORBA_Object    obj,
					   GIOPSendBuffer *buf,
					   gpointer       *p);
GSList          *IOP_profiles_copy        (GSList         *profile_list);


gboolean ORBit_demarshal_IOR (CORBA_ORB orb, GIOPRecvBuffer *buf,
			      char **ret_type_id, GSList **ret_profiles);

int      ORBit_RootObject_shutdown (gboolean moan);
char   **ORBit_get_typelib_paths   (void);
gboolean ORBit_proto_use           (const char *name);
void     _ORBit_object_init        (void);

glong ORBit_get_giop_recv_limit (void);


#ifdef G_OS_WIN32
extern const gchar *ORBit_win32_get_typelib_dir (void);
#undef ORBIT_TYPELIB_DIR
#define ORBIT_TYPELIB_DIR ORBit_win32_get_typelib_dir ()
#endif

#endif
