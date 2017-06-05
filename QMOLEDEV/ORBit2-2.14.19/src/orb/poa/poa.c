#include <config.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef _WIN32
#  include <process.h>
#endif
#include <stdarg.h>

#include <orbit/orbit.h>
#include <orb-core-export.h>
#include <orbit-debug.h>

#include "../util/orbit-purify.h"
#include "../orb-core/orbit-debug.h"
#include "../GIOP/giop-debug.h"
#include "../GIOP/giop-recv-buffer.h"
#include "poa-macros.h"
#include "poa-private.h"
#include "orbit-poa.h"

#ifdef DEBUG_LOCKS
#  define LOCK_DEBUG(a) g_printerr("%p: %6s file %s: line %d (%s)\n", \
				   poa, a, __FILE__, __LINE__, __PRETTY_FUNCTION__);
#else
#  define LOCK_DEBUG(a)
#endif

#define POA_LOCK(poa)   G_STMT_START { \
				LOCK_DEBUG("lock") \
				LINK_MUTEX_LOCK(poa->base.lock); \
			} G_STMT_END

#define POA_UNLOCK(poa) G_STMT_START { \
				LINK_MUTEX_UNLOCK(poa->base.lock); \
				LOCK_DEBUG("unlock") \
			} G_STMT_END

static GMutex     *ORBit_class_assignment_lock    = NULL;
static GHashTable *ORBit_class_assignments        = NULL;
static guint       ORBit_class_assignment_counter = 0;

static PortableServer_Servant ORBit_POA_ServantManager_use_servant(
				     PortableServer_POA poa,
				     ORBit_POAObject pobj,
				     CORBA_Identifier opname,
				     PortableServer_ServantLocator_Cookie *the_cookie,
				     PortableServer_ObjectId *oid,
				     CORBA_Environment *ev );

static void ORBit_POA_ServantManager_unuse_servant(
				       PortableServer_POA poa,
				       ORBit_POAObject pobj,
				       CORBA_Identifier opname,
				       PortableServer_ServantLocator_Cookie cookie,
				       PortableServer_ObjectId *oid,
				       PortableServer_Servant servant,
				       CORBA_Environment *ev );

static void ORBit_POA_handle_request (PortableServer_POA          poa,
				      GIOPRecvBuffer             *recv_buffer,
				      ORBit_ObjectKey            *objkey);


static void               ORBit_POAObject_invoke           (ORBit_POAObject    pobj,
							    gpointer           ret,
							    gpointer          *args,
							    CORBA_Context      ctx,
							    gpointer           data,
							    CORBA_Environment *ev);

static void               ORBit_POAObject_handle_request    (ORBit_POAObject    pobj,
							     CORBA_Identifier   opname,
							     gpointer           ret,
							     gpointer          *args,
							     CORBA_Context      ctx,
							     GIOPRecvBuffer    *recv_buffer,
							     CORBA_Environment *ev);

static void               ORBit_POA_deactivate_object_T   (PortableServer_POA  poa,
							   ORBit_POAObject     pobj,
							   CORBA_boolean       do_etherealize,
							   CORBA_boolean       is_cleanup);
     

static CORBA_Object
ORBit_POA_obj_to_ref (PortableServer_POA  poa,
                      ORBit_POAObject     pobj,
                      const CORBA_char   *intf,
                      CORBA_Environment  *ev);

static void
ORBit_POA_invocation_stack_push (PortableServer_POA  poa,
                                 ORBit_POAObject     pobj)
{
 	LINK_MUTEX_LOCK (poa->orb->lock);
	poa->orb->current_invocations =
		g_slist_prepend (poa->orb->current_invocations, pobj);
	LINK_MUTEX_UNLOCK (poa->orb->lock);
}
                                                                                
static void
ORBit_POA_invocation_stack_pop (PortableServer_POA  poa,
                                ORBit_POAObject pobj)
{
	/* FIXME, we know it is the top element to be removed */ 
	LINK_MUTEX_LOCK (poa->orb->lock);
	poa->orb->current_invocations =
		g_slist_remove (poa->orb->current_invocations, pobj);
	LINK_MUTEX_UNLOCK (poa->orb->lock);
}

static  PortableServer_ObjectId*
ORBit_POA_invocation_stack_lookup_objid (PortableServer_POA          poa,
                                         PortableServer_ServantBase *servant)
{
	PortableServer_ObjectId *objid = NULL;
	GSList *l;

	LINK_MUTEX_LOCK (poa->orb->lock);
	for (l = poa->orb->current_invocations; l; l = l->next) {
		ORBit_POAObject pobj = l->data;
		
		if (pobj->servant == servant)
			objid = ORBit_sequence_CORBA_octet_dup (pobj->object_id);
	}
	LINK_MUTEX_UNLOCK (poa->orb->lock);

	return objid;
}

static  CORBA_Object
ORBit_POA_invocation_stack_lookup_objref (PortableServer_POA           poa,
                                          PortableServer_ServantBase *servant)
{
	CORBA_Object  result = CORBA_OBJECT_NIL; 
	GSList *l;

	LINK_MUTEX_LOCK (poa->orb->lock);
	for (l = poa->orb->current_invocations; l; l = l->next) {
		ORBit_POAObject pobj = l->data;
		
		if (pobj->servant == servant)
			result = ORBit_POA_obj_to_ref (poa, pobj, NULL, NULL);
	}
	LINK_MUTEX_UNLOCK (poa->orb->lock);

	return result;
}

/* FIXME, I would prefer if argument would be of type PortableServer_POA */ 
static  ORBit_POAObject
ORBit_POA_invocation_stack_peek (CORBA_ORB        orb)
{
	ORBit_POAObject pobj = CORBA_OBJECT_NIL;

	LINK_MUTEX_LOCK (orb->lock);
	if (orb->current_invocations == NULL)
		pobj = CORBA_OBJECT_NIL;
	else
		pobj = (ORBit_POAObject) orb->current_invocations->data;
	LINK_MUTEX_UNLOCK (orb->lock);
	
	return pobj;
}

/* PortableServer_Current interface */
static void
ORBit_POACurrent_free_fn (ORBit_RootObject obj_in)
{
	PortableServer_Current poacur = (PortableServer_Current) obj_in;

	ORBit_RootObject_release_T (poacur->orb);
	poacur->orb = NULL;

	p_free (poacur, struct PortableServer_Current_type);
}

static const ORBit_RootObject_Interface ORBit_POACurrent_epv = {
	ORBIT_ROT_POACURRENT,
	ORBit_POACurrent_free_fn
};

PortableServer_Current
ORBit_POACurrent_new (CORBA_ORB orb)
{
	PortableServer_Current poacur;

	poacur = (PortableServer_Current) 
		g_new0 (struct PortableServer_Current_type, 1);

	ORBit_RootObject_init (&poacur->parent, &ORBit_POACurrent_epv);

	poacur->orb = ORBit_RootObject_duplicate (orb);

	return ORBit_RootObject_duplicate (poacur);
}

static ORBit_POAObject
ORBit_POACurrent_get_object (PortableServer_Current  obj,
			     CORBA_Environment      *ev)
{
	ORBit_POAObject pobj = CORBA_OBJECT_NIL;

	g_assert (obj && obj->parent.interface->type == ORBIT_ROT_POACURRENT);
	
	if ( ! (pobj = ORBit_POA_invocation_stack_peek (obj->orb)))
		CORBA_exception_set_system
			(ev, ex_PortableServer_Current_NoContext,
			 CORBA_COMPLETED_NO);
	
	return pobj;
}

PortableServer_ClassInfo *
ORBit_classinfo_lookup (const char *type_id)
{
	PortableServer_ClassInfo *ci = NULL;

	LINK_MUTEX_LOCK   (ORBit_class_assignment_lock);
	if (ORBit_class_assignments)
		ci = g_hash_table_lookup (ORBit_class_assignments, type_id);
	LINK_MUTEX_UNLOCK (ORBit_class_assignment_lock);

	return ci;
}

/* Deprecated & not thread-safe */
void
ORBit_classinfo_register (PortableServer_ClassInfo *ci)
{
	if (*(ci->class_id) != 0)
		return; /* already registered! */

	/* This needs to be pre-increment - we don't want to give out
	 * classid 0, because (a) that is reserved for the base Object class
	 * (b) all the routines allocate a new id if the variable
	 * storing their ID == 0
	 */
	*(ci->class_id) = ++ORBit_class_assignment_counter;

	if (!ORBit_class_assignments)
		ORBit_class_assignments = g_hash_table_new (g_str_hash, g_str_equal);

	g_hash_table_insert (ORBit_class_assignments,
			     (gpointer) ci->class_name, ci);
}

#define VEPV_CACHE_SIZE(c) (c)[0]

void
ORBit_skel_class_register (PortableServer_ClassInfo   *ci,
			   PortableServer_ServantBase *servant,
			   void                      (*opt_finalize) (PortableServer_Servant,
								      CORBA_Environment *),
			   CORBA_unsigned_long         class_offset,
			   CORBA_unsigned_long         first_parent_id,
			   ...)
{
	va_list args;
	CORBA_unsigned_long id;

	va_start (args, first_parent_id);

	/*
	 * FIXME: a double check/lock/write barrier pattern
	 * would be far faster here.
	 */
	LINK_MUTEX_LOCK (ORBit_class_assignment_lock);

	ORBit_classinfo_register (ci);
	if (!ci->vepvmap) {
		CORBA_unsigned_long offset;
		CORBA_unsigned_long epvmap_size;

		epvmap_size = *(ci->class_id) + 1;
		ci->vepvmap = g_new0 (ORBit_VepvIdx, epvmap_size);
		VEPV_CACHE_SIZE(ci->vepvmap) = epvmap_size;

		ci->vepvmap[*(ci->class_id)] = class_offset;
		
		for (id = first_parent_id; id;) {

			offset = va_arg (args, CORBA_unsigned_long);
			
			g_assert (id <= *(ci->class_id));
			ci->vepvmap [id] = offset;

			id = va_arg (args, CORBA_unsigned_long);
		}
	}
	
	LINK_MUTEX_UNLOCK (ORBit_class_assignment_lock);

	if (!servant->vepv[0]->finalize)
		servant->vepv[0]->finalize = opt_finalize;

	ORBIT_SERVANT_SET_CLASSINFO (servant, ci);

	va_end (args);
}

static void
check_child_poa_inuse_T (char               *name,
			 PortableServer_POA  poa,
			 gboolean           *is_inuse)
{
	if (ORBit_POA_is_inuse (poa, CORBA_TRUE, NULL))
		*is_inuse = TRUE;
}

static void
check_object_inuse_T (PortableServer_ObjectId *oid,
		      ORBit_POAObject          pobj, 
		      gboolean                *is_inuse)
{
	if (pobj->use_cnt > 0)
		*is_inuse = TRUE;
}

static gboolean
ORBit_POA_is_inuse_T (PortableServer_POA  poa,
		      CORBA_boolean       consider_children,
		      CORBA_Environment  *ev)
{
	gboolean is_inuse = FALSE;

	if (poa->use_cnt > 0) 
		return TRUE;

	if (consider_children && poa->child_poas)
		g_hash_table_foreach (poa->child_poas,
				      (GHFunc) check_child_poa_inuse_T,
				     &is_inuse);

	if (!is_inuse && poa->oid_to_obj_map)
		g_hash_table_foreach (poa->oid_to_obj_map,
				      (GHFunc) check_object_inuse_T,
				      &is_inuse);

	return is_inuse;
}

gboolean
ORBit_POA_is_inuse (PortableServer_POA  poa,
		    CORBA_boolean       consider_children,
		    CORBA_Environment  *ev)
{
	gboolean inuse;

	POA_LOCK (poa);
	inuse = ORBit_POA_is_inuse_T (poa, consider_children, ev);
	POA_UNLOCK (poa);

	return inuse;
}

static PortableServer_ObjectId *
ORBit_POA_new_system_objid_T (PortableServer_POA poa)
{
	PortableServer_ObjectId *objid;

	g_assert (IS_SYSTEM_ID (poa));

	objid = PortableServer_ObjectId__alloc ();
	objid->_length  = objid->_maximum = sizeof (CORBA_long) + ORBIT_OBJECT_ID_LEN;
	objid->_buffer  = PortableServer_ObjectId_allocbuf (objid->_length);
	objid->_release = CORBA_TRUE;

	ORBit_genuid_buffer (objid->_buffer + sizeof (CORBA_long),
			     ORBIT_OBJECT_ID_LEN, ORBIT_GENUID_OBJECT_ID);
	*((CORBA_long *) objid->_buffer) = ++(poa->next_sysid);

	return objid;
}

static ORBit_ObjectKey*
ORBit_POAObject_object_to_objkey (ORBit_POAObject pobj)
{
	ORBit_ObjectAdaptor  adaptor;
	ORBit_ObjectKey     *objkey;
	guchar              *mem;

	g_return_val_if_fail (pobj != NULL, NULL);

	adaptor = (ORBit_ObjectAdaptor) pobj->poa;

	objkey           = CORBA_sequence_CORBA_octet__alloc ();
	objkey->_length  = adaptor->adaptor_key._length + pobj->object_id->_length;
	objkey->_maximum = objkey->_length;
	objkey->_buffer  = CORBA_sequence_CORBA_octet_allocbuf (objkey->_length);
	objkey->_release = CORBA_TRUE;

 	mem = (guchar *) objkey->_buffer;
 	memcpy (mem, adaptor->adaptor_key._buffer, adaptor->adaptor_key._length);
 
	mem += adaptor->adaptor_key._length;
 	memcpy (mem, pobj->object_id->_buffer, pobj->object_id->_length);

	return objkey;
}

static ORBit_POAObject
ORBit_POA_object_id_lookup_T (PortableServer_POA             poa,
			      const PortableServer_ObjectId *oid)
{
	/* FIXME: needs some locking action */
	return ORBit_RootObject_duplicate (
		g_hash_table_lookup (poa->oid_to_obj_map, oid));
}

static void
ORBit_POA_set_life (PortableServer_POA poa, 
		    CORBA_boolean      etherealize_objects,
		    int                action_do)
{
	if ((poa->life_flags &
	     (ORBit_LifeF_DeactivateDo |
	      ORBit_LifeF_DestroyDo)) == 0) {

		if (etherealize_objects)
			poa->life_flags |= ORBit_LifeF_DoEtherealize;
	}
	poa->life_flags |= action_do;
}

static void
ORBit_POA_add_child (PortableServer_POA poa,
		     PortableServer_POA child)
{
	if (!child)
		return;

	child->parent_poa = ORBit_RootObject_duplicate (poa);
	g_hash_table_insert (poa->child_poas, child->name, child);
}

static void
ORBit_POA_remove_child (PortableServer_POA poa,
			PortableServer_POA child_poa)
{
	if (!child_poa->parent_poa)
		return;

	g_assert (child_poa->parent_poa == poa);

	g_hash_table_remove (poa->child_poas, child_poa->name);

	child_poa->parent_poa = NULL;

	ORBit_RootObject_release (poa);
}

static gboolean
ORBit_POA_destroy_T_R (PortableServer_POA  poa,
		       CORBA_boolean       etherealize_objects,
		       CORBA_Environment  *ev)
{
	GPtrArray *adaptors;
	int        numobjs;
	int        i;

	ORBit_POA_set_life (poa, etherealize_objects, ORBit_LifeF_DestroyDo);

	if (poa->life_flags & ORBit_LifeF_Destroyed)
		return TRUE;	/* already did it */

	if (poa->life_flags & (ORBit_LifeF_Deactivating|ORBit_LifeF_Destroying))
		return FALSE;	/* recursion */

	poa->life_flags |= ORBit_LifeF_Destroying;

	adaptors = poa->orb->adaptors;

	LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);
	POA_UNLOCK (poa);

	/* Destroying the children is tricky, b/c they may die
	 * while we are traversing. We traverse over the
	 * ORB's global list (rather than poa->child_poas) 
	 * to avoid walking into dead children. */
	for (i = 0; i < adaptors->len; i++) {
		PortableServer_POA cpoa = g_ptr_array_index (adaptors, i);

		if (cpoa && cpoa != poa) {
			ORBit_RootObject_duplicate_T (cpoa);
			LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);
			POA_LOCK (cpoa);

			if (cpoa->parent_poa == poa) 
				ORBit_POA_destroy_T_R (cpoa, etherealize_objects, ev);

			POA_UNLOCK (cpoa);
			LINK_MUTEX_LOCK (ORBit_RootObject_lifecycle_lock);
			ORBit_RootObject_release_T (cpoa);
		}
	}

	POA_LOCK (poa);
	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);

	poa->default_servant = NULL;

	if (g_hash_table_size (poa->child_poas) > 0 || poa->use_cnt ||
	    !ORBit_POA_deactivate (poa, etherealize_objects, ev) ) {
		poa->life_flags &= ~ORBit_LifeF_Destroying;

		return FALSE;
	}

	ORBit_POAManager_unregister_poa (poa->poa_manager, poa);

	ORBit_POA_remove_child (poa->parent_poa, poa);

	g_ptr_array_index (adaptors, poa->poa_id) = NULL;
	poa->poa_id = -1;

	/* each objref holds a POAObj, and each POAObj holds a ref 
	 * to the POA. In addition, the app can hold open refs
	 * to the POA itself. */
	numobjs = poa->oid_to_obj_map ? g_hash_table_size (poa->oid_to_obj_map) : 0;
	g_assert (((ORBit_RootObject) poa)->refs > numobjs);

	poa->life_flags |= ORBit_LifeF_Destroyed;
	poa->life_flags &= ~ORBit_LifeF_Destroying;
	ORBit_RootObject_release (poa);

	return TRUE;
}

typedef struct TraverseInfo {
	PortableServer_POA poa;
	gboolean           in_use;
	gboolean           do_etherealize;
} TraverseInfo;

static void
traverse_cb (PortableServer_ObjectId *oid,
	     ORBit_POAObject          pobj, 
	     TraverseInfo            *info)
{
	if (pobj->use_cnt > 0)
		info->in_use = TRUE;

	ORBit_POA_deactivate_object_T (info->poa, pobj, info->do_etherealize, TRUE);
}

static gboolean
remove_cb (PortableServer_ObjectId *oid,
	   ORBit_POAObject          pobj,
	   gpointer                 dummy)
{
	if (pobj->life_flags & ORBit_LifeF_Destroyed) {
		p_free (pobj, struct ORBit_POAObject_type);
		return TRUE;
	}

	return FALSE;
}

CORBA_boolean
ORBit_POA_deactivate (PortableServer_POA poa,
		      CORBA_boolean      etherealize_objects,
		      CORBA_Environment *ev)
{
	CORBA_boolean done = CORBA_TRUE;

	ORBit_POA_set_life (poa, etherealize_objects, ORBit_LifeF_DeactivateDo);

	if (poa->life_flags & ORBit_LifeF_Deactivated)
		return TRUE;	/* already did it */

	if (poa->life_flags & ORBit_LifeF_Deactivating)
		return FALSE;	/* recursion */

	poa->life_flags |= ORBit_LifeF_Deactivating;

	/* bounce all pending requested (OBJECT_NOT_EXIST
	 * exceptions raised); none should get requeued. */
	ORBit_POA_handle_held_requests (poa);
	g_assert (poa->held_requests == NULL);

	if (IS_RETAIN (poa)) {
		TraverseInfo info;

		info.poa            = poa;
		info.in_use         = FALSE;
		info.do_etherealize = (poa->life_flags & ORBit_LifeF_DoEtherealize);

		g_assert (poa->oid_to_obj_map);

		g_hash_table_foreach (
			poa->oid_to_obj_map, (GHFunc) traverse_cb, &info);
		g_hash_table_foreach_remove (
			poa->oid_to_obj_map, (GHRFunc) remove_cb, NULL);

		done = !info.in_use;
	}

	if (done)
		poa->life_flags |= ORBit_LifeF_Deactivated;
	poa->life_flags &= ~ORBit_LifeF_Deactivating;

	return done;
}

/*
 * ORBit_POA_handle_held_requests:
 * @poa:
 *
 * Handle any requests that may been have been queued because the
 * POAManager was in a HOLDING state. Note that if the POAManger
 * is still in the HOLDING state, or is put into the HOLDING
 * state by one of the methods invoked, requests may be re-queued.
 */
void
ORBit_POA_handle_held_requests (PortableServer_POA poa)
{
	GSList *requests;
	GSList *l;

	requests = poa->held_requests;
	poa->held_requests = NULL;

	for (l = requests; l; l = l->next)
		ORBit_handle_request (poa->orb, l->data);

	g_slist_free (requests);
}

static void
ORBit_POA_free_fn (ORBit_RootObject obj)
{
	ORBit_ObjectAdaptor adaptor = (ORBit_ObjectAdaptor) obj;
	PortableServer_POA  poa = (PortableServer_POA) obj;

	giop_thread_key_release (obj);

	if (adaptor->adaptor_key._buffer)
		ORBit_free_T (adaptor->adaptor_key._buffer);

	if (poa->oid_to_obj_map)
		g_hash_table_destroy (poa->oid_to_obj_map);

	if (poa->child_poas)
		g_hash_table_destroy (poa->child_poas);

	if (poa->name)
		g_free (poa->name);

	if (poa->base.lock)
		g_mutex_free (poa->base.lock);

	ORBit_RootObject_release_T (poa->orb);
	ORBit_RootObject_release_T (poa->poa_manager);

	p_free (poa, struct PortableServer_POA_type);
}

static const ORBit_RootObject_Interface ORBit_POA_epv = {
	ORBIT_ROT_ADAPTOR,
	ORBit_POA_free_fn
};

static guint
ORBit_ObjectId_sysid_hash (const PortableServer_ObjectId *object_id)
{
	return *(guint *) object_id->_buffer;
}

static guint
ORBit_sequence_CORBA_octet_hash (const PortableServer_ObjectId *object_id)
{
	const char *start;
	const char *end;
  	const char *p;
  	guint       g, h = 0;

	start = (char *) object_id->_buffer;
	end   = (char *) object_id->_buffer + object_id->_length;

  	for (p = start; p < end; p++) {
    		h = ( h << 4 ) + *p;
		g = h & 0xf0000000;

    		if (g != 0) {
      			h = h ^ (g >> 24);
      			h = h ^ g;
    		}
  	}

  	return h;
}

static gboolean
ORBit_sequence_CORBA_octet_equal (const PortableServer_ObjectId *o1,
				  const PortableServer_ObjectId *o2)
{
	return (o1->_length == o2->_length &&
		!memcmp (o1->_buffer, o2->_buffer, o1->_length));
}

static void
ORBit_POA_set_policy (PortableServer_POA  poa,
		      CORBA_Policy        obj)
{
	struct CORBA_Policy_type *policy = (struct CORBA_Policy_type *) obj;

	switch (policy->type) {
	case PortableServer_THREAD_POLICY_ID:
		poa->p_thread = policy->value;
		break;
	case PortableServer_LIFESPAN_POLICY_ID:
		poa->p_lifespan = policy->value;
		break;
	case PortableServer_ID_UNIQUENESS_POLICY_ID:
		poa->p_id_uniqueness = policy->value;
		break;
	case PortableServer_ID_ASSIGNMENT_POLICY_ID:
		poa->p_id_assignment = policy->value;
		break;
	case PortableServer_IMPLICIT_ACTIVATION_POLICY_ID:
		poa->p_implicit_activation = policy->value;
		break;
	case PortableServer_SERVANT_RETENTION_POLICY_ID:
		poa->p_servant_retention = policy->value;
		break;
	case PortableServer_REQUEST_PROCESSING_POLICY_ID:
		poa->p_request_processing = policy->value;
		break;
	default:
		g_warning ("Unknown policy type, cannot set it on this POA");
		break;
	}
}

static void
ORBit_POA_copy_policies (const PortableServer_POA src,
			 PortableServer_POA dest)
{
	dest->p_thread              = src->p_thread;
	dest->p_lifespan            = src->p_lifespan;
	dest->p_id_uniqueness       = src->p_id_uniqueness;
	dest->p_id_assignment       = src->p_id_assignment;
	dest->p_servant_retention   = src->p_servant_retention;
	dest->p_request_processing  = src->p_request_processing;
	dest->p_implicit_activation = src->p_implicit_activation;
}

static void
ORBit_POA_set_policies (PortableServer_POA      poa,
			const CORBA_PolicyList *policies,
			CORBA_Environment      *ev)
{
	CORBA_unsigned_long i;

	poa->p_thread              = PortableServer_SINGLE_THREAD_MODEL;
	poa->p_lifespan            = PortableServer_TRANSIENT;
	poa->p_id_uniqueness       = PortableServer_UNIQUE_ID;
	poa->p_id_assignment       = PortableServer_SYSTEM_ID;
	poa->p_servant_retention   = PortableServer_RETAIN;
	poa->p_request_processing  = PortableServer_USE_ACTIVE_OBJECT_MAP_ONLY;
	poa->p_implicit_activation = PortableServer_NO_IMPLICIT_ACTIVATION;

	for (i = 0; policies && i < policies->_length; i++)
		ORBit_POA_set_policy (poa, policies->_buffer[i]);

	g_assert (ev->_major == CORBA_NO_EXCEPTION);

	poa_exception_if_fail (!(IS_NON_RETAIN (poa) && IS_USE_ACTIVE_OBJECT_MAP_ONLY (poa)), 
			       ex_PortableServer_POA_InvalidPolicy);

	poa_exception_if_fail (!(IS_USE_DEFAULT_SERVANT (poa) && IS_UNIQUE_ID (poa)),
			       ex_PortableServer_POA_InvalidPolicy);

	poa_exception_if_fail (!(IS_IMPLICIT_ACTIVATION (poa) && (IS_USER_ID (poa) || IS_NON_RETAIN (poa))),
			       ex_PortableServer_POA_InvalidPolicy);
}

static PortableServer_POA
ORBit_POA_new (CORBA_ORB                  orb,
	       const CORBA_char          *adaptor_name,
	       PortableServer_POAManager  manager,
	       const CORBA_PolicyList    *policies,
	       CORBA_Environment         *ev)
{
	PortableServer_POA   poa;
	ORBit_ObjectAdaptor  adaptor;
  
	poa = g_new0 (struct PortableServer_POA_type, 1);

	ORBit_RootObject_init ((ORBit_RootObject) poa, &ORBit_POA_epv);
	/* released in ORBit_POA_destroy */
	ORBit_RootObject_duplicate (poa);
	
	ORBit_POA_set_policies (poa, policies, ev);
	if (ev->_major != CORBA_NO_EXCEPTION) {
		ORBit_RootObject_release (poa);
		return CORBA_OBJECT_NIL;
	}

	if (!manager)
		manager = ORBit_POAManager_new (orb);

	poa->poa_manager = ORBit_RootObject_duplicate (manager);

	adaptor = (ORBit_ObjectAdaptor) poa;
	adaptor->handle_request = (ORBitReqHandlerFunc) ORBit_POA_handle_request;

	poa->name       = g_strdup (adaptor_name);
	poa->child_poas = g_hash_table_new (g_str_hash, g_str_equal);
	poa->orb        = ORBit_RootObject_duplicate (orb);
	poa->poa_id     = ORBit_adaptor_setup (adaptor, orb);

	if (IS_SYSTEM_ID (poa))
		poa->oid_to_obj_map = g_hash_table_new (
			(GHashFunc) ORBit_ObjectId_sysid_hash,
			(GEqualFunc) ORBit_sequence_CORBA_octet_equal);
	else /* USER_ID */
		poa->oid_to_obj_map = g_hash_table_new (
			(GHashFunc) ORBit_sequence_CORBA_octet_hash,
			(GEqualFunc) ORBit_sequence_CORBA_octet_equal);

	poa->base.lock = link_mutex_new ();

	ORBit_POAManager_register_poa (manager, poa);

	return ORBit_RootObject_duplicate (poa);
}

PortableServer_POA
ORBit_POA_new_from (CORBA_ORB                  orb,
		    PortableServer_POA         parent,
		    const CORBA_char          *adaptor_name,
		    const CORBA_PolicyList    *opt_policies,
		    CORBA_Environment         *ev)
{
	PortableServer_POA poa;
	
	g_return_val_if_fail (parent != CORBA_OBJECT_NIL, CORBA_OBJECT_NIL);

	poa = ORBit_POA_new (orb, adaptor_name, parent->poa_manager, NULL, ev);

	g_return_val_if_fail (poa != CORBA_OBJECT_NIL, CORBA_OBJECT_NIL);

	ORBit_POA_copy_policies (parent, poa);

	if (opt_policies) {
		int i;
		for (i = 0; i < opt_policies->_length; i++)
			ORBit_POA_set_policy (poa, opt_policies->_buffer[i]);
	}

	ORBit_POA_add_child (parent, poa);

	return poa;
}

static CORBA_Object
ORBit_POA_obj_to_ref (PortableServer_POA  poa,
		      ORBit_POAObject     pobj,
		      const CORBA_char   *intf,
		      CORBA_Environment  *ev)
{
	const char              *type_id = intf;

	g_assert (pobj && !pobj->base.objref);

	if (!type_id) {
		g_assert (pobj->servant);
		type_id = ORBIT_SERVANT_TO_CLASSINFO (pobj->servant)->class_name;
	}

	g_assert (type_id != NULL);

	pobj->base.objref = ORBit_objref_new (poa->poa_manager->orb,
					      &pobj->base,
					      g_quark_from_string (type_id));

	return ORBit_RootObject_duplicate (pobj->base.objref);
}

PortableServer_POA
ORBit_POA_setup_root (CORBA_ORB orb, CORBA_Environment *ev)
{
	PortableServer_POA poa;
	CORBA_Policy       policybuf[1];
	CORBA_PolicyList   policies = {1, 1, NULL, CORBA_FALSE};

	policies._buffer = (CORBA_Object *) policybuf;

	policies._buffer [0] = (CORBA_Policy)
		PortableServer_POA_create_implicit_activation_policy (
			NULL, PortableServer_IMPLICIT_ACTIVATION, ev);

	poa = ORBit_POA_new (orb, "RootPOA", CORBA_OBJECT_NIL, &policies, ev);
 
	CORBA_Policy_destroy (policies._buffer [0], ev);
	CORBA_Object_release (policies._buffer [0], ev);

	return poa;
}

static gboolean
ORBit_POAObject_is_active (ORBit_POAObject pobj)
{
	if (pobj && pobj->servant)
		return TRUE;

	return FALSE;
}

/* POAObject RootObject stuff */
static void
ORBit_POAObject_release_cb (ORBit_RootObject robj)
{
	ORBit_POAObject    pobj = (ORBit_POAObject) robj;
	PortableServer_POA poa = pobj->poa;
	PortableServer_ObjectId *object_id;
 
	/* object *must* be deactivated */
	g_assert (pobj->servant == NULL);

	giop_thread_key_release (robj);

	object_id = pobj->object_id;
	pobj->object_id = NULL;

	/*
	 * Don't want to remove from oid_to_obj_map if we 
	 * are currently traversing across it !
	 * Just mark it as destroyed
	 */
	if ((poa->life_flags & ORBit_LifeF_Deactivating) == 0) {
		g_hash_table_remove (poa->oid_to_obj_map, object_id);
		p_free (robj, struct ORBit_POAObject_type);
	} else
		pobj->life_flags = ORBit_LifeF_Destroyed;

	object_id->_release = CORBA_TRUE;
	ORBit_free_T (object_id);

	ORBit_RootObject_release_T (poa);
}

static ORBit_RootObject_Interface ORBit_POAObject_if = {
	ORBIT_ROT_OAOBJECT,
	ORBit_POAObject_release_cb
};

static struct 
ORBit_OAObject_Interface_type ORBit_POAObject_methods = {
	ORBIT_ADAPTOR_POA,
	(ORBitStateCheckFunc) ORBit_POAObject_is_active,
	(ORBitKeyGenFunc)     ORBit_POAObject_object_to_objkey,
	(ORBitInvokeFunc)     ORBit_POAObject_invoke,
	(ORBitReqFunc)        ORBit_POAObject_handle_request
};

/*
 *    If USER_ID policy, {oid} must be non-NULL.
 *  If SYSTEM_ID policy, {oid} must ether be NULL, or must have
 *  been previously created by the POA. If the user passes in
 *  a bogus oid under SYSTEM_ID, we will assert or segfault. This
 *  is allowed by the CORBA spec.
 */
static ORBit_POAObject
ORBit_POA_create_object_T (PortableServer_POA             poa,
			   const PortableServer_ObjectId *objid,
			   CORBA_Environment             *ev)
{
	ORBit_POAObject newobj;

	newobj = g_new0 (struct ORBit_POAObject_type, 1);
	ORBit_RootObject_init ((ORBit_RootObject)newobj, &ORBit_POAObject_if);

	/* released in ORBit_POAObject_release_cb */
	newobj->poa = ORBit_RootObject_duplicate (poa);

	((ORBit_OAObject)newobj)->interface = &ORBit_POAObject_methods;

	if (poa->p_id_assignment == PortableServer_SYSTEM_ID) {
		if (objid) {
			g_assert (objid->_length ==
				  sizeof (CORBA_unsigned_long) +
				  ORBIT_OBJECT_ID_LEN);

			newobj->object_id          = PortableServer_ObjectId__alloc ();
			newobj->object_id->_length = objid->_length;
			newobj->object_id->_buffer = PortableServer_ObjectId_allocbuf (objid->_length);
			newobj->object_id->_release = CORBA_TRUE;

			memcpy (newobj->object_id->_buffer, objid->_buffer, objid->_length);
		}
		else
			newobj->object_id = ORBit_POA_new_system_objid_T (poa);
	} else {
		newobj->object_id           = PortableServer_ObjectId__alloc ();
		newobj->object_id->_length  = objid->_length;
		newobj->object_id->_buffer  = PortableServer_ObjectId_allocbuf (objid->_length);
		newobj->object_id->_release = CORBA_TRUE;

		memcpy(newobj->object_id->_buffer, objid->_buffer, objid->_length);
	}

	g_hash_table_insert (poa->oid_to_obj_map, newobj->object_id, newobj);

	return newobj;
}

/*
 *    Normally this is called for normal servants in RETAIN mode. 
 *  However, it may also be invoked on the default servant when
 *  it is installed. In this later case, it may be either RETAIN
 *  or NON_RETAIN.
 */
static void
ORBit_POA_activate_object_T (PortableServer_POA          poa, 
			     ORBit_POAObject             pobj,
			     PortableServer_ServantBase *servant, 
			     CORBA_Environment          *ev) 
{
	PortableServer_ClassInfo *class_info;

	g_assert (pobj->servant == NULL);
	g_assert ((poa->life_flags & ORBit_LifeF_DeactivateDo) == 0);
	g_assert (pobj->use_cnt == 0);

	class_info = ORBIT_SERVANT_TO_CLASSINFO (servant);
	pobj->vepvmap_cache = class_info->vepvmap;

	pobj->servant = servant;

	pobj->next = (ORBit_POAObject) servant->_private;
	servant->_private = pobj;

	/* released in ORBit_POA_deactivate_object */
	ORBit_RootObject_duplicate (pobj);
}

/*
 * Note that this doesn't necessarily remove the object from
 * the oid_to_obj_map; it just removes knowledge of the servant.
 * If the object is currently in use (servicing a request),
 * etherialization and memory release will occur later.
 */
static void
ORBit_POA_deactivate_object_T (PortableServer_POA poa,
			       ORBit_POAObject    pobj,
			       CORBA_boolean      do_etherealize,
			       CORBA_boolean      is_cleanup)
{
	PortableServer_ServantBase *servant = pobj->servant;

	if (!servant) /* deactivation done, or in progress */
		return;

	if (do_etherealize && !(pobj->life_flags & ORBit_LifeF_DeactivateDo))
		pobj->life_flags |= ORBit_LifeF_DoEtherealize;
	
	if (is_cleanup)
		pobj->life_flags |= ORBit_LifeF_IsCleanup;

	if (pobj->use_cnt > 0) {
		pobj->life_flags |= ORBit_LifeF_DeactivateDo;
		pobj->life_flags |= ORBit_LifeF_NeedPostInvoke;
		return;
	}
	pobj->servant = NULL;

	if ((ORBit_POAObject) servant->_private == pobj)
		servant->_private = pobj->next;
	else {
		ORBit_POAObject l = (ORBit_POAObject) servant->_private;

		for (; l && l->next != pobj; l = l->next);

		g_assert (l != NULL && l->next == pobj);

		l->next = pobj->next;
	}
	pobj->next = NULL;

	if (pobj->life_flags & ORBit_LifeF_DoEtherealize) {
		CORBA_Environment env, *ev = &env;

		CORBA_exception_init (ev);

		pobj->use_cnt++; /* prevent re-activation */
		if (poa->p_request_processing == PortableServer_USE_SERVANT_MANAGER) {
			POA_PortableServer_ServantActivator      *sm;
			POA_PortableServer_ServantActivator__epv *epv;

			sm = (POA_PortableServer_ServantActivator *) poa->servant_manager;
			epv = sm->vepv->PortableServer_ServantActivator_epv;

			epv->etherealize (sm, pobj->object_id, poa,
					  servant,
					  pobj->life_flags & ORBit_LifeF_IsCleanup,
					  /* remaining_activations */ CORBA_FALSE,
					  ev);
		}
		{
			PortableServer_ServantBase__epv *epv = servant->vepv[0];
			/* In theory, the finalize fnc should always be non-NULL;
			 * however, for backward compat. and general extended
			 * applications we dont insist on it. */
			if (epv && epv->finalize) {
				POA_UNLOCK (poa);
				epv->finalize (servant, ev);
				POA_LOCK (poa);
			}
		}
		pobj->use_cnt--; /* allow re-activation */

		if (ev->_major != 0) {
		  g_error ("finalize function for object %p threw an exception (%s). This is not allowed.",
			   pobj, CORBA_exception_id (ev));
		}

		CORBA_exception_free (ev);
	}

	pobj->life_flags &= ~(ORBit_LifeF_DeactivateDo |
			      ORBit_LifeF_IsCleanup |
			      ORBit_LifeF_DoEtherealize);

	ORBit_RootObject_release (pobj);
}

struct ORBit_POA_invoke_data {
	ORBitSmallSkeleton small_skel;
	gpointer           imp;
};

static void
ORBit_POAObject_invoke (ORBit_POAObject    pobj,
			gpointer           ret,
			gpointer          *args,
			CORBA_Context      ctx,
			gpointer           data,
			CORBA_Environment *ev)
{
	struct ORBit_POA_invoke_data *invoke_data = (struct ORBit_POA_invoke_data *) data;

	invoke_data->small_skel (pobj->servant, ret, args, ctx, ev, invoke_data->imp);
}

/*
 * giop_recv_buffer_return_sys_exception:
 * @recv_buffer:
 * @m_data:
 * @ev:
 *
 * Return a system exception in @ev to the client. If @m_data
 * is not nil, it used to determine whether the call is a
 * oneway and, hence, whether to return the exception. If
 * @m_data is nil, we are not far enough along in the processing
 * of the reqeust to be able to determine if this is a oneway
 * method.
 */
void
ORBit_recv_buffer_return_sys_exception (GIOPRecvBuffer    *recv_buffer,
					CORBA_Environment *ev)
{
	GIOPSendBuffer *send_buffer;

	if (!recv_buffer) /* In Proc */
		return;

	g_return_if_fail (ev->_major == CORBA_SYSTEM_EXCEPTION);

	send_buffer = giop_send_buffer_use_reply (
		recv_buffer->connection->giop_version,
		giop_recv_buffer_get_request_id (recv_buffer),
		ev->_major);

	ORBit_send_system_exception (send_buffer, ev);

	tprintf ("Return exception:\n");
	do_giop_dump_send (send_buffer);
	giop_send_buffer_write (send_buffer, recv_buffer->connection, FALSE);
	giop_send_buffer_unuse (send_buffer);
}

static void
return_exception (GIOPRecvBuffer    *recv_buffer,
		  ORBit_IMethod     *m_data,
		  CORBA_Environment *ev)
{
	if (!recv_buffer) /* In Proc */
		return;

	g_return_if_fail (ev->_major == CORBA_SYSTEM_EXCEPTION);

	if (m_data && m_data->flags & ORBit_I_METHOD_1_WAY) {
		tprintf ("A serious exception occured on a oneway method");
		return;
	}

	ORBit_recv_buffer_return_sys_exception (recv_buffer, ev);
}

/*
 * If invoked in the local case, recv_buffer == NULL.
 * If invoked in the remote cse, ret = args = ctx == NULL.
 */
static void
ORBit_POAObject_handle_request (ORBit_POAObject    pobj,
				CORBA_Identifier   opname,
				gpointer           ret,
				gpointer          *args,
				CORBA_Context      ctx,
				GIOPRecvBuffer    *recv_buffer,
				CORBA_Environment *ev)
{
	PortableServer_POA                   poa = pobj->poa;
	PortableServer_ServantLocator_Cookie cookie = NULL;
	PortableServer_ObjectId             *oid = NULL;
	PortableServer_ClassInfo            *klass;
	ORBit_IMethod                       *m_data = NULL;
	ORBitSmallSkeleton                   small_skel = NULL;
	gpointer                             imp = NULL;

	if (poa) {
		ORBit_RootObject_duplicate (poa);
		POA_LOCK (poa);
	}

	if (!poa || !poa->poa_manager)
		CORBA_exception_set_system (
			ev, ex_CORBA_OBJECT_NOT_EXIST, 
			CORBA_COMPLETED_NO);
	else {
		switch (poa->poa_manager->state) {

		case PortableServer_POAManager_HOLDING:
			if (recv_buffer) {
				g_warning ("POAManager in holding state. "
					   "Queueing '%s' method request", opname);
						
				poa->held_requests = g_slist_prepend (
					poa->held_requests, recv_buffer);
				goto clean_out;
			} else
				CORBA_exception_set_system (
					ev, ex_CORBA_TRANSIENT,
					CORBA_COMPLETED_NO);
			break;
			
		case PortableServer_POAManager_DISCARDING:
			CORBA_exception_set_system (
				ev, ex_CORBA_TRANSIENT,
				CORBA_COMPLETED_NO);
			break;

		case PortableServer_POAManager_INACTIVE:
			CORBA_exception_set_system (
				ev, ex_CORBA_OBJ_ADAPTER,
				CORBA_COMPLETED_NO);
			break;

		case PortableServer_POAManager_ACTIVE:
			break;

		default:
			g_assert_not_reached ();
			break;
		}
	}
	if (ev->_major != CORBA_NO_EXCEPTION) {
		return_exception (recv_buffer, m_data, ev);
		goto clean_out;
	}

	oid = pobj->object_id;

	if (!pobj->servant) {
		switch (poa->p_request_processing) {

		case PortableServer_USE_ACTIVE_OBJECT_MAP_ONLY:
			CORBA_exception_set_system (
				ev, ex_CORBA_OBJECT_NOT_EXIST, 
				CORBA_COMPLETED_NO);
			break;

		case PortableServer_USE_DEFAULT_SERVANT:
			ORBit_POA_activate_object_T (
				poa, pobj, poa->default_servant, ev);
			break;

		case PortableServer_USE_SERVANT_MANAGER:
			ORBit_POA_ServantManager_use_servant (
				poa, pobj, opname,  &cookie, oid, ev);
			break;
		default:
			g_assert_not_reached();
			break;
		}
	}

	if (ev->_major == CORBA_NO_EXCEPTION && !pobj->servant)
		CORBA_exception_set_system (
			ev, ex_CORBA_OBJECT_NOT_EXIST, 
			CORBA_COMPLETED_NO);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		return_exception (recv_buffer, m_data, ev);
		goto clean_out;
	}

	pobj->use_cnt++;
	ORBit_POA_invocation_stack_push (poa, pobj);
	
	klass = ORBIT_SERVANT_TO_CLASSINFO (pobj->servant);

	if (klass->impl_finder)
		small_skel = klass->impl_finder (
			pobj->servant, opname, (gpointer *)&m_data, &imp);

	/* FIXME: we can only do that in-proc (and for _is_a [and others?]) */
	if (!small_skel)
		small_skel = get_small_skel_CORBA_Object (
			pobj->servant, opname,
			(gpointer *)&m_data, &imp);
	
	if (!small_skel || !imp) {
		if (!imp && small_skel) {
			tprintf ("'%s' not implemented on %p",
				 opname, pobj);
			CORBA_exception_set_system (
				ev, ex_CORBA_NO_IMPLEMENT,
				CORBA_COMPLETED_NO);
		} else {
			tprintf ("Bad operation '%s' on %p",
				 opname, pobj);
			CORBA_exception_set_system (
				ev, ex_CORBA_BAD_OPERATION,
				CORBA_COMPLETED_NO);
		}
	}

	if (ev->_major != CORBA_NO_EXCEPTION)
		return_exception (recv_buffer, m_data, ev);

	else {
		POA_UNLOCK (poa);

		if (recv_buffer) {
			struct ORBit_POA_invoke_data invoke_data;
			
			invoke_data.small_skel = small_skel;
			invoke_data.imp        = imp;
			
			ORBit_small_invoke_adaptor ((ORBit_OAObject) pobj, recv_buffer,
						    m_data, &invoke_data, ev);
		} else
			small_skel (pobj->servant, ret, args, ctx, ev, imp);
		
		POA_LOCK (poa);
	}

	if (recv_buffer)
		CORBA_exception_free (ev);

	if (IS_NON_RETAIN (poa))
		switch (poa->p_request_processing) {
		case PortableServer_USE_SERVANT_MANAGER:
			ORBit_POA_ServantManager_unuse_servant (
				poa, pobj, opname, cookie, 
				oid, pobj->servant, ev);
			break;
		case PortableServer_USE_DEFAULT_SERVANT:
			ORBit_POA_deactivate_object_T (poa, pobj, FALSE, FALSE);
			break;
		default:
			g_assert_not_reached ();
			break;
		}

	ORBit_POA_invocation_stack_pop (poa, pobj);

	pobj->use_cnt--;

	if (pobj->life_flags & ORBit_LifeF_NeedPostInvoke)
		ORBit_POAObject_post_invoke (pobj);             

 clean_out:
	if(poa) {
		POA_UNLOCK (poa);
		ORBit_RootObject_release (poa);
	}
}

static ORBit_POAObject
ORBit_POA_object_key_lookup_T (PortableServer_POA       poa,
			       ORBit_ObjectKey         *objkey,
			       PortableServer_ObjectId *object_id)
{
	object_id->_buffer  = objkey->_buffer + ORBIT_ADAPTOR_PREFIX_LEN;
	object_id->_length  = objkey->_length - ORBIT_ADAPTOR_PREFIX_LEN; 
	object_id->_maximum = object_id->_length;
	object_id->_release = CORBA_FALSE;

	return ORBit_POA_object_id_lookup_T (poa, object_id);
}

static void
ORBit_POAObject_invoke_incoming_request (ORBit_POAObject    pobj,
					 GIOPRecvBuffer    *recv_buffer,
					 CORBA_Environment *opt_ev)
{
	CORBA_Environment real_ev, *ev;
	
	if (!opt_ev)
		CORBA_exception_init ((ev = &real_ev));
	else
		ev = opt_ev;

	if (ev->_major == CORBA_NO_EXCEPTION &&
	    pobj != CORBA_OBJECT_NIL) {
		CORBA_Identifier opname;

		opname = giop_recv_buffer_get_opname (recv_buffer);
		ORBit_POAObject_handle_request (pobj, opname, NULL, NULL, 
						NULL, recv_buffer, ev);
	}

	ORBit_RootObject_release (pobj);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		tprintf ("p %d, Method '%p' invoked with exception '%s'",
			 getpid (), giop_recv_buffer_get_opname (recv_buffer), ev->_id);

		return_exception (recv_buffer, NULL, ev);
	}

	if (!opt_ev)
		CORBA_exception_free (ev);

	giop_recv_buffer_unuse (recv_buffer);
}

static inline void
push_request_T (GIOPThread       *thread,
		ORBit_POAObject  *pobj,
		GIOPRecvBuffer  **recv_buffer)
{
	giop_thread_request_push
		(thread, (gpointer *)pobj, (gpointer *)recv_buffer);
}

static void
pool_push_request_for_T (gpointer          key_object,
			 ORBit_POAObject  *pobj,
			 GIOPRecvBuffer  **recv_buffer)
{
	g_assert (giop_thread_io ());
	giop_thread_request_push_key
		(key_object, (gpointer *)pobj, (gpointer *)recv_buffer);
}			 

typedef struct {
	ORBit_POAObject  pobj;
	GIOPRecvBuffer  *recv_buffer;
} PoaIdleClosure;

static gboolean
poa_invoke_at_idle (gpointer data)
{
	PoaIdleClosure *pcl = data;

	ORBit_POAObject_invoke_incoming_request
		(pcl->pobj, pcl->recv_buffer, NULL);

	pcl->pobj = NULL;
	pcl->recv_buffer = NULL;

	return FALSE;
}

static void
poa_destroy_idle_closure (gpointer data)
{
	PoaIdleClosure *pcl = data;

	if (pcl->pobj)
		ORBit_RootObject_release (pcl->pobj);
	if (pcl->recv_buffer)
		giop_recv_buffer_unuse (pcl->recv_buffer);

	g_free (pcl);
}

static void
push_request_idle (ORBit_POAObject  *pobj,
		   GIOPRecvBuffer  **recv_buffer,
		   GMainContext     *on_context)
{
	PoaIdleClosure *pcl = g_new (PoaIdleClosure, 1);
	GSource *source;

	pcl->pobj = *pobj;
	pcl->recv_buffer = *recv_buffer;

	source = g_idle_source_new ();
	g_source_set_callback (source, poa_invoke_at_idle, pcl, poa_destroy_idle_closure);

	g_source_attach (source, on_context);
	g_source_unref (source);
	
	*pobj = NULL;
	*recv_buffer = NULL;
}

static gboolean
poa_recv_is_oneway (ORBit_POAObject pobj,
		    GIOPRecvBuffer *recv_buffer)
{
	ORBit_IMethod            *m_data = NULL;
	gpointer                  imp = NULL;
	PortableServer_ClassInfo *klass;

	g_return_val_if_fail (pobj != CORBA_OBJECT_NIL, FALSE);

	klass = ORBIT_SERVANT_TO_CLASSINFO (pobj->servant);

	if (!klass->impl_finder)
		return FALSE;

	klass->impl_finder ( pobj->servant,
			     giop_recv_buffer_get_opname (recv_buffer),
			     (gpointer *)&m_data, &imp);

	if (m_data && m_data->flags & ORBit_I_METHOD_1_WAY)
		return TRUE;

	return FALSE;
}

static void
ORBit_POA_handle_request (PortableServer_POA poa,
			  GIOPRecvBuffer    *recv_buffer,
			  ORBit_ObjectKey   *objkey)
{
	ORBit_POAObject         pobj;
	CORBA_Environment       env;
	PortableServer_ObjectId object_id;

	CORBA_exception_init (&env);

	POA_LOCK (poa);

	pobj = ORBit_POA_object_key_lookup_T (poa, objkey, &object_id);

	if (!pobj)
		switch (poa->p_request_processing) {
		case PortableServer_USE_ACTIVE_OBJECT_MAP_ONLY:
			CORBA_exception_set_system (
				&env, ex_CORBA_OBJECT_NOT_EXIST, 
				CORBA_COMPLETED_NO);
			goto send_sys_ex;
			break;

		case PortableServer_USE_DEFAULT_SERVANT: /* drop through */
		case PortableServer_USE_SERVANT_MANAGER:
			pobj = ORBit_POA_create_object_T (poa, &object_id, &env);
			break;

		default:
			g_assert_not_reached ();
			break;
		}
	
	if (!pobj)
		CORBA_exception_set_system (
			&env, ex_CORBA_OBJECT_NOT_EXIST, 
			CORBA_COMPLETED_NO);
	else {
		switch (poa->p_thread) {
			case PortableServer_SINGLE_THREAD_MODEL:
				if (giop_thread_io ())
					push_request_T (giop_thread_get_main (),
							&pobj, &recv_buffer);
				break;
			case PortableServer_ORB_CTRL_MODEL: {
				ORBit_ObjectAdaptor adaptor = (ORBit_ObjectAdaptor) poa;
				
				switch (adaptor->thread_hint) {
				case ORBIT_THREAD_HINT_PER_OBJECT:
					/* Note: If the user bound this object to a specific
					 * thread this thread will be called, otherwise a
					 * new thread will be created for handling requests
					 * to the object. */
					pool_push_request_for_T (pobj, &pobj, &recv_buffer);
					break;

				case ORBIT_THREAD_HINT_PER_POA:
					pool_push_request_for_T (poa, &pobj, &recv_buffer);
					break;

				case ORBIT_THREAD_HINT_PER_CONNECTION:
					pool_push_request_for_T (recv_buffer->connection,
								 &pobj, &recv_buffer);
					break;

				case ORBIT_THREAD_HINT_PER_REQUEST:
					pool_push_request_for_T (NULL, &pobj, &recv_buffer);
					break;

				case ORBIT_THREAD_HINT_ONEWAY_AT_IDLE:
					if (!poa_recv_is_oneway (pobj, recv_buffer))
						push_request_T (giop_thread_get_main (),
								&pobj, &recv_buffer);
					/* drop through */
				case ORBIT_THREAD_HINT_ALL_AT_IDLE:
					push_request_idle (&pobj, &recv_buffer, NULL);
					break;

				case ORBIT_THREAD_HINT_ON_CONTEXT:
					push_request_idle (&pobj, &recv_buffer, adaptor->context);
					break;

				case ORBIT_THREAD_HINT_NONE:
					if (giop_thread_io ())
						push_request_T (giop_thread_get_main (),
								&pobj, &recv_buffer);
					break;
				default:
					g_warning ("Binning incoming requests in threaded mode");
					giop_recv_buffer_unuse (recv_buffer);
					recv_buffer = NULL;
					pobj = NULL;
					break;
				}
				break;
			}
		default:
			g_assert_not_reached ();
			break;
		}
	}
	
 send_sys_ex:
	POA_UNLOCK (poa);

	ORBit_POAObject_invoke_incoming_request (pobj, recv_buffer, &env);
}

static PortableServer_Servant
ORBit_POA_ServantManager_use_servant (PortableServer_POA                    poa,
				      ORBit_POAObject                       pobj,
				      CORBA_Identifier                      opname,
				      PortableServer_ServantLocator_Cookie *the_cookie,
				      PortableServer_ObjectId              *oid,
				      CORBA_Environment                    *ev)
{
	PortableServer_ServantBase *retval;

	if (IS_RETAIN (poa)) {
		POA_PortableServer_ServantActivator__epv *epv;
		POA_PortableServer_ServantActivator *sm;
		
		sm = (POA_PortableServer_ServantActivator *) poa->servant_manager;
		epv = sm->vepv->PortableServer_ServantActivator_epv;

		retval = epv->incarnate (sm, oid, poa, ev);

		if (retval) {

			/* XXX: two POAs sharing servant and having
			 *      different uniqueness policies ??
			 *  see note 11.3.5.1
			 */
			if (IS_UNIQUE_ID (poa) && retval->_private != NULL) {
				CORBA_exception_set_system (ev, ex_CORBA_OBJ_ADAPTER,
							    CORBA_COMPLETED_NO);
				return NULL;
			}

			pobj->next = retval->_private;
			retval->_private = pobj;

			/* released by ORBit_POA_deactivate_object */
			ORBit_RootObject_duplicate (pobj);
			pobj->servant = retval;
		}
	} else { 
		POA_PortableServer_ServantLocator__epv *epv;
		POA_PortableServer_ServantLocator      *sm;

		sm = (POA_PortableServer_ServantLocator *) poa->servant_manager;
		epv = sm->vepv->PortableServer_ServantLocator_epv;

		retval = epv->preinvoke (sm, oid, poa, opname, the_cookie, ev);

		if (retval) {
			/* FIXME: Is this right?
			 *        Is it the same as above?
			 */
			if (IS_UNIQUE_ID (poa) && retval->_private != NULL) {
				CORBA_exception_set_system (ev, ex_CORBA_OBJ_ADAPTER,
							    CORBA_COMPLETED_NO);
				return NULL;
			}

			pobj->next = retval->_private;
			retval->_private = pobj;

			/* released by ORBit_POA_ServantManager_unuse_servant */
			ORBit_RootObject_duplicate (pobj);
			pobj->servant = retval;
		}
	}

	return retval;
}

static void
ORBit_POA_ServantManager_unuse_servant (PortableServer_POA                    poa,
					ORBit_POAObject                       pobj,
					CORBA_Identifier                      opname,
					PortableServer_ServantLocator_Cookie  cookie,
					PortableServer_ObjectId              *oid,
					PortableServer_Servant                serv,
					CORBA_Environment                    *ev)
{
	POA_PortableServer_ServantLocator      *sm;
	POA_PortableServer_ServantLocator__epv *epv;
	PortableServer_ServantBase             *servant = serv;

	g_assert (IS_NON_RETAIN (poa));

	sm = (POA_PortableServer_ServantLocator *) poa->servant_manager;
	epv = sm->vepv->PortableServer_ServantLocator_epv;

	pobj->servant = NULL;
	
	if ((ORBit_POAObject) servant->_private == pobj)
		servant->_private = pobj->next;
	else {
		ORBit_POAObject l = (ORBit_POAObject) servant->_private;

		for (; l && l->next != pobj; l = l->next);

		g_assert (l != NULL && l->next == pobj);

		l->next = pobj->next;
	}
	pobj->next = NULL;

	ORBit_RootObject_release (pobj);

	epv->postinvoke (sm, oid, poa, opname, cookie, servant, ev);
}

/*
 * Was this ever exposed / can we axe it ?
 */
void
ORBit_POAObject_post_invoke (ORBit_POAObject pobj)
{
	if (pobj->use_cnt > 0)
		return;

	if (pobj->life_flags & ORBit_LifeF_DeactivateDo)  {
		/* NOTE that the "desired" values of etherealize and cleanup
		 * are stored in pobj->life_flags and they dont need
		 * to be passed in again!
		 */
		ORBit_POA_deactivate_object_T (
			pobj->poa, pobj, /*ether*/0, /*cleanup*/0);

		/* WATCHOUT: pobj may not exist anymore! */
	}
}

/*
 * C Language Mapping Specific Methods.
 * Section 1.26.2 (C Language Mapping Specification).
 */
CORBA_char *
PortableServer_ObjectId_to_string (PortableServer_ObjectId *id, 
				   CORBA_Environment       *ev)
{
	CORBA_char *str;

	poa_sys_exception_val_if_fail (id != NULL, ex_CORBA_BAD_PARAM, NULL);
	poa_sys_exception_val_if_fail (memchr (id->_buffer, '\0', id->_length),
				       ex_CORBA_BAD_PARAM, NULL);

	str = CORBA_string_alloc (id->_length + 1);
	memcpy (str, id->_buffer, id->_length);
	str [id->_length] = '\0';

	return str;
}

CORBA_wchar *
PortableServer_ObjectId_to_wstring (PortableServer_ObjectId *id,
				    CORBA_Environment       *ev)
{
	CORBA_wchar *retval;
	int          i;

	poa_sys_exception_val_if_fail (id != NULL, ex_CORBA_BAD_PARAM, NULL);
	poa_sys_exception_val_if_fail (memchr (id->_buffer, '\0', id->_length),
				       ex_CORBA_BAD_PARAM, NULL);
  
	retval = CORBA_wstring_alloc (id->_length + 1);
	for (i = 0; i < id->_length; i++)
		retval [i] = id->_buffer [i];
	retval [id->_length] = '\0';

	return retval;
}

PortableServer_ObjectId *
PortableServer_string_to_ObjectId (CORBA_char        *str,
				   CORBA_Environment *ev)
{
	PortableServer_ObjectId tmp;

	poa_sys_exception_val_if_fail (str != NULL, ex_CORBA_BAD_PARAM, NULL);

	tmp._length  = strlen (str);
	tmp._buffer  = str;
  
	return (PortableServer_ObjectId *) ORBit_sequence_CORBA_octet_dup (&tmp);
}

PortableServer_ObjectId *
PortableServer_wstring_to_ObjectId (CORBA_wchar       *str,
				    CORBA_Environment *ev)
{
	PortableServer_ObjectId tmp;
	int                     i;

	poa_sys_exception_val_if_fail (str != NULL, ex_CORBA_BAD_PARAM, NULL);

	for (i = 0; str[i]; i++);

	tmp._length = i*2;
	tmp._buffer = g_alloca (tmp._length);

	for (i = 0; str[i]; i++)
		tmp._buffer[i] = str[i];

	return (PortableServer_ObjectId *) ORBit_sequence_CORBA_octet_dup (&tmp);
}

/*
 * Current Operations.
 * Section 11.3.9
 */

PortableServer_POA
PortableServer_Current_get_POA (PortableServer_Current  obj,
				CORBA_Environment      *ev)
{
	ORBit_POAObject pobj;

	poa_sys_exception_val_if_fail (obj != NULL, ex_CORBA_INV_OBJREF, NULL);

	pobj = ORBit_POACurrent_get_object (obj, ev);

	return ORBit_RootObject_duplicate (pobj->poa);
}

PortableServer_ObjectId *
PortableServer_Current_get_object_id (PortableServer_Current  obj,
				      CORBA_Environment      *ev)
{
	ORBit_POAObject pobj;

	poa_sys_exception_val_if_fail (obj != NULL, ex_CORBA_INV_OBJREF, NULL);

	pobj = ORBit_POACurrent_get_object (obj, ev);

	if (!pobj) 
		return NULL;

	return (PortableServer_ObjectId *)ORBit_sequence_CORBA_octet_dup (pobj->object_id);
}

/*
 * PortableServer::POA interface
 * Section 11.3.8
 */

PortableServer_POA
PortableServer_POA_create_POA (PortableServer_POA               poa,
			       const CORBA_char                *adaptor_name,
			       const PortableServer_POAManager  a_POAManager,
			       const CORBA_PolicyList          *policies,
			       CORBA_Environment               *ev)
{
	PortableServer_POA retval;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (adaptor_name != NULL, ex_CORBA_BAD_PARAM, NULL);
	poa_sys_exception_val_if_fail (policies != NULL, ex_CORBA_BAD_PARAM, NULL);

	if (g_hash_table_lookup (poa->child_poas, adaptor_name)) {
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_PortableServer_POA_AdapterAlreadyExists,
				     NULL);
		return CORBA_OBJECT_NIL;
	}

	retval = ORBit_POA_new (poa->orb, adaptor_name, a_POAManager, policies, ev);

	ORBit_POA_add_child (poa, retval);

	return retval;
}

PortableServer_POA
PortableServer_POA_find_POA (PortableServer_POA   poa,
			     const CORBA_char    *adaptor_name,
			     const CORBA_boolean  activate_it,
			     CORBA_Environment   *ev)
{
	PortableServer_POA child_poa = NULL;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (adaptor_name != NULL, ex_CORBA_BAD_PARAM, NULL);

	if (poa->child_poas)
		child_poa = g_hash_table_lookup (poa->child_poas, adaptor_name);

	if (activate_it)
		g_warning ("Don't yet know how to activate POA named \"%s\"",
			   adaptor_name);

	if (!child_poa)
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_PortableServer_POA_AdapterNonExistent,
				     NULL);	

	return ORBit_RootObject_duplicate (child_poa);
}

void
PortableServer_POA_destroy (PortableServer_POA   poa,
			    const CORBA_boolean  etherealize_objects,
			    const CORBA_boolean  wait_for_completion,
			    CORBA_Environment   *ev)
{
	gboolean done;

	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);

	ORBit_RootObject_duplicate (poa);
	POA_LOCK (poa);

	if (poa->life_flags & ORBit_LifeF_Destroyed)
		;

	else if (wait_for_completion && ORBit_POA_is_inuse_T (poa, CORBA_TRUE, ev))
		CORBA_exception_set_system (ev, ex_CORBA_BAD_INV_ORDER,
					    CORBA_COMPLETED_NO);
	else {
		done = ORBit_POA_destroy_T_R (poa, etherealize_objects, ev);

		g_assert (done || !wait_for_completion);
	}

	POA_UNLOCK (poa);
	ORBit_RootObject_release (poa);
}

CORBA_string
PortableServer_POA__get_the_name (PortableServer_POA  poa,
				  CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return CORBA_string_dup (poa->name);
}

PortableServer_POA
PortableServer_POA__get_the_parent (PortableServer_POA  poa,
				    CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return ORBit_RootObject_duplicate (poa->parent_poa);
}

static void
ORBit_POAList_add_child (char                   *name,
			 PortableServer_POA      poa,
			 PortableServer_POAList *list)
{
	list->_buffer [list->_length++] = ORBit_RootObject_duplicate (poa);
}

PortableServer_POAList *
PortableServer_POA__get_the_children (PortableServer_POA  poa,
				      CORBA_Environment  *ev)
{
	PortableServer_POAList *retval;
	int                     length;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	length = g_hash_table_size (poa->child_poas);

	retval           = PortableServer_POAList__alloc ();
	retval->_length  = 0;
	retval->_maximum = length;
	retval->_buffer  = (CORBA_Object *) PortableServer_POAList_allocbuf (length);
	retval->_release = CORBA_TRUE;

	g_hash_table_foreach (poa->child_poas, (GHFunc) ORBit_POAList_add_child, retval);

	g_assert (retval->_length == length);

	return retval;
}

PortableServer_POAManager
PortableServer_POA__get_the_POAManager (PortableServer_POA  poa,
					CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return ORBit_RootObject_duplicate (poa->poa_manager);
}

PortableServer_AdapterActivator
PortableServer_POA__get_the_activator (PortableServer_POA  poa,
				       CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return ORBit_RootObject_duplicate (poa->the_activator);
}

void
PortableServer_POA__set_the_activator (PortableServer_POA                    poa,
				       const PortableServer_AdapterActivator activator,
				       CORBA_Environment * ev)
{
	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);
	poa_sys_exception_if_fail (activator != NULL, ex_CORBA_BAD_PARAM);

	if (poa->the_activator)
		ORBit_RootObject_release (poa->the_activator);

	poa->the_activator = (PortableServer_AdapterActivator)
					ORBit_RootObject_duplicate (activator);
}

PortableServer_ServantManager
PortableServer_POA_get_servant_manager (PortableServer_POA  poa,
					CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return ORBit_RootObject_duplicate (poa->servant_manager);
}

void
PortableServer_POA_set_servant_manager (PortableServer_POA                   poa,
					const PortableServer_ServantManager  manager,
					CORBA_Environment                   *ev)
{
	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);
	poa_sys_exception_if_fail (manager != NULL, ex_CORBA_BAD_PARAM);
	poa_sys_exception_if_fail (poa->servant_manager == NULL, ex_CORBA_BAD_INV_ORDER);

	poa->servant_manager = (PortableServer_ServantManager)
					ORBit_RootObject_duplicate (manager);
}

PortableServer_Servant
PortableServer_POA_get_servant (PortableServer_POA  poa,
				CORBA_Environment  *ev)
{
	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	return poa->default_servant;
}

void
PortableServer_POA_set_servant (PortableServer_POA            poa,
				const PortableServer_Servant  servant,
				CORBA_Environment            *ev)
{
	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);

	poa->default_servant = servant;
}

PortableServer_ObjectId *
PortableServer_POA_activate_object (PortableServer_POA            poa,
				    const PortableServer_Servant  p_servant,
				    CORBA_Environment            *ev)
{
	PortableServer_ObjectId    *result;
	PortableServer_ServantBase *servant = p_servant;
	ORBit_POAObject             newobj;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (servant != NULL, ex_CORBA_BAD_PARAM, NULL);

	POA_LOCK (poa);

	/* FIXME: unlock on preconditions */
	poa_exception_val_if_fail (IS_RETAIN (poa), ex_PortableServer_POA_WrongPolicy, NULL);
	poa_exception_val_if_fail (IS_SYSTEM_ID (poa), ex_PortableServer_POA_WrongPolicy, NULL);
	poa_exception_val_if_fail (IS_MULTIPLE_ID (poa) || (IS_UNIQUE_ID (poa) && servant->_private == NULL),
				   ex_PortableServer_POA_ServantAlreadyActive, NULL);

	newobj = ORBit_POA_create_object_T (poa, NULL, ev);
	ORBit_POA_activate_object_T (poa, newobj, servant, ev);

	result = ORBit_sequence_CORBA_octet_dup (newobj->object_id);

	POA_UNLOCK (poa);

	return result;
}

void
PortableServer_POA_activate_object_with_id (PortableServer_POA             poa,
					    const PortableServer_ObjectId *objid,
					    const PortableServer_Servant   p_servant,
					    CORBA_Environment             *ev)
{
	ORBit_POAObject pobj;
	PortableServer_ServantBase *servant = p_servant;

	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);
	poa_sys_exception_if_fail (objid != NULL, ex_CORBA_BAD_PARAM);
	poa_sys_exception_if_fail (servant != NULL, ex_CORBA_BAD_PARAM);

	POA_LOCK (poa);

	poa_exception_if_fail (IS_RETAIN (poa), ex_PortableServer_POA_WrongPolicy);

	pobj = ORBit_POA_object_id_lookup_T (poa, objid);

	if (pobj && pobj->servant)
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_PortableServer_POA_ObjectAlreadyActive, 
				     NULL);

	else if (IS_UNIQUE_ID (poa) && servant->_private != NULL)
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_PortableServer_POA_ServantAlreadyActive,
				     NULL);
	else {
		ORBit_POAObject newobj;

		if (!pobj)
			newobj = ORBit_POA_create_object_T (poa, objid, ev);
		else
			newobj = pobj;

		ORBit_POA_activate_object_T (poa, newobj, servant, ev);
	}

	ORBit_RootObject_release (pobj);

	POA_UNLOCK (poa);
}

void
PortableServer_POA_deactivate_object (PortableServer_POA             poa,
				      const PortableServer_ObjectId *oid,
				      CORBA_Environment             *ev)
{
	ORBit_POAObject pobj;

	poa_sys_exception_if_fail (poa != NULL, ex_CORBA_INV_OBJREF);
	poa_sys_exception_if_fail (oid != NULL, ex_CORBA_BAD_PARAM);

	POA_LOCK (poa);

	poa_exception_if_fail (IS_RETAIN (poa), ex_PortableServer_POA_WrongPolicy);

	pobj = ORBit_POA_object_id_lookup_T (poa, oid);

	if (pobj && pobj->servant)
		ORBit_POA_deactivate_object_T (poa, pobj, CORBA_TRUE, CORBA_FALSE);

	POA_UNLOCK (poa);

	ORBit_RootObject_release (pobj);
}

CORBA_Object
PortableServer_POA_create_reference (PortableServer_POA  poa,
				     const CORBA_char   *intf,
				     CORBA_Environment  *ev)
{
	CORBA_Object obj;
	ORBit_POAObject pobj;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);

	POA_LOCK (poa);

	poa_exception_val_if_fail (IS_SYSTEM_ID (poa), ex_PortableServer_POA_WrongPolicy, NULL);

	pobj = ORBit_POA_create_object_T (poa, NULL, ev);

	obj = ORBit_POA_obj_to_ref (poa, pobj, intf, ev);

	POA_UNLOCK (poa);

	return obj;
}

CORBA_Object
PortableServer_POA_create_reference_with_id (PortableServer_POA             poa,
					     const PortableServer_ObjectId *oid,
					     const CORBA_char              *intf,
					     CORBA_Environment             *ev)
{
	CORBA_Object obj;
	ORBit_POAObject	pobj, newobj;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (oid != NULL, ex_CORBA_BAD_PARAM, NULL);

	POA_LOCK (poa);

	pobj = ORBit_POA_object_id_lookup_T (poa, oid);
	if (!pobj)
		newobj = ORBit_POA_create_object_T (poa, oid, ev);
	else
		newobj = CORBA_OBJECT_NIL;

	obj = ORBit_POA_obj_to_ref (poa, pobj, intf, ev);

	if (!newobj)
		ORBit_RootObject_release (pobj);

	POA_UNLOCK (poa);

	return obj;
}

PortableServer_ObjectId *
PortableServer_POA_servant_to_id (PortableServer_POA            poa,
				  const PortableServer_Servant  p_servant,
				  CORBA_Environment            *ev)
{
	PortableServer_ServantBase *servant = p_servant;
	PortableServer_ObjectId    *objid;
	ORBit_POAObject             pobj = servant->_private;
	gboolean                    defserv = IS_USE_DEFAULT_SERVANT (poa);
	gboolean                    retain = IS_RETAIN (poa);
	gboolean                    implicit = IS_IMPLICIT_ACTIVATION (poa);
	gboolean                    unique = IS_UNIQUE_ID (poa);

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (servant != NULL, ex_CORBA_BAD_PARAM, NULL);

	POA_LOCK (poa);

	poa_exception_val_if_fail (defserv || (retain && (unique || implicit)),
				   ex_PortableServer_POA_WrongPolicy, NULL);

	objid = NULL;
	if (retain && unique && pobj && pobj->servant == servant)
		objid = ORBit_sequence_CORBA_octet_dup (pobj->object_id);

	else if (retain && implicit && (!unique || !pobj)) {
		pobj = ORBit_POA_create_object_T (poa, NULL, ev);
		ORBit_POA_activate_object_T (poa, pobj, servant, ev);

		objid = ORBit_sequence_CORBA_octet_dup (pobj->object_id);

	} else {
		/*
		 * FIXME:
		 * This handles case 3 of the spec; but is broader:
		 * it matches invokations on any type of servant, not
		 * just the default servant.
		 * The stricter form could be implemented, 
		 * but it would only add more code...
		 */
		
		objid = ORBit_POA_invocation_stack_lookup_objid (poa, servant);

	}

	if (!objid)
		CORBA_exception_set
			(ev, CORBA_USER_EXCEPTION,
			 ex_PortableServer_POA_ServantNotActive,
			 NULL);


	POA_UNLOCK (poa);

	return objid;
}

CORBA_Object
PortableServer_POA_servant_to_reference (PortableServer_POA            poa,
					 const PortableServer_Servant  p_servant,
					 CORBA_Environment            *ev)
{
	PortableServer_ServantBase *servant = p_servant;
	ORBit_POAObject             pobj = servant->_private;
	CORBA_Object                result;
	gboolean                    retain = IS_RETAIN (poa);
	gboolean                    implicit = IS_IMPLICIT_ACTIVATION (poa);
	gboolean                    unique = IS_UNIQUE_ID (poa);

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, CORBA_OBJECT_NIL);
	poa_sys_exception_val_if_fail (servant != NULL, ex_CORBA_BAD_PARAM, CORBA_OBJECT_NIL);

	POA_LOCK (poa);
	poa_exception_val_if_fail (retain && (unique || implicit),
				   ex_PortableServer_POA_WrongPolicy,
				   CORBA_OBJECT_NIL);

	result = CORBA_OBJECT_NIL;
	if (retain && unique && pobj)
		if (pobj->base.objref)
			result = ORBit_RootObject_duplicate (pobj->base.objref);
		else
			result = ORBit_POA_obj_to_ref (poa, pobj, NULL, ev);

	else if (retain && implicit && (!unique || !pobj)) {
		pobj = ORBit_POA_create_object_T (poa, NULL, ev);
		ORBit_POA_activate_object_T (poa, pobj, servant, ev);

		result = ORBit_POA_obj_to_ref (poa, pobj, NULL, ev);
	} else {
		/*
		 * FIXME:
		 * This case deals with "invoked in the context of
		 * executing a request." Note that there are no policy
		 * restrictions for this case. We must do a forward search
		 * looking for matching {servant}. If unique, we could 
		 * go backward from servant to pobj to use_cnt, but we
		 * dont do this since forward search is more general 
		 */

		
		result = ORBit_POA_invocation_stack_lookup_objref (poa, servant);

	}

	if (result == CORBA_OBJECT_NIL)
		CORBA_exception_set
			(ev, CORBA_USER_EXCEPTION,
			 ex_PortableServer_POA_ServantNotActive,
			 NULL);

	POA_UNLOCK (poa);

	return result;
}

PortableServer_Servant
PortableServer_POA_reference_to_servant (PortableServer_POA  poa,
					 const CORBA_Object  reference,
					 CORBA_Environment  *ev)
{

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (reference != NULL, ex_CORBA_BAD_PARAM, NULL);

	poa_exception_val_if_fail (IS_USE_DEFAULT_SERVANT (poa) || IS_RETAIN (poa),
				   ex_PortableServer_POA_WrongPolicy, NULL);

	if (IS_RETAIN (poa)) {
		ORBit_POAObject pobj;

		poa_exception_val_if_fail (reference->adaptor_obj != NULL,
					   ex_PortableServer_POA_WrongAdapter,
					   NULL);

		pobj = (ORBit_POAObject) reference->adaptor_obj;

		if (pobj->servant)
			return pobj->servant;

	} else if (poa->default_servant)
		return poa->default_servant;

	CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			     ex_PortableServer_POA_ObjectNotActive,
			     NULL);

	return NULL;
}

PortableServer_ObjectId *
PortableServer_POA_reference_to_id (PortableServer_POA  poa,
				    const CORBA_Object  reference,
				    CORBA_Environment  *ev)
{
	ORBit_POAObject pobj;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (reference != NULL, ex_CORBA_BAD_PARAM, NULL);

	poa_exception_val_if_fail (reference->adaptor_obj != NULL,
				   ex_PortableServer_POA_WrongAdapter, 
				   NULL);

	pobj = (ORBit_POAObject) reference->adaptor_obj;

	return (PortableServer_ObjectId *)
				ORBit_sequence_CORBA_octet_dup (pobj->object_id);
}

PortableServer_Servant
PortableServer_POA_id_to_servant (PortableServer_POA             poa,
				  const PortableServer_ObjectId *object_id,
				  CORBA_Environment             *ev)
{
	PortableServer_Servant servant = NULL;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (object_id != NULL, ex_CORBA_BAD_PARAM, NULL);

	POA_LOCK (poa);

	poa_exception_val_if_fail (IS_USE_DEFAULT_SERVANT (poa) || IS_RETAIN (poa),
				   ex_PortableServer_POA_WrongPolicy, NULL);

	if (IS_RETAIN (poa)) {
		ORBit_POAObject pobj = ORBit_POA_object_id_lookup_T (poa, object_id);

		if (pobj && pobj->servant)
			servant = pobj->servant;

		ORBit_RootObject_release (pobj);

	} else if (poa->default_servant)
		servant = poa->default_servant;

	if (!servant)
		CORBA_exception_set
			(ev, CORBA_USER_EXCEPTION,
			 ex_PortableServer_POA_ObjectNotActive,
			 NULL);

	POA_UNLOCK (poa);

	return NULL;
}

CORBA_Object
PortableServer_POA_id_to_reference (PortableServer_POA             poa,
				    const PortableServer_ObjectId *object_id,
				    CORBA_Environment             *ev)
{
	CORBA_Object obj;
	ORBit_POAObject pobj;

	poa_sys_exception_val_if_fail (poa != NULL, ex_CORBA_INV_OBJREF, NULL);
	poa_sys_exception_val_if_fail (object_id != NULL, ex_CORBA_BAD_PARAM, NULL);

	POA_LOCK (poa);

	poa_exception_val_if_fail (IS_RETAIN (poa), ex_PortableServer_POA_WrongPolicy, NULL);

	pobj = ORBit_POA_object_id_lookup_T (poa, object_id);
	if (!pobj || !pobj->servant) {
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
				     ex_PortableServer_POA_ObjectNotActive, NULL);
		obj = CORBA_OBJECT_NIL;
	} else {
		if (pobj->base.objref)
			obj = ORBit_RootObject_duplicate (pobj->base.objref);
		else
			obj = ORBit_POA_obj_to_ref (poa, pobj, NULL, ev);
	}

	POA_UNLOCK (poa);
	ORBit_RootObject_release (pobj);
	
	return obj;
}

void
ORBit_poa_init (void)
{
	ORBit_class_assignment_lock = link_mutex_new ();
	_ORBit_poa_manager_lock = link_mutex_new ();
	giop_thread_set_main_handler (ORBit_POAObject_invoke_incoming_request);
}

gboolean
ORBit_poa_allow_cross_thread_call (ORBit_POAObject   pobj,
				   ORBit_IMethodFlag method_flags)

{
	gpointer key = NULL;
	GIOPThread *self;
	PortableServer_POA poa = pobj->poa;

	if (!poa)
		return TRUE;

	self = giop_thread_self ();

	switch (poa->p_thread) {
	case PortableServer_SINGLE_THREAD_MODEL:
		break;

	case PortableServer_ORB_CTRL_MODEL: {
		ORBit_ObjectAdaptor adaptor = (ORBit_ObjectAdaptor) poa;

		if (method_flags & ORBit_I_METHOD_1_WAY)
			return FALSE;

		switch (adaptor->thread_hint) {
		case ORBIT_THREAD_HINT_PER_OBJECT:
			key = pobj;
			break;
			
		case ORBIT_THREAD_HINT_PER_POA:
			key = poa;
			break;
			
		case ORBIT_THREAD_HINT_PER_CONNECTION:
			/* FIXME: ? */
		case ORBIT_THREAD_HINT_PER_REQUEST:
			return TRUE;
			break;
			
		case ORBIT_THREAD_HINT_ONEWAY_AT_IDLE:
		case ORBIT_THREAD_HINT_ALL_AT_IDLE:
		case ORBIT_THREAD_HINT_ON_CONTEXT:
			/* FIXME: need GThread *g_main_context_get_owner()
			   to do this right ... */
		case ORBIT_THREAD_HINT_NONE:
			break;
		}
		break;
	}
	}

	giop_thread_new_check (self);

	if (!key)
		return (self == giop_thread_get_main ());
	else
		return giop_thread_same_key (key, TRUE);
}

static gpointer
get_c_method (CORBA_Object                 obj,
	      glong                        class_id,
	      PortableServer_ServantBase **servant,
	      glong                        method_offset,
	      ORBit_IMethodFlag            method_flags)
{
	guchar *epv_start;
	ORBit_POAObject pobj;

	if (!obj ||
	    !(pobj = (ORBit_POAObject) obj->adaptor_obj) ||
	    !(pobj->base.interface->adaptor_type & ORBIT_ADAPTOR_POA) ||
	    !(*servant = (PortableServer_ServantBase *) pobj->servant))
		return NULL;

	if (method_offset <= 0 || class_id <= 0)
		return NULL;

	if (!ORBit_poa_allow_cross_thread_call (pobj, method_flags))
		return NULL;

	if (ORBit_small_flags & ORBIT_SMALL_FORCE_GENERIC_MARSHAL)
		return NULL;

	/*
	 * FIXME: we could propagate the size of the vepvmap_cache
	 * to here, and check it to avoid some really bogus stuff.
	 */
	if (!class_id || !pobj->vepvmap_cache ||
	    class_id >= VEPV_CACHE_SIZE (pobj->vepvmap_cache) )
		return NULL;

	epv_start = (guchar *) (*servant)->vepv [ pobj->vepvmap_cache [class_id] ];

	if (!epv_start)
		return NULL;

	return *(gpointer *)(epv_start + method_offset);
}

void
ORBit_c_stub_invoke (CORBA_Object        obj,
		     ORBit_IMethods     *methods,
		     glong               method_index,
		     gpointer            ret,
		     gpointer            args,
		     CORBA_Context       ctx,
		     CORBA_Environment  *ev,

		     glong               class_id,
		     glong               method_offset,
		     ORBitSmallSkeleton  skel_impl)
{
	gpointer method_impl;
	PortableServer_ServantBase *servant;

	if (method_index < 0 || method_index > methods->_length) {
		CORBA_exception_set_system (ev, ex_CORBA_NO_IMPLEMENT,
					    CORBA_COMPLETED_NO);
		return;
	}

	if (skel_impl &&
	    (method_impl = get_c_method (obj, class_id,
					 &servant, method_offset,
					 methods->_buffer[method_index].flags))) {
		
		/* Unwound PreCall
		   POA_LOCK (((ORBit_POAObject)(obj)->adaptor_obj)->poa);
		   ++( ((ORBit_POAObject)(obj)->adaptor_obj)->use_cnt );
		   POA_UNLOCK (((ORBit_POAObject)(obj)->adaptor_obj)->poa);
		   (obj)->orb->current_invocations =
		   g_slist_prepend ((obj)->orb->current_invocations,
		   (obj)->adaptor_obj);
		*/
		
		CORBA_exception_init (ev);
		skel_impl (servant, ret, args, ctx, ev, method_impl);

		/* Unwound PostCall
		   (obj)->orb->current_invocations =
		   g_slist_remove ((obj)->orb->current_invocations, pobj);
		   POA_LOCK (((ORBit_POAObject)(obj)->adaptor_obj)->poa);
		   --(((ORBit_POAObject)(obj)->adaptor_obj)->use_cnt);
		   if (((ORBit_POAObject)(obj)->adaptor_obj)->life_flags & ORBit_LifeF_NeedPostInvoke)
			ORBit_POAObject_post_invoke (((ORBit_POAObject)(obj)->adaptor_obj));
		   POA_UNLOCK (((ORBit_POAObject)(obj)->adaptor_obj)->poa);
		*/

	} else
		ORBit_small_invoke_stub_n
			(obj, methods, method_index,
			 ret, args, ctx, ev);
}
