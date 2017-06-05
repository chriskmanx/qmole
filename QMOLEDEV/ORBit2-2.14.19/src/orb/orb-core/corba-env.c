#include "config.h"
#include <orbit/orbit.h>
#include <string.h>

static void
CORBA_exception_free_T (CORBA_Environment *ev)
{
	if (ev->_major != CORBA_NO_EXCEPTION) {
		ev->_major = CORBA_NO_EXCEPTION;

		ORBit_free_T (ev->_id);
		ev->_id = NULL;

		CORBA_any__freekids (&ev->_any, NULL);
		ev->_any._type = NULL;
		ev->_any._value = NULL;
		ev->_any._release = CORBA_FALSE;
	}
}

void
CORBA_exception_free (CORBA_Environment *ev)
{
	g_return_if_fail (ev != NULL);

	if (ev->_major != CORBA_NO_EXCEPTION) {
		LINK_MUTEX_LOCK   (ORBit_RootObject_lifecycle_lock);

		CORBA_exception_free_T (ev);

		LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);
	}
}

static gpointer
CORBA_exception__freekids (gpointer mem, gpointer dat)
{
	CORBA_Environment *env;

	env = mem;
	CORBA_exception_free_T (env);

	return env + 1;
}

CORBA_Environment *
CORBA_exception__alloc (void)
{
	CORBA_Environment *retval = ORBit_alloc_with_free_fn (
		sizeof (CORBA_Environment), 1, 
		CORBA_exception__freekids);

	CORBA_exception_init (retval);

	return retval;
}

CORBA_Environment *
CORBA_exception__copy (const CORBA_Environment *ev)
{
	CORBA_Environment *dest;

	dest = CORBA_exception__alloc ();

	if (ev->_major != CORBA_NO_EXCEPTION) {
		*dest = *ev;
		dest->_id = CORBA_string_dup (ev->_id);
		if (dest->_any._type != CORBA_OBJECT_NIL)
			CORBA_any__copy (&dest->_any, &ev->_any);
		else /* FIXME: we need good type data here ! */
			dest->_any._value = NULL;
	}

	return dest;
}

void
ORBit_handle_system_exception (CORBA_Environment *ev,
			       const CORBA_char *nom,
			       CORBA_completion_status status,
			       GIOPRecvBuffer *buf,
			       GIOPSendBuffer *sendbuf)
{
	CORBA_exception_set_system (ev, nom, status);
	giop_recv_buffer_unuse (buf);
	giop_send_buffer_unuse (sendbuf);
}

void
CORBA_exception_set_system (CORBA_Environment *ev,
			    const CORBA_char *except_repos_id,
			    CORBA_completion_status completed)
{
	CORBA_SystemException *se;

	g_return_if_fail (ev != NULL);

	se = CORBA_SystemException__alloc ();
	/* I have never seen a case where 'minor' is actually necessary */
	se->minor = 0 /* minor */;
	se->completed = completed;
	CORBA_exception_set (
		ev, CORBA_SYSTEM_EXCEPTION, except_repos_id, se);
}

void
CORBA_exception_set (CORBA_Environment   *ev,
		     CORBA_exception_type major,
		     const CORBA_char    *except_repos_id,
		     void                *param)
{
	g_return_if_fail (ev != NULL);

	CORBA_exception_free(ev);

	ev->_major = major;
	if (major != CORBA_NO_EXCEPTION) {
		ev->_id = CORBA_string_dup (except_repos_id);

                if (ev->_any._release)
                        CORBA_free (ev->_any._value);

		/* FIXME: we can get this from the typelib */
		ev->_any._type = NULL; /* CORBA sucks */
		ev->_any._value = param;
		ev->_any._release = CORBA_TRUE;
	}
}

CORBA_char *
CORBA_exception_id (CORBA_Environment *ev)
{
	if (ev->_major != CORBA_NO_EXCEPTION)
		return ev->_id;

	return NULL;
}

void *
CORBA_exception_value (CORBA_Environment *ev)
{
	if (ev->_major != CORBA_NO_EXCEPTION)
		return ev->_any._value;

	return NULL;
}

/* An ORBit extension that seems to be perpetuated */
void
CORBA_exception_init (CORBA_Environment *ev)
{
	g_return_if_fail (ev != NULL);

	memset (ev, 0, sizeof (CORBA_Environment));
	ev->_major = CORBA_NO_EXCEPTION;
}

CORBA_any *
CORBA_exception_as_any (CORBA_Environment *ev)
{
	return &ev->_any;
}

void
ORBit_handle_exception(GIOPRecvBuffer *rb, CORBA_Environment *ev,
		       const ORBit_exception_demarshal_info *ex_info,
		       CORBA_ORB orb)
{
  CORBA_SystemException *new;
  CORBA_unsigned_long len, completion_status, reply_status;
  CORBA_char *my_repoid;

  CORBA_exception_free(ev);

  rb->cur = ALIGN_ADDRESS(rb->cur, sizeof(len));
  if((rb->cur + 4) > rb->end)
    goto errout;
  len = *(CORBA_unsigned_long *)rb->cur;
  rb->cur += 4;
  if(giop_msg_conversion_needed(rb))
    len = GUINT32_SWAP_LE_BE(len);

  if(len)
    {
      my_repoid = rb->cur;
      rb->cur += len;
    }
  else
    my_repoid = NULL;

  reply_status = giop_recv_buffer_reply_status(rb);
  if(reply_status == CORBA_SYSTEM_EXCEPTION)
    {
      CORBA_unsigned_long minor;

      ev->_major = CORBA_SYSTEM_EXCEPTION;

      rb->cur = ALIGN_ADDRESS(rb->cur, sizeof(minor));
      if((rb->cur + sizeof(minor)) > rb->end)
	goto errout;
      minor = *(CORBA_unsigned_long*)rb->cur;
      rb->cur += 4;
      if(giop_msg_conversion_needed(rb))
	minor = GUINT32_SWAP_LE_BE(minor);

      rb->cur = ALIGN_ADDRESS(rb->cur, sizeof(completion_status));
      if((rb->cur + sizeof(completion_status)) > rb->end)
	goto errout;
      completion_status = *(CORBA_unsigned_long*)rb->cur;
      rb->cur += 4;
      if(giop_msg_conversion_needed(rb))
	completion_status = GUINT32_SWAP_LE_BE(completion_status);

      new = CORBA_SystemException__alloc();
      new->minor=minor;
      new->completed=completion_status;
			
      /* XXX what should the repo ID be? */
      CORBA_exception_set(ev, CORBA_SYSTEM_EXCEPTION,
			  my_repoid,
			  new);
    }
  else if(reply_status == CORBA_USER_EXCEPTION)
    {
      int i;

      if(!ex_info)
	{
	  /* weirdness; they raised an exception that we don't
	     know about */
	  CORBA_exception_set_system(ev, ex_CORBA_MARSHAL,
				     CORBA_COMPLETED_MAYBE);
	}
      else
	{
	  for(i = 0; ex_info[i].tc != CORBA_OBJECT_NIL;
	      i++)
	    if(my_repoid && !strcmp(ex_info[i].tc->repo_id,
		       my_repoid))
	      break;

	  if(ex_info[i].tc == CORBA_OBJECT_NIL)
				/* weirdness; they raised an exception
				   that we don't know about */
	    CORBA_exception_set_system(ev, ex_CORBA_MARSHAL,
				       CORBA_COMPLETED_MAYBE);
	  else
	    ex_info[i].demarshal(rb, ev);
	}
    };

  return;
  
  /* ignore LOCATION_FORWARD here, that gets handled in the stub */
 errout:
  CORBA_exception_set_system(ev, ex_CORBA_MARSHAL,
			     CORBA_COMPLETED_MAYBE);
}

void
ORBit_send_system_exception (GIOPSendBuffer    *buf,
			     CORBA_Environment *ev)
{
	CORBA_SystemException *se = ev->_any._value;

	g_assert (ev->_major == CORBA_SYSTEM_EXCEPTION);

	giop_send_buffer_append_string (buf, ev->_id);
  
	giop_send_buffer_append_aligned (buf, &se->minor, 4);
	giop_send_buffer_append_aligned (buf, &se->completed, 4);
}

void
ORBit_send_user_exception (GIOPSendBuffer    *send_buffer,
			   CORBA_Environment *ev,
			   const ORBit_exception_marshal_info *user_exceptions)
{
	int i;

	for (i = 0; user_exceptions [i].tc != CORBA_OBJECT_NIL; i++) {
		if (!strcmp (user_exceptions [i].tc->repo_id, ev->_id))
			break;
	}

	if (user_exceptions[i].tc == CORBA_OBJECT_NIL) {
		CORBA_Environment fakeev;

		CORBA_exception_init (&fakeev);

		CORBA_exception_set_system (&fakeev, ex_CORBA_UNKNOWN,
					    CORBA_COMPLETED_MAYBE);
		ORBit_send_system_exception (send_buffer, &fakeev);

		CORBA_exception_free (&fakeev);
	} else {
		giop_send_buffer_append_string (send_buffer, ev->_id);

		if (user_exceptions[i].marshal && ev->_any._value)
			user_exceptions[i].marshal (send_buffer, ev);
	}
}
