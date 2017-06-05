/*
 * bonobo-exception.c: a generic exception -> user string converter.
 *
 * Authors:
 *   Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000 Ximian, Inc.
 */
#include <config.h>
#include <glib.h>
#include <string.h>

#define BONOBO_EXPLICIT_TRANSLATION_DOMAIN PACKAGE
#include <bonobo/bonobo-i18n.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-shutdown.h>
#include <bonobo/bonobo-exception.h>

typedef enum {
	EXCEPTION_STR,
	EXCEPTION_FN
} ExceptionType;

typedef struct {
	ExceptionType     type;
	char             *repo_id;
	char             *str;
	BonoboExceptionFn fn;
	gpointer          user_data;
	GDestroyNotify    destroy_fn;
} ExceptionHandle;

static GHashTable *bonobo_exceptions = NULL;

static gboolean
except_destroy (gpointer dummy, ExceptionHandle *e, gpointer dummy2)
{
	if (e) {
		if (e->type == EXCEPTION_FN &&
		    e->destroy_fn)
			e->destroy_fn (e->user_data);
		e->destroy_fn = NULL;
		g_free (e->repo_id);
		g_free (e->str);
		g_free (e);
	}
	return TRUE;
}

void
bonobo_exception_shutdown (void)
{
	if (bonobo_exceptions) {
		g_hash_table_foreach_remove (
			bonobo_exceptions, (GHRFunc) except_destroy, NULL);
		g_hash_table_destroy (bonobo_exceptions);
		bonobo_exceptions = NULL;
	}
}

static GHashTable *
get_hash (void)
{
	if (!bonobo_exceptions)
		bonobo_exceptions = g_hash_table_new (
			g_str_hash, g_str_equal);

	return bonobo_exceptions;
}

/**
 * bonobo_exception_add_handler_str:
 * @repo_id: exception repository id
 * @str: the user readable, translated exception text.
 * 
 * This routine adds a simple string mapping for an exception
 * with repository id @repo_id, such that when we call
 * bonobo_exception_get_text on an exception of id @repo_id we
 * get @str back.
 **/
void
bonobo_exception_add_handler_str (const char *repo_id, const char *str)
{
	ExceptionHandle *e;
	GHashTable *hash;

	g_return_if_fail (str != NULL);
	g_return_if_fail (repo_id != NULL);

	hash = get_hash ();

	e = g_new0 (ExceptionHandle, 1);

	e->type = EXCEPTION_STR;
	e->repo_id = g_strdup (repo_id);
	e->str = g_strdup (str);

	g_hash_table_insert (hash, e->repo_id, e);
}

/**
 * bonobo_exception_add_handler_fn:
 * @repo_id: exception repository id
 * @fn: function to make exception human readable
 * @user_data: the user data
 * @destroy_fn: user data destroy function or NULL.
 * 
 * This routine adds a method mapping for an exception
 * with repository id @repo_id, such that when we call
 * bonobo_exception_get_text on an exception of id @repo_id
 * the @fn is called and passed @user_data.
 * When the handler is removed the @destroy_fn is called
 * on its @user_data.
 **/
void
bonobo_exception_add_handler_fn (const char *repo_id,
				 BonoboExceptionFn fn,
				 gpointer          user_data,
				 GDestroyNotify    destroy_fn)
{
	ExceptionHandle *e;
	GHashTable *hash;

	g_return_if_fail (fn != NULL);
	g_return_if_fail (repo_id != NULL);

	hash = get_hash ();

	e = g_new0 (ExceptionHandle, 1);

	e->type = EXCEPTION_STR;
	e->repo_id = g_strdup (repo_id);
	e->fn = fn;
	e->user_data = user_data;
	e->destroy_fn = destroy_fn;

	g_hash_table_insert (hash, e->repo_id, e);
}

/**
 * bonobo_exception_repoid_to_text:
 * @repo_id: exception repository id
 * 
 *  This maps builtin bonobo exceptions that the system
 * knows about to user readable strings.
 * 
 * Return value: a user string or NULL for an unknown repo_id
 **/
char *
bonobo_exception_repoid_to_text  (const char *repo_id)
{
	/* Bonobo */ 
	if (!strcmp (repo_id, ex_Bonobo_NotSupported))
		return g_strdup (_("An unsupported action was attempted"));
	
	else if (!strcmp (repo_id, ex_Bonobo_IOError))
		return g_strdup (_("IO Error"));
	
	else if (!strcmp (repo_id, ex_Bonobo_BadArg))
		return g_strdup (_("Invalid argument value"));
	
	/* Bonobo::ItemContainer */
	else if (!strcmp (repo_id, ex_Bonobo_ItemContainer_NotFound))
		return g_strdup (_("Object not found"));

	else if (!strcmp (repo_id, ex_Bonobo_ItemContainer_SyntaxError))
		return g_strdup (_("Syntax error in object description"));

#if 0
	/* Bonobo::GenericFactory */
	else if (!strcmp (repo_id, ex_GNOME_ObjectFactory_CannotActivate))
		return g_strdup (_("Cannot activate object from factory"));
#endif

	/* Bonobo::Stream */
	else if (!strcmp (repo_id, ex_Bonobo_Stream_NoPermission))
		return g_strdup (_("No permission to access stream"));

	else if (!strcmp (repo_id, ex_Bonobo_Stream_NotSupported))
		return g_strdup (_("An unsupported stream action was attempted"));
	
	else if (!strcmp (repo_id, ex_Bonobo_Stream_IOError))
		return g_strdup (_("IO Error on stream"));

	/* Bonobo::Storage */
	else if (!strcmp (repo_id, ex_Bonobo_Storage_IOError))
		return g_strdup (_("IO Error on storage"));

	else if (!strcmp (repo_id, ex_Bonobo_Storage_NameExists))
		return g_strdup (_("Name already exists in storage"));

	else if (!strcmp (repo_id, ex_Bonobo_Storage_NotFound))
		return g_strdup (_("Object not found in storage"));

	else if (!strcmp (repo_id, ex_Bonobo_Storage_NoPermission))
		return g_strdup (_("No permission to do operation on storage"));
	else if (!strcmp (repo_id, ex_Bonobo_Storage_NotSupported))
		return g_strdup (_("An unsupported storage action was attempted"));
	else if (!strcmp (repo_id, ex_Bonobo_Storage_NotStream))
		return g_strdup (_("Object is not a stream"));

	else if (!strcmp (repo_id, ex_Bonobo_Storage_NotStorage))
		return g_strdup (_("Object is not a storage"));

	else if (!strcmp (repo_id, ex_Bonobo_Storage_NotEmpty))
		return g_strdup (_("Storage is not empty"));

	/* Bonobo::UIContainer */
	else if (!strcmp (repo_id, ex_Bonobo_UIContainer_MalformedXML))
		return g_strdup (_("malformed user interface XML description"));

	else if (!strcmp (repo_id, ex_Bonobo_UIContainer_InvalidPath))
		return g_strdup (_("invalid path to XML user interface element"));

	else if (!strcmp (repo_id, ex_Bonobo_UIContainer_NonExistentAttr))
		return g_strdup (_("the requested UI attribute didn't exist"));

	else if (!strcmp (repo_id, ex_Bonobo_UIContainer_Unknown))
		return g_strdup (_("Unknown command or verb"));

	else if (!strcmp (repo_id, ex_Bonobo_UIContainer_Insensitive))
		return g_strdup (_("Command is insensitive"));
		
	/* Bonobo::Persist */
	else if (!strcmp (repo_id, ex_Bonobo_Persist_WrongDataType))
		return g_strdup (_("incorrect data type"));

	else if (!strcmp (repo_id, ex_Bonobo_Persist_FileNotFound))
		return g_strdup (_("stream not found"));

	/* Bonobo::PropertyBag */
	else if (!strcmp (repo_id, ex_Bonobo_PropertyBag_NotFound))
		return g_strdup (_("property not found"));

	else if (!strcmp (repo_id, ex_Bonobo_PropertyBag_InvalidType))
		return g_strdup (_("property has invalid type"));

	else if (!strcmp (repo_id, ex_Bonobo_PropertyBag_ReadOnly))
		return g_strdup (_("property is read only"));

	else if (!strcmp (repo_id, ex_Bonobo_PropertyBag_BackendFailed))
		return g_strdup (_("config database backend failed "));

	/* Bonobo::Moniker */
	else if (!strcmp (repo_id, ex_Bonobo_Moniker_InterfaceNotFound))
		return g_strdup (_("Moniker interface cannot be found"));

	else if (!strcmp (repo_id, ex_Bonobo_Moniker_TimeOut))
		return g_strdup (_("Moniker activation timed out"));
		
	else if (!strcmp (repo_id, ex_Bonobo_Moniker_InvalidSyntax))
		return g_strdup (_("Syntax error within moniker"));

	else if (!strcmp (repo_id, ex_Bonobo_Moniker_UnknownPrefix))
		return g_strdup (_("Moniker has an unknown moniker prefix"));

	else
		return NULL;
}

/**
 * bonobo_exception_get_text:
 * @ev: the corba environment.
 * 
 * Returns a user readable description of the exception.  First
 * checks @ev against builtin Bonobo exceptions, then falls back to
 * exception names added through bonobo_exception_add_handler_str
 * or bonobo_exception_add_handler_fn.
 * 
 * Return value: A g_malloc'd description, which the caller must free.
 * NULL is never returned.
 **/
char *
bonobo_exception_get_text (CORBA_Environment *ev)
{
	char *rval;

	if (!ev || !BONOBO_EX (ev))
		return g_strdup (_("Error checking error; no exception"));

	if ((rval = bonobo_exception_repoid_to_text (ev->_id)))
		return rval;
	
	else if (!strcmp (ev->_id, "IDL:Bonobo/GeneralError:1.0")) {
		Bonobo_GeneralError *err = ev->_any._value;
		
		if (!err || !err->description)
			return g_strdup (_("General activation error with no description"));
		else
			return g_strdup (err->description);

	} else {
		ExceptionHandle *e;
		GHashTable *hash = get_hash ();
		char *str = NULL;
		
		if ((e = g_hash_table_lookup (hash, ev->_id))) {
			if (e->type == EXCEPTION_STR)
				str = g_strdup (e->str);
			else
				str = e->fn (ev, e->user_data);
		}

		if (str)
			return str;
		else
			return g_strdup_printf (
				"Unknown CORBA exception id: '%s'", 
				ev->_id);
	}
}

void
bonobo_exception_general_error_set (CORBA_Environment *ev,
				    CORBA_TypeCode     opt_deriv,
				    const char        *format,
				    ...)
{
	va_list              args;
	Bonobo_GeneralError *err;
	char                *str;
	CORBA_TypeCode       type;

	va_start (args, format);

	str = g_strdup_vprintf (format, args);

	va_end (args);

	if (opt_deriv)
		type = opt_deriv;
	else
		type = TC_Bonobo_GeneralError;

	err = ORBit_small_alloc (type);
	err->description = CORBA_string_dup (str);

	CORBA_exception_set (ev, CORBA_USER_EXCEPTION,
			     ex_Bonobo_GeneralError, err);
}

const char *
bonobo_exception_general_error_get (CORBA_Environment *ev)
{
	Bonobo_GeneralError *gerr;

	if (!BONOBO_EX (ev))
		return NULL;

	if (strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_GeneralError))
		return NULL;

	gerr = CORBA_exception_value (ev);

	return gerr->description;
}
