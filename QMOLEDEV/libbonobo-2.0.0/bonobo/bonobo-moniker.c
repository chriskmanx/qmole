/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker: Object naming abstraction
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000, Ximian, Inc.
 */
#include "config.h"
#include <string.h>

#define BONOBO_EXPLICIT_TRANSLATION_DOMAIN PACKAGE
#include <bonobo/bonobo-i18n.h>
#include <bonobo/bonobo-exception.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-util.h>
#include <bonobo/bonobo-moniker-extender.h>

struct _BonoboMonikerPrivate {
	Bonobo_Moniker parent;

	int            prefix_len;
	char          *prefix;

	char          *name;

	gboolean       sensitive;
};

#define PARENT_TYPE BONOBO_TYPE_OBJECT

static GObjectClass *bonobo_moniker_parent_class;

#define CLASS(o) BONOBO_MONIKER_CLASS (G_OBJECT_GET_CLASS (o))

static inline BonoboMoniker *
bonobo_moniker_from_servant (PortableServer_Servant servant)
{
	return BONOBO_MONIKER (bonobo_object_from_servant (servant));
}

static Bonobo_Moniker
impl_get_parent (PortableServer_Servant servant,
		 CORBA_Environment     *ev)
{
	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);

	return bonobo_moniker_get_parent (moniker, ev);
}

static void
impl_set_parent (PortableServer_Servant servant,
 		 const Bonobo_Moniker   value,
 		 CORBA_Environment     *ev)
{
 	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);

	bonobo_moniker_set_parent (moniker, value, ev);
}
 
/**
 * bonobo_moniker_set_parent:
 * @moniker: the moniker
 * @parent: the parent
 * @opt_ev: an optional corba exception environment
 * 
 *  This sets the monikers parent; a moniker is really a long chain
 * of hierarchical monikers; referenced by the most local moniker.
 * This sets the parent pointer.
 **/
void
bonobo_moniker_set_parent (BonoboMoniker     *moniker,
 			   Bonobo_Moniker     parent,
			   CORBA_Environment *opt_ev)
{
	CORBA_Environment ev, *my_ev;

 	bonobo_return_if_fail (BONOBO_IS_MONIKER (moniker), opt_ev);

	if (!opt_ev) {
		CORBA_exception_init (&ev);
		my_ev = &ev;
	} else
		my_ev = opt_ev;
	
	if (moniker->priv->parent != CORBA_OBJECT_NIL) {
		Bonobo_Moniker_setParent (moniker->priv->parent, parent, my_ev);
	} else
		moniker->priv->parent = bonobo_object_dup_ref (parent, my_ev);
	
	if (!opt_ev)
		CORBA_exception_free (&ev);
}
 
/**
 * bonobo_moniker_get_parent:
 * @moniker: the moniker
 * @opt_ev: an optional corba exception environment
 * 
 * See bonobo_moniker_set_parent;
 *
 * Return value: the parent of this moniker
 **/
Bonobo_Moniker
bonobo_moniker_get_parent (BonoboMoniker     *moniker,
			   CORBA_Environment *opt_ev)
{
	CORBA_Environment ev, *my_ev;
	Bonobo_Moniker rval;

	bonobo_return_val_if_fail (BONOBO_IS_MONIKER (moniker),
				   CORBA_OBJECT_NIL, opt_ev);
	
	if (moniker->priv->parent == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;

	if (!opt_ev) {
		CORBA_exception_init (&ev);
		my_ev = &ev;
	} else
		my_ev = opt_ev;

	rval = bonobo_object_dup_ref (moniker->priv->parent, opt_ev);

	if (!opt_ev)
		CORBA_exception_free (&ev);

	return rval;
}

/**
 * bonobo_moniker_get_name:
 * @moniker: the moniker
 * 
 * gets the unescaped name of the moniker less the prefix eg
 * file:/tmp/hash\#.gz returns /tmp/hash#.gz
 * 
 * Return value: the string or NULL in case of failure
 **/
const char *
bonobo_moniker_get_name (BonoboMoniker *moniker)
{	
	const char *str;

	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), NULL);

	str = CLASS (moniker)->get_internal_name (moniker);

	if (str)
		return str + moniker->priv->prefix_len;
	else
		return "";
}

/**
 * bonobo_moniker_get_name_full:
 * @moniker: the moniker
 * 
 * gets the full unescaped name of the moniker eg.
 * file:/tmp/hash\#.gz returns file:/tmp/hash#.gz
 * 
 * Return value: the string in case of failure
 **/
const char *
bonobo_moniker_get_name_full (BonoboMoniker *moniker)
{	
	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), NULL);

	return CLASS (moniker)->get_internal_name (moniker);
}

/**
 * bonobo_moniker_get_name_escaped:
 * @moniker: a moniker
 * 
 * Get the full, escaped name of the moniker eg.
 * file:/tmp/hash\#.gz returns file:/tmp/hash\#.gz
 * 
 * Return value: the dynamically allocated string,
 * or NULL in case of failure.
 * Must release with g_free().
 **/
char *
bonobo_moniker_get_name_escaped (BonoboMoniker *moniker)
{
	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), NULL);

	return bonobo_moniker_util_escape (
		CLASS (moniker)->get_internal_name (moniker), 0);
}

/**
 * bonobo_moniker_set_name:
 * @moniker: the BonoboMoniker to configure.
 * @name: new name for this moniker.
 * @num_chars: number of characters in name to copy.
 *
 * This functions sets the moniker name in @moniker to be @name.
 */
void
bonobo_moniker_set_name (BonoboMoniker *moniker,
			 const char    *name)
{
	char *str;

	g_return_if_fail (BONOBO_IS_MONIKER (moniker));

	str = bonobo_moniker_util_unescape (name, strlen (name));

	CLASS (moniker)->set_internal_name (moniker, str);

	g_free (str);
}

/**
 * bonobo_moniker_get_prefix:
 * @moniker: a moniker
 * 
 * Return value: the registered prefix for this moniker or
 * NULL if there isn't one. eg "file:", or in case of failure
 **/
const char *
bonobo_moniker_get_prefix (BonoboMoniker *moniker)
{
	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), NULL);

	return moniker->priv->prefix;
}

static void
impl_bonobo_moniker_set_internal_name (BonoboMoniker *moniker,
				       const char    *unescaped_name)
{
	g_return_if_fail (BONOBO_IS_MONIKER (moniker));
	g_return_if_fail (strlen (unescaped_name) >= moniker->priv->prefix_len);

	g_free (moniker->priv->name);
	moniker->priv->name = g_strdup (unescaped_name);
}

static const char *
impl_bonobo_moniker_get_internal_name (BonoboMoniker *moniker)
{
	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), NULL);

	return moniker->priv->name;
}

static CORBA_char *
impl_get_name (PortableServer_Servant servant,
	       CORBA_Environment     *ev)
{
	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);
	CORBA_char    *ans, *parent_name;
	char          *tmp;
	
	parent_name = Bonobo_Moniker_getName (moniker->priv->parent, ev);

	if (BONOBO_EX (ev))
		return NULL;

	if (!parent_name)
		return CORBA_string_dup (moniker->priv->name);

	if (!moniker->priv->name)
		return parent_name;

	tmp = g_strdup_printf ("%s%s", parent_name, moniker->priv->name);

	if (parent_name)
		CORBA_free (parent_name);

	ans = CORBA_string_dup (tmp);

	g_free (tmp);
	
	return ans;
}

static void
impl_set_name (PortableServer_Servant servant,
	       const CORBA_char      *name,
	       CORBA_Environment     *ev)
{
	const char    *mname;
	int            plen;
	Bonobo_Moniker parent;
	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);

	bonobo_return_if_fail (moniker->priv != NULL, ev);
	bonobo_return_if_fail (strlen (name) >= moniker->priv->prefix_len, ev);

	mname = bonobo_moniker_util_parse_name (name, &plen);

	if (plen) {
		char *pname;

		pname = g_strndup (name, plen);

		parent = bonobo_moniker_client_new_from_name (pname, ev);
		
		g_free (pname);

		if (BONOBO_EX (ev))
			return;

		bonobo_object_release_unref (moniker->priv->parent, NULL);
		moniker->priv->parent = bonobo_object_dup_ref (parent, ev);
	}

	bonobo_moniker_set_name (moniker, mname);
}

static Bonobo_Unknown
impl_resolve (PortableServer_Servant       servant,
	      const Bonobo_ResolveOptions *options,
	      const CORBA_char            *requested_interface,
	      CORBA_Environment           *ev)
{
	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);
	Bonobo_Unknown retval;

	/* Try a standard resolve */
	retval = CLASS (moniker)->resolve (moniker, options,
					   requested_interface, ev);

	/* Try an extender */
	if (!BONOBO_EX (ev) && retval == CORBA_OBJECT_NIL &&
	    moniker->priv->prefix) {

		Bonobo_Unknown extender;
		
		extender = bonobo_moniker_find_extender (
			moniker->priv->prefix,
			requested_interface, ev);
		
		if (BONOBO_EX (ev))
			return CORBA_OBJECT_NIL;

		else if (extender != CORBA_OBJECT_NIL) {
			retval = Bonobo_MonikerExtender_resolve (
				extender, BONOBO_OBJREF (moniker),
				options, moniker->priv->name,
				requested_interface, ev);

			bonobo_object_release_unref (extender, ev);
		}
	}

	if (!BONOBO_EX (ev) && retval == CORBA_OBJECT_NIL) {
		bonobo_exception_general_error_set (
			ev, NULL, _("Failed to resolve, or extend '%s"),
			bonobo_moniker_get_name_full (moniker));
	}

	return retval;
}

static CORBA_long
impl_equal (PortableServer_Servant servant,
	    const CORBA_char      *moniker_name,
	    CORBA_Environment     *ev)
{
	int            i, retval;
	CORBA_long     offset;
	const char    *p;
	char          *name;
	BonoboMoniker *moniker = bonobo_moniker_from_servant (servant);
	
	if (moniker->priv->parent != CORBA_OBJECT_NIL) {
		offset = Bonobo_Moniker_equal (
			moniker->priv->parent, moniker_name, ev);
		if (BONOBO_EX (ev) || offset == 0)
			return 0;
	} else
		offset = 0;
	
	p = &moniker_name [offset];

	i = bonobo_moniker_util_seek_std_separator (p, moniker->priv->prefix_len);
	
	name = bonobo_moniker_get_name_escaped (moniker);

/*	g_warning ("Compare %d chars of '%s' to '%s' - case sensitive ?%c",
	i, name, p, moniker->priv->sensitive?'y':'n');*/
	
	if (( moniker->priv->sensitive && !strncmp       (name, p, i)) ||
	    (!moniker->priv->sensitive && !g_ascii_strncasecmp (name, p, i))) {
/*		g_warning ("Matching moniker - equal");*/
		retval = i + offset;
	} else {
/*		g_warning ("No match");*/
		retval = 0;
	}
 
	g_free (name);
 
	return retval;
}

static void
bonobo_moniker_finalize (GObject *object)
{
	BonoboMoniker *moniker = BONOBO_MONIKER (object);

	if (moniker->priv->parent != CORBA_OBJECT_NIL)
		bonobo_object_release_unref (moniker->priv->parent, NULL);

	g_free (moniker->priv->prefix);
	g_free (moniker->priv->name);
	g_free (moniker->priv);

	bonobo_moniker_parent_class->finalize (object);
}

static void
bonobo_moniker_class_init (BonoboMonikerClass *klass)
{
	GObjectClass *oclass = (GObjectClass *)klass;
	POA_Bonobo_Moniker__epv *epv = &klass->epv;

	bonobo_moniker_parent_class = g_type_class_peek_parent (klass);

	oclass->finalize = bonobo_moniker_finalize;

	klass->set_internal_name = impl_bonobo_moniker_set_internal_name;
	klass->get_internal_name = impl_bonobo_moniker_get_internal_name;

	epv->getParent           = impl_get_parent;
	epv->setParent           = impl_set_parent;
	epv->getName             = impl_get_name;
	epv->setName             = impl_set_name;
	epv->resolve             = impl_resolve;
	epv->equal               = impl_equal;
}

static void
bonobo_moniker_init (GObject *object)
{
	BonoboMoniker *moniker = BONOBO_MONIKER (object);

	moniker->priv = g_new (BonoboMonikerPrivate, 1);

	moniker->priv->parent = CORBA_OBJECT_NIL;
	moniker->priv->name   = NULL;
	moniker->priv->prefix = NULL;
}

BONOBO_TYPE_FUNC_FULL (BonoboMoniker, 
		       Bonobo_Moniker,
		       PARENT_TYPE,
		       bonobo_moniker);

/**
 * bonobo_moniker_construct:
 * @moniker: an un-constructed moniker object.
 * @corba_moniker: a CORBA handle inheriting from Bonobo::Moniker, or
 * CORBA_OBJECT_NIL, in which case a base Bonobo::Moniker is created.
 * @prefix: the prefix name of the moniker eg. 'file:', '!' or 'tar:' or NULL
 * @shlib_id:
 * 
 *  Constructs a newly created bonobo moniker with the given arguments.
 * 
 * Return value: the constructed moniker or NULL on failure.
 **/
BonoboMoniker *
bonobo_moniker_construct (BonoboMoniker *moniker,
			  const char    *prefix)
{
	if (prefix) {
		moniker->priv->prefix = g_strdup (prefix);
		moniker->priv->prefix_len = strlen (prefix);
	}

	moniker->priv->sensitive = TRUE;

	return moniker;
}

/**
 * bonobo_moniker_set_case_sensitive:
 * @moniker: the moniker
 * @sensitive: whether to see case on equality compare
 * 
 * Sets up whether we use case sensitivity for the default equal impl.
 **/
void
bonobo_moniker_set_case_sensitive (BonoboMoniker *moniker,
				   gboolean       sensitive)
{
	g_return_if_fail (BONOBO_IS_MONIKER (moniker));

	moniker->priv->sensitive = sensitive;
}

/**
 * bonobo_moniker_get_case_sensitive:
 * @moniker: the moniker
 *
 * Return value: whether we use case sensitivity for the default equal impl.
 **/
gboolean
bonobo_moniker_get_case_sensitive (BonoboMoniker *moniker)
{
	g_return_val_if_fail (BONOBO_IS_MONIKER (moniker), FALSE);

	return moniker->priv->sensitive;
}
