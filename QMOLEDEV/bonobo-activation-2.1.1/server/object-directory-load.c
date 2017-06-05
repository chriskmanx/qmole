/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  oafd: OAF CORBA dameon.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Elliot Lee <sopwith@redhat.com>
 *           Maciej Stachowiak <mjs@eazel.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#include <libxml/xmlmemory.h>

#include "bonobo-activation/bonobo-activation-i18n.h"
#include "server.h"

/* SAX Parser */
typedef enum {
        STATE_START,
        STATE_OAF_INFO,
        STATE_OAF_SERVER,
        STATE_OAF_ATTRIBUTE,
        STATE_ITEM,
        STATE_UNKNOWN,
        STATE_ERROR
} ParseState;

typedef struct {
        ParseState state;
        ParseState prev_state;
        int unknown_depth;
        
        const char *host;
        GSList **entries;
        
        Bonobo_ServerInfo *cur_server;
        Bonobo_ActivationProperty *cur_prop;
        GList *cur_props;
        GList *cur_items;
        
} ParseInfo;

#define IS_ELEMENT(x) (!strcmp (name, x))

static ParseInfo *
parse_info_new (void)
{
        ParseInfo *info = g_new0 (ParseInfo, 1);

        info->prev_state = STATE_UNKNOWN;
        info->state = STATE_START;
        
        return info;
}

static void
parse_info_free (ParseInfo *info)
{
        g_free (info);
}

static char *
od_validate (const char *iid, const char *type, const char *location)
{
        int i;

        if (iid == NULL) {
                return g_strdup (_("a NULL iid is not valid"));
        }

        if (type == NULL) {
                return g_strdup_printf (_("iid %s has a NULL type"), iid);
        }

        if (location == NULL) {
                return g_strdup_printf (_("iid %s has a NULL location"), iid);
        }

        for (i = 0; iid && iid [i]; i++) {
                char c = iid [i];

                if (c == ',' || c == '[' || c == ']' ||
                    /* Reserved for future expansion */
                    c == '!' || c == '#' || c == '|') {
                        return g_strdup_printf (_("invalid character '%c' in iid '%s'"),
                                                c, iid);
                }
        }

        return NULL;
}

static void
parse_oaf_server_attrs (ParseInfo      *info,
                        const xmlChar **attrs)
{
        const char *iid = NULL;
        const char *type = NULL;
        const char *location = NULL;
        const char *att, *val;
        char *error;
        int i = 0;

        info->state = STATE_OAF_SERVER;
        
        if (!attrs)
                return;

        do {
                att = attrs[i++];
                val = attrs[i++];

                if (att && val) {
                        if (!iid && !strcmp (att, "iid"))
                                iid = val;
                        else if (!type && !strcmp (att, "type"))
                                type = val;
                        else if (!location && !strcmp (att, "location"))
                                location = val;
                }
                
        } while (att && val);

        error = od_validate (iid, type, location);
        
        if (error != NULL) {
                /* FIXME: should syslog */
                g_print ("%s\n", error);
                
                g_free (error);

                return;
        }

        /* Now create the ServerInfo object */
        info->cur_server = g_new0 (Bonobo_ServerInfo, 1);

        info->cur_server->iid = CORBA_string_dup (iid);
        info->cur_server->server_type = CORBA_string_dup (type);
        info->cur_server->location_info = CORBA_string_dup (location);
        info->cur_server->hostname = CORBA_string_dup (info->host);
        info->cur_server->username = CORBA_string_dup (g_get_user_name ());
        info->cur_server->domain = CORBA_string_dup ("unused");
}

static GHashTable *interesting_locales = NULL;

void
add_initial_locales (void)
{
        const char *tmp;
        char *tmp2, *lang, *lang_with_locale, *equal_char;

        if (!interesting_locales) {
                interesting_locales = g_hash_table_new (
                        g_str_hash, g_str_equal);
        }
        lang_with_locale = NULL;
        
        tmp = g_getenv ("LANGUAGE");

        if (!tmp)
                tmp = g_getenv ("LANG");
        
        lang = g_strdup (tmp);
        tmp2 = lang;

        if (lang) {
                /* envs can be in NAME=VALUE form */
		equal_char = strchr (lang, '=');
		if (equal_char)
			lang = equal_char + 1;

                /* check if the locale has a _ */
                equal_char = strchr (lang, '_');
                if (equal_char != NULL) {
                        lang_with_locale = g_strdup (lang);
                        *equal_char = 0;
                }

                if (lang_with_locale && strcmp (lang_with_locale, "")) {
                        g_hash_table_insert (interesting_locales,
                                             lang_with_locale,
                                             GUINT_TO_POINTER (1));
#ifdef LOCALE_DEBUG
                        g_warning ("Init lang '%s'", lang_with_locale);
#endif
                }

                if (lang && strcmp (lang, "")) {
                        g_hash_table_insert (interesting_locales,
                                             g_strdup (lang),
                                             GUINT_TO_POINTER (1));
#ifdef LOCALE_DEBUG
                        g_warning ("Init lang(2) '%s'", lang);
#endif
                }
        }

        g_free (tmp2);
}

gboolean
register_interest_in_locales (const char *locales)
{
        int i;
        char **localev;
        gboolean new_locale = FALSE;

        localev = g_strsplit (locales, ",", 0);

        for (i = 0; localev[i]; i++) {
                if (!g_hash_table_lookup (interesting_locales, localev[i])) {
#ifdef LOCALE_DEBUG
                        g_warning ("New locale '%s' (%d)!",
                                   localev[i], g_list_length (locale_list));
#endif
                        g_hash_table_insert (interesting_locales,
                                             g_strdup (localev[i]),
                                             GUINT_TO_POINTER (1));
                        new_locale = TRUE;
                }
        }
        g_strfreev (localev);

        return new_locale;
}

static gboolean
is_locale_interesting (const char *name_with_locale)
{
        const char *locale;

        if (!name_with_locale)
                return FALSE;

        if (!(locale = strchr (name_with_locale, '-')))
                return TRUE;
        locale++;

        return g_hash_table_lookup (interesting_locales, locale) != NULL;
}

static gboolean 
od_string_to_boolean (const char *str)
{
	if (!g_ascii_strcasecmp (str, "true") ||
            !g_ascii_strcasecmp (str, "yes") ||
	    !strcmp (str, "1"))
		return TRUE;
	else
		return FALSE;
}

static void
parse_oaf_attribute (ParseInfo     *info,
                     const xmlChar **attrs)
{
        int i = 0;
        const char *type = NULL;
        const char *name = NULL;
        const char *value = NULL;
        const char *att, *val;
        
        g_assert (info->cur_server);

        info->state = STATE_OAF_ATTRIBUTE;
        
        if (!attrs)
                return;

        do {
                att = attrs[i++];
                val = attrs[i++];
                
                if (att && val) {
                        if (!strcmp (att, "type"))
                                type = val;

                        else if (!strcmp (att, "name")) {
                                name = val;
                                if (!is_locale_interesting (name))
                                        return;
                                
                        } else if (!strcmp (att, "value"))
                                value = val;
                }

        } while (att && val);

        if (!type || !name)
                return;
        
        if (name[0] == '_')
                g_error ("%s is an invalid property name "
                         "- property names beginning with '_' are reserved",
                         name);
        
        info->cur_prop = g_new0 (Bonobo_ActivationProperty, 1);
        info->cur_prop->name = CORBA_string_dup (name);

        if (g_ascii_strcasecmp (type, "stringv") == 0) {
                info->cur_prop->v._d = Bonobo_ACTIVATION_P_STRINGV;

        } else if (g_ascii_strcasecmp (type, "number") == 0) {
                info->cur_prop->v._d = Bonobo_ACTIVATION_P_NUMBER;
                info->cur_prop->v._u.value_number = atof (value);

        } else if (g_ascii_strcasecmp (type, "boolean") == 0) {
                info->cur_prop->v._d = Bonobo_ACTIVATION_P_BOOLEAN;
                info->cur_prop->v._u.value_boolean = od_string_to_boolean (value);

        } else {
                /* Assume string */
                info->cur_prop->v._d = Bonobo_ACTIVATION_P_STRING;
                if (value != NULL) {
                        info->cur_prop->v._u.value_string = CORBA_string_dup (value);
                } else {
                        g_warning (_("Property '%s' has no value"),
                                   info->cur_prop->name);
                        info->cur_prop->v._u.value_string =
                                CORBA_string_dup ("");
                }
        }
}

static void
parse_stringv_item (ParseInfo     *info,
                    const xmlChar **attrs)
{
        const char *value = NULL;
        const char *att, *val;
        int i = 0;

        if (!attrs)
                return;

        do {
                att = attrs[i++];
                val = attrs[i++];
                
                if (att && val) {
                        if (!value && !strcmp (att, "value")) {
                                value = val;
                                break;
                        }

                }
                
        } while (att && val);

        if (value) 
                info->cur_items = g_list_prepend (info->cur_items, CORBA_string_dup (value));

        info->state = STATE_ITEM;
        
}

static void
od_StartElement (ParseInfo     *info,
                 const xmlChar *name,
                 const xmlChar **attrs)
{
        switch (info->state) {
        case STATE_START:
                if (IS_ELEMENT ("oaf_info")) 
                        info->state = STATE_OAF_INFO;
                else {
                        info->prev_state = info->state;
                        info->state = STATE_UNKNOWN;
                        info->unknown_depth++;
                }
                break;
        case STATE_OAF_INFO:
                if (IS_ELEMENT ("oaf_server")) 
                        parse_oaf_server_attrs (info, attrs);
                else {
                        info->prev_state = info->state;
                        info->state = STATE_UNKNOWN;
                        info->unknown_depth++;
                }
                break;
        case STATE_OAF_SERVER:
                if (IS_ELEMENT ("oaf_attribute")) 
                        parse_oaf_attribute (info, attrs);
                else {
                        info->prev_state = info->state;
                        info->state = STATE_UNKNOWN;
                        info->unknown_depth++;
                }
                break;
        case STATE_OAF_ATTRIBUTE:
                if (IS_ELEMENT ("item"))
                        parse_stringv_item (info, attrs);
                else {
                        info->prev_state = info->state;
                        info->state = STATE_UNKNOWN;
                        info->unknown_depth++;
                }
                break;
        case STATE_UNKNOWN:
		info->unknown_depth++;
		break;
        case STATE_ERROR:
                break;
                break;
        default:
                g_error ("start element, unknown state: %d", info->state);
        }
}

static void
add_entry (ParseInfo *info)
{
        GSList *l;

        for (l = *(info->entries); l; l = l->next) {
                Bonobo_ServerInfo *si = l->data;

                if (!strcmp (si->iid, info->cur_server->iid))
                        return;
        }

        *(info->entries) = g_slist_prepend (*(info->entries), info->cur_server);
}

static void
od_EndElement (ParseInfo     *info,
               const xmlChar *name)
{

        switch (info->state) {
        case STATE_ITEM:
                info->state = STATE_OAF_ATTRIBUTE;
                break;
        case STATE_OAF_ATTRIBUTE: {
                if (info->cur_prop && info->cur_prop->v._d == Bonobo_ACTIVATION_P_STRINGV) {
                        gint i, len;
                        GList *p;
                        
                        len = g_list_length (info->cur_items);

                        info->cur_prop->v._u.value_stringv._length = len;
                        info->cur_prop->v._u.value_stringv._buffer =
                                CORBA_sequence_CORBA_string_allocbuf (len);
                        
                        for (i = 0, p = g_list_reverse (info->cur_items); p; p = p->next, i++)
                                info->cur_prop->v._u.
                                        value_stringv._buffer[i] = p->data;
                        g_list_free (info->cur_items);
                        info->cur_items = NULL;
                }

                if (info->cur_prop) {
                        info->cur_props = g_list_prepend (info->cur_props, info->cur_prop);
                        info->cur_prop = NULL;
                }
                
                info->state = STATE_OAF_SERVER;
                break;
        }
        case STATE_OAF_SERVER: {
                if (info->cur_server) {
                        GList *p;
                        gint len, i;

                        len = g_list_length (info->cur_props);

                        info->cur_server->props._length = len;
                        info->cur_server->props._buffer = g_new0 (Bonobo_ActivationProperty, len);

                        for (i = 0, p = g_list_reverse (info->cur_props); p; p = p->next, i++) {
                                info->cur_server->props._buffer[i] = *((Bonobo_ActivationProperty *)p->data);
                                g_free (p->data);
                        }
                        g_list_free (info->cur_props);
                        info->cur_props = NULL;

                        add_entry (info);
                        info->cur_server = NULL;
                }
                info->state = STATE_OAF_INFO;
                break;
        }
        case STATE_OAF_INFO: {
                info->state = STATE_START;
                break;
        }
        case STATE_UNKNOWN:
		info->unknown_depth--;
		if (info->unknown_depth == 0)
			info->state = info->prev_state;
		break;
        case STATE_START:
                break;
        default:
                g_error ("end element, unknown state: %d", info->state);
        }
}

static xmlEntityPtr
od_GetEntity (ParseState *ps, const xmlChar *name)
{
	return xmlGetPredefinedEntity (name);
}

static void
od_Warning (ParseInfo *ps,
            const char *msg,
            ...)
{
	va_list args;

	va_start (args, msg);
	g_logv   ("XML", G_LOG_LEVEL_WARNING, msg, args);
	va_end   (args);
}

static void
od_Error (ParseInfo *ps, const char *msg, ...)
{
	va_list args;

	va_start (args, msg);
	g_logv   ("XML", G_LOG_LEVEL_CRITICAL, msg, args);
	va_end   (args);
}

static void
od_FatalError (ParseInfo *ps, const char *msg, ...)
{
	va_list args;

	va_start (args, msg);
	g_logv   ("XML", G_LOG_LEVEL_ERROR, msg, args);
	va_end   (args);
}

static xmlSAXHandler od_SAXParser = {
	NULL, /* internalSubset */
	NULL, /* isStandalone */
	NULL, /* hasInternalSubset */
	NULL, /* hasExternalSubset */
	NULL, /* resolveEntity */
        (getEntitySAXFunc) od_GetEntity, /* getEntity */
	NULL, /* entityDecl */
	NULL, /* notationDecl */
	NULL, /* attributeDecl */
	NULL, /* elementDecl */
	NULL, /* unparsedEntityDecl */
	NULL, /* setDocumentLocator */
	NULL, /* startDocument */
	(endDocumentSAXFunc) NULL, /* endDocument */
	(startElementSAXFunc) od_StartElement, /* startElement */
	(endElementSAXFunc) od_EndElement, /* endElement */
	NULL, /* reference */
	NULL, /* characters */
	NULL, /* ignorableWhitespace */
	NULL, /* processingInstruction */
	NULL, /* comment */
	(warningSAXFunc) od_Warning, /* warning */
	(errorSAXFunc) od_Error, /* error */
	(fatalErrorSAXFunc) od_FatalError, /* fatalError */
};

static void
od_load_file (const char *file,
              GSList    **entries,
              const char *host)
{
        ParseInfo *info;
	xmlSAXHandlerPtr oldsax;
        xmlParserCtxt *ctxt;
        int ret = 0;
        
        info = parse_info_new ();
        info->host = host;
        info->entries = entries;

        ctxt = xmlCreateFileParserCtxt (file);
        oldsax = ctxt->sax;
        ctxt->sax = &od_SAXParser;
        ctxt->userData = info;
        /* Magic to make entities work as expected */
	ctxt->replaceEntities = TRUE;

        xmlParseDocument (ctxt);

        if (ctxt->wellFormed)
                ret = 0;
        else {
                if (ctxt->errNo != 0)
                        ret = ctxt->errNo;
                else
                        ret = -1;
        }
        ctxt->sax = oldsax;
        xmlFreeParserCtxt (ctxt);

        parse_info_free (info);

        if (ret < 0) {
                /* FIXME: syslog the error */
                return;
        }
}

static gboolean
od_filename_has_extension (const char *filename,
                           const char *extension)
{
        char *last_dot;
        
        last_dot = strrchr (filename, '.');

        return last_dot != NULL && strcmp (last_dot, extension) == 0;
}

static void
od_load_directory (const char *directory,
                   GSList    **entries,
                   const char *host)
{
	DIR *directory_handle;
	struct dirent *directory_entry;
        char *pathname;

        
        /* FIXME: Should be a syslog message. */
        /* g_print (_("Trying dir %s\n"), directory); */

        directory_handle = opendir (directory);

        if (directory_handle == NULL) {
                /* FIXME */
                return;
        }
        
        for (directory_entry = readdir (directory_handle);
             directory_entry != NULL;
             directory_entry = readdir (directory_handle)) {
                pathname = g_strdup_printf ("%s/%s", directory, directory_entry->d_name);

                if (od_filename_has_extension (pathname, ".server")) {
                        od_load_file (pathname, entries, host);
                }

		g_free (pathname);
        }

        closedir (directory_handle);
}


void
bonobo_server_info_load (char **directories,
                         Bonobo_ServerInfoList   *servers,
                         GHashTable **iid_to_server_info_map,
                         const char *host)
{
	GSList *entries;
        int length;
        GSList *p;
	int i, j; 
        
	g_return_if_fail (directories);
	g_return_if_fail (iid_to_server_info_map);

        entries = NULL;

	if (*iid_to_server_info_map != NULL) {
		g_hash_table_destroy (*iid_to_server_info_map);
        }

	*iid_to_server_info_map = g_hash_table_new (g_str_hash, g_str_equal);

        /* Load each directory */
	for (i = 0; directories[i] != NULL; i++)
                od_load_directory (directories[i], &entries, host);

	/* Now convert 'entries' into something that the server can store and pass back */
	length = g_slist_length (entries);

	servers->_buffer = CORBA_sequence_Bonobo_ServerInfo_allocbuf (length);
        servers->_length = length;

	for (j = 0, p = entries; j < length; j++, p = p->next) {
		memcpy (&servers->_buffer[j], p->data, sizeof (Bonobo_ServerInfo));
		g_hash_table_insert (*iid_to_server_info_map,
                                     servers->_buffer[j].iid,
                                     &servers->_buffer[j]);
	}

        g_slist_foreach (entries, (GFunc) g_free, NULL);
        g_slist_free (entries);
}
