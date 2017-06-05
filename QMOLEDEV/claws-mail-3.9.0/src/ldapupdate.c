/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 Michael Rasmussen and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * Functions necessary to access LDAP servers.
 */

/*
 * Outstanding bugs
 * 1) When adding a contact to an empty addressbook from the pop-up menu
 * when right-clicking on an email address causes claws-mail to crash in
 * addritem.c line 965. Severity: Show stopper. Solved in 2.9.2cvs17
 * 2) Updating a contact gets lost if the user makes a new search on the
 * same LdapServer. Severity: Medium. Solved in 2.9.2cvs17 (patch added to solve 1) also solved this bug)
 * 3) After adding a new contact the displayName for the contact is empty
 * until the user makes a reread from the LdapServer. Severity: minor.
 * Solved in 2.9.2cvs24
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_LDAP

#include <glib.h>
#include <glib/gi18n.h>
#include <sys/time.h>
#include <string.h>

#include "ldapupdate.h"
#include "mgutils.h"
#include "addritem.h"
#include "addrcache.h"
#include "ldapctrl.h"
#include "ldapquery.h"
#include "ldapserver.h"
#include "ldaputil.h"
#include "utils.h"
#include "adbookbase.h"
#include "editaddress_other_attributes_ldap.h"

/**
 * Structure to hold user defined attributes
 * from contacts
 */
typedef struct _AttrKeyValue AttrKeyValue;
struct _AttrKeyValue {
	gchar *key;
	gchar *value;
};

/**
 * Structure to hold contact information.
 * Each addressbook will have 0..N contacts.
 */
typedef struct _EmailKeyValue EmailKeyValue;
struct _EmailKeyValue {
	gchar *mail;
	gchar *alias;
	gchar *remarks;
};

/**
 * Structure to hold information about RDN.
 */
typedef struct _Rdn Rdn;
struct _Rdn {
	gchar *attribute;
	gchar *value;
	gchar *new_dn;
};

/**
 * Retrieve address group item for update.
 * \param group  Group to print.
 * \param array  GHashTAble of item_group, or <i>NULL</i> if none created.
 */
void ldapsvr_retrieve_item_group(ItemGroup *group, GHashTable *array) {
	/* Not implemented in this release */
	cm_return_if_fail(group != NULL);
}

/**
 * Create an initial EmailKeyValue structure
 * \return empty structure
 */
EmailKeyValue *emailkeyvalue_create() {
	EmailKeyValue *buf;

	buf = g_new0(EmailKeyValue, 1);
	buf->alias = NULL;
	buf->mail = NULL;
	buf->remarks = NULL;
	return buf;
}

/**
 * Create an initial AttrKeyValue structure
 * \return empty structure
 */
AttrKeyValue *attrkeyvalue_create() {
	AttrKeyValue *buf;

	buf = g_new0(AttrKeyValue, 1);
	buf->key = NULL;
	buf->value = NULL;
	return buf;
}

/**
 * Free created AttrKeyValue structure
 * \param akv AttrKeyValue structure to free
 */
void attrkeyvalue_free(AttrKeyValue *akv) {
	if (akv->key) {
		g_free(akv->key);
		akv->key = NULL;
	}
	if (akv->value) {
		g_free(akv->value);
		akv->value = NULL;
	}
	g_free(akv);
	akv = NULL;
}

/**
 * Retrieve E-Mail address object for update.
 * \param item   ItemEmail to update.
 * \return object, or <i>NULL</i> if none created.
 */
EmailKeyValue *ldapsvr_retrieve_item_email(ItemEMail *item) {
	EmailKeyValue *newItem;
	cm_return_val_if_fail(item != NULL, NULL);
	newItem = emailkeyvalue_create();		
	newItem->alias = g_strdup(ADDRITEM_NAME(item));
	newItem->mail = g_strdup(item->address);
	newItem->remarks = g_strdup(item->remarks);
	return newItem;
}

/**
 * Retrieve user attribute object for update.
 * \param item   UserAttribute to update.
 * \return object, or <i>NULL</i> if none created.
 */
AttrKeyValue *ldapsvr_retrieve_attribute(UserAttribute *item) {
	AttrKeyValue *newItem;
	cm_return_val_if_fail(item != NULL, NULL);
	newItem = attrkeyvalue_create();
	newItem->key = g_strdup(item->name);
	newItem->value = g_strdup(item->value);
	return newItem;
}

/**
 * Retrieve person object for update.
 * \param person ItemPerson to update.
 * \param array GHashTable with user input.
 * \return false if update is not needed, or true if update is needed.
 */
gboolean ldapsvr_retrieve_item_person(ItemPerson *person, GHashTable *array) {
	GList *node, *attr;

	cm_return_val_if_fail(person != NULL, FALSE);
	switch (person->status) {
		case NONE: return FALSE;
		case ADD_ENTRY: g_hash_table_insert(array, "status", "new"); break;
		case UPDATE_ENTRY: g_hash_table_insert(array, "status", "update"); break;
		case DELETE_ENTRY: g_hash_table_insert(array, "status", "delete"); break;
		default: g_critical(_("ldapsvr_retrieve_item_person->Unknown status: %d"), person->status);
	}
	g_hash_table_insert(array, "uid", ADDRITEM_ID(person));
	g_hash_table_insert(array, "cn", ADDRITEM_NAME(person));
	g_hash_table_insert(array, "givenName", person->firstName);
	g_hash_table_insert(array, "sn", person->lastName);
	g_hash_table_insert(array, "nickName", person->nickName);
	g_hash_table_insert(array, "dn", person->externalID);
	g_hash_table_insert(array, "person", person);
	node = person->listEMail;
	attr = NULL;
	while (node) {
		EmailKeyValue *newEmail = ldapsvr_retrieve_item_email(node->data);
		if (newEmail)
			attr = g_list_append(attr, newEmail);
		node = g_list_next(node);
	}
	g_hash_table_insert(array, "mail", attr);
	node = person->listAttrib;
	attr = NULL;
	while (node) {
		AttrKeyValue *newAttr = ldapsvr_retrieve_attribute(node->data);
		if (newAttr)
			attr = g_list_append(attr, newAttr);
		node = g_list_next(node);
	}
	g_hash_table_insert(array, "attribute", attr);
	return TRUE;
}

/**
 * Print contents of contacts hashtable for debug.
 * This function must be called with g_hash_table_foreach.
 * \param key Key to process.
 * \param data Data to process.
 * \param fd Output stream.
 */
void ldapsvr_print_contacts_hashtable(gpointer key, gpointer data, gpointer fd) {
	gchar *keyName = (gchar *) key;
	GList *node;

	if (g_ascii_strcasecmp("mail", keyName) == 0) {
		node = (GList *) data;
		while (node) {
			EmailKeyValue *item = node->data;
			if (debug_get_mode()) {
				debug_print("\t\talias = %s\n", item->alias?item->alias:"null");
				debug_print("\t\tmail = %s\n", item->mail?item->mail:"null");
				debug_print("\t\tremarks = %s\n", item->remarks?item->remarks:"null");
			}
			else if (fd) {
				FILE *stream = (FILE *) fd;
				fprintf(stream, "\t\talias = %s\n", item->alias?item->alias:"null");
				fprintf(stream, "\t\tmail = %s\n", item->mail?item->mail:"null");
				fprintf(stream, "\t\tremarks = %s\n", item->remarks?item->remarks:"null");
			}
			node = g_list_next(node);
		}
	}
	else if (g_ascii_strcasecmp("attribute", keyName) == 0) {
		node = (GList *) data;
		while (node) {
			AttrKeyValue *item = node->data;
			if (debug_get_mode()) {
				debug_print("\t\t%s = %s\n", item->key?item->key:"null",
						item->value?item->value:"null");
			}
			else if (fd) {
				FILE *stream = (FILE *) fd;
				fprintf(stream, "\t\t%s = %s\n", item->key?item->key:"null",
						item->value?item->value:"null");
			}
			node = g_list_next(node);
		}
	}
	else {
		if (debug_get_mode())
			debug_print("\t\t%s = %s\n", keyName?keyName:"null", data?(gchar *)data:"null");
		else if (fd) {
			FILE *stream = (FILE *) fd;
			fprintf(stream, "\t\t%s = %s\n", keyName?keyName:"null", data?(gchar *)data:"null");
		}
	}
}

/**
 * Free list of changed contacts
 *
 * \param list List of GHashTable
 */
void ldapsvr_free_hashtable(GList *list) {
	GList *tmp = list;
	while (tmp) {
		g_hash_table_destroy(tmp->data);
		tmp->data = NULL;
		tmp = g_list_next(tmp);
	}
	g_list_free(list);
	list = NULL;
}

/**
 * Get person object from cache
 *
 * \param server Resource to LDAP
 * \param uid PersonID in cache
 * \return person object, or <i>NULL</i> if fail
 */
ItemPerson *ldapsvr_get_contact(LdapServer *server, gchar *uid) {
	AddrItemObject *aio;
	cm_return_val_if_fail(server != NULL || uid != NULL, NULL);
	aio = addrcache_get_object(server->addressCache, uid);
	if (aio) {
		if(aio->type == ITEMTYPE_PERSON) {
			return (ItemPerson *) aio;
		}
	}
	return NULL;
}

/**
 * Create an initial Rdn structure
 *
 * \return empty structure
 */
Rdn *rdn_create() {
	Rdn *buf;

	buf = g_new0(Rdn, 1);
	buf->attribute = NULL;
	buf->value = NULL;
	buf->new_dn = NULL;
	return buf;
}

/**
 * Free a created Rdn structure
 * \param rdn Structure to free
 */
void rdn_free(Rdn *rdn) {
	if (rdn->attribute) {
		g_free(rdn->attribute);
		rdn->attribute = NULL;
	}
	if (rdn->value) {
		g_free(rdn->value);
		rdn->value = NULL;
	}
	if (rdn->new_dn) {
		g_free(rdn->new_dn);
		rdn->new_dn = NULL;
	}
	g_free(rdn);
	rdn = NULL;
}

/**
 * update Rdn structure
 *
 * \param rdn Rdn structure to update
 * \param head Uniq part of dn
 * \param tail Common part of dn
 */
void update_rdn(Rdn *rdn, gchar *head, gchar *tail) {
	rdn->value = g_strdup(head);
	rdn->new_dn = g_strdup_printf("%s=%s%s", rdn->attribute, head, tail);
}

/**
 * Deside if dn needs to be changed
 *
 * \param hash GHashTable with user input.
 * \param dn dn for current object
 * \return Rdn structure
 */
Rdn *ldapsvr_modify_dn(GHashTable *hash, gchar *dn) {
	Rdn *rdn;
	gchar *pos, *compare;
	gchar *rest;
	gchar *val;
	cm_return_val_if_fail(hash != NULL || dn != NULL, NULL);
	
	pos = g_strstr_len(dn, strlen(dn), "=");
	if (!pos)
		return NULL;

	compare = g_strndup(dn, pos - dn);

	pos++;
	rest = g_strstr_len(pos, strlen(pos), ",");
	val = g_strndup(pos, rest - pos);
	if (val == NULL) {
		if (compare)
			g_free(compare);
		return NULL;
	}
	rdn = rdn_create();
	rdn->value = val;
	rdn->attribute = compare;

	if (strcmp("mail", rdn->attribute) == 0) {
		GList *list = g_hash_table_lookup(hash, rdn->attribute);
		while (list) {
			EmailKeyValue *item = list->data;
			compare = (gchar *) item->mail;
			if (strcmp(compare, rdn->value) == 0) {
				update_rdn(rdn, compare, rest);
				return rdn;
			}
			list = g_list_next(list);
		}
		/* if compare and rdn->attribute are equal then last email removed/empty  */
		if (strcmp(compare, rdn->attribute) != 0) {
	 		/* RDN changed. Find new */
			update_rdn(rdn, compare, rest);
			return rdn;
		}
		else {
			/* We cannot remove dn */
			rdn_free(rdn);
			return NULL;
		}
	}
	else {
		compare = g_hash_table_lookup(hash, rdn->attribute);
		/* if compare and rdn->attribute are equal then dn removed/empty */
		if (strcmp(compare, rdn->attribute) != 0) {
			update_rdn(rdn, compare, rest);
			return rdn;
		}
		else {
			/* We cannot remove dn */
			rdn_free(rdn);
			return NULL;
		}
	}
	rdn_free(rdn);
	return NULL;
}

/**
 * This macro is borrowed from the Balsa project
 * Creates a LDAPMod structure
 *
 * \param mods Empty LDAPMod structure
 * \param modarr Array with values to insert
 * \param op Operation to perform on LDAP
 * \param attr Attribute to insert
 * \param strv Empty array which is NULL terminated
 * \param val Value for attribute
 */
#define SETMOD(mods,modarr,op,attr,strv,val) \
   do { (mods) = &(modarr); (modarr).mod_type=attr; (modarr).mod_op=op;\
        (strv)[0]=(val); (modarr).mod_values=strv; \
      } while(0)

/**
 * Creates a LDAPMod structure
 *
 * \param mods Empty LDAPMod structure
 * \param modarr Array with values to insert
 * \param op Operation to perform on LDAP
 * \param attr Attribute to insert
 * \param strv Array with values to insert. Must be NULL terminated
 */
#define SETMODS(mods,modarr,op,attr,strv) \
   do { (mods) = &(modarr); (modarr).mod_type=attr; \
	   	(modarr).mod_op=op; (modarr).mod_values=strv; \
      } while(0)
#define MODSIZE 10

/**
 * Clean up, close LDAP connection, and refresh cache
 *
 * \param ld Resource to LDAP
 * \param server AddressBook resource
 * \param contact GHashTable with current changed object
 */
void clean_up(LDAP *ld, LdapServer *server, GHashTable *contact) {
	ItemPerson *person = 
		ldapsvr_get_contact(server, g_hash_table_lookup(contact , "uid"));
	if (person) {
		gchar *displayName;
		person->status = NONE;
		displayName = g_hash_table_lookup(contact, "displayName");
		if (displayName)
			person->nickName = g_strdup(displayName);
	}
	if (server->retVal != LDAPRC_SUCCESS) {
		if (person) {
			ItemPerson *res = 
				addrcache_remove_person(server->addressCache, person);
			if (!res)
				g_critical(N_("ldapsvr_update_book: Could not clean cache\n"));
			else
				addritem_free_item_person(res);
		}
	}
	if (ld)
		ldapsvr_disconnect(ld);
}

/**
 * Get cn attribute from dn
 *
 * \param dn Distinguesh Name for current object
 * \return AttrKeyValue, or <i>NULL</i> if none created
 */
AttrKeyValue *get_cn(gchar *dn) {
	AttrKeyValue *cn;
	gchar *start;
	gchar *end;
	gchar *item;
	gchar **key_value;
	cm_return_val_if_fail(dn != NULL, NULL);
	
	cn = attrkeyvalue_create();
	start = g_strstr_len(dn, strlen(dn), "cn");
	if (start == NULL) {
		attrkeyvalue_free(cn);
		return NULL;
	}
	end = g_strstr_len(start, strlen(start), ",");
	item = g_strndup(start, end - start);
	if (item == NULL) {
		attrkeyvalue_free(cn);
		return NULL;
	}
	key_value = g_strsplit(item, "=", 2);
	cn->key = g_strdup(key_value[0]);
	cn->value = g_strdup(key_value[1]);
	g_strfreev(key_value);
	g_free(item);
	return cn;
}

/**
 * Get mail attribute from dn
 *
 * \param dn Distinguesh Name for current object
 * \return AttrKeyValue, or <i>NULL</i> if none created
 */
AttrKeyValue *get_mail(gchar *dn) {
	AttrKeyValue *mail;
	gchar *start;
	gchar *end;
	gchar *item;
	gchar **key_value;
	cm_return_val_if_fail(dn != NULL, NULL);
	
	mail = attrkeyvalue_create();
	start = g_strstr_len(dn, strlen(dn), "mail");
	if (start == NULL) {
		attrkeyvalue_free(mail);
		return NULL;
	}
	end = g_strstr_len(start, strlen(start), ",");
	item = g_strndup(start, end - start);
	if (item == NULL) {
		attrkeyvalue_free(mail);
		return NULL;
	}
	key_value = g_strsplit(item, "=", 2);
	mail->key = g_strdup(key_value[0]);
	mail->value = g_strdup(key_value[1]);
	g_strfreev(key_value);
	g_free(item);
	return mail;
}

/**
 * Get ou or o attribute from dn
 *
 * \param dn Distinguesh Name for current object
 * \return AttrKeyValue, or <i>NULL</i> if none created
 */
AttrKeyValue *get_ou(gchar *dn) {
	AttrKeyValue *ou;
	gchar *start;
	gchar *end;
	gchar *item;
	gchar **key_value;
	
	cm_return_val_if_fail(dn != NULL, NULL);
	ou = attrkeyvalue_create();
	start = g_strstr_len(dn, strlen(dn), ",o=");
	if (start == NULL)
		start = g_strstr_len(dn, strlen(dn), ",ou=");
	if (start == NULL) {
		attrkeyvalue_free(ou);
		return NULL;
	}
	start++;
	end = g_strstr_len(start, strlen(start), ",");
	item = g_strndup(start, end - start);
	if (item == NULL) {
		attrkeyvalue_free(ou);
		return NULL;
	}
	key_value = g_strsplit(item, "=", 2);
	ou->key = g_strdup(key_value[0]);
	ou->value = g_strdup(key_value[1]);
	g_strfreev(key_value);
	g_free(item);
	return ou;
}

/**
 * Print the contents of a LDAPMod structure for debuging purposes
 *
 * \param mods LDAPMod structure
 */
void ldapsvr_print_ldapmod(LDAPMod *mods[]) {
	gchar *mod_op;
	int i;

	cm_return_if_fail(mods != NULL);
	g_printerr( "Type\n");
	for (i = 0; NULL != mods[i]; i++) {
		LDAPMod *mod = (LDAPMod *) mods[i];
		gchar **vals;
		switch (mod->mod_op) {
			case LDAP_MOD_ADD: mod_op = g_strdup("ADD"); break;
			case LDAP_MOD_REPLACE: mod_op = g_strdup("MODIFY"); break;
			case LDAP_MOD_DELETE: mod_op = g_strdup("DELETE"); break;
			default: mod_op = g_strdup("UNKNOWN");
		}
		g_printerr( "Operation: %s\tType:%s\nValues:\n", mod_op, mod->mod_type);
		vals = mod->mod_vals.modv_strvals;
		while (*vals) {
			g_printerr( "\t%s\n", *vals++);
		}
	}
}

/**
 * Make a compare for every new value we want to store in the
 * directory with the current values. Great tool for debugging
 * against invalid syntax in attributes
 *
 * \param ld AddressBook resource
 * \param dn dn for the entry
 * \param cnt Number of attributes to compare
 * \param  mods LDAPMod structure
 */
void ldapsvr_compare_attr(LDAP *ld, gchar *dn, gint cnt, LDAPMod *mods[]) {
	int i, rc;

#ifdef OPEN_LDAP_API_AT_LEAST_3000

	struct berval val;

#endif

	cm_return_if_fail(ld != NULL || dn != NULL || cnt >= 0 || mods != NULL);
	for (i = 0; i < cnt; i++) {
		gchar *value = g_strdup(mods[i]->mod_vals.modv_strvals[0]);
		if (!value || strcmp(value, "") == 0)
			value = g_strdup("thisisonlyadummy");

#ifdef OPEN_LDAP_API_AT_LEAST_3000

		val.bv_val = value;
		val.bv_len = strlen(value);

		rc = ldap_compare_ext_s(ld, dn, mods[i]->mod_type, &val, NULL, NULL);

#else

		/* This is deprecated as of OpenLDAP-2.3.0 */
		rc = ldap_compare_s(ld, dn, mods[i]->mod_type, value);

#endif

		g_printerr("ldap_compare for (%s:%s)\" failed[0x%x]: %s\n",
        	mods[i]->mod_type, value, rc, ldaputil_get_error(ld));
		g_free(value);
	}
}

/**
 * compare attribute to LDAP in case of LDAP_INAPPROPRIATE_MATCHING
 *
 * \param ld AddressBook resource
 * \param server Reference to server
 * \param dn dn for the entry
 * \param attr Attribute
 * \param value New value
 * \return int, return will be LDAP_MOD_ADD, LDAP_MOD_REPLACE, or LDAP_MOD_DELETE
 */
int ldapsvr_compare_manual_attr(LDAP *ld, LdapServer *server, gchar *dn, char *attr, char *value) {
	LDAPMessage *res, *e = NULL;
	BerElement *ber;
	struct berval **vals;
	int rc;
	LdapControl *ctl;
	gchar *filter;
	gchar *attribute;
	int retVal = -2, i;
	AttrKeyValue *mail;

	cm_return_val_if_fail(ld != NULL || server != NULL || attr != NULL, -1);
	ctl = server->control;
	mail = get_mail(dn);
	if (! mail)
		return -2;
	filter = g_strdup_printf("(&(mail=%s)(%s=*))", mail->value, attr);
	attrkeyvalue_free(mail);
	if (ctl) {

		rc = ldap_search_ext_s(ld, ctl->baseDN, LDAP_SCOPE_ONELEVEL, filter, NULL, 0, NULL, NULL, NULL, 0, &res);

		if (rc) {
			g_printerr("ldap_search for attr=%s\" failed[0x%x]: %s\n",attr, rc, ldaputil_get_error(ld));
			retVal = -2;
		}
		else {
			e = ldap_first_entry(ld, res);
			/* entry has this attribute */
			if (e) {
				attribute = ldap_first_attribute( ld, e, &ber );
				if (attribute) {
					if (value) {
						if( ( vals = ldap_get_values_len( ld, e, attr ) ) != NULL ) {
							for( i = 0; vals[i] != NULL; i++ ) {
								debug_print("Compare: %s=%s\n", attr, vals[i]->bv_val);
								/* attribute has same value */
								if (strcmp(vals[i]->bv_val, value) == 0)
									retVal = -1;
								/* attribute has new value */
								else
									retVal = LDAP_MOD_REPLACE;
							}
						}
						ldap_value_free_len(vals);
					}
					else
						retVal = LDAP_MOD_DELETE;
				}
				if( ber != NULL ) {
					ber_free( ber, 0 );
				}
				ldap_memfree(attribute);
			}
			/* entry does not have this attribute */
			else {
				/* Only add if this is a real attribute */
				if (value)
					retVal = LDAP_MOD_ADD;
				/* This is dummy value used to avoid ldap_compare error */
				else
					retVal = -1;
			}
		}
	}
	else
		retVal = -2;
	g_free(filter);
	return retVal;
}

/**
 * Deside which kind of operation is required to handle
 * updating the specified attribute
 *
 * \param ld AddressBook resource
 * \param server Reference to server
 * \param dn dn for the entry
 * \param attr Attribute
 * \param value New value
 * \return int, return will be LDAP_MOD_ADD, LDAP_MOD_REPLACE, or LDAP_MOD_DELETE
 */
int ldapsvr_deside_operation(LDAP *ld, LdapServer *server, char *dn, char *attr, char *value) {
	int rc;
	gboolean dummy = FALSE;

#ifdef OPEN_LDAP_API_AT_LEAST_3000

	struct berval val;

#endif

	cm_return_val_if_fail(ld != NULL || server != NULL || dn != NULL || attr != NULL, -1);
	if (value == NULL)
		return -1;
	/* value containing empty string cause invalid syntax. A bug in
	 * the LDAP library? Therefore we add a dummy value
	 */
	if (strcmp(value,"") == 0) {
		value = g_strdup("thisisonlyadummy");
		dummy = TRUE;
	}

#ifdef OPEN_LDAP_API_AT_LEAST_3000

	val.bv_val = value;
	val.bv_len = strlen(value);

	rc = ldap_compare_ext_s(ld, dn, attr, &val, NULL, NULL);

#else

	/* This is deprecated as of OpenLDAP-2.3.0 */
	rc = ldap_compare_s(ld, dn, attr, value);

#endif

	debug_print("ldap_compare for (%s:%s)\" error_code[0x%x]: %s\n",
       	attr, value, rc, ldaputil_get_error(ld));
	switch (rc) {
		case LDAP_COMPARE_FALSE: 
			if (dummy)
				return LDAP_MOD_DELETE;
			else
				return LDAP_MOD_REPLACE;
		case LDAP_COMPARE_TRUE: return -1;
		case LDAP_NO_SUCH_ATTRIBUTE: return LDAP_MOD_ADD;
		/* LDAP_INAPPROPRIATE_MATCHING needs extensive testing because I
		 * am not aware off the condition causing this return value!
		 */
		case LDAP_INAPPROPRIATE_MATCHING:
			if (dummy)
				value = NULL;
			return ldapsvr_compare_manual_attr(ld, server, dn, attr, value);
		case LDAP_UNDEFINED_TYPE: return -2;
		case LDAP_INVALID_SYNTAX: return -2;
		default: return -2;
	}
}

/**
 * Check if attribute is part of the current search criteria
 *
 * \param list Array containing attributes in the current search criteria
 * \param attr Attribute to check
 * \result <i>TRUE</i> if attribute is found in the current search criteria
 */
gboolean ldapsvr_check_search_attributes(char **list, char *attr) {
	while (*list) {
		if (strcmp(*list++, attr) == 0)
			return TRUE;
	}
	return FALSE;
}

/**
 * Deside which other attributes needs updating
 *
 * \param ld LDAP resource
 * \param server AddressBook resource
 * \param dn dn for the entry
 * \param contact GHashTable with information for the current contact
 */
void ldapsvr_handle_other_attributes(LDAP *ld, LdapServer *server, char *dn, GHashTable *contact) {
	GList *node;
	gboolean CHECKED_ATTRIBUTE[ATTRIBUTE_SIZE + 1];
	LDAPMod *mods[ATTRIBUTE_SIZE + 1];
	LDAPMod modarr[ATTRIBUTE_SIZE];
	gint cnt = 0;
	char *attr[ATTRIBUTE_SIZE + 1][2];
	int mod_op, rc, i;

	cm_return_if_fail(server != NULL || dn != NULL || contact != NULL);
	for (i = 0; i <= ATTRIBUTE_SIZE; i++) {
		CHECKED_ATTRIBUTE[i] = FALSE;
		attr[i][0] = attr[i][1] = NULL;
	}
	node = g_hash_table_lookup(contact , "attribute");
	while (node) {
		AttrKeyValue *item = node->data;
		if (item) {
			int index = get_attribute_index(item->key);
			if (index >= 0) {
				debug_print("Found other attribute: %s = %s\n",
						item->key?item->key:"null", item->value?item->value:"null");
				mod_op = ldapsvr_deside_operation(ld, server, dn, item->key, item->value);
				/* Only consider attributes which we no how to handle.
				 * Set to TRUE in CHECKED_ATTRIBUTE array to indicate no further action
				 */
				if (mod_op < 0) {
					CHECKED_ATTRIBUTE[index] = TRUE;
					node = g_list_next(node);
					continue;
				}
				if (mod_op == LDAP_MOD_DELETE) {
					/* Setting param to NULL instructs OpenLDAP to remove any
			 		* value stored for this attribute and remove the attribute
			 		* completely. Should multiple instances of an attribute be
			 		* allowed in the future param is required to have the value
			 		* store for the attribute which is going to be deleted
			 		*/
					item->value = NULL;
				}
				if (mod_op == LDAP_MOD_REPLACE && strcmp(item->value, "") == 0) {
					/* Having an empty string is considered a syntax error in
			 		* ldap. E.g attributes with empty strings are not allowed
			 		* in which case we treate this as a request for deleting
			 		* the attribute.
			 		*/
					mod_op = LDAP_MOD_DELETE;
					item->value = NULL;
				}
				if (mod_op == LDAP_MOD_ADD && strcmp(item->value, "") == 0) {
					/* Adding an empty string is considered a syntax error in
			 		* ldap. E.g attributes with empty strings are not allowed
			 		* in which case we silently refuse to add this entry
			 		*/
				}
				else {
					SETMOD(mods[cnt], modarr[cnt], mod_op, g_strdup(item->key), attr[cnt], g_strdup(item->value));
					cnt++;
					CHECKED_ATTRIBUTE[index] = TRUE;
				}
			}
		}
		node = g_list_next(node);
	}
	char **attribs = ldapctl_full_attribute_array(server->control);
	for (i = 0; i < ATTRIBUTE_SIZE; i++) {
		/* Attributes which holds no information are to be removed */
		if (CHECKED_ATTRIBUTE[i] == FALSE) {
			/* Only consider those attributes which is currently part of the search criteria.
			 * If attributes are not part of the search criteria they would seem to hold
			 * no information since their values will not be populated in the GUI
			 */
			if (!strcmp(ATTRIBUTE[i], "jpegPhoto")) {
				debug_print("not updating jpegPhoto\n");
				continue;
			}
			if (ldapsvr_check_search_attributes(attribs, (char *) ATTRIBUTE[i])) {
				mod_op = ldapsvr_deside_operation(ld, server, dn, (char *) ATTRIBUTE[i], "");
				if (mod_op == LDAP_MOD_DELETE) {
					SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_DELETE, g_strdup((char *) ATTRIBUTE[i]), attr[cnt], NULL);
					cnt++;
				}
			}
		}
	}
	ldapctl_free_attribute_array(attribs);
	mods[cnt] = NULL;
	if (debug_get_mode())
		ldapsvr_print_ldapmod(mods);
	server->retVal = LDAPRC_SUCCESS;
	rc = ldap_modify_ext_s(ld, dn, mods, NULL, NULL);
	if (rc) {
		switch (rc) {
			case LDAP_ALREADY_EXISTS: 
				server->retVal = LDAPRC_ALREADY_EXIST;
				break;
			default:
				g_printerr("ldap_modify for dn=%s\" failed[0x%x]: %s\n", dn, rc, ldaputil_get_error(ld));
				if (rc == 0x8)
					server->retVal = LDAPRC_STRONG_AUTH;
				else
					server->retVal = LDAPRC_NAMING_VIOLATION;
		}
	}
	else {
		char **attribs = ldapctl_full_attribute_array(server->control);
		for (i = 0; i < ATTRIBUTE_SIZE; i++) {
			if (!strcmp(ATTRIBUTE[i], "jpegPhoto")) {
				debug_print("not updating jpegPhoto\n");
				continue;
			}
			if (ldapsvr_check_search_attributes(attribs, (char *) ATTRIBUTE[i])) {
				if (CHECKED_ATTRIBUTE[i] == FALSE) {
					AddrItemObject *aio = addrcache_get_object(server->addressCache, g_hash_table_lookup(contact , "uid"));
					ItemPerson *person = (ItemPerson *) aio;
					addritem_person_remove_attribute(person, (const gchar *) ATTRIBUTE[i]);
				}
			}
		}
		ldapctl_free_attribute_array(attribs);
	}
}

/**
 * Add new contact to LDAP
 *
 * \param server AddressBook resource
 * \param contact GHashTable with object to add
 */
void ldapsvr_add_contact(LdapServer *server, GHashTable *contact) {
	gchar *email = NULL, *param = NULL;
	LDAP *ld = NULL;
	LDAPMod *mods[MODSIZE];
	LDAPMod modarr[7];
	gint cnt = 0;
	char *cn[] = {NULL, NULL};
	char *displayName[] = {NULL, NULL};
	char *givenName[] = {NULL, NULL};
	char **mail = NULL;
	char *sn[] = {NULL, NULL};
	char *org[] = {NULL, NULL};
	char *obj[] = {/*"top",*/ "person", "organizationalPerson", "inetOrgPerson", NULL}; 
	int rc=0;
	GList *node;
	AttrKeyValue *ou, *commonName;
	ItemPerson *person;
	gchar *base_dn;
	GList *mailList;

	cm_return_if_fail(server != NULL || contact != NULL);
	node = g_hash_table_lookup(contact , "mail");
	if (node) {
		EmailKeyValue *newEmail = node->data;
		email = g_strdup(newEmail->mail);
	}
	if (email == NULL) {
		server->retVal = LDAPRC_NODN;
		clean_up(ld, server, contact);
		return;
	}
	base_dn = g_strdup_printf("mail=%s,%s",
			email, server->control->baseDN?server->control->baseDN:"null");
	g_free(email);
	person = 
		ldapsvr_get_contact(server, g_hash_table_lookup(contact , "uid"));
	person->externalID = g_strdup(base_dn);
	debug_print("dn: %s\n", base_dn);
	ld = ldapsvr_connect(server->control);
	if (ld == NULL) {
		clean_up(ld, server, contact);
		debug_print("no ldap found\n");
		return;
	}
	SETMODS(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "objectClass", obj);
	cnt++;
	ou = get_ou(base_dn);
	if (ou != NULL) {
		SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, g_strdup(ou->key), org, g_strdup(ou->value));
		cnt++;
		attrkeyvalue_free(ou);
	}
	
	commonName = get_cn(base_dn);
	if (commonName == NULL) {
		param = g_hash_table_lookup(contact , "cn");
		if (param) {
			SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "cn", cn, param);
		}
		else {
			clean_up(ld, server, contact);
			debug_print("no CN found\n");
			return;
		}
	}
	else {
		SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, g_strdup(commonName->key), cn, g_strdup(commonName->value));
		cnt++;
		param = g_hash_table_lookup(contact , "cn");
		SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "displayName", displayName, param);
		g_hash_table_insert(contact, "displayName", param);
		attrkeyvalue_free(commonName);
	}
	cnt++;
	param = g_hash_table_lookup(contact , "givenName");
	if (param) {
		SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "givenName", givenName, param);
		cnt++;
	}
	mailList = g_hash_table_lookup(contact , "mail");
	if (mailList) {
		char **tmp;
		tmp = g_malloc(sizeof(*tmp) * (g_list_length(mailList)+1));
		mail = tmp;
		while (mailList) {
			EmailKeyValue *item = mailList->data;
			*tmp++ = g_strdup((gchar *) item->mail);
			mailList = g_list_next(mailList);
		}
		*tmp = NULL;
		SETMODS(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "mail", mail);
		cnt++;
	}
	param = g_hash_table_lookup(contact, "sn");
	if (param == NULL)
		param = g_strdup(N_("Some SN"));
	SETMOD(mods[cnt], modarr[cnt], LDAP_MOD_ADD, "sn", sn, param);
	cnt++;
	mods[cnt] = NULL;
	if (debug_get_mode()) {
		ldapsvr_print_ldapmod(mods);
	}
	server->retVal = LDAPRC_SUCCESS;
	rc = ldap_add_ext_s(ld, base_dn, mods, NULL, NULL);
	if (rc) {
		switch (rc) {
			case LDAP_ALREADY_EXISTS: 
				server->retVal = LDAPRC_ALREADY_EXIST;
				break;
			default:
				g_printerr("ldap_modify for dn=%s\" failed[0x%x]: %s\n",
						base_dn, rc, ldaputil_get_error(ld));
				if (rc == 0x8)
					server->retVal = LDAPRC_STRONG_AUTH;
				else
					server->retVal = LDAPRC_NAMING_VIOLATION;
		}
	}
	ldapsvr_handle_other_attributes(ld, server, base_dn, contact);
	g_free(base_dn);
	clean_up(ld, server, contact);
}

/**
 * Update contact to LDAP
 *
 * \param server AddressBook resource
 * \param contact GHashTable with object to update
 */
void ldapsvr_update_contact(LdapServer *server, GHashTable *contact) {
	LDAP *ld = NULL;
	LDAPMod *mods[MODSIZE];
	LDAPMod modarr[4];
	gint cnt = 0;
	gchar *param, *dn;
	Rdn *NoRemove = NULL;
	char *cn[] = {NULL, NULL};
	char *givenName[] = {NULL, NULL};
	char **mail = NULL;
	char *sn[] = {NULL, NULL};
	GList *mailList;
	int mod_op;

	cm_return_if_fail(server != NULL || contact != NULL);
	ld = ldapsvr_connect(server->control);
	if (ld == NULL) {
		clean_up(ld, server, contact);
		return;
	}
	dn = g_hash_table_lookup(contact, "dn");

	if (dn == NULL) {
		clean_up(ld, server, contact);
		return;
	}
	NoRemove = ldapsvr_modify_dn(contact, dn);
	if (NoRemove) {
		/* We are trying to change RDN */
		gchar *newRdn = g_strdup_printf("%s=%s", NoRemove->attribute, NoRemove->value);

#ifdef OPEN_LDAP_API_AT_LEAST_3000

		int rc = ldap_rename_s(ld, dn, newRdn, NULL, 1, NULL, NULL);

#else

		/* This is deprecated as of OpenLDAP-2.3.0 */
		int rc = ldap_modrdn2_s(ld, dn, newRdn, 1);

#endif

		if(rc != LDAP_SUCCESS) {
			if (rc ==  LDAP_ALREADY_EXISTS) {
				/* We are messing with a contact with more than one listed email
				 * address and the email we are changing is not the one used for dn
				 */
				/* It needs to be able to handle renaming errors to an already defined
				 * dn. For now we just refuse the update. It will be caught later on as
				 * a LDAPRC_NAMING_VIOLATION error.
				 */
			}
			else {
				g_printerr("Current dn: %s\n", dn);
				g_printerr("new dn: %s\n", newRdn);
				g_printerr("LDAP Error(ldap_modrdn2_s) failed[0x%x]: %s\n", rc, ldaputil_get_error(ld));
				g_free(newRdn);
				clean_up(ld, server, contact);
				return;
			}
		}
		else {
			ItemPerson *person = g_hash_table_lookup(contact, "person");
			g_free(newRdn);
			dn = g_strdup(NoRemove->new_dn);
			g_hash_table_replace(contact, "dn", dn);
			if (person) {
				g_free(person->externalID);
				person->externalID = dn;
			}
		}
	}
	else {
		server->retVal = LDAPRC_NODN;
		clean_up(ld, server, contact);
		return;
	}
	param = g_hash_table_lookup(contact , "cn");
	mod_op = ldapsvr_deside_operation(ld, server, dn, "displayName", param);
	if (mod_op >= 0 && (strcmp(param, NoRemove->value) != 0 && strcmp("cn", NoRemove->attribute) != 0)) {
		if (mod_op == LDAP_MOD_DELETE) {
			/* Setting param to NULL instructs OpenLDAP to remove any
			 * value stored for this attribute and remove the attribute
			 * completely. Should multiple instances of an attribute be
			 * allowed in the future param is required to have the value
			 * store for the attribute which is going to be deleted
			 */
			param = NULL;
		}
		if (mod_op == LDAP_MOD_REPLACE && strcmp(param, "") == 0) {
			/* Having an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we treate this as a request for deleting
			 * the attribute.
			 */
			mod_op = LDAP_MOD_DELETE;
			param = NULL;
		}
		if (mod_op == LDAP_MOD_ADD && strcmp(param, "") == 0) {
			/* Adding an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we silently refuse to add this entry
			 */
		}
		else {
			SETMOD(mods[cnt], modarr[cnt], mod_op, "displayName", cn, param);
			cnt++;
			g_hash_table_insert(contact, "displayName", param);
		}
	}
	param = g_hash_table_lookup(contact , "givenName");
	mod_op = ldapsvr_deside_operation(ld, server, dn, "givenName", param);
	if (mod_op >= 0 && (strcmp(param, NoRemove->value) != 0 && strcmp("givenName", NoRemove->attribute) != 0)) {
		if (mod_op == LDAP_MOD_DELETE) {
			/* Setting param to NULL instructs OpenLDAP to remove any
			 * value stored for this attribute and remove the attribute
			 * completely. Should multiple instances of an attribute be
			 * allowed in the future param is required to have the value
			 * store for the attribute which is going to be deleted
			 */
			param = NULL;
		}
		if (mod_op == LDAP_MOD_REPLACE && strcmp(param, "") == 0) {
			/* Having an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we treate this as a request for deleting
			 * the attribute.
			 */
			mod_op = LDAP_MOD_DELETE;
			param = NULL;
		}
		if (mod_op == LDAP_MOD_ADD && strcmp(param, "") == 0) {
			/* Adding an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we silently refuse to add this entry
			 */
		}
		else {
			SETMOD(mods[cnt], modarr[cnt], mod_op, "givenName", givenName, param);
			cnt++;
		}
	}
	mailList = g_hash_table_lookup(contact , "mail");
	if (mailList) {
		debug_print("# of mail: %d\n", g_list_length(mailList));
		if (!(strcmp("mail", NoRemove->attribute) == 0 && g_list_length(mailList) == 1)) {
			char **tmp;
			tmp = g_malloc(sizeof(*tmp) * (g_list_length(mailList)+1));
			mail = tmp;
			while (mailList) {
				EmailKeyValue *item = mailList->data;
				*tmp++ = g_strdup((gchar *) item->mail);
				mailList = g_list_next(mailList);
			}
			*tmp = NULL;
			/*
			 * At least one email address is required
			 * in which case it will always be a replace
			 */
			SETMODS(mods[cnt], modarr[cnt], LDAP_MOD_REPLACE, "mail", mail);
			cnt++;
		}
	}
	else {
		/*
		 * an error condition since at least one email adress
		 * is required. Should never occur though.
		 */
	}
	param = g_hash_table_lookup(contact , "sn");
	mod_op = ldapsvr_deside_operation(ld, server, dn, "sn", param);
	if (mod_op >= 0 && (strcmp(param, NoRemove->value) != 0 && strcmp("sn", NoRemove->attribute) != 0)) {
		if (mod_op == LDAP_MOD_DELETE) {
			/* Setting param to NULL instructs OpenLDAP to remove any
			 * value stored for this attribute and remove the attribute
			 * completely. Should multiple instances of an attribute be
			 * allowed in the future param is required to have the value
			 * store for the attribute which is going to be deleted
			 */
			param = NULL;
		}
		if (mod_op == LDAP_MOD_REPLACE && strcmp(param, "") == 0) {
			/* Having an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we treate this as a request for deleting
			 * the attribute.
			 */
			mod_op = LDAP_MOD_DELETE;
			param = NULL;
		}
		if (mod_op == LDAP_MOD_ADD && strcmp(param, "") == 0) {
			/* Adding an empty string is considered a syntax error in
			 * ldap. E.g attributes with empty strings are not allowed
			 * in which case we silently refuse to add this entry
			 */
		}
		else {
			SETMOD(mods[cnt], modarr[cnt], mod_op, "sn", sn, param);
			cnt++;
		}
	}
	debug_print("newDN: %s\n", dn);
	if (NoRemove)
		rdn_free(NoRemove);
	server->retVal = LDAPRC_SUCCESS;
	if (cnt > 0) {
		int rc;
		mods[cnt] = NULL;
		rc = ldap_modify_ext_s(ld, dn, mods, NULL, NULL);
		if (rc) {
			g_printerr("ldap_modify for dn=%s\" failed[0x%x]: %s\n",
                    dn, rc, ldaputil_get_error(ld));
			server->retVal = LDAPRC_NAMING_VIOLATION;
		}
		if (mail)
			g_free(mail);
	}
	ldapsvr_handle_other_attributes(ld, server, dn, contact);
	/* If we do not make changes persistent at this point then changes
	 * will be lost if the user makes new search on the same server since
	 * changes are only present in Claws' internal cache. This issue has to
	 * be solved in addressbook.c since this involves access to structures
	 * which are only accessible in addressbook.c */
	clean_up(ld, server, contact);
}

/**
 * Delete contact from LDAP
 *
 * \param server AddressBook resource
 * \param contact GHashTable with object to delete
 */
void ldapsvr_delete_contact(LdapServer *server, GHashTable *contact) {
	LDAP *ld = NULL;
	gchar *dn;
	int rc;

	cm_return_if_fail(server != NULL || contact != NULL);
	ld = ldapsvr_connect(server->control);
	if (ld == NULL) {
		clean_up(ld, server, contact);
		return;
	}
	dn = g_hash_table_lookup(contact, "dn");
	if (dn == NULL) {
		clean_up(ld, server, contact);
		return;
	}
	server->retVal = LDAPRC_SUCCESS;
	rc = ldap_delete_ext_s(ld, dn, NULL, NULL);
	if (rc) {
		g_printerr("ldap_modify for dn=%s\" failed[0x%x]: %s\n",
				dn, rc, ldaputil_get_error(ld));
		server->retVal = LDAPRC_NODN;
	}
	clean_up(ld, server, contact);
}

/**
 * Update any changes to the server.
 *
 * \param server AddressBook resource.
 * \param person ItemPerson holding user input.
 */
void ldapsvr_update_book(LdapServer *server, ItemPerson *item) {
	GList *node = NULL;
	GHashTable *contact = NULL;
	GList *contacts = NULL, *head = NULL;

	cm_return_if_fail(server != NULL);
	debug_print("updating ldap addressbook\n");

	contact = g_hash_table_new(g_str_hash, g_str_equal);
	if (item) {
		gboolean result = ldapsvr_retrieve_item_person(item, contact);
		debug_print("Found contact to update: %s\n", result? "Yes" : "No");
		if (result) {
			if (debug_get_mode()) {
				addritem_print_item_person(item, stdout);
			}
			contacts = g_list_append(contacts, contact);
		}
	}
	else {
		ItemFolder *folder = server->addressCache->rootFolder;
		node = folder->listFolder;
		if (node) {
			while (node) {
				AddrItemObject *aio = node->data;
				if (aio) {
					if (aio->type == ITEMTYPE_FOLDER) {
						ItemFolder *folder = (ItemFolder *) aio;
						GList *persons = folder->listPerson;
						while (persons) {
							AddrItemObject *aio = persons->data;
							if (aio) {
								if (aio->type == ITEMTYPE_PERSON) {
									ItemPerson *item = (ItemPerson *) aio;
									gboolean result = ldapsvr_retrieve_item_person(item, contact);
									debug_print("Found contact to update: %s\n", result? "Yes" : "No");
									if (result) {
										if (debug_get_mode()) {
											gchar *uid = g_hash_table_lookup(contact, "uid");
											item = ldapsvr_get_contact(server, uid);
											addritem_print_item_person(item, stdout);
										}
										contacts = g_list_append(contacts, contact);
									}
								}
							}
							persons = g_list_next(persons);
						}
					}
				}
				else {
					g_printerr("\t\tpid : ???\n");
				}
				node = g_list_next(node);
			}
		}
	}
	head = contacts;
	if (debug_get_mode()) {
		if (contacts)
			debug_print("Contacts which must be updated in LDAP:\n");
		while (contacts) {
			debug_print("\tContact:\n");
			g_hash_table_foreach(contacts->data, 
				ldapsvr_print_contacts_hashtable, stderr);
			contacts = g_list_next(contacts);
		}
	}
	if (contacts == NULL)
		contacts = head;
	while (contacts) {
		gchar *status;
		contact = (GHashTable *) contacts->data;
		status = (gchar *) g_hash_table_lookup(contact, "status");
		if (status == NULL)
			status = g_strdup("NULL");
		if (g_ascii_strcasecmp(status, "new") == 0) {
			ldapsvr_add_contact(server, contact);
		}
		else if (g_ascii_strcasecmp(status, "update") == 0) {
			ldapsvr_update_contact(server, contact);
		}
		else if (g_ascii_strcasecmp(status, "delete") == 0) {
			ldapsvr_delete_contact(server, contact);
		}
		else
			g_critical(_("ldapsvr_update_book->Unknown status: %s\n"), status);
		contacts = g_list_next(contacts);
	}
	ldapsvr_free_hashtable(head);
}

#endif	/* USE_LDAP */

/*
 * End of Source.
 */

