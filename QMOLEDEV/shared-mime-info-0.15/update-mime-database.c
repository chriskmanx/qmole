#include <config.h>

#define N_(x) x
#define _(x) (x)

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <glib.h>
#include <errno.h>
#include <dirent.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <sys/stat.h>
#include <sys/types.h>

#define XML_NS XML_XML_NAMESPACE
#define XMLNS_NS "http://www.w3.org/2000/xmlns/"
#define FREE_NS "http://www.freedesktop.org/standards/shared-mime-info"

#define COPYING								\
	     N_("Copyright (C) 2003 Thomas Leonard.\n"			\
		"update-mime-database comes with ABSOLUTELY NO WARRANTY,\n" \
		"to the extent permitted by law.\n"			\
		"You may redistribute copies of update-mime-database\n"	\
		"under the terms of the GNU General Public License.\n"	\
		"For more information about these matters, "		\
		"see the file named COPYING.\n")

#define MIME_ERROR g_quark_from_static_string("mime-error-quark")

/* This is the list of directories to scan when finding old type files to
 * delete. It is also used to warn about invalid MIME types.
 */
const char *media_types[] = {
	"text",
	"application",
	"image",
	"audio",
	"inode",
	"video",
	"message",
	"model",
	"multipart",
};

/* Represents a MIME type */
typedef struct _Type Type;

/* A parsed <magic> element */
typedef struct _Magic Magic;

/* A parsed <match> element */
typedef struct _Match Match;

struct _Type {
	char *media;
	char *subtype;

	/* Contains xmlNodes for elements that are being copied to the output.
	 * That is, <comment>, <sub-class-of> and <alias> nodes, and anything
	 * with an unknown namespace.
	 */
	xmlDoc	*output;
};

struct _Magic {
	int priority;
	Type *type;
	GList *matches;
};

struct _Match {
	long range_start;
	int range_length;
	char word_size;
	int data_length;
	char *data;
	char *mask;
	GList *matches;
};

/* Maps MIME type names to Types */
static GHashTable *types = NULL;

/* Maps "namespaceURI localName" strings to Types */
static GHashTable *namespace_hash = NULL;

/* Maps glob patterns to Types */
static GHashTable *globs_hash = NULL;

/* 'magic' nodes */
static GPtrArray *magic_array = NULL;

/* Static prototypes */
static Magic *magic_new(xmlNode *node, Type *type, GError **error);


static void usage(const char *name)
{
	fprintf(stderr, _("Usage: %s [-hv] MIME-DIR\n"), name);
}

static void free_type(gpointer data)
{
	Type *type = (Type *) data;

	g_free(type->media);
	g_free(type->subtype);

	xmlFreeDoc(type->output);

	g_free(type);
}

/* If we've seen this type before, return the existing object.
 * Otherwise, create a new one. Checks that the name looks sensible;
 * if not, sets error and returns NULL.
 * Also warns about unknown media types, but does not set error.
 */
static Type *get_type(const char *name, GError **error)
{
	xmlNode *root;
	xmlNs *ns;
	const char *slash;
	Type *type;
	int i;

	slash = strchr(name, '/');
	if (!slash || strchr(slash + 1, '/'))
	{
		g_set_error(error, MIME_ERROR, 0,
				_("Invalid MIME-type '%s'"), name);
		return NULL;
	}

	type = g_hash_table_lookup(types, name);
	if (type)
		return type;

	type = g_new(Type, 1);
	type->media = g_strndup(name, slash - name);
	type->subtype = g_strdup(slash + 1);
	g_hash_table_insert(types, g_strdup(name), type);

	type->output = xmlNewDoc("1.0");
	root = xmlNewDocNode(type->output, NULL, "mime-type", NULL);
	ns = xmlNewNs(root, FREE_NS, NULL);
	xmlSetNs(root, ns);
	xmlDocSetRootElement(type->output, root);
	xmlSetNsProp(root, NULL, "type", name);
	xmlAddChild(root, xmlNewDocComment(type->output,
		"Created automatically by update-mime-database. DO NOT EDIT!"));

	for (i = 0; i < G_N_ELEMENTS(media_types); i++)
	{
		if (strcmp(media_types[i], type->media) == 0)
			return type;
	}

	g_print("* Warning: Unknown media type in type '%s'\n", name);

	return type;
}

/* Test that this node has the expected name and namespace */
static gboolean match_node(xmlNode *node,
			   const char *namespaceURI,
			   const char *localName)
{
	if (namespaceURI)
		return node->ns &&
			strcmp(node->ns->href, namespaceURI) == 0 &&
			strcmp(node->name, localName) == 0;
	else
		return strcmp(node->name, localName) == 0 && !node->ns;
}

/* Return the priority of a <magic> node.
 * Returns 50 if no priority is given, or -1 if a priority is given but
 * is invalid.
 */
static int get_priority(xmlNode *node)
{
	char *prio_string;
	int p;

	prio_string = xmlGetNsProp(node, "priority", NULL);
	if (prio_string)
	{
		char *end;

		p = strtol(prio_string, &end, 10);
		if (*prio_string == '\0' || *end != '\0')
			p = -1;
		xmlFree(prio_string);
		if (p < 0 || p > 100)
			return -1;
		return p;
	}
	else
		return 50;
}

/* Process a <root-XML> element by adding a rule to namespace_hash */
static void add_namespace(Type *type, const guchar *namespaceURI,
			  const guchar *localName, GError **error)
{
	g_return_if_fail(type != NULL);

	if (!namespaceURI)
	{
		g_set_error(error, MIME_ERROR, 0,
			_("Missing 'namespaceURI' attribute'"));
		return;
	}

	if (!localName)
	{
		g_set_error(error, MIME_ERROR, 0,
			_("Missing 'localName' attribute'"));
		return;
	}

	if (!*namespaceURI && !*localName)
	{
		g_set_error(error, MIME_ERROR, 0,
			_("namespaceURI and localName attributes can't "
			  "both be empty"));
		return;
	}

	if (strpbrk(namespaceURI, " \n") || strpbrk(localName, " \n"))
	{
		g_set_error(error, MIME_ERROR, 0,
			_("namespaceURI and localName cannot contain "
			  "spaces or newlines"));
		return;
	}

	g_hash_table_insert(namespace_hash,
			g_strconcat(namespaceURI, " ", localName, NULL),
			type);
}

/* 'field' was found in the definition of 'type' and has the freedesktop.org
 * namespace. If it's a known field, process it and return TRUE, else
 * return FALSE to add it to the output XML document.
 * On error, returns FALSE and sets 'error'.
 */
static gboolean process_freedesktop_node(Type *type, xmlNode *field,
					 GError **error)
{
	if (strcmp(field->name, "glob") == 0)
	{
		gchar *pattern;
		
		pattern = xmlGetNsProp(field, "pattern", NULL);

		if (pattern && *pattern)
		{
			g_hash_table_insert(globs_hash,
					    g_strdup(pattern), type);
			xmlFree(pattern);
		}
		else
		{
			if (pattern)
				xmlFree(pattern);
			g_set_error(error, MIME_ERROR, 0,
				_("Missing 'pattern' attribute in <glob> "
				  "element"));
		}
	}
	else if (strcmp(field->name, "magic") == 0)
	{
		Magic *magic;

		magic = magic_new(field, type, error);

		if (!*error)
		{
			g_return_val_if_fail(magic != NULL, FALSE);
			g_ptr_array_add(magic_array, magic);
		}
		else
			g_return_val_if_fail(magic == NULL, FALSE);
	}
	else if (strcmp(field->name, "comment") == 0)
		return FALSE;
	else if (strcmp(field->name, "alias") == 0 ||
		 strcmp(field->name, "sub-class-of") == 0)
	{
		char *other_type;
		gboolean valid;
		other_type = xmlGetNsProp(field, "type", NULL);
		valid = other_type && strchr(other_type, '/');
		xmlFree(other_type);
		if (valid)
			return FALSE;	/* Copy through */

		g_set_error(error, MIME_ERROR, 0,
			_("Incorrect or missing 'type' attribute "
			  "in <%s>"), field->name);
	}
	else if (strcmp(field->name, "root-XML") == 0)
	{
		char *namespaceURI, *localName;

		namespaceURI = xmlGetNsProp(field, "namespaceURI", NULL);
		localName = xmlGetNsProp(field, "localName", NULL);

		add_namespace(type, namespaceURI, localName, error);

		if (namespaceURI)
			xmlFree(namespaceURI);
		if (localName)
			xmlFree(localName);
	}
	else
	{
		g_set_error(error, MIME_ERROR, 0,
			_("Unknown freedesktop.org field '%s'"),
			field->name);
	}
	
	return !*error;
}

/* Checks to see if 'node' has the given value for xml:lang.
 * If 'lang' is NULL, checks that 'node' doesn't have an xml:lang.
 */
static gboolean has_lang(xmlNode *node, const char *lang)
{
	char *lang2;

	lang2 = xmlGetNsProp(node, "lang", XML_NS);
	if (!lang2)
		return !lang;

	if (lang && strcmp(lang, lang2) == 0)
	{
		xmlFree(lang2);
		return TRUE;
	}
	xmlFree(lang2);
	return FALSE;
}

/* We're about to add 'new' to the list of fields to be output for the
 * type. Remove any existing nodes which it replaces.
 */
static void remove_old(Type *type, xmlNode *new)
{
	xmlNode *field, *fields;
	gchar *lang;

	if (new->ns == NULL || strcmp(new->ns->href, FREE_NS) != 0)
		return;	/* No idea what we're doing -- leave it in! */

	if (strcmp(new->name, "comment") != 0)
		return;

	lang = xmlGetNsProp(new, "lang", XML_NS);

	fields = xmlDocGetRootElement(type->output);
	for (field = fields->xmlChildrenNode; field; field = field->next)
	{
		if (match_node(field, FREE_NS, "comment") &&
		    has_lang(field, lang))
		{
			xmlUnlinkNode(field);
			xmlFreeNode(field);
			break;
		}
	}

	xmlFree(lang);
}

/* 'node' is a <mime-type> node from a source file, whose type is 'type'.
 * Process all the child elements, setting 'error' if anything goes wrong.
 */
static void load_type(Type *type, xmlNode *node, GError **error)
{
	xmlNode *field;

	g_return_if_fail(type != NULL);
	g_return_if_fail(node != NULL);
	g_return_if_fail(error != NULL);

	for (field = node->xmlChildrenNode; field; field = field->next)
	{
		xmlNode *copy;

		if (field->type != XML_ELEMENT_NODE)
			continue;

		if (field->ns && strcmp(field->ns->href, FREE_NS) == 0)
		{
			if (process_freedesktop_node(type, field, error))
			{
				g_return_if_fail(*error == NULL);
				continue;
			}
		}

		if (*error)
			return;

		copy = xmlDocCopyNode(field, type->output, 1);
		
		/* Ugly hack to stop the xmlns= attributes appearing on
		 * every node...
		 */
		if (copy->ns && copy->ns->prefix == NULL &&
			strcmp(copy->ns->href, FREE_NS) == 0)
		{
			if (copy->nsDef)
			{
				/* Still used somewhere... */
				/* xmlFreeNsList(copy->nsDef); */
				/* (this leaks) */
				copy->nsDef = NULL;
			}
		}

		remove_old(type, field);

		xmlAddChild(xmlDocGetRootElement(type->output), copy);
	}
}

/* Parse 'filename' as an XML file and add all the information to the
 * database. If called more than once, information read in later calls
 * overrides information read previously.
 */
static void load_source_file(const char *filename)
{
	xmlDoc *doc;
	xmlNode *root, *node;

	doc = xmlParseFile(filename);
	if (!doc)
	{
		g_warning(_("Failed to parse '%s'\n"), filename);
		return;
	}

	root = xmlDocGetRootElement(doc);

	if (root->ns == NULL || strcmp(root->ns->href, FREE_NS) != 0)
	{
		g_print("* Wrong namespace on document element\n"
			"*   in '%s'\n"
			"*   (should be %s)\n", filename, FREE_NS);
		goto out;
	}
	
	if (strcmp(root->name, "mime-info") != 0)
	{
		g_print("* Root element <%s> is not <mime-info>\n"
			"*   (in '%s')\n", root->name, filename);
		goto out;
	}

	for (node = root->xmlChildrenNode; node; node = node->next)
	{
		Type *type = NULL;
		guchar *type_name = NULL;
		GError *error = NULL;

		if (node->type != XML_ELEMENT_NODE)
			continue;

		if (!match_node(node, FREE_NS, "mime-type"))
			g_set_error(&error, MIME_ERROR, 0,
				_("Excepted <mime-type>, but got wrong name "
				  "or namespace"));

		if (!error)
		{
			type_name = xmlGetNsProp(node, "type", NULL);

			if (!type_name)
				g_set_error(&error, MIME_ERROR, 0,
					_("<mime-type> element has no 'type' "
					  "attribute"));
		}

		if (type_name)
		{
			type = get_type(type_name, &error);
			xmlFree(type_name);
		}

		if (!error)
		{
			g_return_if_fail(type != NULL);
			load_type(type, node, &error);
		}
		else
			g_return_if_fail(type == NULL);

		if (error)
		{
			g_print("* Error in type '%s/%s'\n"
				"*   (in %s):\n"
				"*   %s.\n",
				type ? type->media : _("unknown"),
				type ? type->subtype : _("unknown"),
				filename, error->message);
			g_error_free(error);
		}
	}
out:
	xmlFreeDoc(doc);
}

/* Used as the sort function for sorting GPtrArrays */
static gint strcmp2(gconstpointer a, gconstpointer b)
{
	const char *aa = *(char **) a;
	const char *bb = *(char **) b;

	return strcmp(aa, bb);
}

/* 'path' should be a 'packages' directory. Loads the information from
 * every file in the directory.
 */
static void scan_source_dir(const char *path)
{
	DIR *dir;
	struct dirent *ent;
	char *filename;
	GPtrArray *files;
	int i;
	gboolean have_override = FALSE;

	dir = opendir(path);
	if (!dir)
	{
		perror("scan_source_dir");
		exit(EXIT_FAILURE);
	}

	files = g_ptr_array_new();
	while ((ent = readdir(dir)))
	{
		int l;
		l = strlen(ent->d_name);
		if (l < 4 || strcmp(ent->d_name + l - 4, ".xml") != 0)
			continue;
		if (strcmp(ent->d_name, "Override.xml") == 0)
		{
			have_override = TRUE;
			continue;
		}
		g_ptr_array_add(files, g_strdup(ent->d_name));
	}
	closedir(dir);

	g_ptr_array_sort(files, strcmp2);

	if (have_override)
		g_ptr_array_add(files, g_strdup("Override.xml"));

	for (i = 0; i < files->len; i++)
	{
		gchar *leaf = (gchar *) files->pdata[i];

		filename = g_strconcat(path, "/", leaf, NULL);
		load_source_file(filename);
		g_free(filename);
	}

	for (i = 0; i < files->len; i++)
		g_free(files->pdata[i]);
	g_ptr_array_free(files, TRUE);
}

/* Save doc as XML as filename, 0 on success or -1 on failure */
static int save_xml_file(xmlDocPtr doc, const gchar *filename)
{
#if LIBXML_VERSION > 20400
	if (xmlSaveFormatFileEnc(filename, doc, "utf-8", 1) < 0)
		return 1;
#else
	FILE *out;
	
	out = fopen(filename, "w");
	if (!out)
		return 1;

	xmlDocDump(out, doc);  /* Some versions return void */

	if (fclose(out))
		return 1;
#endif

	return 0;
}

/* Write out one line of the 'globs' file */
static void write_out_glob(gpointer key, gpointer value, gpointer data)
{
	const gchar *pattern = (gchar *) key;
	Type *type = (Type *) value;
	FILE *stream = (FILE *) data;

	if (strchr(pattern, '\n'))
		g_print("* Glob patterns can't contain literal newlines "
			"(%s in type %s/%s)\n", pattern,
			type->media, type->subtype);
	else
		fprintf(stream, "%s/%s:%s\n",
				type->media, type->subtype, pattern);
}

/* Renames pathname by removing the .new extension */
static void atomic_update(const guchar *pathname)
{
	guchar *new_name;
	int len;

	len = strlen(pathname);

	g_return_if_fail(strcmp(pathname + len - 4, ".new") == 0);

	new_name = g_strndup(pathname, len - 4);

	if (rename(pathname, new_name))
		g_warning("Failed to rename %s as %s\n", pathname, new_name);

	g_free(new_name);
}

/* Write out an XML file for one type */
static void write_out_type(gpointer key, gpointer value, gpointer data)
{
	Type *type = (Type *) value;
	const char *mime_dir = (char *) data;
	char *media, *filename;

	media = g_strconcat(mime_dir, "/", type->media, NULL);
	mkdir(media, 0755);

	filename = g_strconcat(media, "/", type->subtype, ".xml.new", NULL);
	g_free(media);
	media = NULL;
	
	if (save_xml_file(type->output, filename) != 0)
		g_warning("Failed to write out '%s'\n", filename);

	atomic_update(filename);

	g_free(filename);
}

/* Comparison function to get the magic rules in priority order */
static gint cmp_magic(gconstpointer a, gconstpointer b)
{
	Magic *aa = *(Magic **) a;
	Magic *bb = *(Magic **) b;
	int retval;

	if (aa->priority > bb->priority)
		return -1;
	else if (aa->priority < bb->priority)
		return 1;

	retval = strcmp(aa->type->media, bb->type->media);
	if (!retval)
		retval = strcmp(aa->type->subtype, bb->type->subtype);

	return retval;
}

/* Write out 'n' as a two-byte big-endian number to 'stream' */
static void write16(FILE *stream, guint32 n)
{
	guint16 big = GUINT16_TO_BE(n);

	g_return_if_fail(n <= 0xffff);

	fwrite(&big, sizeof(big), 1, stream);
}

/* Single hex char to int; -1 if not a hex char.
 * From file(1).
 */
static int hextoint(int c)
{
	if (!isascii((unsigned char) c))
		return -1;
	if (isdigit((unsigned char) c))
		return c - '0';
	if ((c >= 'a')&&(c <= 'f'))
		return c + 10 - 'a';
	if (( c>= 'A')&&(c <= 'F'))
		return c + 10 - 'A';
	return -1;
}

/*
 * Convert a string containing C character escapes.  Stop at an unescaped
 * space or tab.
 * Copy the converted version to "p", returning its length in *slen.
 * Return updated scan pointer as function result.
 * Stolen from file(1) and heavily modified.
 */
static void getstr(const char *s, GString *out)
{
	int	c;
	int	val;

	while ((c = *s++) != '\0') {
		if(c == '\\') {
			switch(c = *s++) {

			case '\0':
				return;

			default:
				g_string_append_c(out, (char) c);
				break;

			case 'n':
				g_string_append_c(out, '\n');
				break;

			case 'r':
				g_string_append_c(out, '\r');
				break;

			case 'b':
				g_string_append_c(out, '\b');
				break;

			case 't':
				g_string_append_c(out, '\t');
				break;

			case 'f':
				g_string_append_c(out, '\f');
				break;

			case 'v':
				g_string_append_c(out, '\v');
				break;

			/* \ and up to 3 octal digits */
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				val = c - '0';
				c = *s++;  /* try for 2 */
				if(c >= '0' && c <= '7') {
					val = (val<<3) | (c - '0');
					c = *s++;  /* try for 3 */
					if(c >= '0' && c <= '7')
						val = (val<<3) | (c-'0');
					else
						--s;
				}
				else
					--s;
				g_string_append_c(out, (char)val);
				break;

			/* \x and up to 2 hex digits */
			case 'x':
				val = 'x';	/* Default if no digits */
				c = hextoint(*s++);	/* Get next char */
				if (c >= 0) {
					val = c;
					c = hextoint(*s++);
					if (c >= 0)
						val = (val << 4) + c;
					else
						--s;
				} else
					--s;
				g_string_append_c(out, (char)val);
				break;
			}
		} else
			g_string_append_c(out, (char)c);
	}
}

/* Parse the value and mask attributes of a <match> element with a
 * numerical type (anything except "string").
 */
static void parse_int_value(int bytes, const char *in, const char *in_mask,
			    GString *parsed_value, char **parsed_mask,
			    gboolean big_endian, GError **error)
{
	char *end;
	char *out_mask = NULL;
	unsigned long value;
	int b;

	value = strtol(in, &end, 0);
	if (*end != '\0')
	{
		g_set_error(error, MIME_ERROR, 0, "Value is not a number");
		return;
	}

	for (b = 0; b < bytes; b++)
	{
		int shift = (big_endian ? (bytes - b - 1) : b) * 8;
		g_string_append_c(parsed_value, (value >> shift) & 0xff);
	}

	if ((bytes == 1 && (value & ~0xff)) ||
	    (bytes == 2 && (value & ~0xffff)))
	{
		g_set_error(error, MIME_ERROR, 0,
			    "Number out-of-range (%lx should fit in %d bytes)",
			    value, bytes);
		return;
	}

	if (in_mask)
	{
		int b;
		long mask;
		
		mask = strtol(in_mask, &end, 0);
		if (*end != '\0')
		{
			g_set_error(error, MIME_ERROR, 0,
				    "Mask is not a number");
			return;
		}

		out_mask = g_new(char, bytes);
		for (b = 0; b < bytes; b++)
		{
			int shift = (bytes - b - 1) * 8;
			out_mask[b] = (mask >> shift) & 0xff;
		}
	}

	*parsed_mask = out_mask;
}

/* 'len' is the length of the value. The mask created will be the same
 * length.
 */
static char *parse_string_mask(const char *mask, int len, GError **error)
{
	int i;
	char *parsed_mask = NULL;

	g_return_val_if_fail(mask != NULL, NULL);
	g_return_val_if_fail(len > 0, NULL);

	if (mask[0] != '0' || mask[1] != 'x')
	{
		g_set_error(error, MIME_ERROR, 0,
			"String masks must be in base 16 (starting with 0x)");
		goto err;
	}
	mask += 2;

	parsed_mask = g_new0(char, len);

	i = 0; /* Nybble to write to next */
	while (mask[i])
	{
		int c;

		c = hextoint(mask[i]);
		if (c == -1)
		{
			g_set_error(error, MIME_ERROR, 0,
				"'%c' is not a valid hex digit", mask[i]);
			goto err;
		}

		if (i >= len * 2)
		{
			g_set_error(error, MIME_ERROR, 0,
				"Mask is longer than value");
			goto err;
		}
		
		if (i & 1)
			parsed_mask[i >> 1] |= c;
		else
			parsed_mask[i >> 1] |= c << 4;

		i++;
	}

	return parsed_mask;
err:
	g_return_val_if_fail(error == NULL || *error != NULL, NULL);
	g_free(parsed_mask);
	return NULL;
}

/* Parse the value and mask attributes for a <match> element */
static void parse_value(const char *type, const char *in, const char *in_mask,
			GString *parsed_value, char **parsed_mask,
			GError **error)
{
	*parsed_mask = NULL;

	if (in == NULL || !in[0])
	{
		g_set_error(error, MIME_ERROR, 0, "No value specified");
		return;
	}

	if (strstr(type, "16"))
		parse_int_value(2, in, in_mask, parsed_value, parsed_mask,
				type[0] != 'l', error);
	else if (strstr(type, "32"))
		parse_int_value(4, in, in_mask, parsed_value, parsed_mask,
				type[0] != 'l', error);
	else if (strcmp(type, "byte") == 0)
		parse_int_value(1, in, in_mask, parsed_value, parsed_mask,
				FALSE, error);
	else if (strcmp(type, "string") == 0)
	{
		getstr(in, parsed_value);
		if (in_mask)
			*parsed_mask = parse_string_mask(in_mask,
						parsed_value->len, error);
	}
	else
		g_assert_not_reached();
}

static Match *match_new(void)
{
	Match *match;

	match = g_new(Match, 1);
	match->range_start = 0;
	match->range_length = 1;
	match->word_size = 1;
	match->data_length = 0;
	match->data = NULL;
	match->mask = NULL;
	match->matches = NULL;

	return match;
}

static void match_free(Match *match)
{
	GList *next;

	g_return_if_fail(match != NULL);

	for (next = match->matches; next; next = next->next)
		match_free((Match *) next->data);

	g_list_free(match->matches);

	g_free(match->data);
	g_free(match->mask);

	g_free(match);
}

/* Sets match->range_start and match->range_length */
static void match_offset(Match *match, xmlNode *node, GError **error)
{
	char *offset = NULL;
	char *end;

	offset = xmlGetNsProp(node, "offset", NULL);
	if (offset == NULL || !*offset)
	{
		g_set_error(error, MIME_ERROR, 0, "Missing 'offset' attribute");
		goto err;
	}

	match->range_start = strtol(offset, &end, 10);
	if (*end == ':' && end[1] && match->range_start >= 0)
	{
		int last;

		last = strtol(end + 1, &end, 10);
		if (*end == '\0' && last >= match->range_start)
			match->range_length = last - match->range_start + 1;
		else
			g_set_error(error, MIME_ERROR, 0, "Invalid offset");
	}
	else if (*end != '\0')
		g_set_error(error, MIME_ERROR, 0, "Invalid offset");
err:
	xmlFree(offset);
}

/* Sets match->data, match->data_length and match->mask */
static void match_value_and_mask(Match *match, xmlNode *node, GError **error)
{
	char *mask = NULL;
	char *value = NULL;
	char *type = NULL;
	char *parsed_mask = NULL;
	GString *parsed_value;

	type = xmlGetNsProp(node, "type", NULL);
	g_return_if_fail(type != NULL);

	mask = xmlGetNsProp(node, "mask", NULL);
	value = xmlGetNsProp(node, "value", NULL);

	parsed_value = g_string_new(NULL);

	parse_value(type, value, mask, parsed_value,
			&parsed_mask, error);

	if (*error)
	{
		g_string_free(parsed_value, TRUE);
		g_return_if_fail(parsed_mask == NULL);
	}
	else
	{
		match->data = parsed_value->str;
		match->data_length = parsed_value->len;
		match->mask = parsed_mask;

		g_string_free(parsed_value, FALSE);
	}

	if (mask)
		xmlFree(mask);
	if (value)
		xmlFree(value);
	xmlFree(type);
}

/* Sets match->word_size */
static void match_word_size(Match *match, xmlNode *node, GError **error)
{
	char *type;

	type = xmlGetNsProp(node, "type", NULL);

	if (!type)
	{
		g_set_error(error, MIME_ERROR, 0,
			_("Missing 'type' attribute in <match>"));
		return;
	}

	if (strcmp(type, "host16") == 0)
		match->word_size = 2;
	else if (strcmp(type, "host32") == 0)
		match->word_size = 4;
	else if (!*error && strcmp(type, "big16") &&
			strcmp(type, "big32") &&
			strcmp(type, "little16") && strcmp(type, "little32") &&
			strcmp(type, "string") && strcmp(type, "byte"))
	{
		g_set_error(error, MIME_ERROR, 0,
				"Unknown magic type '%s'", type);
	}

	xmlFree(type);
}

/* Turn the list of child nodes of 'parent' into a list of Matches */
static GList *build_matches(xmlNode *parent, GError **error)
{
	xmlNode *node;
	GList *out = NULL;

	g_return_val_if_fail(error != NULL, NULL);

	for (node = parent->xmlChildrenNode; node; node = node->next)
	{
		Match *match;

		if (node->type != XML_ELEMENT_NODE)
			continue;

		if (node->ns == NULL || strcmp(node->ns->href, FREE_NS) != 0)
		{
			g_set_error(error, MIME_ERROR, 0,
				_("Element found with non-freedesktop.org "
				  "namespace"));
			break;
		}

		if (strcmp(node->name, "match") != 0)
		{
			g_set_error(error, MIME_ERROR, 0,
				_("Expected <match> element, but found "
				  "<%s> instead"), node->name);
			break;
		}

		match = match_new();
		match_offset(match, node, error);
		if (!*error)
			match_word_size(match, node, error);
		if (!*error)
			match_value_and_mask(match, node, error);

		if (*error)
		{
			match_free(match);
			break;
		}

		out = g_list_append(out, match);

		match->matches = build_matches(node, error);
		if (*error)
			break;
	}

	return out;
}

static void magic_free(Magic *magic)
{
	GList *next;

	g_return_if_fail(magic != NULL);

	for (next = magic->matches; next; next = next->next)
		match_free((Match *) next->data);
	g_list_free(magic->matches);

	g_free(magic);
}

/* Create a new Magic object by parsing 'node' (a <magic> element) */
static Magic *magic_new(xmlNode *node, Type *type, GError **error)
{
	Magic *magic = NULL;
	int prio;

	g_return_val_if_fail(node != NULL, NULL);
	g_return_val_if_fail(type != NULL, NULL);
	g_return_val_if_fail(error != NULL, NULL);

	prio = get_priority(node);

	if (prio == -1)
	{
		g_set_error(error, MIME_ERROR, 0,
			_("Bad priority (%d) in <magic> element"), prio);
	}
	else
	{
		magic = g_new(Magic, 1);
		magic->priority = prio;
		magic->type = type;
		magic->matches = build_matches(node, error);

		if (*error)
		{
			gchar *old = (*error)->message;
			magic_free(magic);
			magic = NULL;
			(*error)->message = g_strconcat(
				_("Error in <match> element: "), old, NULL);
			g_free(old);
		}
	}

	return magic;
}

/* Write a list of Match elements (and their children) to the 'magic' file */
static void write_magic_children(FILE *stream, GList *matches, int indent)
{
	GList *next;

	for (next = matches; next; next = next->next)
	{
		Match *match = (Match *) next->data;

		if (indent)
			fprintf(stream, "%d>%ld=", indent, match->range_start);
		else
			fprintf(stream, ">%ld=", match->range_start);

		write16(stream, match->data_length);
		fwrite(match->data, match->data_length, 1, stream);
		if (match->mask)
		{
			fputc('&', stream);
			fwrite(match->mask, match->data_length, 1, stream);
		}
		if (match->word_size != 1)
			fprintf(stream, "~%d", match->word_size);
		if (match->range_length != 1)
			fprintf(stream, "+%d", match->range_length);

		fputc('\n', stream);

		write_magic_children(stream, match->matches, indent + 1);
	}
}

/* Write a whole Magic element to the 'magic' file */
static void write_magic(FILE *stream, Magic *magic)
{
	fprintf(stream, "[%d:%s/%s]\n", magic->priority,
		magic->type->media, magic->type->subtype);

	write_magic_children(stream, magic->matches, 0);
}

/* Check each of the directories with generated XML files, looking for types
 * which we didn't get on this scan, and delete them.
 */
static void delete_old_types(const gchar *mime_dir)
{
	int i;

	for (i = 0; i < G_N_ELEMENTS(media_types); i++)
	{
		gchar *media_dir;
		DIR   *dir;
		struct dirent *ent;
		
		media_dir = g_strconcat(mime_dir, "/", media_types[i], NULL);
		dir = opendir(media_dir);
		g_free(media_dir);
		if (!dir)
			continue;

		while ((ent = readdir(dir)))
		{
			char *type_name;
			int l;
			l = strlen(ent->d_name);
			if (l < 4 || strcmp(ent->d_name + l - 4, ".xml") != 0)
				continue;

			type_name = g_strconcat(media_types[i], "/",
						ent->d_name, NULL);
			type_name[strlen(type_name) - 4] = '\0';
			if (!g_hash_table_lookup(types, type_name))
			{
				char *path;
				path = g_strconcat(mime_dir, "/",
						type_name, ".xml", NULL);
#if 0
				g_print("* Removing old info for type %s\n",
						path);
#endif
				unlink(path);
				g_free(path);
			}
			g_free(type_name);
		}
		
		closedir(dir);
	}
}

/* Extract one entry from namespace_hash and put it in the GPtrArray so
 * we can sort it.
 */
static void add_ns(gpointer key, gpointer value, gpointer data)
{
	GPtrArray *lines = (GPtrArray *) data;
	const guchar *ns = (guchar *) key;
	Type *type = (Type *) value;

	g_ptr_array_add(lines, g_strconcat(ns, " ", type->media,
					   "/", type->subtype, "\n", NULL));
}

/* Write all the collected namespace rules to 'XMLnamespaces' */
static void write_namespaces(FILE *stream)
{
	GPtrArray *lines;
	int i;
	
	lines = g_ptr_array_new();

	g_hash_table_foreach(namespace_hash, add_ns, lines);

	g_ptr_array_sort(lines, strcmp2);

	for (i = 0; i < lines->len; i++)
	{
		char *line = (char *) lines->pdata[i];

		fwrite(line, 1, strlen(line), stream);

		g_free(line);
	}

	g_ptr_array_free(lines, TRUE);
}

/* Issue a warning if 'path' won't be found by applications */
static void check_in_path_xdg_data(const char *mime_path)
{
	struct stat path_info, dir_info;
	const char *env;
	char **dirs;
	char *path;
	int i, n;

	path = g_path_get_dirname(mime_path);

	if (stat(path, &path_info))
	{
		g_printerr("Can't stat '%s' directory: %s",
				path, g_strerror(errno));
		goto out;
	}

	env = getenv("XDG_DATA_DIRS");
	if (!env)
		env = "/usr/local/share/:/usr/share/";
	dirs = g_strsplit(env, ":", 0);
	g_return_if_fail(dirs != NULL);
	for (n = 0; dirs[n]; n++)
		;
	env = getenv("XDG_DATA_HOME");
	if (env)
		dirs[n] = g_strdup(env);
	else
		dirs[n] = g_build_filename(g_get_home_dir(), ".local",
						"share", NULL);
	n++;
	
	for (i = 0; i < n; i++)
	{
		if (stat(dirs[i], &dir_info) == 0 &&
		    dir_info.st_ino == path_info.st_ino &&
		    dir_info.st_dev == path_info.st_dev)
			break;
	}

	if (i == n)
	{
		g_print(_("\nNote that '%s' is not in the search path\n"
			"set by the XDG_DATA_HOME and XDG_DATA_DIRS\n"
			"environment variables, so applications may not\n"
			"be able to find it until you set them. The\n"
			"directories currently searched are:\n\n"), path);
		g_print("- %s\n", dirs[n - 1]);
		for (i = 0; i < n - 1; i++)
			g_print("- %s\n", dirs[i]);
		g_print("\n");
	}

	for (i = 0; i < n; i++)
		g_free(dirs[i]);
	g_free(dirs);
out:
	g_free(path);
}

int main(int argc, char **argv)
{
	char *mime_dir = NULL;
	char *package_dir = NULL;
	int opt;

	while ((opt = getopt(argc, argv, "hv")) != -1)
	{
		switch (opt)
		{
			case '?':
				usage(argv[0]);
				return EXIT_FAILURE;
			case 'h':
				usage(argv[0]);
				return EXIT_SUCCESS;
			case 'v':
				fprintf(stderr,
					"update-mime-database (" PACKAGE ") "
					VERSION "\n" COPYING);
				return EXIT_SUCCESS;
			default:
				abort();
		}
	}

	if (optind != argc - 1)
	{
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	LIBXML_TEST_VERSION;

	mime_dir = argv[optind];

	/* Strip trailing / characters */
	{
		int l = strlen(mime_dir);
		while (l > 1 && mime_dir[l - 1] == '/')
		{
			l--;
			mime_dir[l] = '\0';
		}
	}

	package_dir = g_strconcat(mime_dir, "/packages", NULL);

	if (access(mime_dir, W_OK))
	{
		g_printerr(_("%s: I don't have write permission on %s.\n"
			     "Try rerunning me as root.\n"), argv[0], mime_dir);
		return EXIT_FAILURE;
	}

	g_print("***\n* Updating MIME database in %s...\n", mime_dir);

	if (access(package_dir, F_OK))
	{
		fprintf(stderr,
			_("Directory '%s' does not exist!\n"), package_dir);
		return EXIT_FAILURE;
	}

	types = g_hash_table_new_full(g_str_hash, g_str_equal,
					g_free, free_type);
	globs_hash = g_hash_table_new_full(g_str_hash, g_str_equal,
					g_free, NULL);
	namespace_hash = g_hash_table_new_full(g_str_hash, g_str_equal,
					g_free, NULL);
	magic_array = g_ptr_array_new();

	scan_source_dir(package_dir);
	g_free(package_dir);

	delete_old_types(mime_dir);

	g_hash_table_foreach(types, write_out_type, (gpointer) mime_dir);

	{
		FILE *globs;
		char *globs_path;
		globs_path = g_strconcat(mime_dir, "/globs.new", NULL);
		globs = fopen(globs_path, "wb");
		if (!globs)
			g_error("Failed to open '%s' for writing\n",
				globs_path);
		fprintf(globs,
			"# This file was automatically generated by the\n"
			"# update-mime-database command. DO NOT EDIT!\n");
		g_hash_table_foreach(globs_hash, write_out_glob, globs);
		fclose(globs);

		atomic_update(globs_path);
		g_free(globs_path);
	}

	{
		FILE *stream;
		char *magic_path;
		int i;
		magic_path = g_strconcat(mime_dir, "/magic.new", NULL);
		stream = fopen(magic_path, "wb");
		if (!stream)
			g_error("Failed to open '%s' for writing\n",
					magic_path);
		fwrite("MIME-Magic\0\n", 1, 12, stream);

		if (magic_array->len)
			g_ptr_array_sort(magic_array, cmp_magic);
		for (i = 0; i < magic_array->len; i++)
		{
			Magic *magic = (Magic *) magic_array->pdata[i];

			write_magic(stream, magic);

			magic_free(magic);
		}
		fclose(stream);

		atomic_update(magic_path);
		g_free(magic_path);
	}
	
	{
		FILE *stream;
		char *ns_path;

		ns_path = g_strconcat(mime_dir, "/XMLnamespaces.new", NULL);
		stream = fopen(ns_path, "wb");
		if (!stream)
			g_error("Failed to open '%s' for writing\n",
					ns_path);

		write_namespaces(stream);

		atomic_update(ns_path);
		g_free(ns_path);
	}

	g_ptr_array_free(magic_array, TRUE);

	g_hash_table_destroy(types);
	g_hash_table_destroy(globs_hash);
	g_hash_table_destroy(namespace_hash);

	g_print("***\n");

	check_in_path_xdg_data(mime_dir);

	return EXIT_SUCCESS;
}
