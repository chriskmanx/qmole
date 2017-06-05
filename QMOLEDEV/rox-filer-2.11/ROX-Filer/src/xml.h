/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __XML_H__
#define __XML_H__

#include <glib-object.h>

typedef struct _XMLwrapperClass XMLwrapperClass;

struct _XMLwrapperClass {
	GObjectClass parent;
};

struct _XMLwrapper {
	GObject object;
	xmlDocPtr doc;
};

XMLwrapper *xml_new(const char *pathname);
xmlNode *xml_get_section(XMLwrapper *xml, const gchar *ns, const gchar *name);
xmlNode *get_subnode(xmlNode *node, const char *namespaceURI, const char *name);

#endif /* __XML_H__ */
