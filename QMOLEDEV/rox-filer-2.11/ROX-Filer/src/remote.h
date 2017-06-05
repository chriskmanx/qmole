/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _REMOTE_H
#define _REMOTE_H

gboolean remote_init(xmlDocPtr rpc, gboolean new_copy);
xmlDocPtr run_soap(xmlDocPtr soap);

gchar **extract_soap_errors(xmlDocPtr reply);

#endif /* _REMOTE_H */
