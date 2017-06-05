/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _APPINFO_H
#define _APPINFO_H

/* Name of the XML file where the info is stored */
#define APPINFO_FILENAME		"AppInfo.xml"

/* External interface */
XMLwrapper *appinfo_get(const gchar *app_dir, DirItem *item);
void appinfo_unref(XMLwrapper *info);

#endif   /* _APPINFO_H */
