/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _BULK_RENAME_H
#define _BULK_RENAME_H

void bulk_rename(const char *dir, GList *items);

#ifdef UNIT_TESTS
void bulk_rename_tests(void);
#endif

#endif /* _BULK_RENAME_H */
