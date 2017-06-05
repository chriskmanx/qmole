/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * based on the code developped for ELinks by Laurent Monin
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef SECURE_SAVE_H
#define SECURE_SAVE_H

extern SecureSaveErrno secsave_errno; /**< internal secsave error number */

SecureSaveInfo *secure_open(const gchar *);

gint secure_close(SecureSaveInfo *);

gint secure_fputs(SecureSaveInfo *, const gchar *);
gint secure_fputc(SecureSaveInfo *, gint);

gint secure_fprintf(SecureSaveInfo *, const gchar *, ...);
size_t secure_fwrite(gconstpointer ptr, size_t size, size_t nmemb, SecureSaveInfo *ssi);

gchar *secsave_strerror(SecureSaveErrno);

#endif /* SECURE_SAVE_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
