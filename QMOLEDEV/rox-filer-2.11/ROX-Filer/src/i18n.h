/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#include <gtk/gtk.h>

extern char *current_lang;

void i18n_init(void);
GtkItemFactoryEntry *translate_entries(GtkItemFactoryEntry *entries, gint n);
void free_translated_entries(GtkItemFactoryEntry *entries, gint n);
