/*
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _CHOICES_H
#define _CHOICES_H

void 		choices_init	       (void);
void 		choices_migrate	       (void);

void		choices_free_list      (GPtrArray *list);
gchar 		*choices_find_xdg_path_load(const char *leaf, const char *dir,
					    const char *site);
gchar	   	*choices_find_xdg_path_save(const char *leaf, const char *dir,
					    const char *site, gboolean create);
GPtrArray       *choices_list_xdg_dirs(char *dir, char *site);


#endif /* _CHOICES_H */
