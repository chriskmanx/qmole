/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _MAIN_H
#define _MAIN_H

#include <sys/types.h>
#include <gtk/gtk.h>

typedef struct _Callback Callback;
typedef void (*CallbackFn)(gpointer data);

extern GtkTooltips *tooltips;

struct _Callback
{
	CallbackFn	callback;
	gpointer	data;
};

extern int number_of_windows;
extern Option o_override_redirect;
extern int to_wakeup_pipe;

extern uid_t euid;
extern gid_t egid;
extern int ngroups;			/* Number of supplemental groups */
extern gid_t *supplemental_groups;
extern const gchar *show_user_message;
extern int home_dir_len;
extern const char *home_dir, *app_dir;
extern Option o_dnd_no_hostnames;

/* Prototypes */
int main(int argc, char **argv);
void on_child_death(gint child, CallbackFn callback, gpointer data);
void one_less_window(void);

#endif /* _MAIN_H */
