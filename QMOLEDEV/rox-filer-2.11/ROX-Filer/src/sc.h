/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

/* sc.h - XSMP client support */

#ifndef _SC_ALU_H_
#define _SC_ALU_H_

#include <gtk/gtk.h>
#include <X11/SM/SMlib.h>

typedef struct _SmProperty {
	SmProp prop;
	gboolean set;
} SmProperty;

typedef struct _SmClient SmClient;

struct _SmClient {
	gchar *id;
	GSList *props;		
	gboolean (*save_yourself_fn)(SmClient *client);
	void (*shutdown_cancelled_fn)(SmClient *client);
	void (*save_complete_fn)(SmClient *client);
	void (*die_fn)(SmClient *client);
	SmcConn conn;
	IceConn ice_conn;
	gint fd;
	gint input_tag;
};

gboolean sc_session_up(void);
SmClient *sc_new(const gchar *client_id);
gboolean sc_connect(SmClient *client);
void sc_get_prop_value(SmClient *client, const gchar *name,
			SmPropValue **val_ret, gint *nvals_ret);
void sc_set_list_of_array_prop(SmClient *client, const gchar *name,
				const gchar *vals[], gint nvals);
void sc_set_array_prop(SmClient *client, const gchar *name, const gchar *vals);
void sc_set_card_prop(SmClient *client, const gchar *name, gchar val);
void sc_register_properties(SmClient *client);
void sc_destroy(SmClient *client);

#endif /* _SC_ALU_H */
