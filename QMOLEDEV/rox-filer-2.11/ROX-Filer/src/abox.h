/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef __ABOX_H__
#define __ABOX_H__

#include <gtk/gtk.h>

#define ABOX(obj) GTK_CHECK_CAST((obj), abox_get_type(), ABox)
#define ABOX_CLASS(klass) GTK_CHECK_CLASS_CAST((klass), \
					abox_get_type(), ABoxClass)
#define IS_ABOX(obj) G_TYPE_CHECK_INSTANCE_TYPE((obj), abox_get_type())

typedef struct _ABoxClass  ABoxClass;
typedef struct _ABox ABox;

struct _ABox
{
	GtkDialog 	parent_widget;

	GtkWidget	*quiet;
	GtkWidget	*flag_box;	/* HBox for flags */
	GtkWidget	*dir_label;	/* Shows what is being processed now */
	GtkWidget	*log;		/* The TextView for the messages */
	GtkWidget	*log_hbox;
	GtkWidget	*results;	/* List of filenames found */
	GtkWidget	*entry;		/* Plain entry, or part of combo */
	FilerWindow	*preview;

	GtkWidget       *cmp_area;      /* Area where files are compared */
	GtkWidget       *cmp_icon[2];
	GtkWidget       *cmp_name[2];
	GtkWidget       *cmp_size[2];
	GtkWidget       *cmp_date[2];
	GtkWidget       *cmp_arrow;

	GtkWidget       *progress;      /* Progress bar, NULL until set */

	gchar		*next_dir;	/* NULL => no timer active */
	gint		next_timer;

	gboolean	question;	/* Asking a question? */
};

struct _ABoxClass
{
	GtkDialogClass 	parent_class;

	void		(*flag_toggled)(ABox *abox, gint response);
	void		(*abort_operation)(ABox *abox);
};

GType	abox_get_type   		(void);
GtkWidget* abox_new			(const gchar *title, gboolean quiet);
GtkWidget *abox_add_flag		(ABox *abox,
					 const gchar *label,
					 const gchar *tip,
					 gint response,
					 gboolean default_value);
void	abox_ask			(ABox *abox,
					 const gchar *question);
void	abox_cancel_ask			(ABox *abox);
void	abox_set_current_object		(ABox *abox,
					 const gchar *message);
void	abox_log			(ABox *abox,
					 const gchar *message,
					 const gchar *style);
void	abox_add_results		(ABox *abox);
void	abox_add_filename		(ABox *abox,
					 const gchar *pathname);
void	abox_clear_results		(ABox *abox);
void	abox_add_combo			(ABox *abox,
					 const gchar *tlabel, 
					 GList *presets,
					 const gchar *text,
					 GtkWidget *help_button);
void	abox_add_entry			(ABox *abox,
					 const gchar *text,
					 GtkWidget *help_button);

void	abox_show_compare		(ABox *abox, gboolean show);
void	abox_set_file			(ABox *abox, int file,
					 const gchar *path);
void    abox_set_percentage             (ABox *abox, int per);

#endif /* __ABOX_H__ */
