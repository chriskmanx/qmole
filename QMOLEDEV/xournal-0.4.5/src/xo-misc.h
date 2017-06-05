// data manipulation misc functions

struct Page *new_page(struct Page *template);
struct Page *new_page_with_bg(struct Background *bg, double width, double height);
void realloc_cur_path(int n);
void realloc_cur_widths(int n);
void clear_redo_stack(void);
void clear_undo_stack(void);
void prepare_new_undo(void);
void delete_journal(struct Journal *j);
void delete_page(struct Page *pg);
void delete_layer(struct Layer *l);

// referenced strings

struct Refstring *new_refstring(const char *s);
struct Refstring *refstring_ref(struct Refstring *rs);
void refstring_unref(struct Refstring *rs);

// helper functions

void get_pointer_coords(GdkEvent *event, double *ret);
double get_pressure_multiplier(GdkEvent *event);
void fix_xinput_coords(GdkEvent *event);
void update_item_bbox(struct Item *item);
void make_page_clipbox(struct Page *pg);
void make_canvas_items(void);
void make_canvas_item_one(GnomeCanvasGroup *group, struct Item *item);
void update_canvas_bg(struct Page *pg);
gboolean is_visible(struct Page *pg);
void rescale_bg_pixmaps(void);

gboolean have_intersect(struct BBox *a, struct BBox *b);
void lower_canvas_item_to(GnomeCanvasGroup *g, GnomeCanvasItem *item, GnomeCanvasItem *after);

void rgb_to_gdkcolor(guint rgba, GdkColor *color);
guint32 gdkcolor_to_rgba(GdkColor gdkcolor, guint16 alpha);

// interface misc functions

void update_thickness_buttons(void);
void update_color_buttons(void);
void update_tool_buttons(void);
void update_tool_menu(void);
void update_ruler_indicator(void);
void update_color_menu(void);
void update_pen_props_menu(void);
void update_eraser_props_menu(void);
void update_highlighter_props_menu(void);
void update_mappings_menu_linkings(void);
void update_mappings_menu(void);
void update_page_stuff(void);
void update_toolbar_and_menu(void);
void update_file_name(char *filename);
void update_undo_redo_enabled(void);
void update_copy_paste_enabled(void);
void update_vbox_order(int *order);

gchar *make_cur_font_name(void);
void update_font_button(void);

void update_mapping_linkings(int toolno);
void do_switch_page(int pg, gboolean rescroll, gboolean refresh_all);
void set_cur_color(int color_no, guint color_rgba);
void recolor_temp_text(int color_no, guint color_rgba);
void process_color_activate(GtkMenuItem *menuitem, int color_no, guint color_rgba);
void process_thickness_activate(GtkMenuItem *menuitem, int tool, int val);
void process_papercolor_activate(GtkMenuItem *menuitem, int color, guint rgba);
void process_paperstyle_activate(GtkMenuItem *menuitem, int style);

gboolean ok_to_close(void);

void reset_focus(void);

// selection / clipboard stuff

void reset_selection(void);
void move_journal_items_by(GList *itemlist, double dx, double dy,
                           struct Layer *l1, struct Layer *l2, GList *depths);
void resize_journal_items_by(GList *itemlist, double scaling_x, double scaling_y,
                             double offset_x, double offset_y);


// switch between mappings

void switch_mapping(int m);
void process_mapping_activate(GtkMenuItem *menuitem, int m, int tool);

// always allow accels
void allow_all_accels(void);
gboolean can_accel(GtkWidget *widget, guint id, gpointer data);
void add_scroll_bindings(void);

gboolean is_event_within_textview(GdkEventButton *event);

void hide_unimplemented(void);

void do_fullscreen(gboolean active);

// fix GTK+ 2.16/2.17 issues with XInput events
gboolean filter_extended_events(GtkWidget *widget, GdkEvent *event, gpointer user_data);
// gboolean fix_extended_events(GtkWidget *widget, GdkEvent *event, gpointer user_data);

// help with focus
gboolean handle_activate_signal(GtkWidget *widget, gpointer user_data);
void unset_flags(GtkWidget *w, gpointer flag);
gboolean intercept_activate_events(GtkWidget *w, GdkEvent *ev, gpointer data);
void install_focus_hooks(GtkWidget *w, gpointer data);

// defines for paper rulings

#define RULING_MARGIN_COLOR 0xff0080ff
#define RULING_COLOR 0x40a0ffff
#define RULING_THICKNESS 0.5
#define RULING_LEFTMARGIN 72.0
#define RULING_TOPMARGIN 80.0
#define RULING_SPACING 24.0
#define RULING_BOTTOMMARGIN RULING_SPACING
#define RULING_GRAPHSPACING 14.17
