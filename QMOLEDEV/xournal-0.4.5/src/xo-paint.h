void set_cursor_busy(gboolean busy);
void update_cursor(void);
void update_cursor_for_resize(double *pt);

void create_new_stroke(GdkEvent *event);
void continue_stroke(GdkEvent *event);
void finalize_stroke(void);

void do_eraser(GdkEvent *event, double radius, gboolean whole_strokes);
void finalize_erasure(void);

void do_hand(GdkEvent *event);

void start_selectrect(GdkEvent *event);
void finalize_selectrect(void);
gboolean start_movesel(GdkEvent *event);
void start_vertspace(GdkEvent *event);
void continue_movesel(GdkEvent *event);
void finalize_movesel(void);
gboolean start_resizesel(GdkEvent *event);
void continue_resizesel(GdkEvent *event);
void finalize_resizesel(void);

void selection_delete(void);
void selection_to_clip(void);
void clipboard_paste(void);

void recolor_selection(int color_no, guint color_rgba);
void rethicken_selection(int val);

/* text functions */

#define DEFAULT_FONT "Sans"
#define DEFAULT_FONT_SIZE 12

void start_text(GdkEvent *event, struct Item *item);
void end_text(void);
void update_text_item_displayfont(struct Item *item);
void rescale_text_items(void);
struct Item *click_is_in_text(struct Layer *layer, double x, double y);
void refont_text_item(struct Item *item, gchar *font_name, double font_size);
void process_font_sel(gchar *str);
