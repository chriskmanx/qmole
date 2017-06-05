#define DEFAULT_SHORTEN_MENUS \
  "optionsProgressiveBG optionsLeftHanded optionsButtonSwitchMapping"

#define GS_CMDLINE \
  "gs -sDEVICE=bmp16m -r%f -q -sOutputFile=- " \
  "-dNOPAUSE -dBATCH -dEPSCrop -dTextAlphaBits=4 -dGraphicsAlphaBits=4 %s"

extern int GS_BITMAP_DPI, PDFTOPPM_PRINTING_DPI;

#define TMPDIR_TEMPLATE "/tmp/xournalpdf.XXXXXX"

void new_journal(void);
gboolean save_journal(const char *filename);
gboolean close_journal(void);
gboolean open_journal(char *filename);

struct Background *attempt_load_pix_bg(char *filename, gboolean attach);
GList *attempt_load_gv_bg(char *filename);
struct Background *attempt_screenshot_bg(void);

void cancel_bgpdf_request(struct BgPdfRequest *req);
gboolean add_bgpdf_request(int pageno, double zoom);
gboolean bgpdf_scheduler_callback(gpointer data);
void shutdown_bgpdf(void);
gboolean init_bgpdf(char *pdfname, gboolean create_pages, int file_domain);

void bgpdf_create_page_with_bg(int pageno, struct BgPdfPage *bgpg);
void bgpdf_update_bg(int pageno, struct BgPdfPage *bgpg);

void init_mru(void);
void update_mru_menu(void);
void new_mru_entry(char *name);
void delete_mru_entry(int which);
void save_mru_list(void);

void init_config_default(void);
void load_config_from_file(void);
void save_config_to_file(void);
