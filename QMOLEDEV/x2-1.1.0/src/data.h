// Data structures
struct node
{
	GtkWidget *Box;
	gchar *Name;			// File name
	gchar *Location;		// File location
	GtkSourceBuffer *Buffer;	// Buffer for text view
	GtkWidget *Scroller;		// Scrolled widget for text view
	GtkWidget *Doc;			// Text view
	GtkWidget *Label;		// Tab label
	GtkWidget *TabBox;		// Tab box
	GtkWidget *Image;		// Close image
	GtkWidget *Button;		// Close button
	#ifdef G_OS_UNIX
	GtkWidget *Term;		// VTE object
	GtkWidget *TermScroll;		// VTE scroller
	#endif
	GtkWidget *Paned;		// Paned object
	GtkWidget *Frame;		// Frame object

	// Search stuff
	GtkWidget *Searchlbl1;
	GtkWidget *SearchBox;
	GtkWidget *Search1;		// Search box

	// Replace stuff
	GtkWidget *Searchlbl2;
	GtkWidget *Replacelbl;
	GtkWidget *ReplaceBox;
	GtkWidget *Search2;
	GtkWidget *Replace2;
	
	// Jump to line stuff
	GtkWidget *JumpLbl;
	GtkWidget *JumpSpin;
	GtkWidget *JumpBox;

	// Template stuff
	GtkWidget *templatecmb;
	GtkWidget *templatebtn_img;
	GtkWidget *templatebtn;
	GtkWidget *templatebox;
	GtkWidget *templatelbl;
};

struct prefs_data
{
	GtkWidget *Notebook;
	GtkToolItem *tb_cmd;
	struct prefs *pref;
};

struct str_data
{
	GtkWidget *Widget;
	GtkWidget *Notebook;
};

struct prefs
{
	GtkWidget *Prefs_Win;
	GtkWidget *IconView;
	GtkListStore *IconModel;
	GdkPixbuf *p1, *p2, *p3, *p4;
	GtkWidget *Box;
	GtkWidget *General_Box;
	GtkWidget *Appearence_Box;
	GtkWidget *Notes;
	GtkWidget *BtnBox;
	GtkWidget *Apply;
	GtkWidget *Cancel;
	GtkWidget *GeneralLbl;
	GtkWidget *AppearenceLbl;
	GtkWidget *LicenceLbl;
	GtkWidget *Licence;
	GtkTextBuffer *Licence_Buffer;
	GtkWidget *Scroller;
	GtkWidget *Font;	
	GtkWidget *FontFrame;
	GtkWidget *UIBox;  
	GtkWidget *Programming_Box;
	GtkWidget *ProgrammingFrame;
	GtkWidget *Programming_Check;
	GtkWidget *Highlight_Check;
	GtkWidget *Lines_Check;	
	GtkWidget *Terminal_Check;
	GtkWidget *FindFrame;
	GtkWidget *FindBox;
	GtkWidget *FindInWords;
	GtkWidget *FindSensitive;
	GtkWidget *HighlightLine;
	GtkWidget *Search_Box;
	GtkWidget *SearchLbl;
};

struct settings
{
	gchar *prog_features;
	gchar *highlighting;
	gchar *lines;
	gchar *terminal;
	gchar *find_in_words;
	gchar *find_sensitive;
	gchar *font;
	gchar *highlightline;
};

struct search_data
{
	struct node *tmp;
	gchar *textmark;
	GtkWidget *Notebook;
	GtkWidget *Search;
};

// Varibles
gboolean prefs_win_open = FALSE;
struct settings *sets; 
GtkWidget *window = NULL;
gint ids = 0;

enum 
{
	TARGET_UTF8_STRING,
	TARGET_COMPOUND_TEXT,
	TARGET_PLAIN,
	TARGET_URI_LIST,
};

enum
{
	COL_DISPLAY_NAME,
	COL_PIXBUF,
	NUM_COLS
};

static GtkTargetEntry targets[ ] =
{
	{ "UTF8_STRING", 0, TARGET_UTF8_STRING },
	{ "COMPOUND_TEXT", 0, TARGET_COMPOUND_TEXT },
	{ "text/plain", 0, TARGET_PLAIN },
	{ "text/uri-list", 0, TARGET_URI_LIST }
};

static guint n_targets = G_N_ELEMENTS( targets );