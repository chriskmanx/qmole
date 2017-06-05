typedef struct XrefTable {
  int *data;
  int last;
  int n_alloc;
} XrefTable;

typedef struct PdfPageDesc {
  struct PdfObj *resources, *mediabox, *contents;
  int rotate;
} PdfPageDesc;

typedef struct PdfInfo {
  int startxref;
  struct PdfObj *trailerdict;
  int npages;
  struct PdfPageDesc *pages;
} PdfInfo;

typedef struct PdfObj {
  int type;
  int intval;
  double realval;
  char *str;
  int len, num;
  struct PdfObj **elts;
  char **names;
} PdfObj;

typedef struct PdfFont {
  int n_obj;
  gboolean used_in_this_page;
  char *filename;
  int font_id;
  gboolean is_truetype;
  int glyph_page;
  int glyphmap[256];
  int advance[256];
  char *glyphpsnames[256];
  int num_glyphs_used;
  // fields from the FT_Face
  gdouble ft2ps;
  int nglyphs;
  int ascender, descender, xmin, xmax, ymin, ymax; // in PDF font units
  gchar *fontname;
  int flags;
} PdfFont;

#define PDFTYPE_CST 0    // intval: true=1, false=0, null=-1
#define PDFTYPE_INT 1    // intval
#define PDFTYPE_REAL 2   // realval
#define PDFTYPE_STRING 3 // str, len
#define PDFTYPE_NAME 4   // str
#define PDFTYPE_ARRAY 5  // num, elts
#define PDFTYPE_DICT 6   // num, elts, names
#define PDFTYPE_STREAM 7 // dict: num, elts, names; data: str, len
#define PDFTYPE_REF 8    // intval, num

struct PdfObj *parse_pdf_object(char **ptr, char *eof);
void free_pdfobj(struct PdfObj *obj);
struct PdfObj *dup_pdfobj(struct PdfObj *obj);
struct PdfObj *get_pdfobj(GString *pdfbuf, struct XrefTable *xref, struct PdfObj *obj);
void make_xref(struct XrefTable *xref, int nobj, int offset);

gboolean pdf_parse_info(GString *pdfbuf, struct PdfInfo *pdfinfo, struct XrefTable *xref);

// main printing functions

gboolean print_to_pdf(char *filename);

#if GTK_CHECK_VERSION(2, 10, 0)
void print_job_render_page(GtkPrintOperation *print, GtkPrintContext *context, gint pageno, gpointer user_data);
#endif
