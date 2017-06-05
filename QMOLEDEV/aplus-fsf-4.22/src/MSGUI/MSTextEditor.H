#ifndef MSTextEditorHEADER
#define MSTextEditorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSLabel.H>
#include <MSGUI/MSFontManager.H>
#include <MSGUI/MSWidget.H>
#include <MSGUI/MSBackingStorePixmap.H>

typedef struct ccf_ContextRec * ccf_Context;
typedef void * MSTextEditorTextTag;

class InsertContext;
class MSTextEditor;

class MSGUIExport MSTextEditorTypes
{
public:
  class InsertContext;

  enum ContentFormat {
	FormatUnknown,		// Unknown format 
	FormatIso2022,		// ISO 2022 octet stream 
	FormatTiff,		// TIFF 6.0 Base line 
	FormatJpeg,		// JPEG/JFIF stream 
	FormatGif,		// GIF89a stream 
	FormatCgm,		// CGM octet stream 
	FormatH261,		// H.261 octet stream 
	FormatOdif,		// ODA/ODIF stream 
	FormatRLE,		// RLE stream 
	FormatPBM		// PBMPLUS Image streams 
  };

  enum Scaling {
	NOSCALING,		// No scaling, visualize in real size 
	WIDTH,			// Width controlled, preserve aspect ratio 
	HEIGHT,			// Height controlled, preserve aspect ratio 
	MAXASPECT,		// Max size withing window, preserve aspect 
	FITAREA			// Fit exactly into window, change aspect 
  };

  enum InsertMode {
	RESET     = 0,	/* From initial state only */
	CURRENT   = -1,	/* From insert pnt + initial state */
	FOLLOWING = 1	/* Following insert pnt + init.state */
  };
  
  class TextFlowCallback;

  class MSGUIExport SnipMode
  {
  public:
    SnipMode(unsigned long bits_=0, MSTextEditorTextTag tag_=0, 
             unsigned short size_mod_=100)
        : bits(bits_), tag(tag_), size_modification(size_mod_), _callback(0) { }
    ~SnipMode();
    
    unsigned long bits;	/* Mode bit vector, must be at least 32 bits */
    MSTextEditorTextTag tag;		/* Logical Content TAG */
    unsigned short		/* Graphic Size Modification for the font
				** height. Specified as a percentage of the
				** actual font size as defined by fonts
				** resource. Default value is 100.
				*/
    size_modification;
    TextFlowCallback *_callback;
    SnipMode& operator=(const SnipMode&);
  };


  //
  // SnipData
  //
  struct SnipData
  {
	unsigned short refs;	/* # of Snip's referencing this data */
	unsigned short bytes;	/* # of bytes in character 1, 2 or 4 */
	char *character_set;	/* Character Set Registry & Encoding */
	XFontStruct *font;	/* Font information associated with data */
	char s[1];		/* Start of data, dynamically allocated */
  };

  enum SnipBreak
  {
	Break_NONE,
	Break_BPH,	/* Break Permitted Here */
	Break_NBH	/* No Break Permitted Here */
  };

  //
  // SnipSequenceEnd
  //	Is non-zero for the last Snip belonging to the sequence. End of
  //	sequence indicates also that the current position should be moved
  //	the left margin of the positioning area. Additionally, "NL" can
  //	be coded into it and causes the current position to move into the
  //	next line.
  //
  enum SnipSequenceEnd
  {
	NoEnd = 0,		/* Next Snip will continue the same sequence */
	End = 1,		/* End sequence, move to same "line origin" */
	EndLine = 2,	/* End sequence && terminate line */
	EndParagraph = 3	/* End sequence && terminate paragraph */
  };

  //
  // text flow callbacks
  //
  class MSGUIExport TextFlowCallback {
  public:
    typedef void (*CallbackFunction)(MSWidget*, const MSSymbol&);
    TextFlowCallback(CallbackFunction function_, MSWidget* owner_, 
						    const MSSymbol name_) :
      _function(function_), _name(name_), _owner(owner_), _ref_count(1) {}
    TextFlowCallback(TextFlowCallback *c_);
    MSWidget* owner()       { return _owner; }
    const MSSymbol&  name() { return _name; }
    CallbackFunction func() { return _function; }
    static void deref(TextFlowCallback *);
  private:
    CallbackFunction _function;
    MSSymbol _name;			// keep a local copy
    MSWidget * _owner;
    unsigned	_ref_count;		// callbacks are reference counted
  };

  //
  // Snip
  //

  class MSGUIExport Snip
  {
  public:
        Snip();
        ~Snip();
        MSBoolean hasLayoutContents(); 
        MSBoolean hasEditableContents();
        MSBoolean hasProtectedContents();
        MSBoolean hasEndPosition()      { return MSBoolean((unsigned int)endseq != NoEnd);              }
	MSBoolean hasEndLine()          { return MSBoolean(endseq & 2);                   }
	MSBoolean hasEndParagraph()     { return MSBoolean((unsigned int)endseq != EndParagraph);       }
	int virtualLength() 	        { return MSBoolean(length + hasEndPosition());    }
        Snip *previous();	      


	Snip *next;	/* Link to next Snip or NULL, if last */
	Snip **back;	/* Back link to from where this was pointed */
	SnipMode mode;		/* Current Snip state */
	/*
	** "Spot" flags concerning this snip (tab, tabref and quad are
	** only examined if endseq is non-zero).
	*/
	unsigned int tabref:16;	/* Tabulation Reference */
	unsigned int tab:1;	/* MSTrue, if Tabulation Reference present */
	unsigned int quad:3;	/* Forced quad (Alignment) */
	unsigned int brk:2;	/* NONE, BPH, NBH */
	unsigned int endseq:2;	/* This marks end of sequence */
	unsigned int space:1;	/* MSTrue, if Snip represents white space */
	unsigned int layout:1;	/* MSTrue, if layout information computed */
	unsigned int valid:1;	/* MSTrue, if layout need not be redone */
	unsigned int widget:1;	/* MSTrue, if this Snip is a child Widget */
	/*
	** 'floating' content is actually placed somewhere else and the
	** Snip acts as a place holder only. Sometimes it will be
	** useful to know this and ignore the xWidth, ascent and descent
	** temporarily for such Snip (currently only for widget Snips).
	*/
	unsigned int floating:1;/* MSTrue, if floating content */
	/*
	** NOTE:
	**	because the Widget variant is very rare compared to the
	**	text variant of Snip, it is guaranteed that the 'data'
	**	field is NULL and 'length' is ZERO when the widget
	**	variant is set. This way most processing has no need to test
	**	widget variant specially.
	*/
	union
	    {
		SnipData *head;		/* Pointer to head of Data */
		MSWidget *widget;	/* Data==NULL, length==0 */
	    } content;
	char *data;		/* Pointer to actual character codes */
	int length;		/* Number of characters */
	/*
	** Fields filled by the content layout process (layout information)
	** Snip rectangle:
	**	Upper Left Corner = (x, y - ascent)
	**	Height = ascent + descent
	**	Width = xWidth
	**
	** The flags layout and valid used as follows
	**
	**	layout	valid
	**	False	False	Default initial state. Snip has not been
	**			assigned a position on display yet.
	**	True	True	State, when layout information has been
	**			computed and it matches the current display
	**	True	False	State, when layout information reflects the
	**			current display, but the real position after
	**			layout would be elsewhere (needed when the
	**			content is deleted).
	**	False	True	Illegal combination. Should never appear.
	*/
	int x, y;		/* Position of this Snip */
	int xWidth;		/* The width of this element */
	short ascent;		/* Backward extent of this Snip */
	short descent;		/* Forward extent of this Snip */
	/*
	** 'offset' is mostly ZERO. When displaying the actual content
	** of the Snip, it is added to the 'x' value to get the real
	** x coordinate of the actual content. This is currently needed
	** in the implementation of the 'framed' rendition, but may have
	** other uses.
	*/
	short offset;		/* Offset from x point to real content */

	static Snip *Insert(Snip **list_);
	static Snip *Split(InsertContext *cx, Snip *t, int offset);
	static void Delete(Snip **list_);

	//
	// callback implementation support, creates a copy of the callback
	// object passed to it, and deletes the copy when the snip is
	// deleted.
	// Initially, callback() return 0. It returns the callback object
	// when a text flow callback has been set for this text.
	//
	MSBoolean activateCallback();
	TextFlowCallback *callback() { return mode._callback; }
	void callback(TextFlowCallback *callback_);
  };

  class MSGUIExport TextLocation {
  public:
	Snip *snip;
	int offset;
  };

  enum Alignment {
	NOALIGNMENT,
	START,
	END,
	CENTER,
	JUSTIFIED,
	AROUND
  };

  // TextExport
  //	Defines the valid values for the exportFormat resource. This
  //	resource selects the format for the exported selections,
  //	GetString and TextSaveAsFile. In selections, the formats
  //	are mapped to target atoms
  //
  //		STRING*	<-> STRING
  //		ODIF*	<-> COMPOUND_TEXT, STRING
  //
  //	If exportFormat is STRING*start_, the widget will only
  //	recognize STRING target; if ODIF*, the widget will
  //	recognize both STRING and COMPOUND_TEXT, and the ODIF variants
  //	are returned as COMPOUND_TEXT.
  //
  //	The 'formatted' variants will include the line breaks caused
  //	by possible word/line breaking algorithms. Source variant includes
  //	only original "hard" line breaks. The 'formatted processable'
  //	includes both source text and layout generated information
  //	delimited by <SOS>..<ST> controls (as defined in ODA standard).
  //
  // NOTE: The numeric values have been designed (and are used) such that
  //		value & 1 => include formatted information
  //		value & 2 => produce ODIF (COMPOUND_TEXT)
  //		value & 4 => produce ODIF <SOS> <ST> enclosings for layout
  //				control functions
  enum TextExport {
	STRING = 0,		// 0, Latin1 source text 
	STRING_F = 1,		// 1, Latin1 formatted text 
	ODIF = 2,		// 2, COMPOUND_TEXT source text 
	ODIF_F = 3,		// 3, COMPOUND_TEXT formatted text 
	ODIF_FP = 7		// 7, ODIF formatted processable 
  };

typedef struct EditorFontListRec *EditorFontList; 	// Defined in TextFont.c 

  class MSGUIExport EditorFont
  {
  public:
	int font_size;
	EditorFontList font_list;
  };

  class MSGUIExport Itemization
  {
  public:
	int identifier_alignment;
	int identifier_start_offset;
	int identifier_end_offset;
  };

  // TabStop
  //	XtNlineLayoutTable is a pointer to an array of TabStop. The
  //	last entry of the array must have negative value as a tabulation
  //	reference.
  class MSGUIExport TabStop
  {
  public:
	int reference;
	int position;
	int alignment;
	char alignment_string[4]; 	// Currently only 3 chars can be used 
  };


typedef struct _ClassRec *TextWidgetClass;
typedef struct _Rec      *TextWidget;


  //  Run all of the widget content trough the layout process and
  //  redisplay the new content.
  void Layout ();

  enum TextUnit {
	POINT,			// Current point 
	POSITION,		// Virtual positions 
	WORD,			// A word 
	LINE,			// A line 
	LINE_BEGIN,		// Beginning of current line 
	LINE_END,		// End of current line 
	PAGE,			// Page 
	PARAGRAPH,		// Paragraph 
	TAG,			// Content with the same tag 
	ALL			// All content, End/Begin "file" 
  };

  enum TextHighlight {
	UNUSED,		// Do not use in application 
	NOHIGHLIGHT,	// No special highlighting 
	REVERSE,	// Highlite with reverse video 
	FRAME		// Highlite with wire frame 
  } ;

  class MSGUIExport TextRegion {
  public:
  	TextRegion() : nrect(0), mode(NOHIGHLIGHT) { range[0]=range[1]=0; }
	TextHighlight mode;		// Highlite mode
	int nrect;			// Number rectangles in 'rect'
	long range[2];			// Affected text area (virtual offsets)
	XRectangle rect[3];		// Covering rectangles (for expose)
  };
	
  class MSGUIExport TextSelection {
  public:
  	TextSelection() : size(0), length(0), data(0) { }
  	~TextSelection() { if (data) free(data); }
	int size;  // Current Size of data buffer 
	int length;  // Bytes in the data buffer 
	char *data;  // Allocated data buffer
  };

  class MSGUIExport InsertContext {
  public:
	InsertContext();
	~InsertContext();
	void adjustLastPointer(Snip *s_);
	void updateExposeArea(int x_, int y_, int w_, int h_);
	void flushAppend();
	void initGxMap() { /* cmap?? */ }
	void lock(MSBoolean mode_) { _locked = mode_; }
        int  feedContent(const char *s_, long n_);
        void insertLocation(TextLocation *dot_, Region expose_);
        Snip *endContent(Region expose_);

	Snip *beginAppend(int position_, char *character_set_, int nbytes_);
	Snip *insertSnip(Snip **where_);
	Snip *appendSnip();

	SnipMode _mode;		    // Current Mode
	SnipMode _lock;		    // Locked Modes (mask bits)
	int _locked;		    // True if modes locked (InitialState)
	Snip **_list;		    // Current Snip List Head Pointer
	Snip *_last;		    // Current Last Snip
	int _pendingAccent;	    // Special for ISO 6937 conversion
	MSTextEditor *_editor;	    // Text Widget being processed
	Snip *_first;		    // First snip referencing current data
	int _size;		    // Total available bytes in data
	int _used;		    // Used bytes in data
	unsigned long _fontinfo;    // Font information of the current data
	SnipData *_head;	    // Current data being collected
	ccf_Context _ccf;	    // Text filtering context
	Region _expose;		    // Bounding region of changed area. This
				    // only covers the changes into snips that
				    // already had layout=MSTrue and which were
				    // changed. It cannot cover *new* inserted
				    // content. Empty, until width is non-zero.
  };

  enum MarkerId
  {
	MARK_Single,
	MARK_Double,
	MARK_Crossed,
	MARK_Overlined,
	MARK_Framed,
	MARK_Encircled,
	MARK_EnumSize	/* Number of elements in Enum */
  };

  class MSGUIExport CornerMark
  {
  public:
	int x, y, h;
	Snip *s;
  };

  class MSGUIExport ExposeContext
  {
  public:
	ExposeContext() : p(0,0,100) { }
	void markArea(Snip *s, Drawable d, MarkerId id, int x, int y, int h);
	void changeUnderline(Snip *s, Drawable d, int x, int y);
	void changeCrossedOut(Snip *s, Drawable d, int x, int y);
	void changeFraming(Snip *s, Drawable d, int x, int y);
        void changeForeground(Snip *s, Drawable d);
        void changeBackground(Snip *s, Drawable d);
	void flushPendingGraphics(Snip *s, int x, int y);

	MSTextEditor *editor;	/* Current Text Widget */
	Region my_r;		/* Expose Area */
	SnipMode p;		/* Current Snip Mode */
	CornerMark markers[(int)MARK_EnumSize];
  };

  //
  // LAYOUT_CONTENT_TAG
  //  is an internal and special tag value that is associated with the
  //  snips that are generated by this layout process self. It is used
  //  to differentiate between application generated layout and normal
  //  layout content. Currently application generated layout content
  //  gets NULL tag.
  //
  
  //
  //  The layout process needs to keep up quite a
  //  lot of state information. This state is kept in static
  //  variables local to this module to lessen the need to
  //  pass many between local functions here.
  //
  class MSGUIExport SequenceState
  {
  public:
    short maxAscent;  // Max character backward extent //
    short maxDescent;  // Max character forward extent //
    MSTextEditorTextTag tag;  // Current tag at this point //
    int wNow;  // Total width up to this point //
    int spaces;  // Number adjustable spaces *inside* line //
    int framed;  // Non-zero, if framed state //
  };
  
  //
  // ExpectEntry
  //  stores the original Snip layout information if it was laid out
  //  before this layout process. This is needed only because of the need
  //  to generate good expose area and terminate layout process as
  //  soon as possible after no changes start to happen to the line
  //  boxes (This is a huge ugly hack of jugling x,y values in Snips
  //  and I don't like it a bit.. --msa)
  //
  class MSGUIExport ExpectEntry
  {
  public:
    Snip *s;
    int x, y;
    int xWidth;
    short ascent, descent, offset;
    MSBoolean snip_moved;
  };
  
  //
  // LayoutContext
  //  collects all of the layout state variables into single structure
  //  which will be dynamically allocated for each layout process (cannot
  //  use global variables because it can happen that the layout process
  //  is called for another widget in the middle of layout.
  //
  class MSGUIExport LayoutContext
  {
  public:
    MSTextEditor *editor;  // Back Link to the Widget //
    int firstLine;  // True, if this is the first LineBox //
    int lineLength;  // Current available line length //
    int lineOffset;  // Offset of from the line Home //
    int lineSpacing;  // Current line spacing //
    short backwardExtent;  // Line box backward extent //
    short forwardExtent;  // Line box forward extent //
    short previousExtent;  // Forward extent of the previous line box //
    short    // Framing values for framed texts //
    top_framing, bottom_framing, left_framing, right_framing;
    int yShift;  // Font baseline shift from line baseline //
    //
    // Wrap around parameters
    //
    int leftWidgetIndent;
    int rightWidgetIndent;
  
    SequenceState current;  // Current sequence state //
    SequenceState lastSpace;// State at last space //
    Snip *firstSnip;  // First Snip of the sequence //
    Snip *beginLine;  // First Snip of the line box //
  };

  class MSGUIExport InsertCursor {
  public:
	enum CursorMode { UnderLine, IBar, Solid, NoCursor };
    InsertCursor(Display *, Window);
    ~InsertCursor();
    void draw(GC gc, Drawable d, XPoint *origin, int lineSpacing, int width);
    void getBounds(XRectangle *rect);
    void cursorMode(CursorMode newMode) { draw_mode = newMode; }
    CursorMode cusorMode() { return draw_mode; }
 
    int x, y;
    unsigned int w, h;
    char *bits;
    Pixmap pixmap;
    int valid;
    TextLocation location;
    short goal;
    long position;
    Display *display;
    Window  window;
    CursorMode draw_mode;
  };

  enum DeleteOrKill { TEXT_KILL, TEXT_DELETE };

  class MSGUIExport Vsb : public MSVScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
  public:
    Vsb(MSWidget *);
    ~Vsb(void);
  };

  class MSGUIExport Hsb : public MSHScrollBar
  {
  protected:
    virtual void drag(void);
    virtual void change(void);
  public:
    Hsb(MSWidget *);
    ~Hsb(void);
  };

  class MSGUIExport CursorTimer : public MSIntervalTimer
  {
  protected:
    MSTextEditor *_editor;
  public:
    CursorTimer(MSTextEditor *,unsigned long);
    ~CursorTimer(void);
    virtual void process(void);
  };

  class MSGUIExport Panner : public MSPrimitive
  {
  friend class MSTextEditor;
  protected:
    virtual void expose(const XEvent *);
  public:
    Panner(MSWidget *);
    ~Panner(void);
  };

};

class MSGUIExport MSTextEditor : public MSTextEditorTypes, public MSWidgetCommon
{
  friend class Vsb;
  friend class Hsb;
  friend class Panner;

public:
  MSTextEditor(MSWidget *,const char *title_=0);
  MSTextEditor(MSWidget *,const MSStringVector&);
  ~MSTextEditor(void);
  
  typedef int (*ExtractFeedFunction) (char *text, int len,		
  	   MSTextEditorTextTag content_TAG_value, void * client_data);
  void unfreeze();
  void freeze();
  void redraw();
  void redrawImmediately();
  MSBoolean saveInString(MSString &string_, MSBoolean textOnly_);
  MSBoolean saveAsFile(const char *name_ );
  MSBoolean loadFromFile(const char *name_ );
  char * getString(long *len_ );
  char * getSubstring(long *len_, long start_, long end_ );
  void extract(long start_, long end_, ExtractFeedFunction f_, void *data_);
  void replace(long start_, long end_, char *octects_, long len_);

  void insert(const char *str_, long len_);
  void insert(const char *str_);

  virtual void keyPress(const XEvent *pEvent_,KeySym keysym_,
				unsigned int state_,const char *buffer_);

  virtual void buttonPress(const XEvent *pEvent_);
  virtual void motionNotify(const XEvent *pEvent_);
  virtual void buttonRelease(const XEvent *pEvent_);

  void readonly(MSBoolean mode_);
  MSBoolean readonly();

  void deleteBackwardChar();
  void deleteBackwardWord();
  void deleteCurrentSelection();
  void deleteForwardChar();
  void deleteForwardWord();
  void deleteWholeContents();
  void insertNewLine();
  void insertNewLineAndBackup();
  void insertSelection();
  void killBackwardWord();
  void killCurrentSelection();
  void killForwardWord();
  void killToEndOfLine();
  void killToEndOfParagraph();
  void moveBackwardChar();
  void moveBackwardParagraph();
  void moveBackwardWord();
  void moveBackwardPage();
  void moveBeginningOfFile();
  void moveEndOfFile();
  void moveForwardChar();
  void moveForwardParagraph();
  void moveForwardWord();
  void moveForwardPage();
  void moveNextLine();
  void movePreviousLine();
  void moveToLineEnd();
  void moveToLineStart();
  void moveToXY(int x, int y);
  void redrawDisplay();

  void setColorBlack();
  void setColorRed();
  void setColorGreen();
  void setColorYellow();
  void setColorBlue();
  void setColorMagenta();
  void setColorCyan();
  void setColorWhite();
  void setColorDefault();
       
  void setBackColorBlack();
  void setBackColorRed();
  void setBackColorGreen();
  void setBackColorYellow();
  void setBackColorBlue();
  void setBackColorMagenta();
  void setBackColorCyan();
  void setBackColorWhite();
  void setBackColorDefault();

  static const char *GRSetFontDefault;
  static const char *GRSetFont1;  
  static const char *GRSetFont2;  
  static const char *GRSetFont3;  
  static const char *GRSetFont4;  
  static const char *GRSetFont5;  
  static const char *GRSetFont6;  
  static const char *GRSetFont7;  
  static const char *GRSetFont8;  
  static const char *GRSetFont9;  

  static const char *GRSetForegroundDefault;
  static const char *GRSetForegroundBlack;
  static const char *GRSetForegroundRed;
  static const char *GRSetForegroundGreen;
  static const char *GRSetForegroundYellow;
  static const char *GRSetForegroundBlue;
  static const char *GRSetForegroundMagenta;
  static const char *GRSetForegroundCyan;
  static const char *GRSetForegroundWhite;

  static const char *GRSetBackgroundDefault;
  static const char *GRSetBackgroundBlack;
  static const char *GRSetBackgroundRed;
  static const char *GRSetBackgroundGreen;
  static const char *GRSetBackgroundYellow;
  static const char *GRSetBackgroundBlue;
  static const char *GRSetBackgroundMagenta;
  static const char *GRSetBackgroundCyan;
  static const char *GRSetBackgroundWhite;

  void insertWidget(MSWidget *widget_);

  void escape();
  void tab();
  void shiftTab();

  TextFlowCallback *getInsertionPointCurrentCallback() const;
  static TextFlowCallback *NoCallback;

  const char * getInsertionPointCurrentForeground() const;
  const char * getInsertionPointCurrentBackground() const;
  const char * getInsertionPointCurrentFont() const;

  void replaceTagged(long start_, long end_, char *octects_, 
						long len_, MSTextEditorTextTag tag_);
  void insertPrefix(char *octects_, long len_);
  long getInsertionPoint();
  long scanLocation(TextUnit type_, int amount_, TextLocation *location_);
  long search(int amount_, char *string_, long len_, int caseSensitive_);
  MSTextEditorTextTag getInsertionTag();
  void setInsertionTag(MSTextEditorTextTag tag_);
  void getSelectionPosition(Snip **start_, Snip **end_);
  void setSelection(Snip *start_, Snip *end_);
  void unsetSelection();
  void getRegionPosition(TextRegion id_, long *start_, long *end_);
  int setRegion(long start_, long end_, TextHighlight mode_);
  int unsetRegion (TextRegion id_);
  Pixmap pixmap(void) { return _pixmap->pixmap(); }

  void setColors(const MSStringVector color_names_);
  const MSStringVector & getColors(void) { return _color_names; }

  void setFonts(const MSStringVector font_names_);

  void setFontDefault();
  void setFont1();
  void setFont2();
  void setFont3();
  void setFont4();
  void setFont5();
  void setFont6();
  void setFont7();
  void setFont8();
  void setFont9();

  // update notification methods - i.e. notify the widget that its font has
  // changed. The old value of the attribute is passed to the function
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  void selectionStart(const XEvent *e);
  void selectionExtendAdjust(const XEvent *e);
  void selectionAdjust(const XEvent *e);
  void selectionEnd(const XEvent *e);
  void selectionExtendStart(const XEvent *e);
  void selectWord(const XEvent *e);
  void selectAll(const XEvent *e);

  void moveInsertionPoint(unsigned long location_);

  friend MSTextEditor& operator<<(MSTextEditor&, const char *);

  MSBoolean isInFormattingMode() { return _format; }
  void format(MSBoolean yes_or_no_) { _format = yes_or_no_; }

  //
  // text flow callbacks
  //
  void insert(const char *str_, TextFlowCallback *callback_);
  friend MSTextEditor& operator<<(MSTextEditor&, TextFlowCallback *);

  void selectLine();
  void selectWord();
  void selectAll();

  static char *charsetDefault();
  static int  copy1_GL(InsertContext *cx, int n, unsigned char *s, 
					unsigned char *tt, char *cset);
  unsigned long color(int n);
  void setForeground(unsigned long pixel_);
  void setBackground(unsigned long pixel_);
  void switchColor();
  static void findExtents(Snip *s, Snip *e, int *ascent, int *descent);
  GC gc_Normal()    { return _mygc; }
  GC gc_XOR()       { return _mygcXOR; }


  int _rotation;           		// Rotation angle (degrees) [0-360]
  MSBoolean _resize;
  MSBoolean _content_loaded;
  MSBoolean _use_shm;
  MSBoolean _mirror_image;
  MSBoolean _parent_background;

  char *  _content_file;
  char *  _content_filter;
  FILE *  _content_stream;
  char *  _content_string;
  long    _content_length;
  long    _content_offset;

  Alignment _alignment;
  TextExport _export_format;
  int  _first_line_offset;
  EditorFont*  _fonts;
  Itemization * _itemization;
  TabStop * _line_layout_table;
  int * _kerning_offset;
  char * _graphic_rendition;
  char * _initial_state;
  MSBoolean _format;
  MSBoolean _proportional;
  MSBoolean _invert_shadow;
  int _line_spacing;
  int _defaultFontSize;
  int _column_width;
  int _indentation;
  int _shadow_contrast[2];   

  MSBoolean _allow_edit;			// True, if editing allowed
  MSBoolean _display_caret;		// True, if caret symbol is visible

  InsertCursor *cursor();		// Current insert cursor
  
protected:

  virtual void configure();

  void resetScrollbars();
  void updateScrollbars();
  void scrollbarsUpdated();
  int  numLines(void);
  int  computeMaxNumLines(void);
  int  numColumns(void);
  int  computeMaxNumColumns(void);
  int  firstLine(void);
  int  firstColumn(void);
  Vsb  *vsb() { return _vsb; }
  Hsb  *hsb() { return _hsb; }
  Panner *panner(void) const { return _panner; }

  int pWidth();
  int offset();
  int pHeight();

  InsertCursor *_cursor;		// Current insert cursor

private:

  void redisplay(XExposeEvent *event_, Region r, Drawable d);
  void initRefreshRegion();
  void updateCursorPosition(MSBoolean goal_, MSBoolean expose_, MSBoolean bounds_);
  void updateTextRegions(MSBoolean bounds_);
  void updateTextRegion(TextRegion *region_, MSBoolean bounds_);
  void doRefreshRegions();
  int  startEditing(InsertMode mode_, XEvent *event_);
  void endEditing(int deleted_);
  void layout(int text_width_, Snip *start_, Region region_);
  void adjustSnipFirst(TextLocation *location_);
  void adjustSnipBeginning(TextLocation *location_);
  void adjustSnipEnding(TextLocation *location_);
  InsertContext *insertContent(Snip *after_, int position_, int mode_);
  InsertContext *beginContent();
  long offset(TextLocation *dot_);
  void locations(long *offsets_, int n_, TextLocation *loc_return_, XPoint *p_);
  void coordinates(Snip *s, int offset, XPoint *xy);
  int  snipWidth(Snip *snip_, char *str_, int len_);
  long findPosition(XPoint *goal, TextLocation *dot);
  long findPosition(int x, int y);
  void snipExtents(Snip *snip_);
  void checkBounds(XRectangle *area_);
  XFontStruct *findFont(char *charset, EditorFontList font, int font_nr,
				int weight, int posture, int size);
  int setupContent(int dolayout, int allow_resize);
  void insetSnipExtents(LayoutContext *cx, Snip *t);
  void computeWidth(LayoutContext *cx, Snip *t);
  int  closeLineBox(LayoutContext *cx, Snip *end);
  Snip *wrapUpSequence(LayoutContext *cx, SequenceState *s, 
  						Snip *end, int x_adj);
  Snip *position_TEXT(LayoutContext *cx, SequenceState *text,
  					Snip *last, Snip *end);
  Snip *position_ITEM(LayoutContext *cx, SequenceState *item, Snip *last);
  char *lookFor(char *d, char *s, int n);
  Snip *splitLine(LayoutContext *cx, 
  					SequenceState *text, Snip *last);
  void updateExposeArea(XRectangle *r, int x, int y, 
  							int width, int height);
  int  updateExpose(XRectangle *expose, ExpectEntry *start, 
  				ExpectEntry *end, Snip *last, int mode);
  int  adjustForWidgets(LayoutContext *cx, Snip *s);
  void moveCursor(unsigned long location_);
  void moveCursor(TextUnit move, int amount);
  void moveCursor(int x, int y);
  void startMoving();
  void setOrigin();
  void endMoving();
  void notePosition(XEvent *event, XPoint *p);
  int deleteOrKillRange(int amount, DeleteOrKill mode);
  void deleteOrKill(TextUnit move, int amount, DeleteOrKill mode);
  Snip *deleteContent(InsertContext *cx, long amount);
  void initKeyTable();
  void init();
  void initColors();
  void freeColors();
  void modifySelection(const XEvent *event, int mode);
  void markSelection(Drawable d);
  MSString extractSelectedContent();
  CursorTimer *blinkTimer(void) const  { return _blinkTimer; }
  void processCursorTimer(void);
  void drawCursor(void);

  Vsb     *_vsb;
  Hsb     *_hsb;
  Panner  *_panner;

  //
  // insert_prefix and insert_prefix_length define an octet string
  // that will be implicitly inserted every time an insert point
  // is opened. It should contain only control sequences to change
  // the graphic presentation from the default initialState. The
  // modes set by insert_prefix will be locked. The value can be set
  // with InsertPrefix function.
  //
  char *_insert_prefix;
  long _insert_prefix_length;
  
  //
  // Information about the cursor location in various levels. To
  // speed up some processing, the some precomputed information
  // is maintained (matching the cursor_location).
  //
  // (cursor_x, cursor_y) is the current (x,y) location matching
  //	cursor_location.
  // cursor_location is the current (Snip, offset) pointer
  //	matching the cursor_position. This is information is
  //	the primary for defining the cursor position.
  // cursor_position
  //	is the virtual offset matching the cursor_location. This
  //	value is valid if cursor_valid is True (always True when
  //	the control is outside the TextEd widget).
  //
  TextSelection _s;			// Current text Selection 
  int _mult;				// Repeater count for actions
  Time _time;				// Time of the last event
  Region _refresh;			// Region requiring refresh on display
  //
  // Current Text Regions. A text region is a text segment defined by
  // two virtual offsets, and it may optionally be assocated with some
  // automatic highlighting. The first text region (id = 0) always
  // exists and is reserved for the selection mechanism. The remaining
  // entries are available for the application use and text_regions
  // array is resized as needed. The region identifier is an index
  // to this array. Unset region entries will be re-used, and the
  // text_regions array never shrinks.
  //
  int _num_regions;		// Highest region entry in use
  int _max_regions;		// Current size of text regions array
  TextRegion *_regions;		// Text regions array pointer

  int _enable_display;		// No layout and update processing while < 0
  InsertContext *_inserting;	// Non-NULL, if text inserting in progress
  int _num_tabs;		// Number of entries in lineLayoutTable
  int _set_width;		// The target width of the last layout
  Snip *_first;			// Snips of this block
  GC _mygc;
  GC _mygcXOR;
  MSBoolean _highlight;		// True when widget highlight is ON
  int _num_fonts;		// Number of fonts 

  MSStringVector _font_names; 	// user defined array of font names
  int _n_font_names;		// Number of font names

  MSStringVector _color_names; 	// user defined array of color names
  int _n_color_names;		// Number of color names
  unsigned long *_colors;	// n_color_names longs, if texts uses colors

  int _num_colors;         	// Actual count of different colors in image
  int _num_allocated;      	// Number of allocated colors
  unsigned long *_alloc;   	// List of allocated colors
  Colormap _cmap;          	// Private Colormap

  MSBackingStorePixmap *_pixmap;// used for double buffering

  XPoint  _origin;		// Origin used when scrolling

  void computeVisibleSequence(Region r);
  MSBoolean _readonly;

  CursorTimer    *_blinkTimer;
  friend class CursorTimer;
};

#endif 
