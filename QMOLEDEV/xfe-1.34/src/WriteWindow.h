#ifndef WRITEWINDOW_H
#define WRITEWINDOW_H

#include "InputDialog.h"


class WriteWindow;
class XFileWrite;

// Undo record for text fragment
class FXTextCommand : public FXCommand
{
    FXDECLARE_ABSTRACT(FXTextCommand)
protected:
    FXText *text;     // Text widget
    char *buffer;   // Character buffer
    int   pos;      // Character position
    int   ndel;     // Deleted characters
    int   nins;     // Inserted characters
public:
    FXTextCommand(FXText* txt,int p,int nd,int ni):text(txt),buffer(NULL),pos(p),ndel(nd),nins(ni)
    {}
    virtual unsigned int size() const;
    virtual ~FXTextCommand()
    {
        FXFREE(&buffer);
    }
};


// Insert command
class FXTextInsert : public FXTextCommand
{
    FXDECLARE_ABSTRACT(FXTextInsert)
public:
    FXTextInsert(FXText* txt,int p,int ni,const char* ins);
    virtual FXString undoName() const
    {
        return "Undo insert";
    }
    virtual FXString redoName() const
    {
        return "Redo insert";
    }
    virtual void undo();
    virtual void redo();
};


// Delete command
class FXTextDelete : public FXTextCommand
{
    FXDECLARE_ABSTRACT(FXTextDelete)
public:
    FXTextDelete(FXText* txt,int p,int nd,const char* del);
    virtual FXString undoName() const
    {
        return "Undo delete";
    }
    virtual FXString redoName() const
    {
        return "Redo delete";
    }
    virtual void undo();
    virtual void redo();
};


// Replace command
class FXTextReplace : public FXTextCommand
{
    FXDECLARE_ABSTRACT(FXTextReplace)
public:
    FXTextReplace(FXText* txt,int p,int nd,int ni,const char* del,const char* ins);
    virtual FXString undoName() const
    {
        return "Undo replace";
    }
    virtual FXString redoName() const
    {
        return "Redo replace";
    }
    virtual void undo();
    virtual void redo();
};


class Preferences : public DialogBox
{
    FXDECLARE(Preferences)
protected:
    FXIcon          *pal;
    FXIcon          *ind;
	FXTextField*    wrapmargin;
	FXString        wrapmargin_prev; 
	FXTextField*    tabsize;
	FXString        tabsize_prev; 
	FXCheckButton*	stripcr;
	FXbool	     	stripcr_prev;
	FXText*         editor;
	WriteWindow*     editwin;
	int           value_prev;
	FXColor         textcolor_prev;
	FXColor         backcolor_prev;
	FXColor         seltextcolor_prev;
	FXColor         selbackcolor_prev;
	FXColor         hilitetextcolor_prev;
	FXColor         hilitebackcolor_prev;
	FXColor         cursorcolor_prev;
	FXColor         barcolor_prev;
	FXColor         numbercolor_prev;
private:
    Preferences()
    {}
    Preferences(const Preferences&);
    Preferences& operator=(const Preferences&);
public:
    enum{
        ID_ACCEPT=DialogBox::ID_LAST,
		ID_CANCEL,
        ID_TEXT_BACK,
        ID_TEXT_FORE,
        ID_TEXT_SELBACK,
        ID_TEXT_SELFORE,
        ID_TEXT_HILITEBACK,
        ID_TEXT_HILITEFORE,
        ID_TEXT_CURSOR,
        ID_TEXT_NUMBACK,
        ID_TEXT_NUMFORE,
        ID_LAST
    };
public:

    // Create preferences dialog
    Preferences(WriteWindow *owner);

    // Owner is text window
    XFileWrite* getApp() const
    {
        return (XFileWrite*)DialogBox::getApp();
    }
	unsigned int execute(unsigned int);
	long onCmdCancel(FXObject*,FXSelector,void*);
	long onCmdAccept(FXObject*,FXSelector,void*);
    long onCmdTextBackColor(FXObject*,FXSelector,void*);
    long onUpdTextBackColor(FXObject*,FXSelector,void*);
    long onCmdTextForeColor(FXObject*,FXSelector,void*);
    long onUpdTextForeColor(FXObject*,FXSelector,void*);
    long onCmdTextSelBackColor(FXObject*,FXSelector,void*);
    long onUpdTextSelBackColor(FXObject*,FXSelector,void*);
    long onCmdTextSelForeColor(FXObject*,FXSelector,void*);
    long onUpdTextSelForeColor(FXObject*,FXSelector,void*);
    long onCmdTextHiliteBackColor(FXObject*,FXSelector,void*);
    long onUpdTextHiliteBackColor(FXObject*,FXSelector,void*);
    long onCmdTextHiliteForeColor(FXObject*,FXSelector,void*);
    long onUpdTextHiliteForeColor(FXObject*,FXSelector,void*);
    long onCmdTextCursorColor(FXObject*,FXSelector,void*);
    long onUpdTextCursorColor(FXObject*,FXSelector,void*);
    long onCmdTextBarColor(FXObject*,FXSelector,void*);
    long onUpdTextBarColor(FXObject*,FXSelector,void*);
    long onCmdTextNumberColor(FXObject*,FXSelector,void*);
    long onUpdTextNumberColor(FXObject*,FXSelector,void*);
};

// Editor main window
class WriteWindow : public FXMainWindow
{
    FXDECLARE(WriteWindow)
protected:
    FXToolBarShell      *dragshell;               // Shell for floating toolbar
    FXMenuPane          *filemenu;                // File menu
    FXMenuPane          *editmenu;                // Edit menu
    FXMenuPane          *searchmenu;              // Search menu
    FXMenuPane          *prefsmenu;               // Preferences menu
    FXMenuPane          *viewmenu;                // View menu
    FXMenuPane          *windowmenu;              // Window menu
    FXMenuPane          *helpmenu;                // Help menu
    FXMenuPane          *popupmenu;               // Popup menu
    FXHorizontalFrame   *undoredoblock;           // Undo/redo block on status line
    FXText              *editor;                  // Multiline text widget
    FXMenuBar           *menubar;                 // Menu bar
    FXToolBar           *toolbar;                 // Tool bar
    FXStatusBar         *statusbar;               // Status bar
    FXFont              *font;                    // Text window font
    FXUndoList           undolist;                // Undo list
    FXRecentFiles        mrufiles;                // Recent files list
    FXString             filename;                // File being edited
    FXTime               filetime;                // Original modtime of file
    FXbool               filenameset;             // Filename is set
    FXString             searchpath;              // To search for files
    FXbool               stripcr;                 // Strip carriage returns
    FXbool               linesnum;                // Lines numbering
	InputDialog         *printdialog;
	Preferences			*prefsdialog;
	FXSearchDialog      *searchdialog;
	FXReplaceDialog     *replacedialog;
	FXbool				smoothscroll;
	FXbool 				fromreg;			// Read window size and position from the regsitry
	unsigned int 				ww;			        // Window width
	unsigned int 				hh;					// Window height
	unsigned int 				xx;					// Window x position 
	unsigned int 				yy;					// Window y position
protected:
    void loadConfig();
    void saveConfig();
    FXString unique() const;
    WriteWindow *findUnused() const;
    WriteWindow *findWindow(const FXString& file) const;
    int backwardByContext(int pos) const;
    int forwardByContext(int pos) const;
protected:
    enum{
        MAXUNDOSIZE    = 1000000,               // Don't let the undo buffer get out of hand
        KEEPUNDOSIZE   = 500000                 // When MAXUNDOSIZE was exceeded, trim down to this size
    };
private:
    WriteWindow()
    {}
    WriteWindow(const WriteWindow&);
    WriteWindow& operator=(const WriteWindow&);
public:
    long onUpdateTitle(FXObject*,FXSelector,void*);
    long onFocusIn(FXObject*,FXSelector,void*);
    long onCmdAbout(FXObject*,FXSelector,void*);
	long onSigHarvest(FXObject*,FXSelector,void*);

    // File management
    long onCmdNew(FXObject*,FXSelector,void*);
    long onCmdOpen(FXObject*,FXSelector,void*);
    long onCmdOpenRecent(FXObject*,FXSelector,void*);
    long onCmdOpenSelected(FXObject*,FXSelector,void*);
    long onCmdSave(FXObject*,FXSelector,void*);
    long onUpdSave(FXObject*,FXSelector,void*);
    long onCmdSaveAs(FXObject*,FXSelector,void*);
    long onCmdFont(FXObject*,FXSelector,void*);
    long onCmdPrint(FXObject*,FXSelector,void*);

    // Text display
    long onCmdLineNumbers(FXObject*,FXSelector,void*);
    long onUpdLineNumbers(FXObject*,FXSelector,void*);
    long onCmdWrap(FXObject*,FXSelector,void*);
    long onUpdWrap(FXObject*,FXSelector,void*);
	long onCmdLinesNum(FXObject*,FXSelector,void*);
	long onUpdLinesNum(FXObject*,FXSelector,void*);

    // Text changes
    long onTextInserted(FXObject*,FXSelector,void*);
    long onTextReplaced(FXObject*,FXSelector,void*);
    long onTextDeleted(FXObject*,FXSelector,void*);
    long onTextRightMouse(FXObject*,FXSelector,void*);
    long onTextChanged(FXObject*,FXSelector,void*);
    long onEditDNDMotion(FXObject*,FXSelector,void*);
    long onEditDNDDrop(FXObject*,FXSelector,void*);

    // Miscellaneous
    long onUpdOverstrike(FXObject*,FXSelector,void*);
    long onUpdNumRows(FXObject*,FXSelector,void*);
    long onCmdMorePrefs(FXObject*,FXSelector,void*);
    long onCmdWindow(FXObject*,FXSelector,void*);
    long onUpdWindow(FXObject*,FXSelector,void*);
	long onCmdSearch(FXObject*,FXSelector,void*);
	long onCmdReplace(FXObject*,FXSelector,void*);
	long onCmdSearchSel(FXObject*,FXSelector,void*);
	long onCmdGotoLine(FXObject*,FXSelector,void*);

public:
    enum{
        ID_ABOUT=FXMainWindow::ID_LAST,
        ID_NEW,
        ID_OPEN,
        ID_OPEN_TREE,
        ID_OPEN_SELECTED,
        ID_OPEN_RECENT,
        ID_HARVEST,
        ID_SAVE,
        ID_SAVEAS,
        ID_FONT,
        ID_WINDOW,
        ID_PRINT,
        ID_TEXT_LINENUMS,
		ID_SEARCH,
		ID_REPLACE,
		ID_SEARCH_FORW_SEL,
		ID_SEARCH_BACK_SEL,
		ID_GOTO_LINE,
		ID_TOGGLE_WRAP,
        ID_TOGGLE_LINES_NUM,
        ID_TEXT,
        ID_INCLUDE_PATH,
        ID_OVERSTRIKE,
        ID_PREFERENCES,
        ID_NUM_ROWS,
        ID_WINDOW_1,
        ID_WINDOW_2,
        ID_WINDOW_3,
        ID_WINDOW_4,
        ID_WINDOW_5,
        ID_WINDOW_6,
        ID_WINDOW_7,
        ID_WINDOW_8,
        ID_WINDOW_9,
        ID_WINDOW_10,
        ID_WINDOW_11,
        ID_WINDOW_12,
        ID_WINDOW_13,
        ID_WINDOW_14,
        ID_WINDOW_15,
        ID_WINDOW_16,
        ID_WINDOW_17,
        ID_WINDOW_18,
        ID_WINDOW_19,
        ID_WINDOW_20,
        ID_WINDOW_21,
        ID_WINDOW_22,
        ID_WINDOW_23,
        ID_WINDOW_24,
        ID_WINDOW_25,
        ID_WINDOW_26,
        ID_WINDOW_27,
        ID_WINDOW_28,
        ID_WINDOW_29,
        ID_WINDOW_30,
        ID_WINDOW_31,
        ID_WINDOW_32,
        ID_WINDOW_33,
        ID_WINDOW_34,
        ID_WINDOW_35,
        ID_WINDOW_36,
        ID_WINDOW_37,
        ID_WINDOW_38,
        ID_WINDOW_39,
        ID_WINDOW_40,
        ID_WINDOW_41,
        ID_WINDOW_42,
        ID_WINDOW_43,
        ID_WINDOW_44,
        ID_WINDOW_45,
        ID_WINDOW_46,
        ID_WINDOW_47,
        ID_WINDOW_48,
        ID_WINDOW_49,
        ID_WINDOW_50,
        ID_LAST
    };
public:

    // Create new text window
    WriteWindow(XFileWrite* a,const FXString& file);

    // Create window
    virtual void create();

    // Detach window
    virtual void detach();

    // Close the window, return TRUE if actually closed
    virtual FXbool close(FXbool notify=FALSE);

    // Return XFileWrite application
    XFileWrite* getApp() const
    {
        return (XFileWrite*)FXMainWindow::getApp();
    }

    // Return this window's filename
    const FXString& getFilename() const
    {
        return filename;
    }

    // Change this window's filename
    void setFilename(const FXString& file)
    {
        filename=file;
    }

    // Has a filename been set or is it a new window
    FXbool isFilenameSet() const
    {
        return filenameset;
    }
	
	// Obtain a pointer on the text widget
	FXText* getEditor() const
	{
		return editor;
	}
	
	// Get the value of the stripcr flag
	FXbool getStripcr() const
	{
		return stripcr;
	}
	
	// Set the value of the stripcr flag
	void setStripcr(FXbool val)
	{
		stripcr=val;
	}

	void setSmoothScroll(FXbool smooth)
	{
		smoothscroll=smooth;
	}

    // Is it modified
    FXbool isModified() const;

    // Load text from file
    FXbool loadFile(const FXString& file);

    // Save text to file
    FXbool saveFile(const FXString& file);

    // Return TRUE if changes have been saved
    FXbool saveChanges();

    // Visit given line
    void visitLine(int line);

    // Delete text window
    virtual ~WriteWindow();
};


typedef FXObjectListOf<WriteWindow> WriteWindowList;

#endif

