#ifndef VIEWWINDOW_H
#define VIEWWINDOW_H

#include "InputDialog.h"


class ViewWindow;
class XFileView;

class ViewWindow : public FXMainWindow
{
    FXDECLARE(ViewWindow)
protected:
    FXMenuBar			*menubar;			// Menu bar
    FXMenuPane			*filemenu;			// File menu
    FXMenuPane			*editmenu;			// Edit menu
	FXMenuPane			*searchmenu;		// Search menu
    FXMenuPane			*prefsmenu;			// Preferences menu
	FXMenuPane			*windowmenu;		// Windows menu
    FXMenuPane			*helpmenu;			// Help menu
    FXHorizontalFrame	*contents;			// Sunken border for text widget
    FXToolBar			*toolbar;			// Toolbar
    FXText				*viewer;			// Multiline text widget
    FXStatusBar			*statusbar;			// Status bar
    FXString			filename;			// File being edited
    FXbool              filenameset;        // Filename is set
    FXRecentFiles       mrufiles;           // Recent files list
    FXFont              *font;              // Text window font
	InputDialog         *printdialog;
	DialogBox			*finddialog;
	FXTextField			*findtext;
	FXbool				smoothscroll;
	FXbool 				fromreg;			// Read window size and position from the regsitry
	unsigned int 				ww;			        // Window width
	unsigned int 				hh;					// Window height
	unsigned int 				xx;					// Window x position 
	unsigned int 				yy;					// Window y position
	FXString			searchstr;
private:
    ViewWindow()
    {}
    ViewWindow(const ViewWindow&);
    ViewWindow& operator=(const ViewWindow&);
protected:
    FXString unique() const;
    ViewWindow *findUnused() const;
    ViewWindow *findWindow(const FXString& file) const;
public:
    long onCmdAbout(FXObject*,FXSelector,void*);
    long onCmdOpen(FXObject*,FXSelector,void*);
    long onCmdOpenRecent(FXObject*,FXSelector,void*);
    long onCmdFont(FXObject*,FXSelector,void*);
	long onSigHarvest(FXObject*,FXSelector,void*);
    long onCmdQuit(FXObject*,FXSelector,void*);
    long onCmdFind(FXObject*,FXSelector,void*);
	long onCmdPrint(FXObject*,FXSelector,void*);
	long onUpdMenu(FXObject* sender,FXSelector,void*);
    long onCmdWindow(FXObject*,FXSelector,void*);
    long onUpdWindow(FXObject*,FXSelector,void*);
    long onUpdTitle(FXObject*,FXSelector,void*);

public:
    enum{
        ID_ABOUT=FXMainWindow::ID_LAST,
        ID_OPEN,
        ID_OPEN_RECENT,
        ID_FIND,
        ID_FIND_AGAIN,
        ID_FONT,
        ID_HARVEST,
        ID_QUIT,
		ID_PRINT,
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
    ViewWindow(XFileView* a,const FXString& file);

	// Create window
    virtual void create();

	// Destructor
	virtual ~ViewWindow();

	// Return XFileWrite application
    XFileView* getApp() const
    {
        return (XFileView*)FXMainWindow::getApp();
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

	void setSmoothScroll(FXbool smooth)
	{
		smoothscroll=smooth;
	}
    virtual FXbool close(FXbool notify=FALSE);
    FXbool loadFile(const FXString& file);
	void saveConfig();
    int find(const FXString,int);
};

typedef FXObjectListOf<ViewWindow> ViewWindowList;
#endif

