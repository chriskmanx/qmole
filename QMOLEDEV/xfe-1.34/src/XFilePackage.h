#ifndef XFILEPACKAGE_H
#define XFILEPACKAGE_H

#include "FileDialog.h"

class XFilePackage : public FXMainWindow
{
    FXDECLARE(XFilePackage)
protected:
    FXMenuBar			*menubar;			// Menu bar
    FXMenuPane			*filemenu;			// File menu
    FXMenuPane			*helpmenu;			// Help menu
    FXMenuPane			*prefsmenu;			// Preferences menu
    FXHorizontalFrame	*contents;			// Sunken border for text widget
    FXToolBar			*toolbar;			// Toolbar
    FXString 			filename;			// Current package name
    FXTreeList 			*list;				// File list
    FXText 				*description;		// Package description
    FXbool 				desc_clean;			// Clean description flag
    FXbool 				list_clean;			// Clean list flag
	FXbool				smoothscroll;
	FXbool 				errorflag;
protected:
    XFilePackage()
    {}
public:
    enum{
        ID_DESCRIPTION=FXMainWindow::ID_LAST,
        ID_FILELIST,
        ID_UNINSTALL,
        ID_INSTALL,
		ID_ABOUT,
		ID_OPEN,
        ID_HARVEST,
        ID_QUIT,
        ID_LAST
    };
    void start(FXString);
    void create();
    XFilePackage(FXApp*);
	~XFilePackage();
	void setSmoothScroll(FXbool smooth)
	{
		smoothscroll=smooth;
	}
    long onCmdUninstall(FXObject*,FXSelector,void*);
    long onCmdInstall(FXObject*,FXSelector,void*);
	long onCmdAbout(FXObject*,FXSelector,void*);
	long onCmdOpen(FXObject*,FXSelector,void*);
    long onUpdDescription(FXObject*,FXSelector,void*);
    long onUpdFileList(FXObject*,FXSelector,void*);
	void saveConfig();
	long onSigHarvest(FXObject*,FXSelector,void*);
    long onCmdQuit(FXObject*,FXSelector,void*);
};

#endif
