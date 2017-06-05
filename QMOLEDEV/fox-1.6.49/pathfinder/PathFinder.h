/********************************************************************************
*                                                                               *
*              T h e   P a t h F i n d e r   F i l e   B r o w s e r            *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: PathFinder.h,v 1.42 2006/01/22 17:58:15 fox Exp $                        *
********************************************************************************/
#ifndef PATHFINDER_H
#define PATHFINDER_H


class PropertyDialog;
class Preferences;


// PathFinder Main Window
class PathFinderMain : public FXMainWindow {
  FXDECLARE(PathFinderMain)
protected:
  PropertyDialog    *property;
  FXToolBarShell    *dragshell1;        // For floating menu bar
  FXToolBarShell    *dragshell2;        // For floating tool bar
  FXToolBarShell    *dragshell3;        // For floating location bar
  FXToolBar         *toolbar;           // Toolbar
  FXToolBar         *locationbar;       // Location bar
  FXStatusBar       *statusbar;         // Status bar
  FXMenuPane        *filemenu;
  FXMenuPane        *editmenu;
  FXMenuPane        *viewmenu;
  FXMenuPane        *gomenu;
  FXMenuPane        *arrangemenu;
  FXMenuPane        *sortmenu;
  FXMenuPane        *prefmenu;
  FXMenuPane        *helpmenu;
  FXMenuPane        *bookmarkmenu;
  FXDirBox          *dirbox;		// Directory box
  FXSwitcher        *switcher;		// Switcher for preview
  FXSplitter        *splitter;
  FXVerticalFrame   *group1;
  FXVerticalFrame   *group2;
  FXDirList         *dirlist;		// Directory tree
  FXFileList        *filelist;		// File list
  FXComboBox        *pattern;		// Filter pattern
  FXTextField       *address;		// Address field
  FXImageView       *imagepreview;	// Image preview panel
  FXRecentFiles      bookmarkeddirs;	// Bookmarked directories
  FXFileDict        *associations;	// File associations
  FXIcon            *foxbigicon;	// Icons
  FXIcon            *foxminiicon;
  FXIcon            *cuticon;
  FXIcon            *copyicon;
  FXIcon            *moveicon;
  FXIcon            *linkicon;
  FXIcon            *renameicon;
  FXIcon            *pasteicon;
  FXIcon            *upicon;
  FXIcon            *homeicon;
  FXIcon            *backicon;
  FXIcon            *forwicon;
  FXIcon            *bigiconsicon;
  FXIcon            *miniiconsicon;
  FXIcon            *detailsicon;
  FXIcon            *mapicon;
  FXIcon            *unmapicon;
  FXIcon            *propicon;
  FXIcon            *deleteicon;
  FXIcon            *setbookicon;
  FXIcon            *clrbookicon;
  FXIcon            *workicon;
  FXIcon            *closeicon;
  FXIcon            *locationicon;
  FXIcon            *entericon;
  FXIcon            *rotatelefticon;
  FXIcon            *rotaterighticon;
  FXIcon            *quiticon;
  FXString           editor;		// Editor command
  FXString           terminal;		// Terminal command
  FXString           program;		// Last program
  FXString           visiteddir[10];	// Visited directories
  FXint              visiting;		// Currently visited directory
  FXbool             preview;		// Preview mode
  FXbool             blending;		// Icon blending
protected:
  PathFinderMain(){}
  FXint getNumFilenames() const;
  FXString* getFilenames() const;
  void visitDirectory(const FXString& dir);
  void setPatterns(const FXString& patterns);
  FXString getPatterns() const;
  void setCurrentPattern(FXint n);
  FXint getCurrentPattern() const;
  FXbool previewImage(const FXString& filename);
  void closePreview();
public:
  long onCmdAbout(FXObject*,FXSelector,void*);
  long onCmdClosePreview(FXObject*,FXSelector,void*);
  long onUpdClosePreview(FXObject*,FXSelector,void*);
  long onCmdDirTree(FXObject*,FXSelector,void*);
  long onCmdDirectory(FXObject*,FXSelector,void*);
  long onCmdFileClicked(FXObject*,FXSelector,void*);
  long onCmdFileDblClicked(FXObject*,FXSelector,void*);
  long onCmdGotoLocation(FXObject*,FXSelector,void*);
  long onCmdClearLocation(FXObject*,FXSelector,void*);
  long onCmdUpDirectory(FXObject*,FXSelector,void*);
  long onUpdTitle(FXObject*,FXSelector,void*);
  long onUpdFiles(FXObject*,FXSelector,void*);
  long onCmdHomeDirectory(FXObject*,FXSelector,void*);
  long onCmdWorkDirectory(FXObject*,FXSelector,void*);
  long onCmdRecentDirectory(FXObject*,FXSelector,void*);
  long onCmdSaveSettings(FXObject*,FXSelector,void*);
  long onCmdNewPathFinder(FXObject*,FXSelector,void*);
  long onFileListPopup(FXObject*,FXSelector,void*);
  long onUpdStatusline(FXObject*,FXSelector,void*);
  long onCmdProperties(FXObject*,FXSelector,void*);
  long onUpdProperties(FXObject*,FXSelector,void*);
  long onCmdBookmark(FXObject*,FXSelector,void*);
  long onCmdBackDirectory(FXObject*,FXSelector,void*);
  long onUpdBackDirectory(FXObject*,FXSelector,void*);
  long onCmdForwardDirectory(FXObject*,FXSelector,void*);
  long onUpdForwardDirectory(FXObject*,FXSelector,void*);

  long onCmdClipboardCut(FXObject*,FXSelector,void*);
  long onCmdClipboardCopy(FXObject*,FXSelector,void*);
  long onCmdClipboardPaste(FXObject*,FXSelector,void*);

  long onCmdNew(FXObject*,FXSelector,void*);
  long onUpdNew(FXObject*,FXSelector,void*);
  long onCmdCopy(FXObject*,FXSelector,void*);
  long onCmdMove(FXObject*,FXSelector,void*);
  long onCmdLink(FXObject*,FXSelector,void*);
  long onCmdRename(FXObject*,FXSelector,void*);
  long onCmdDelete(FXObject*,FXSelector,void*);
  long onUpdSelected(FXObject*,FXSelector,void*);

  long onCmdFilter(FXObject*,FXSelector,void*);
  long onCmdGotoDir(FXObject*,FXSelector,void*);
  long onCmdChmod(FXObject*,FXSelector,void*);
  long onUpdChmod(FXObject*,FXSelector,void*);
  long onUpdOwner(FXObject*,FXSelector,void*);
  long onUpdGroup(FXObject*,FXSelector,void*);
  long onUpdCreateTime(FXObject*,FXSelector,void*);
  long onUpdModifyTime(FXObject*,FXSelector,void*);
  long onUpdAccessTime(FXObject*,FXSelector,void*);
  long onUpdFileType(FXObject*,FXSelector,void*);
  long onUpdFileLocation(FXObject*,FXSelector,void*);
  long onUpdFileSize(FXObject*,FXSelector,void*);
  long onUpdFileDesc(FXObject*,FXSelector,void*);
  long onCmdOpen(FXObject*,FXSelector,void*);
  long onCmdOpenWith(FXObject*,FXSelector,void*);
  long onCmdOpenWithEditor(FXObject*,FXSelector,void*);
  long onUpdHaveItem(FXObject*,FXSelector,void*);
  long onCmdRun(FXObject*,FXSelector,void*);
  long onCmdTerminal(FXObject*,FXSelector,void*);
  long onSigHarvest(FXObject*,FXSelector,void*);
  long onClickedImagePreview(FXObject*,FXSelector,void*);
  long onCmdRotateImage(FXObject*,FXSelector,void*);
  long onUpdRotateImage(FXObject*,FXSelector,void*);
  long onCmdPreferences(FXObject*,FXSelector,void*);
public:
  enum{
    ID_ABOUT=FXMainWindow::ID_LAST,
    ID_FILEFILTER,
    ID_DIRECTORYLIST,
    ID_FILELIST,
    ID_DIRBOX,
    ID_UPDATE_FILES,
    ID_UPDIRECTORY,
    ID_MAINWINDOW,
    ID_GOTO_LOCATION,
    ID_CLEAR_LOCATION,
    ID_GO_WORK,
    ID_GO_HOME,
    ID_GO_BACKWARD,
    ID_GO_FORWARD,
    ID_GO_RECENT,
    ID_SAVE_SETTINGS,
    ID_NEW_PATHFINDER,
    ID_STATUSLINE,
    ID_PROPERTIES,
    ID_BOOKMARK,
    ID_DELETE,
    ID_CLIPBOARD_CUT,
    ID_CLIPBOARD_COPY,
    ID_CLIPBOARD_PASTE,
    ID_COPY,
    ID_MOVE,
    ID_LINK,
    ID_RENAME,
    ID_NEW,
    ID_GOTO_DIR,
    ID_OPEN,
    ID_OPEN_WITH,
    ID_OPEN_WITH_EDITOR,
    ID_RUN,
    ID_TERMINAL,
    ID_RUSR,          // File modes
    ID_WUSR,
    ID_XUSR,
    ID_RGRP,
    ID_WGRP,
    ID_XGRP,
    ID_ROTH,
    ID_WOTH,
    ID_XOTH,
    ID_SUID,
    ID_SGID,
    ID_SVTX,
    ID_OWNER,         // File ownership
    ID_GROUP,
    ID_FILE_CREATED,
    ID_FILE_ACCESSED,
    ID_FILE_MODIFIED,
    ID_FILE_TYPE,
    ID_FILE_LOCATION,
    ID_FILE_SIZE,
    ID_FILE_DESC,
    ID_CLOSE_PREVIEW,
    ID_IMAGE_PREVIEW,
    ID_IMAGE_ROTATE_LEFT,
    ID_IMAGE_ROTATE_RIGHT,
    ID_HARVEST,
    ID_PREFERENCES,
    ID_LAST
    };
public:
  static FXchar* pathfindercommand;
public:

  // Construct window
  PathFinderMain(FXApp* a);

  // Create
  virtual void create();

  // Closed window
  virtual FXbool close(FXbool notify=FALSE);

  // Switch current directory
  void setDirectory(const FXString& dir);

  // Return current directory
  FXString getDirectory() const;

  // Return file associations
  FXFileDict *getAssociations() const { return associations; }

  // Save settings
  void saveSettings();

  // Load settings
  void loadSettings();

  // Destroy
  virtual ~PathFinderMain();
  };


#endif

