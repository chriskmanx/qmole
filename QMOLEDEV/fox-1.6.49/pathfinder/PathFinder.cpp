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
* $Id: PathFinder.cpp,v 1.125 2006/01/22 17:58:15 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fx.h"
#include "fxkeys.h"
#include "PathFinder.h"
#include "PropertyDialog.h"
#include "Preferences.h"
#include "CopyDialog.h"
#include "icons.h"
#include <stdio.h>
#include <stdlib.h>

// Support for extra image formats
#ifdef HAVE_PNG_H
#include "FXPNGImage.h"
#endif
#ifdef HAVE_JPEG_H
#include "FXJPGImage.h"
#endif
#ifdef HAVE_TIFF_H
#include "FXTIFImage.h"
#endif
#include "FXICOImage.h"
#include "FXTGAImage.h"
#include "FXRGBImage.h"
#include "FXPCXImage.h"



/*
  Notes:

  - Copy / Paste /Cut doesn't seem to work
  - If you drag some files to a certain directory in the dir-list, hilight the directory so
    we're sure we are dropping it in the right directory...
  - Settings dialog like the one in TextEdit....
  - Edit menu layout should change:

      Undo
      Redo
      separator
      Cut
      Copy
      Paste
      Delete
      separator
      Select
      DeSelect
      Invert Selection


  - Mount/Unmount functionality....
  - A special bookmark button like home/work for /mnt/cdrom or/and just the /mnt directory
  - Selecting multiple files and clicking Open With only displays the first file ...
    not the whole range you've selected.... mayby we should show the first and the last
    file of the selection: (file1.htm ... file99.htm)
  - Change 'Delete Files' dynamically  depending on the amount of files you've selected:
    so 1 file shows only Delete File....  same thing for the dialog that shows up....
  - Time in statusbar as in TextEdit

*/



/*******************************************************************************/

// Map
FXDEFMAP(PathFinderMain) PathFinderMainMap[]={
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_MAINWINDOW,PathFinderMain::onUpdTitle),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_DIRECTORYLIST,PathFinderMain::onCmdDirectory),
  FXMAPFUNC(SEL_CLICKED,PathFinderMain::ID_FILELIST,PathFinderMain::onCmdFileClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,PathFinderMain::ID_FILELIST,PathFinderMain::onCmdFileDblClicked),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,PathFinderMain::ID_FILELIST,PathFinderMain::onFileListPopup),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_ABOUT,PathFinderMain::onCmdAbout),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_DIRBOX,PathFinderMain::onCmdDirTree),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_UPDIRECTORY,PathFinderMain::onCmdUpDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_CLEAR_LOCATION,PathFinderMain::onCmdClearLocation),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GOTO_LOCATION,PathFinderMain::onCmdGotoLocation),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_UPDATE_FILES,PathFinderMain::onUpdFiles),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_STATUSLINE,PathFinderMain::onUpdStatusline),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GO_WORK,PathFinderMain::onCmdWorkDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GO_HOME,PathFinderMain::onCmdHomeDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GO_RECENT,PathFinderMain::onCmdRecentDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GO_BACKWARD,PathFinderMain::onCmdBackDirectory),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_GO_BACKWARD,PathFinderMain::onUpdBackDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GO_FORWARD,PathFinderMain::onCmdForwardDirectory),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_GO_FORWARD,PathFinderMain::onUpdForwardDirectory),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_SAVE_SETTINGS,PathFinderMain::onCmdSaveSettings),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_NEW_PATHFINDER,PathFinderMain::onCmdNewPathFinder),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_PROPERTIES,PathFinderMain::onCmdProperties),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_PROPERTIES,PathFinderMain::onUpdProperties),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_BOOKMARK,PathFinderMain::onCmdBookmark),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_CLIPBOARD_CUT,PathFinderMain::onCmdClipboardCut),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_CLIPBOARD_CUT,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_CLIPBOARD_COPY,PathFinderMain::onCmdClipboardCopy),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_CLIPBOARD_COPY,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_CLIPBOARD_PASTE,PathFinderMain::onCmdClipboardPaste),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_NEW,PathFinderMain::onCmdNew),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_NEW,PathFinderMain::onUpdNew),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_COPY,PathFinderMain::onCmdCopy),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_COPY,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_MOVE,PathFinderMain::onCmdMove),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_MOVE,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_LINK,PathFinderMain::onCmdLink),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_LINK,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_RENAME,PathFinderMain::onCmdRename),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_RENAME,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_DELETE,PathFinderMain::onCmdDelete),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_DELETE,PathFinderMain::onUpdSelected),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_FILEFILTER,PathFinderMain::onCmdFilter),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_GOTO_DIR,PathFinderMain::onCmdGotoDir),
  FXMAPFUNCS(SEL_COMMAND,PathFinderMain::ID_RUSR,PathFinderMain::ID_SVTX,PathFinderMain::onCmdChmod),
  FXMAPFUNCS(SEL_UPDATE,PathFinderMain::ID_RUSR,PathFinderMain::ID_SVTX,PathFinderMain::onUpdChmod),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_OWNER,PathFinderMain::onUpdOwner),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_GROUP,PathFinderMain::onUpdGroup),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_CREATED,PathFinderMain::onUpdCreateTime),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_MODIFIED,PathFinderMain::onUpdModifyTime),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_ACCESSED,PathFinderMain::onUpdAccessTime),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_TYPE,PathFinderMain::onUpdFileType),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_LOCATION,PathFinderMain::onUpdFileLocation),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_SIZE,PathFinderMain::onUpdFileSize),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_FILE_DESC,PathFinderMain::onUpdFileDesc),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_OPEN,PathFinderMain::onUpdHaveItem),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_OPEN,PathFinderMain::onCmdOpen),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_OPEN_WITH,PathFinderMain::onUpdHaveItem),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_OPEN_WITH,PathFinderMain::onCmdOpenWith),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_OPEN_WITH_EDITOR,PathFinderMain::onUpdHaveItem),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_OPEN_WITH_EDITOR,PathFinderMain::onCmdOpenWithEditor),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_RUN,PathFinderMain::onCmdRun),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_TERMINAL,PathFinderMain::onCmdTerminal),
  FXMAPFUNC(SEL_SIGNAL,PathFinderMain::ID_HARVEST,PathFinderMain::onSigHarvest),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_CLOSE_PREVIEW,PathFinderMain::onCmdClosePreview),
  FXMAPFUNC(SEL_UPDATE,PathFinderMain::ID_CLOSE_PREVIEW,PathFinderMain::onUpdClosePreview),
  FXMAPFUNCS(SEL_COMMAND,PathFinderMain::ID_IMAGE_ROTATE_LEFT,PathFinderMain::ID_IMAGE_ROTATE_RIGHT,PathFinderMain::onCmdRotateImage),
  FXMAPFUNCS(SEL_UPDATE,PathFinderMain::ID_IMAGE_ROTATE_LEFT,PathFinderMain::ID_IMAGE_ROTATE_RIGHT,PathFinderMain::onUpdRotateImage),
  FXMAPFUNC(SEL_COMMAND,PathFinderMain::ID_PREFERENCES,PathFinderMain::onCmdPreferences),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,PathFinderMain::ID_IMAGE_PREVIEW,PathFinderMain::onClickedImagePreview)
  };


// Object implementation
FXIMPLEMENT(PathFinderMain,FXMainWindow,PathFinderMainMap,ARRAYNUMBER(PathFinderMainMap))

/*******************************************************************************/


// Executable for another PathFinder
FXchar* PathFinderMain::pathfindercommand;

/*******************************************************************************/

// Make some windows
PathFinderMain::PathFinderMain(FXApp* a):FXMainWindow(a,"PathFinder",NULL,NULL,DECOR_ALL,0,0,800,600,0,0),bookmarkeddirs(a,"Bookmarked Directories"){

  // Make some icons
  foxbigicon=new FXGIFIcon(getApp(),foxbig);
  foxminiicon=new FXGIFIcon(getApp(),foxmini);
  cuticon=new FXBMPIcon(getApp(),cut,0,IMAGE_ALPHAGUESS);
  copyicon=new FXGIFIcon(getApp(),copyit);
  moveicon=new FXGIFIcon(getApp(),moveit);
  linkicon=new FXGIFIcon(getApp(),linkit);
  renameicon=new FXGIFIcon(getApp(),renameit);
  pasteicon=new FXBMPIcon(getApp(),paste,0,IMAGE_ALPHAGUESS);
  upicon=new FXBMPIcon(getApp(),dirup,0,IMAGE_ALPHAGUESS);
  homeicon=new FXGIFIcon(getApp(),home);
  backicon=new FXBMPIcon(getApp(),goback,0,IMAGE_ALPHAGUESS);
  forwicon=new FXBMPIcon(getApp(),goforw,0,IMAGE_ALPHAGUESS);
  bigiconsicon=new FXBMPIcon(getApp(),bigicons,0,IMAGE_ALPHAGUESS);
  miniiconsicon=new FXBMPIcon(getApp(),smallicons,0,IMAGE_ALPHAGUESS);
  detailsicon=new FXBMPIcon(getApp(),details,0,IMAGE_ALPHAGUESS);
  mapicon=new FXBMPIcon(getApp(),maphost,0,IMAGE_ALPHAGUESS);
  unmapicon=new FXBMPIcon(getApp(),unmaphost,0,IMAGE_ALPHAGUESS);
  propicon=new FXBMPIcon(getApp(),properties,0,IMAGE_ALPHAGUESS);
  deleteicon=new FXBMPIcon(getApp(),deleteit,0,IMAGE_ALPHAGUESS);
  setbookicon=new FXGIFIcon(getApp(),setbook);
  clrbookicon=new FXGIFIcon(getApp(),clrbook);
  workicon=new FXGIFIcon(getApp(),work);
  closeicon=new FXGIFIcon(getApp(),closepanel);
  locationicon=new FXGIFIcon(getApp(),location);
  entericon=new FXGIFIcon(getApp(),enter);
  rotatelefticon=new FXGIFIcon(getApp(),rotateleft);
  rotaterighticon=new FXGIFIcon(getApp(),rotateright);
  quiticon=new FXGIFIcon(getApp(),quit_gif);

  // Set application icons for Window Manager
  setIcon(foxbigicon);
  setMiniIcon(foxminiicon);

  // Make main window; set myself as the target
  setTarget(this);
  setSelector(ID_MAINWINDOW);

  // Site where to dock
  FXDockSite *docksite=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // Menu Bar
  dragshell1=new FXToolBarShell(this,FRAME_RAISED);
  FXMenuBar *menubar=new FXMenuBar(docksite,dragshell1,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);
  new FXToolBarGrip(menubar,menubar,FXMenuBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

  // Tool Bar
  dragshell2=new FXToolBarShell(this,FRAME_RAISED);
  toolbar=new FXToolBar(docksite,dragshell2,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);
  new FXToolBarGrip(toolbar,toolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

  // Location Bar
  dragshell3=new FXToolBarShell(this,FRAME_RAISED);
  locationbar=new FXToolBar(docksite,dragshell3,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);
  new FXToolBarGrip(locationbar,locationbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);


  // Properties panel
  property=new PropertyDialog(this);


  // Status bar
  statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|FRAME_RAISED|STATUSBAR_WITH_DRAGCORNER);
  FXStatusLine *statusline=statusbar->getStatusLine();
  statusline->setTarget(this);
  statusline->setSelector(ID_STATUSLINE);

  // Subtle plug for LINUX
  new FXButton(statusbar,"\tAbout PathFinder",foxminiicon,this,ID_ABOUT,LAYOUT_TOP|LAYOUT_RIGHT);

  // Pattern
  pattern=new FXComboBox(statusbar,25,this,ID_FILEFILTER,COMBOBOX_STATIC|LAYOUT_RIGHT|LAYOUT_FIX_WIDTH|FRAME_SUNKEN|FRAME_THICK, 0,0,200,0, 0,0,1,1);
  pattern->setNumVisible(4);

  // Caption before pattern
  new FXLabel(statusbar,"Pattern:",NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);


  // Make file associations object; shared between FXFileList and FXDirList
  associations=new FXFileDict(getApp());


  // Main window interior
  FXHorizontalFrame * splitterbox=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED,0,0,0,0, 2,2,2,2, 0,0);
  splitter=new FXSplitter(splitterbox,LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y|SPLITTER_TRACKING);
  group1=new FXVerticalFrame(splitter,LAYOUT_FIX_WIDTH|LAYOUT_FILL_Y|FRAME_THICK|FRAME_SUNKEN, 0,0,180,0, 0,0,0,0,0,0);
  group2=new FXVerticalFrame(splitter,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_THICK|FRAME_SUNKEN, 0,0,0,0, 0,0,0,0,0,0);

  // Header above folders
  FXHorizontalFrame *header1=new FXHorizontalFrame(group1,LAYOUT_FILL_X|FRAME_RAISED|FRAME_THICK,0,0,0,0, 0,0,0,0, 0,0);
  new FXLabel(header1,"Folders",NULL,LAYOUT_FILL_X|JUSTIFY_LEFT);
  new FXButton(header1,"",closeicon,group1,FXWindow::ID_HIDE,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 0,0,0,0);

  // Folder List
  dirlist=new FXDirList(group1,this,ID_DIRECTORYLIST,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_RIGHT|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|TREELIST_BROWSESELECT|DIRLIST_NO_OWN_ASSOC);
  dirlist->setAssociations(associations);
  dirlist->dropEnable();

  // Header above files
  FXHorizontalFrame *header2=new FXHorizontalFrame(group2,LAYOUT_FILL_X|FRAME_RAISED|FRAME_THICK,0,0,0,0, 0,0,0,0, 0,0);
  FXLabel* fileslabel=new FXLabel(header2,"Files in: ",NULL,LAYOUT_FILL_X|JUSTIFY_LEFT);
  fileslabel->setTarget(this);
  fileslabel->setSelector(ID_UPDATE_FILES);
  new FXButton(header2,"\tRotate left\tRotate image leftward 90 degrees.",rotatelefticon,this,ID_IMAGE_ROTATE_LEFT,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0,0,0,0,0);
  new FXButton(header2,"\tRotate right\tRotate image rightward 90 degrees.",rotaterighticon,this,ID_IMAGE_ROTATE_RIGHT,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0,0,0,0,0);
  new FXButton(header2,FXString::null,closeicon,this,ID_CLOSE_PREVIEW,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 0,0,0,0);

  // Switcher to either image or filelist
  switcher=new FXSwitcher(group2,LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);

  // File List
  filelist=new FXFileList(switcher,this,ID_FILELIST,LAYOUT_FILL_X|LAYOUT_FILL_Y|ICONLIST_BIG_ICONS|ICONLIST_AUTOSIZE|FILELIST_NO_OWN_ASSOC);
  filelist->setAssociations(associations);
  filelist->dropEnable();

  // Image view
  imagepreview=new FXImageView(switcher,NULL,this,ID_IMAGE_PREVIEW,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  imagepreview->enable(); // enable so we receive mouse messages


  // File menu pane
  filemenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);
  new FXMenuCommand(filemenu,"Open\t\tOpen selected file.",NULL,this,ID_OPEN);
  new FXMenuCommand(filemenu,"Open with...\t\tOpen selected file using program.",NULL,this,ID_OPEN_WITH);
  new FXMenuCommand(filemenu,"Open with editor\t\tOpen selected file in the editor.",NULL,this,ID_OPEN_WITH_EDITOR);
  new FXMenuSeparator(filemenu);
  new FXMenuCommand(filemenu,"&Run...\tCtrl-R\tRun a command.",NULL,this,ID_RUN);
  new FXMenuCommand(filemenu,"&Terminal...\tCtrl-T\tOpen Terminal.",NULL,this,ID_TERMINAL);
  new FXMenuSeparator(filemenu);
  new FXMenuCommand(filemenu,"Re&load\tCtl-L\tReload this directory.",NULL,filelist,FXFileList::ID_REFRESH);
  new FXMenuSeparator(filemenu);
  new FXMenuCommand(filemenu,"&New Directory...",NULL,this,ID_NEW);
  new FXMenuCommand(filemenu,"Delete\tDel\tDelete the selection.",deleteicon,this,ID_DELETE);
  new FXMenuSeparator(filemenu);
  new FXMenuCommand(filemenu,"New &PathFinder...\tCtrl-P\tStart another PathFinder.",foxminiicon,this,ID_NEW_PATHFINDER);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q\tQuit PathFinder",quiticon,this,ID_CLOSE);

  // Edit Menu Pane
  editmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Edit",NULL,editmenu);
  new FXMenuCommand(editmenu,"Cu&t\tCtl-X\tCut to clipboard.",cuticon,this,ID_CLIPBOARD_CUT);
  new FXMenuCommand(editmenu,"&Copy\tCtl-C\tCopy to clipboard.",copyicon,this,ID_CLIPBOARD_COPY);
  new FXMenuCommand(editmenu,"&Paste\tCtl-V\tPaste from clipboard.",pasteicon,this,ID_CLIPBOARD_PASTE);
  new FXMenuSeparator(editmenu);
  new FXMenuCommand(editmenu,"&Select All\tCtl-A\tSelect all icons",NULL,filelist,FXFileList::ID_SELECT_ALL);
  new FXMenuCommand(editmenu,"&Deselect All\t\tDeselect all icons",NULL,filelist,FXFileList::ID_DESELECT_ALL);
  new FXMenuCommand(editmenu,"&Invert Selection\t\tInvert selection",NULL,filelist,FXFileList::ID_SELECT_INVERSE);

  // Go Menu Pane
  gomenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Go",NULL,gomenu);
  new FXMenuCommand(gomenu,"&Up\t\tChange up one level.",upicon,this,ID_UPDIRECTORY);
  new FXMenuCommand(gomenu,"&Back\tCtl-B\tChange to previous directory.",backicon,this,ID_GO_BACKWARD);
  new FXMenuCommand(gomenu,"&Forward\tCtl-F\tChange to next directory.",forwicon,this,ID_GO_FORWARD);
  new FXMenuCommand(gomenu,"&Home\tCtl-H\tChange to home directory.",homeicon,this,ID_GO_HOME);
  new FXMenuCommand(gomenu,"&Work\tCtl-W\tChange to current working directory.",workicon,this,ID_GO_WORK);
  new FXMenuCommand(gomenu,"&Goto...\tCtl-G\tGoto directory.",NULL,this,ID_GOTO_DIR);
  new FXMenuCommand(gomenu,"Bookmark...\t\tBookmark current directory.",setbookicon,this,ID_BOOKMARK);
  new FXMenuCommand(gomenu,"&Clear...\t\tClear bookmarks.",clrbookicon,&bookmarkeddirs,FXRecentFiles::ID_CLEAR);
  FXMenuSeparator* sep1=new FXMenuSeparator(gomenu);
  sep1->setTarget(&bookmarkeddirs);
  sep1->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_1);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_2);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_3);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_4);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_5);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_6);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_7);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_8);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_9);
  new FXMenuCommand(gomenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_10);

  // Arrange menu
  arrangemenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Arrange",NULL,arrangemenu);
  new FXMenuRadio(arrangemenu,"&Details\t\tShow detail view.",filelist,FXFileList::ID_SHOW_DETAILS);
  new FXMenuRadio(arrangemenu,"&Small Icons\t\tShow small icons.",filelist,FXFileList::ID_SHOW_MINI_ICONS);
  new FXMenuRadio(arrangemenu,"&Big Icons\t\tShow big icons.",filelist,FXFileList::ID_SHOW_BIG_ICONS);
  new FXMenuSeparator(arrangemenu);
  new FXMenuRadio(arrangemenu,"&Rows\t\tView row-wise.",filelist,FXFileList::ID_ARRANGE_BY_ROWS);
  new FXMenuRadio(arrangemenu,"&Columns\t\tView column-wise.",filelist,FXFileList::ID_ARRANGE_BY_COLUMNS);

  // Sort menu
  sortmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Sort",NULL,sortmenu);
  new FXMenuRadio(sortmenu,"&Name\t\tSort by file name.",filelist,FXFileList::ID_SORT_BY_NAME);
  new FXMenuRadio(sortmenu,"&Type\t\tSort by file type.",filelist,FXFileList::ID_SORT_BY_TYPE);
  new FXMenuRadio(sortmenu,"&Size\t\tSort by file size.",filelist,FXFileList::ID_SORT_BY_SIZE);
  new FXMenuRadio(sortmenu,"T&ime\t\tSort by modification time.",filelist,FXFileList::ID_SORT_BY_TIME);
  new FXMenuRadio(sortmenu,"&User\t\tSort by user name.",filelist,FXFileList::ID_SORT_BY_USER);
  new FXMenuRadio(sortmenu,"&Group\t\tSort by group name.",filelist,FXFileList::ID_SORT_BY_GROUP);
  new FXMenuCheck(sortmenu,"&Reverse\t\tReverse sort direction.",filelist,FXFileList::ID_SORT_REVERSE);
  new FXMenuCheck(sortmenu,"&Ignore case\t\tIgnore case of names.",filelist,FXFileList::ID_SORT_CASE);

  // Preferences menu
  prefmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Options",NULL,prefmenu);
  new FXMenuCommand(prefmenu,"&Preferences...\t\tEdit Preferences.",NULL,this,ID_PREFERENCES);
  new FXMenuSeparator(prefmenu);
  new FXMenuCommand(prefmenu,"&Save Settings...\t\tSave current settings.",NULL,this,ID_SAVE_SETTINGS);

  // View Menu Pane
  viewmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&View",NULL,viewmenu);
  new FXMenuCheck(viewmenu,"Hidden &Directories\t\tShow hidden directories.",dirlist,FXDirList::ID_TOGGLE_HIDDEN);
  new FXMenuCheck(viewmenu,"Hidden &Files\t\tShow hidden files and directories.",filelist,FXFileList::ID_TOGGLE_HIDDEN);
  new FXMenuCheck(viewmenu,"&Preview Images\t\tShow thumbnail images.",filelist,FXFileList::ID_TOGGLE_IMAGES);
  new FXMenuSeparator(viewmenu);
  new FXMenuCheck(viewmenu,"Tree list\t\tShow or hide the tree list",group1,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,"Toolbar\t\tShow or hide tool bar",toolbar,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,"Location bar\t\tShow or hide location bar",locationbar,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCheck(viewmenu,"Status bar\t\tShow or hide status bar",statusbar,FXWindow::ID_TOGGLESHOWN);

  // Help menu
  helpmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Help",NULL,helpmenu,LAYOUT_RIGHT);
  new FXMenuCommand(helpmenu,"&About PathFinder...\t\tDisplay PathFinder About Panel.",NULL,this,ID_ABOUT,0);

  // Book Mark Menu
  bookmarkmenu=new FXMenuPane(this,POPUP_SHRINKWRAP);
  new FXMenuCommand(bookmarkmenu,"&Bookmark...\t\tBookmark current directory.",setbookicon,this,ID_BOOKMARK);
  new FXMenuCommand(bookmarkmenu,"&Clear...\t\tClear bookmarks.",clrbookicon,&bookmarkeddirs,FXRecentFiles::ID_CLEAR);
  FXMenuSeparator* sep2=new FXMenuSeparator(bookmarkmenu);
  sep2->setTarget(&bookmarkeddirs);
  sep2->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_1);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_2);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_3);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_4);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_5);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_6);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_7);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_8);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_9);
  new FXMenuCommand(bookmarkmenu,FXString::null,NULL,&bookmarkeddirs,FXRecentFiles::ID_FILE_10);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,2,0);

  // Directory box
  dirbox=new FXDirBox(toolbar,this,ID_DIRBOX,DIRBOX_NO_OWN_ASSOC|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FIX_WIDTH|LAYOUT_CENTER_Y,0,0,180,0, 0,0, 1,1);
  dirbox->setAssociations(associations);
  dirbox->setNumVisible(5);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,2,0);

  // Add some toolbar buttons
  new FXButton(toolbar,"\tUp\tChange up one level.",upicon,this,ID_UPDIRECTORY,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tHome\tChange to home directory.",homeicon,this,ID_GO_HOME,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tWork\tChange to current working directory.",workicon,this,ID_GO_WORK,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXMenuButton(toolbar,"\tBookmarks\tVisit bookmarked directories.",setbookicon,bookmarkmenu,BUTTON_TOOLBAR|MENUBUTTON_NOARROWS|MENUBUTTON_ATTACH_LEFT|FRAME_RAISED);
  new FXButton(toolbar,"\tBack\tChange to previous directory.",backicon,this,ID_GO_BACKWARD,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tForward\tChange to next directory.",forwicon,this,ID_GO_FORWARD,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);
  new FXFrame(toolbar,FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,2,22);
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);

  new FXButton(toolbar,"\tMount\tMount device.",mapicon,NULL,0,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tUnmount\tUnmount device.",unmapicon,NULL,0,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

   // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);
  new FXFrame(toolbar,FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,2,22);
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);

//  new FXButton(toolbar,"\tCut\tCut to clipboard.",cuticon,this,ID_CLIPBOARD_CUT,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
//  new FXButton(toolbar,"\tCopy\tCopy to clipboard.",copyicon,this,ID_CLIPBOARD_COPY,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
//  new FXButton(toolbar,"\tPaste\tPaste from clipboard.",pasteicon,this,ID_CLIPBOARD_PASTE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tCopy\tCopy to clipboard.",copyicon,this,ID_COPY,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tMove\tMove file.",moveicon,this,ID_MOVE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tLink\tLink file.",linkicon,this,ID_LINK,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tRename\tRename file.",renameicon,this,ID_RENAME,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);
  new FXFrame(toolbar,FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,2,22);
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);

  new FXButton(toolbar,"\tProperties\tDisplay file properties.",propicon,this,ID_PROPERTIES,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);
  new FXFrame(toolbar,FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,2,22);
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);

  // Switch display modes
  new FXButton(toolbar,"\tBig Icons\tShow big icons.",bigiconsicon,filelist,FXFileList::ID_SHOW_BIG_ICONS,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tSmall Icons\tShow small icons.",miniiconsicon,filelist,FXFileList::ID_SHOW_MINI_ICONS,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
  new FXButton(toolbar,"\tDetails\tShow detail view.",detailsicon,filelist,FXFileList::ID_SHOW_DETAILS,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

  // Spacer
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);
  new FXFrame(toolbar,FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FIX_HEIGHT|LAYOUT_FIX_WIDTH,0,0,2,22);
  new FXFrame(toolbar,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH,0,0,4,0);

  // Delete button far away
  new FXButton(toolbar,"\tDelete\tDelete file.",deleteicon,this,ID_DELETE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);


  // Location bar
  new FXLabel(locationbar,"&Location:",NULL,LAYOUT_CENTER_Y);
  new FXButton(locationbar,"\tClear Location bar\tClear Location bar.",locationicon,this,ID_CLEAR_LOCATION,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_CENTER_Y);
  address=new FXTextField(locationbar,10,this,ID_GOTO_LOCATION,TEXTFIELD_NORMAL|JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 1,1,1,1);
  new FXButton(locationbar,"\tGo\tGo to location.",entericon,this,ID_GOTO_LOCATION,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_CENTER_Y);


  // Install some accelerators
  getAccelTable()->addAccel(MKUINT(KEY_BackSpace,0),this,FXSEL(SEL_COMMAND,ID_UPDIRECTORY));
  getAccelTable()->addAccel(MKUINT(KEY_Delete,0),this,FXSEL(SEL_COMMAND,ID_DELETE));
  getAccelTable()->addAccel(MKUINT(KEY_F1,0),this,FXSEL(SEL_COMMAND,ID_ABOUT));
  getAccelTable()->addAccel(MKUINT(KEY_Return,ALTMASK),this,FXSEL(SEL_COMMAND,ID_PROPERTIES));

  // Make a tool tip
  new FXToolTip(getApp(),0);

  // Recent directories
  visiting=0;

  // Image previewing and blending
  preview=TRUE;
  blending=FALSE;

  // Bookmarked directories
  bookmarkeddirs.setTarget(this);
  bookmarkeddirs.setSelector(ID_GO_RECENT);

  // Set patterns
  setPatterns("All Files (*)");
  setCurrentPattern(0);
  }


// Clean up
PathFinderMain::~PathFinderMain(){
  delete dragshell1;
  delete dragshell2;
  delete dragshell3;
  delete imagepreview->getImage();
  delete property;
  delete associations;
  delete filemenu;
  delete editmenu;
  delete viewmenu;
  delete gomenu;
  delete arrangemenu;
  delete sortmenu;
  delete prefmenu;
  delete helpmenu;
  delete bookmarkmenu;
  delete foxbigicon;
  delete foxminiicon;
  delete cuticon;
  delete copyicon;
  delete moveicon;
  delete linkicon;
  delete renameicon;
  delete pasteicon;
  delete upicon;
  delete homeicon;
  delete backicon;
  delete forwicon;
  delete bigiconsicon;
  delete miniiconsicon;
  delete detailsicon;
  delete mapicon;
  delete unmapicon;
  delete propicon;
  delete deleteicon;
  delete setbookicon;
  delete clrbookicon;
  delete workicon;
  delete closeicon;
  delete locationicon;
  delete entericon;
  delete rotatelefticon;
  delete rotaterighticon;
  delete quiticon;
  }


/*******************************************************************************/


// Open
long PathFinderMain::onCmdOpen(FXObject*,FXSelector,void*){
  FXint index=filelist->getCurrentItem();
  if(0<=index){

    // If directory, open the directory
    if(filelist->isItemDirectory(index)){
      FXString directory=filelist->getItemPathname(index);
      FXTRACE((100,"directory=%s\n",directory.text()));
      filelist->setDirectory(directory);
      dirlist->setDirectory(directory);
      dirbox->setDirectory(directory);
      address->setText(directory);
      visitDirectory(directory);
      }

    // If executable, execute it!
    else if(filelist->isItemExecutable(index)){
      FXString executable=FXPath::enquote(filelist->getItemPathname(index)) + " &";
      FXTRACE((100,"system(%s)\n",executable.text()));
      system(executable.text());
      }

    // If regular file return as the selected file
    else if(filelist->isItemFile(index)){
      FXFileAssoc *association=filelist->getItemAssoc(index);
      if(association){
        if(association->command.text()){
          FXString command=FXStringFormat(association->command.text(),FXPath::enquote(filelist->getItemPathname(index)).text());
          FXTRACE((100,"system(%s)\n",command.text()));
          system(command.text());
          }
        else{
          FXMessageBox::information(this,MBOX_OK,"Unknown Command","No command defined for file: %s",filelist->getItemFilename(index).text());
          }
        }
      else{
        FXMessageBox::information(this,MBOX_OK,"Unknown File Type","No association has been set for file: %s",filelist->getItemFilename(index).text());
        }
      }
    }
  return 1;
  }


// Gray out if no item selected
long PathFinderMain::onUpdHaveItem(FXObject* sender,FXSelector,void*){
  sender->handle(this,filelist->getCurrentFile().empty()?FXSEL(SEL_COMMAND,ID_DISABLE):FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// File Item was double-clicked
long PathFinderMain::onCmdFileDblClicked(FXObject*,FXSelector,void* ptr){
  FXint index=(FXint)(FXival)ptr;
  if(0<=index){

    // If directory, open the directory
    if(filelist->isItemDirectory(index)){
      FXString directory=filelist->getItemPathname(index);
      FXTRACE((100,"directory=%s\n",directory.text()));
      filelist->setDirectory(directory);
      dirlist->setDirectory(directory);
      dirbox->setDirectory(directory);
      address->setText(directory);
      visitDirectory(directory);
      }

    // If executable, execute it!
    else if(filelist->isItemExecutable(index)){
      FXString executable=FXPath::enquote(filelist->getItemPathname(index)) + " &";
      FXTRACE((100,"system(%s)\n",executable.text()));
      system(executable.text());
      }

    // If regular file return as the selected file
    else if(filelist->isItemFile(index)){

      // Load image if preview mode on
      if(preview){
        if(previewImage(filelist->getItemPathname(index))) return 1;
        }

      FXFileAssoc *association=filelist->getItemAssoc(index);
      if(association){
        if(association->command.text()){
          FXString command=FXStringFormat(association->command.text(),FXPath::enquote(filelist->getItemPathname(index)).text());
          FXTRACE((100,"system(%s)\n",command.text()));
          system(command.text());
          }
        else{
          FXMessageBox::information(this,MBOX_OK,"Unknown Command","No command defined for file: %s",filelist->getItemFilename(index).text());
          }
        }
      else{
        FXMessageBox::information(this,MBOX_OK,"Unknown File Type","No association has been set for file: %s",filelist->getItemFilename(index).text());
        }
      }
    }
  return 1;
  }


// File Item was clicked
long PathFinderMain::onCmdFileClicked(FXObject*,FXSelector,void*){
//   FXchar path[MAXPATHLEN+1],name[MAXPATHLEN+1],dir[MAXPATHLEN+1],*p;
//   FXOldIconItem *item=(FXOldIconItem*)ptr;
//   name[0]=0;
//   strcpy(path,filelist->getDirectory());
//   if(item){
//     strcpy(name,filelist->getItemText(item));
//     if((p=strchr(name,'\t'))) *p=0;
//     }
//   fxpathname(dir,path,name);
//   filename->setText(dir);
  return 1;
  }


// Goto location entered into the text field; a relative path or
// a path containing environment variable expansions is good too.
long PathFinderMain::onCmdGotoLocation(FXObject*,FXSelector,void*){
  FXString path=FXPath::absolute(getDirectory(),address->getText());
  FXString dir=path;

  // Go up to the lowest directory which still exists
  while(!FXPath::isTopDirectory(dir) && !FXStat::isDirectory(dir)){
    dir=FXPath::upLevel(dir);
    }

  // Move to this existing directory
  filelist->setDirectory(dir);
  dirlist->setDirectory(dir);
  dirbox->setDirectory(dir);
  address->setText(path);

  // Select tail end, was not good path
  if(dir.length()<path.length()){
    if(ISPATHSEP(path[dir.length()]))
      address->setSelection(dir.length()+1,path.length()-dir.length()-1);
    else
      address->setSelection(dir.length(),path.length()-dir.length());
    }

  // Mark this directory
  visitDirectory(dir);

  // Close preview
  closePreview();
  return 1;
  }


// Clear location bar
long PathFinderMain::onCmdClearLocation(FXObject*,FXSelector,void*){
  address->setText(FXString::null);
  return 1;
  }


// Popup menu for item in file list
long PathFinderMain::onFileListPopup(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  if(event->moved) return 1;
  FXint index=filelist->getItemAt(event->win_x,event->win_y);
  FXMenuPane pane(this);

  // We clicked in the background
  if(index<0){
    new FXMenuCommand(&pane,"&Up\t\tChange up one level.",upicon,this,ID_UPDIRECTORY);
    new FXMenuCommand(&pane,"&Back\t\tChange to previous directory.",backicon,this,ID_GO_BACKWARD);
    new FXMenuCommand(&pane,"&Forward\t\tChange to next directory.",forwicon,this,ID_GO_FORWARD);
    new FXMenuCommand(&pane,"&Home\t\tChange to home directory.",homeicon,this,ID_GO_HOME);
    new FXMenuCommand(&pane,"&Work\t\tChange to current working directory.",workicon,this,ID_GO_WORK);
    new FXMenuCommand(&pane,"Bookmark...\t\tBookmark this directory.",setbookicon,this,ID_BOOKMARK);
    new FXMenuSeparator(&pane);
    new FXMenuCascade(&pane,"Arrange",NULL,arrangemenu);
    new FXMenuSeparator(&pane);
    new FXMenuCascade(&pane,"Sort by",NULL,sortmenu);
    new FXMenuSeparator(&pane);
    new FXMenuCommand(&pane,"Reload\t\tReload this directory.",NULL,filelist,FXFileList::ID_REFRESH);
    }

  // We clicked on an item
  else{
    new FXMenuCommand(&pane,"Open...",NULL,this,ID_OPEN);
    new FXMenuCommand(&pane,"Open with...",NULL,this,ID_OPEN_WITH);
    new FXMenuCommand(&pane,"Open with editor",NULL,this,ID_OPEN_WITH_EDITOR);
    new FXMenuSeparator(&pane);
    new FXMenuCommand(&pane,"Copy",copyicon,this,ID_COPY);
    new FXMenuCommand(&pane,"Move",moveicon,this,ID_MOVE);
    new FXMenuCommand(&pane,"Link",linkicon,this,ID_LINK);
    new FXMenuCommand(&pane,"Rename",renameicon,this,ID_RENAME);
    new FXMenuCommand(&pane,"Delete",deleteicon,this,ID_DELETE);
    new FXMenuSeparator(&pane);
    new FXMenuCommand(&pane,"Properties...",propicon,this,ID_PROPERTIES);
    }
  pane.create();
  pane.popup(NULL,event->root_x,event->root_y);
  getApp()->runModalWhileShown(&pane);
  return 1;
  }


// About
long PathFinderMain::onCmdAbout(FXObject*,FXSelector,void*){
  FXMessageBox about(this,"About PathFinder","PathFinder File Browser V1.0\n\nUsing the FOX C++ GUI Library (http://www.fox-tookit.org)\n\nCopyright (C) 1998,2003 Jeroen van der Zijp (jeroen@fox-toolkit.org)",foxbigicon,MBOX_OK|DECOR_TITLE|DECOR_BORDER);
  about.execute();
  return 1;
  }


// Change the directory from the FXDirList
long PathFinderMain::onCmdDirectory(FXObject*,FXSelector,void* ptr){
  FXTreeItem *item=(FXTreeItem*)ptr;
  FXString path=dirlist->getItemPathname(item);
  filelist->setDirectory(path);
  dirbox->setDirectory(path);
  address->setText(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Change the directory from the FXDirBox
long PathFinderMain::onCmdDirTree(FXObject*,FXSelector,void*){
  FXString path=dirbox->getDirectory();
  dirlist->setDirectory(path);
  filelist->setDirectory(path);
  address->setText(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Move up one directory
long PathFinderMain::onCmdUpDirectory(FXObject*,FXSelector,void*){
  FXString path=FXPath::upLevel(filelist->getDirectory());
  setDirectory(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Move to home directory
long PathFinderMain::onCmdHomeDirectory(FXObject*,FXSelector,void*){
  FXString path=FXSystem::getHomeDirectory();
  setDirectory(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Move to work directory
long PathFinderMain::onCmdWorkDirectory(FXObject*,FXSelector,void*){
  FXString path=FXSystem::getCurrentDirectory();
  setDirectory(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Bookmark this directory
long PathFinderMain::onCmdBookmark(FXObject*,FXSelector,void*){
  bookmarkeddirs.appendFile(filelist->getDirectory());
  return 1;
  }


// Move to recent directory
long PathFinderMain::onCmdRecentDirectory(FXObject*,FXSelector,void* ptr){
  FXString path((FXchar*)ptr);
  setDirectory(path);
  visitDirectory(path);
  closePreview();
  return 1;
  }


// Move to previous directory
long PathFinderMain::onCmdBackDirectory(FXObject*,FXSelector,void*){
  if(visiting<9 && !visiteddir[visiting+1].empty()){
    setDirectory(visiteddir[++visiting]);
    closePreview();
    }
  return 1;
  }


// Update move to previous directory
long PathFinderMain::onUpdBackDirectory(FXObject* sender,FXSelector,void*){
  sender->handle(this,(visiting<9 && !visiteddir[visiting+1].empty())?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Move to next directory
long PathFinderMain::onCmdForwardDirectory(FXObject*,FXSelector,void*){
  if(0<visiting){
    setDirectory(visiteddir[--visiting]);
    closePreview();
    }
  return 1;
  }


// Update move to next directory
long PathFinderMain::onUpdForwardDirectory(FXObject* sender,FXSelector,void*){
  sender->handle(this,(0<visiting)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update title
long PathFinderMain::onUpdTitle(FXObject* sender,FXSelector,void*){
  FXString title="PathFinder:- " + filelist->getDirectory();
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&title);
  return 1;
  }


// Update files heading
long PathFinderMain::onUpdFiles(FXObject* sender,FXSelector,void*){
  FXString string="Files in: " + filelist->getDirectory();
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
  return 1;
  }


// Update status line to show some info about the icon the cursor is over
long PathFinderMain::onUpdStatusline(FXObject* sender,FXSelector,void*){
  FXint index;
  index=filelist->getCursorItem();
  if(0<=index){
    FXString info;
    FXFileItem *item=(FXFileItem*)filelist->getItem(index);
    FXFileAssoc *assoc=item->getAssoc();

    // What is this thing?
    if(item->isDirectory())       info="Directory: ";
    else if(item->isSymlink())    info="Symlink: ";
    else if(item->isSocket())     info="Socket: ";
    else if(item->isFifo())       info="Fifo: ";
    else if(item->isBlockdev())   info="BlockDev: ";
    else if(item->isChardev())    info="CharDev: ";
    else if(item->isExecutable()) info="Application: ";
    else                          info="File: ";

    // Add the name
    info+=filelist->getItemFilename(index);

    // Add size if its a file
    if(item->isFile()) info+=" ("+FXStringVal(item->getSize())+" bytes) ";

    // Add the extension
    if(assoc) info+=assoc->extension;

    // Set the status line
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&info);
    return 1;
    }
  return 0;
  }


// Save settings to disk
long PathFinderMain::onCmdSaveSettings(FXObject*,FXSelector,void*){
  saveSettings();
  getApp()->reg().write();
  return 1;
  }


// Spawn new PathFinder
long PathFinderMain::onCmdNewPathFinder(FXObject*,FXSelector,void*){
  FXString path=filelist->getDirectory();
  saveSettings();
  getApp()->reg().write();
  FXString command=FXStringFormat("%s %s &",pathfindercommand,path.text());
  system(command.text());
  return 1;
  }


// Pop up properties panel
long PathFinderMain::onCmdProperties(FXObject*,FXSelector,void*){
  property->show(PLACEMENT_OWNER);
  return 1;
  }


// Update Pop up properties panel buttons
long PathFinderMain::onUpdProperties(FXObject* sender,FXSelector,void*){
  if(!property->shown())
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }

/*******************************************************************************/

long PathFinderMain::onCmdClipboardCut(FXObject*,FXSelector,void*){
  return 1;
  }

long PathFinderMain::onCmdClipboardCopy(FXObject*,FXSelector,void*){
  return 1;
  }

long PathFinderMain::onCmdClipboardPaste(FXObject*,FXSelector,void*){
  return 1;
  }

/*******************************************************************************/

// Create new directory
long PathFinderMain::onCmdNew(FXObject*,FXSelector,void*){
  FXDialogBox dialog(this,"Create New Directory",DECOR_TITLE|DECOR_BORDER);
  const FXchar suggestedname[]="DirectoryName";
  FXVerticalFrame* content=new FXVerticalFrame(&dialog,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,10,10,10,10,10,10);
  new FXLabel(content,"Create new directory in: "+filelist->getDirectory(),NULL,LAYOUT_FILL_X|JUSTIFY_LEFT);
  FXTextField *text=new FXTextField(content,40,&dialog,FXDialogBox::ID_ACCEPT,TEXTFIELD_ENTER_ONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X);
  new FXHorizontalSeparator(content,SEPARATOR_GROOVE|LAYOUT_FILL_X);
  FXHorizontalFrame* buttons=new FXHorizontalFrame(content,LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,0,0,0,0);
  new FXButton(buttons,"&OK",NULL,&dialog,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT);
  new FXButton(buttons,"&Cancel",NULL,&dialog,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);
  dialog.create();
  text->setText(suggestedname);
  text->setFocus();
  text->setSelection(0,sizeof(suggestedname));
  if(dialog.execute()){
    FXString dirname=FXPath::absolute(filelist->getDirectory(),text->getText());
    if(FXStat::exists(dirname)){
      FXMessageBox::error(this,MBOX_OK,"Already Exists","File or directory %s already exists.\n",dirname.text());
      }
    else if(!FXDir::create(dirname)){
      FXMessageBox::error(this,MBOX_OK,"Cannot Create","Cannot create directory %s.\n",dirname.text());
      }
    }
  return 1;
  }


// Update create new directory
long PathFinderMain::onUpdNew(FXObject* sender,FXSelector,void*){
  FXString path=filelist->getDirectory();
  if(FXStat::isWritable(path))
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Copy file or directory
long PathFinderMain::onCmdCopy(FXObject*,FXSelector,void*){
  FXString *filenamelist=getFilenames();
  if(filenamelist){
    CopyDialog copydialog(this,"Copy File");
    FXString newname;
    for(FXint i=0; !filenamelist[i].empty(); i++){
      copydialog.setOldName(filenamelist[i]);
      copydialog.setNewName(FXPath::absolute(FXPath::directory(filenamelist[i]),"CopyOf"+FXPath::name(filenamelist[i])));
      if(!copydialog.execute()) break;
      newname=copydialog.getNewName();
      if(!FXFile::copyFiles(filenamelist[i],newname,FALSE)){
        if(FXMessageBox::error(this,MBOX_YES_NO,"Error Copying File","Unable to copy file:\n\n%s  to:  %s\n\nContinue with operation?",filenamelist[i].text(),newname.text())==MBOX_CLICKED_NO) break;
        }
      }
    delete [] filenamelist;
    }
  return 1;
  }


// Move file or directory
long PathFinderMain::onCmdMove(FXObject*,FXSelector,void*){
  FXString *filenamelist=getFilenames();
  if(filenamelist){
    CopyDialog copydialog(this,"Move File");
    FXString newname;
    for(FXint i=0; !filenamelist[i].empty(); i++){
      copydialog.setOldName(filenamelist[i]);
      copydialog.setNewName(filenamelist[i]);
      if(!copydialog.execute()) break;
      newname=copydialog.getNewName();
      if(!FXFile::moveFiles(filenamelist[i],newname,FALSE)){
	if(FXMessageBox::error(this,MBOX_YES_NO,"Error Moving File","Unable to move file:\n\n%s  to:  %s\n\nContinue with operation?",filenamelist[i].text(),newname.text())==MBOX_CLICKED_NO) break;
	}
      }
    delete [] filenamelist;
    }
  return 1;
  }


// Link file
long PathFinderMain::onCmdLink(FXObject*,FXSelector,void*){
  FXString *filenamelist=getFilenames();
  if(filenamelist){
    CopyDialog copydialog(this,"Link File");
    FXString newname;
    for(FXint i=0; !filenamelist[i].empty(); i++){
      copydialog.setOldName(filenamelist[i]);
      copydialog.setNewName(FXPath::absolute(FXPath::directory(filenamelist[i]),"LinkTo"+FXPath::name(filenamelist[i])));
      if(!copydialog.execute()) break;
      newname=copydialog.getNewName();
      if(!FXFile::symlink(filenamelist[i],newname)){
	if(FXMessageBox::error(this,MBOX_YES_NO,"Error Linking File","Unable to link file:\n\n%s  to:  %s\n\nContinue with operation?",filenamelist[i].text(),newname.text())==MBOX_CLICKED_NO) break;
	}
      }
    delete [] filenamelist;
    }
  return 1;
  }


// Rename file or directory
long PathFinderMain::onCmdRename(FXObject*,FXSelector,void*){
  FXString *filenamelist=getFilenames();
  if(filenamelist){
    CopyDialog copydialog(this,"Rename File");
    FXString newname;
    for(FXint i=0; !filenamelist[i].empty(); i++){
      copydialog.setOldName(FXPath::name(filenamelist[i]));
      copydialog.setNewName(FXPath::name(filenamelist[i]));
      if(!copydialog.execute()) break;
      newname=copydialog.getNewName();
      if(!FXFile::moveFiles(filenamelist[i],FXPath::absolute(FXPath::directory(filenamelist[i]),newname),FALSE)){
	if(FXMessageBox::error(this,MBOX_YES_NO,"Error Renaming File","Unable to rename file:\n\n%s  to:  %s\n\nContinue with operation?",filenamelist[i].text(),newname.text())==MBOX_CLICKED_NO) break;
	}
      }
    delete [] filenamelist;
    }
  return 1;
  }


// Delete file or directory
long PathFinderMain::onCmdDelete(FXObject*,FXSelector,void*){
/*
  FXDialogBox deletedialog(this,"Delete File",DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE);
  FXString file;
  FXHorizontalFrame* buttons=new FXHorizontalFrame(&deletedialog,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0, 0,0,0,0);
  new FXButton(buttons,"&Cancel",NULL,&deletedialog,FXDialogBox::ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y,0,0,0,0,20,20);
  new FXButton(buttons,"&OK",NULL,&deletedialog,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y);
  new FXHorizontalSeparator(&deletedialog,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
  FXMatrix *matrix=new FXMatrix(&deletedialog,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
  new FXLabel(matrix,"Delete files:",NULL,LAYOUT_FILL_X|JUSTIFY_LEFT|LAYOUT_FILL_ROW);
  FXHorizontalFrame *frame=new FXHorizontalFrame(matrix,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
  FXList *files=new FXList(frame,5,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LIST_MULTIPLESELECT|HSCROLLING_OFF);
  for(FXint i=0; i<filelist->getNumItems(); i++){
    if(filelist->isItemSelected(i)){
      file=filelist->getItemFilename(i);
      if(file=="..") continue;
      files->selectItem(files->appendItem(file));
      }
    }
  deletedialog.create();
  deletedialog.execute();
  */

  FXuint answer=FXMessageBox::warning(this,MBOX_YES_NO,"Deleting files","Are you sure you want to delete these files?");
  if(answer==MBOX_CLICKED_YES){
    FXString filetoremove,file;
    for(FXint i=0; i<filelist->getNumItems(); i++){
      if(filelist->isItemSelected(i)){
        file=filelist->getItemFilename(i);
        if(file=="..") continue;
        filetoremove=FXPath::absolute(filelist->getDirectory(),file);
        FXTRACE((100,"filetoremove=%s\n",filetoremove.text()));
        if(!FXFile::removeFiles(filetoremove,TRUE)){
          if(MBOX_CLICKED_NO==FXMessageBox::error(this,MBOX_YES_NO,"Error Deleting File","Unable to delete file: %s\nContinue with operation?",filetoremove.text())){
            break;
            }
          }
        }

      }
    }

  return 1;
  }


// Enable sender when items have been selected
long PathFinderMain::onUpdSelected(FXObject* sender,FXSelector,void*){
  sender->handle(this,getNumFilenames()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


/*******************************************************************************/


// Get number of selected filenames, not including "." and ".."
FXint PathFinderMain::getNumFilenames() const {
  register FXint num=0;
  register FXint i;
  if(filelist->getNumItems()){
    for(i=0; i<filelist->getNumItems(); i++){
      if(filelist->isItemSelected(i) && filelist->getItemFilename(i)!=".." && filelist->getItemFilename(i)!=".") num++;
      }
    }
  return num;
  }


// Return selected filenames, not including "." and ".."
FXString* PathFinderMain::getFilenames() const {
  register FXint num=getNumFilenames();
  register FXString *files=NULL;
  register FXint i,n;
  if(0<num){
    files=new FXString [num+1];
    for(i=n=0; i<filelist->getNumItems(); i++){
      if(filelist->isItemSelected(i) && filelist->getItemFilename(i)!=".." && filelist->getItemFilename(i)!="."){
	files[n++]=filelist->getItemPathname(i);
	}
      }
    files[n]=FXString::null;
    }
  return files;
  }


/*******************************************************************************/

// Sort functions
static const FXIconListSortFunc sortfuncs[]={
  FXFileList::ascendingCase,
  FXFileList::descendingCase,
  FXFileList::ascending,
  FXFileList::descending,
  FXFileList::ascendingType,
  FXFileList::descendingType,
  FXFileList::ascendingSize,
  FXFileList::descendingSize,
  FXFileList::ascendingTime,
  FXFileList::descendingTime,
  FXFileList::ascendingUser,
  FXFileList::descendingUser,
  FXFileList::ascendingGroup,
  FXFileList::descendingGroup
  };


// Save settings
void PathFinderMain::saveSettings(){
  FXString path;
  FXString filter;
  FXuint   iconview;
  FXuint   hiddenfiles;
  FXuint   thumbnails;
  FXuint   hiddendirs;
  FXuint   index;
  FXIconListSortFunc sortfunc;

  // Save pathfinder directory
  path=filelist->getDirectory();
  getApp()->reg().writeStringEntry("PathFinder Settings","directory",path.text());

  // Save file list mode
  iconview=filelist->getListStyle();
  getApp()->reg().writeUnsignedEntry("PathFinder Settings","iconview",iconview);

  // Showing hidden files...
  hiddenfiles=filelist->showHiddenFiles();
  getApp()->reg().writeUnsignedEntry("PathFinder Settings","hiddenfiles",hiddenfiles);

  // Showing hidden directories...
  hiddendirs=dirlist->showHiddenFiles();
  getApp()->reg().writeUnsignedEntry("PathFinder Settings","hiddendirs",hiddendirs);

  // Showing thumbnails...
  thumbnails=filelist->showImages();
  getApp()->reg().writeUnsignedEntry("PathFinder Settings","thumbnails",thumbnails);

  // Write new window size back to registry
  getApp()->reg().writeIntEntry("PathFinder Settings","x",getX());
  getApp()->reg().writeIntEntry("PathFinder Settings","y",getY());
  getApp()->reg().writeIntEntry("PathFinder Settings","width",getWidth());
  getApp()->reg().writeIntEntry("PathFinder Settings","height",getHeight());

  // Width of tree
  getApp()->reg().writeIntEntry("PathFinder Settings","dirwidth",splitter->getSplit(0));

  // Filter
  filter=filelist->getPattern();
  getApp()->reg().writeStringEntry("PathFinder Settings","filter",filter.text());

  // Header sizes
  getApp()->reg().writeIntEntry("PathFinder Settings","nameheader",filelist->getHeaderSize(0));
  getApp()->reg().writeIntEntry("PathFinder Settings","typeheader",filelist->getHeaderSize(1));
  getApp()->reg().writeIntEntry("PathFinder Settings","sizeheader",filelist->getHeaderSize(2));
  getApp()->reg().writeIntEntry("PathFinder Settings","dateheader",filelist->getHeaderSize(3));
  getApp()->reg().writeIntEntry("PathFinder Settings","userheader",filelist->getHeaderSize(4));
  getApp()->reg().writeIntEntry("PathFinder Settings","attrheader",filelist->getHeaderSize(5));

  // Visited directories
  getApp()->reg().writeStringEntry("Visited Directories","0",visiteddir[0].text());
  getApp()->reg().writeStringEntry("Visited Directories","1",visiteddir[1].text());
  getApp()->reg().writeStringEntry("Visited Directories","2",visiteddir[2].text());
  getApp()->reg().writeStringEntry("Visited Directories","3",visiteddir[3].text());
  getApp()->reg().writeStringEntry("Visited Directories","4",visiteddir[4].text());
  getApp()->reg().writeStringEntry("Visited Directories","5",visiteddir[5].text());
  getApp()->reg().writeStringEntry("Visited Directories","6",visiteddir[6].text());
  getApp()->reg().writeStringEntry("Visited Directories","7",visiteddir[7].text());
  getApp()->reg().writeStringEntry("Visited Directories","8",visiteddir[8].text());
  getApp()->reg().writeStringEntry("Visited Directories","9",visiteddir[9].text());

  // Visiting
  getApp()->reg().writeIntEntry("Visited Directories","visiting",visiting);

  // Editor command
  getApp()->reg().writeStringEntry("PathFinder Settings","editor",editor.text());

  // Terminal command
  getApp()->reg().writeStringEntry("PathFinder Settings","terminal",terminal.text());

  // Program last ran
  getApp()->reg().writeStringEntry("PathFinder Settings","program",program.text());

  // Preview
  getApp()->reg().writeIntEntry("PathFinder Settings","preview",preview);
  getApp()->reg().writeIntEntry("PathFinder Settings","blending",blending);

  // Toolbar, location bar, status bar shown
  getApp()->reg().writeIntEntry("PathFinder Settings","toolbar",toolbar->shown());
  getApp()->reg().writeIntEntry("PathFinder Settings","locationbar",locationbar->shown());
  getApp()->reg().writeIntEntry("PathFinder Settings","statusbar",statusbar->shown());

  // File patterns
  getApp()->reg().writeIntEntry("PathFinder Settings","filepatternno",getCurrentPattern());
  getApp()->reg().writeStringEntry("PathFinder Settings","filepatterns",getPatterns().text());

  // Sort function
  sortfunc=filelist->getSortFunc();
  for(index=ARRAYNUMBER(sortfuncs)-1; index; index--){ if(sortfuncs[index]==sortfunc) break; }
  getApp()->reg().writeIntEntry("PathFinder Settings","sorting",index);
  }


// Load settings
void PathFinderMain::loadSettings(){
  FXString path;
  FXString filter;
  FXuint   iconview;
  FXuint   hiddenfiles;
  FXuint   thumbnails;
  FXuint   hiddendirs;
  FXuint   sortfunc;
  FXint    ww,hh,xx,yy,tbshown,lbshown,sbshown;

  // Read last path setting
  path=getApp()->reg().readStringEntry("PathFinder Settings","directory","~");
  setDirectory(path);

  // Read icon view mode
  iconview=getApp()->reg().readUnsignedEntry("PathFinder Settings","iconview",ICONLIST_BIG_ICONS|ICONLIST_AUTOSIZE);
  filelist->setListStyle(iconview);

  // Showing hidden files...
  hiddenfiles=getApp()->reg().readUnsignedEntry("PathFinder Settings","hiddenfiles",FALSE);
  filelist->showHiddenFiles(hiddenfiles);

  // Showing thumbnails...
  thumbnails=getApp()->reg().readUnsignedEntry("PathFinder Settings","thumbnails",FALSE);
  filelist->showImages(thumbnails);

  // Showing hidden directories...
  hiddendirs=getApp()->reg().readUnsignedEntry("PathFinder Settings","hiddendirs",FALSE);
  dirlist->showHiddenFiles(hiddendirs);

  // Get size
  xx=getApp()->reg().readIntEntry("PathFinder Settings","x",100);
  yy=getApp()->reg().readIntEntry("PathFinder Settings","y",100);
  ww=getApp()->reg().readIntEntry("PathFinder Settings","width",800);
  hh=getApp()->reg().readIntEntry("PathFinder Settings","height",600);

  setX(xx);
  setY(yy);
  setWidth(ww);
  setHeight(hh);

  // Set tree width
  splitter->setSplit(0,getApp()->reg().readIntEntry("PathFinder Settings","dirwidth",100));

  // Filter
  filter=getApp()->reg().readStringEntry("PathFinder Settings","filter","*");
  filelist->setPattern(filter);

  // Header sizes
  filelist->setHeaderSize(0,getApp()->reg().readIntEntry("PathFinder Settings","nameheader",200));
  filelist->setHeaderSize(1,getApp()->reg().readIntEntry("PathFinder Settings","typeheader",100));
  filelist->setHeaderSize(2,getApp()->reg().readIntEntry("PathFinder Settings","sizeheader",60));
  filelist->setHeaderSize(3,getApp()->reg().readIntEntry("PathFinder Settings","dateheader",150));
  filelist->setHeaderSize(4,getApp()->reg().readIntEntry("PathFinder Settings","userheader",50));
  filelist->setHeaderSize(5,getApp()->reg().readIntEntry("PathFinder Settings","attrheader",60));

  // Visited directories
  visiteddir[0]=getApp()->reg().readStringEntry("Visited Directories","0",FXString::null);
  visiteddir[1]=getApp()->reg().readStringEntry("Visited Directories","1",FXString::null);
  visiteddir[2]=getApp()->reg().readStringEntry("Visited Directories","2",FXString::null);
  visiteddir[3]=getApp()->reg().readStringEntry("Visited Directories","3",FXString::null);
  visiteddir[4]=getApp()->reg().readStringEntry("Visited Directories","4",FXString::null);
  visiteddir[5]=getApp()->reg().readStringEntry("Visited Directories","5",FXString::null);
  visiteddir[6]=getApp()->reg().readStringEntry("Visited Directories","6",FXString::null);
  visiteddir[7]=getApp()->reg().readStringEntry("Visited Directories","7",FXString::null);
  visiteddir[8]=getApp()->reg().readStringEntry("Visited Directories","8",FXString::null);
  visiteddir[9]=getApp()->reg().readStringEntry("Visited Directories","9",FXString::null);

  // Visiting
  visiting=getApp()->reg().readIntEntry("Visited Directories","visiting",0);

  // Editor command
  editor=getApp()->reg().readStringEntry("PathFinder Settings","editor","adie");

  // Terminal command
  terminal=getApp()->reg().readStringEntry("PathFinder Settings","terminal","xterm");

  // Program last ran
  program=getApp()->reg().readStringEntry("PathFinder Settings","program","adie");

  // Preview
  preview=getApp()->reg().readIntEntry("PathFinder Settings","preview",TRUE);
  blending=getApp()->reg().readIntEntry("PathFinder Settings","blending",FALSE);

  // Toolbar, location bar, status bar shown
  tbshown=getApp()->reg().readIntEntry("PathFinder Settings","toolbar",1);
  lbshown=getApp()->reg().readIntEntry("PathFinder Settings","locationbar",1);
  sbshown=getApp()->reg().readIntEntry("PathFinder Settings","statusbar",1);
  if(!tbshown) toolbar->hide();
  if(!lbshown) locationbar->hide();
  if(!sbshown) statusbar->hide();

  // File patterns
  setPatterns(getApp()->reg().readStringEntry("PathFinder Settings","filepatterns","All Files (*)"));
  setCurrentPattern(getApp()->reg().readIntEntry("PathFinder Settings","filepatternno",0));

  // Sort function
  sortfunc=getApp()->reg().readIntEntry("PathFinder Settings","sorting",0);
  if(sortfunc>=ARRAYNUMBER(sortfuncs)) sortfunc=0;
  filelist->setSortFunc(sortfuncs[sortfunc]);
  }


// Close the window, saving settings
FXbool PathFinderMain::close(FXbool notify){
  saveSettings();
  return FXMainWindow::close(notify);
  }


// Switch to given directory
void PathFinderMain::setDirectory(const FXString& dir){
  FXString oldpath=getDirectory();
  FXString newpath=FXPath::absolute(oldpath,dir);
  filelist->setDirectory(newpath);
  dirbox->setDirectory(newpath);
  dirlist->setDirectory(newpath);
  address->setText(newpath);
  }


// Get current directory
FXString PathFinderMain::getDirectory() const {
  return filelist->getDirectory();
  }


// Visit directory
void PathFinderMain::visitDirectory(const FXString& dir){
  FXint i;
  if(visiting==0){
    for(i=9; i; i--) visiteddir[i]=visiteddir[i-1];
    }
  else{
    for(i=1; i+visiting-1<=9; i++) visiteddir[i]=visiteddir[i+visiting-1];
    for( ; i<10; i++) visiteddir[i]=FXString::null;
    }
  visiteddir[0]=dir;
  visiting=0;
  }


// Change patterns, each pattern separated by newline
void PathFinderMain::setPatterns(const FXString& patterns){
  FXString pat; FXint i;
  pattern->clearItems();
  for(i=0; !(pat=patterns.section('\n',i)).empty(); i++){
    pattern->appendItem(pat);
    }
  if(!pattern->getNumItems()) pattern->appendItem("All Files (*)");
  setCurrentPattern(0);
  }


// Return list of patterns
FXString PathFinderMain::getPatterns() const {
  FXString pat; FXint i;
  for(i=0; i<pattern->getNumItems(); i++){
    if(!pat.empty()) pat+='\n';
    pat+=pattern->getItemText(i);
    }
  return pat;
  }


// Set current pattern
void PathFinderMain::setCurrentPattern(FXint n){
  n=FXCLAMP(0,n,pattern->getNumItems()-1);
  pattern->setCurrentItem(n);
  dirlist->setPattern(FXFileSelector::patternFromText(pattern->getItemText(n)));
  }


// Return current pattern
FXint PathFinderMain::getCurrentPattern() const {
  return pattern->getCurrentItem();
  }


// Make application
void PathFinderMain::create(){
  loadSettings();
  FXMainWindow::create();
  show();
  }


// Change the pattern
long PathFinderMain::onCmdFilter(FXObject*,FXSelector,void* ptr){
  filelist->setPattern(FXFileSelector::patternFromText((FXchar*)ptr));
  return 1;
  }


// Goto directory
long PathFinderMain::onCmdGotoDir(FXObject*,FXSelector,void*){
  FXBMPIcon icon(getApp(),gotodir,0,IMAGE_ALPHAGUESS);
  FXString dir=getDirectory();
  if(FXInputDialog::getString(dir,this,"Goto Directory","&Goto directory:",&icon)){
    setDirectory(dir);
    closePreview();
    }
  return 1;
  }


// Open with program
long PathFinderMain::onCmdOpenWith(FXObject*,FXSelector,void*){
  FXString cmd=getApp()->reg().readStringEntry("SETTINGS","command","adie");
  FXString filename=filelist->getCurrentFile();
  if(FXInputDialog::getString(cmd,this,"Open File With","Open " + FXPath::name(filename) + " with:")){
    getApp()->reg().writeStringEntry("SETTINGS","command",cmd.text());
    FXString command=cmd+" "+filename+" &";
    system(command.text());
    /*
    // Spawn child
    if(fork()==0){
      // Close on exec of file descriptor
      //fcntl(fd,F_SETFD,TRUE);
      // Start command and pass it the filename
      execlp(cmd.text(),cmd.text(),filename.text(),NULL);

      // Just in case we failed to execute the command
      ::exit(0);
      }
    */
    }
  return 1;
  }


// Open this file with the editor
long PathFinderMain::onCmdOpenWithEditor(FXObject*,FXSelector,void*){
  FXString currentfile=filelist->getCurrentFile();
  if(!currentfile.empty()){
    FXString executable=editor+" "+FXPath::enquote(currentfile)+" &";
    FXTRACE((100,"system(%s)\n",executable.text()));
    system(executable.text());
    }
  return 1;
  }


// Run program
long PathFinderMain::onCmdRun(FXObject*,FXSelector,void*){
  FXString newprogram=program;
  if(FXInputDialog::getString(newprogram,this,"Run Program","Run Program:")){
    program=newprogram;
    FXString executeable="cd "+FXPath::enquote(getDirectory())+"; "+program+" &";
    system(executeable.text());
    }
  return 1;
  }


// Run terminal
long PathFinderMain::onCmdTerminal(FXObject*,FXSelector,void*){
  FXString executable="cd "+FXPath::enquote(getDirectory())+"; "+terminal+" &";
  system(executable.text());
  return 1;
  }


// Show preferences dialog
long PathFinderMain::onCmdPreferences(FXObject*,FXSelector,void*){
  Preferences preferences(this);
  preferences.setPatterns(getPatterns());
  preferences.setEditor(editor);
  preferences.setTerminal(terminal);
  preferences.setPreview(preview);
  preferences.setBlend(blending);
  preferences.setIconPath(associations->getIconPath());
  if(preferences.execute()){
    setPatterns(preferences.getPatterns());
    terminal=preferences.getTerminal();
    editor=preferences.getEditor();
    preview=preferences.getPreview();
    blending=preferences.getBlend();
    associations->setIconPath(preferences.getIconPath());
    }
  return 1;
  }


// Change mode
long PathFinderMain::onCmdChmod(FXObject*,FXSelector sel,void*){
  FXString filename=filelist->getCurrentFile();
  FXuint mode=FXStat::mode(filename);
#ifndef WIN32
  switch(FXSELID(sel)) {
    case ID_RUSR: mode^=FXIO::OwnerRead; break;
    case ID_WUSR: mode^=FXIO::OwnerWrite; break;
    case ID_XUSR: mode^=FXIO::OwnerExec; break;
    case ID_RGRP: mode^=FXIO::GroupRead; break;
    case ID_WGRP: mode^=FXIO::GroupWrite; break;
    case ID_XGRP: mode^=FXIO::GroupExec; break;
    case ID_ROTH: mode^=FXIO::OtherRead; break;
    case ID_WOTH: mode^=FXIO::OtherWrite; break;
    case ID_XOTH: mode^=FXIO::OtherExec; break;
    case ID_SUID: mode^=FXIO::SetUser; break;
    case ID_SGID: mode^=FXIO::SetGroup; break;
    case ID_SVTX: mode^=FXIO::Sticky; break;
    }
#endif
  if(!FXStat::mode(filename,mode)){
    FXMessageBox::error(this,MBOX_OK,"Error Changing Permissions","Unable to change permissions on file: %s",filename.text());
    }
  return 1;
  }


// Update change mode
long PathFinderMain::onUpdChmod(FXObject* sender,FXSelector sel,void*){
  FXString filename=filelist->getCurrentFile();
  FXuint mode=FXStat::mode(filename);
  FXuint test=0;
  switch(FXSELID(sel)) {
    case ID_RUSR: test=(mode&FXIO::OwnerRead); break;
    case ID_WUSR: test=(mode&FXIO::OwnerWrite); break;
    case ID_XUSR: test=(mode&FXIO::OwnerExec); break;
    case ID_RGRP: test=(mode&FXIO::GroupRead); break;
    case ID_WGRP: test=(mode&FXIO::GroupWrite); break;
    case ID_XGRP: test=(mode&FXIO::GroupExec); break;
    case ID_ROTH: test=(mode&FXIO::OtherRead); break;
    case ID_WOTH: test=(mode&FXIO::OtherWrite); break;
    case ID_XOTH: test=(mode&FXIO::OtherExec); break;
    case ID_SUID: test=(mode&FXIO::SetUser); break;
    case ID_SGID: test=(mode&FXIO::SetGroup); break;
    case ID_SVTX: test=(mode&FXIO::Sticky); break;
    }
  sender->handle(this,test ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Update owner
long PathFinderMain::onUpdOwner(FXObject* sender,FXSelector,void*){
  FXStat info;
  FXStat::statFile(filelist->getCurrentFile(),info);
  FXString owner=FXSystem::userName(info.user());
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&owner);
  return 1;
  }


// Update group
long PathFinderMain::onUpdGroup(FXObject* sender,FXSelector,void*){
  FXStat info;
  FXStat::statFile(filelist->getCurrentFile(),info);
  FXString group=FXSystem::groupName(info.group());
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&group);
  return 1;
  }


// Update create time
long PathFinderMain::onUpdCreateTime(FXObject* sender,FXSelector,void*){
  FXString time=FXSystem::time(FXStat::created(filelist->getCurrentFile()));
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&time);
  return 1;
  }


// Update modified time
long PathFinderMain::onUpdModifyTime(FXObject* sender,FXSelector,void*){
  FXString time=FXSystem::time(FXStat::modified(filelist->getCurrentFile()));
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&time);
  return 1;
  }


// Update access time
long PathFinderMain::onUpdAccessTime(FXObject* sender,FXSelector,void*){
  FXString time=FXSystem::time(FXStat::accessed(filelist->getCurrentFile()));
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&time);
  return 1;
  }


// Update file location
long PathFinderMain::onUpdFileLocation(FXObject* sender,FXSelector,void*){
  FXString location=filelist->getCurrentFile();
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&location);
  return 1;
  }

// Update file type
long PathFinderMain::onUpdFileSize(FXObject* sender,FXSelector,void*){
  FXString size=FXStringVal(FXStat::size(filelist->getCurrentFile()));
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&size);
  return 1;
  }



// Update file type
long PathFinderMain::onUpdFileType(FXObject* sender,FXSelector,void*){
  FXString filename=filelist->getCurrentFile();
  FXFileAssoc *fileassoc=NULL;
  FXString type;
  if(FXStat::isDirectory(filename)){
    fileassoc=associations->findDirBinding(filename.text());
    type="Folder";
    }
  else if(FXStat::isExecutable(filename)){
    fileassoc=associations->findExecBinding(filename.text());
    type="Application";
    }
  else{
    fileassoc=associations->findFileBinding(filename.text());
    type="Document";
    }
  if(fileassoc) type=fileassoc->extension;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&type);
  return 1;
  }


// Update file type
long PathFinderMain::onUpdFileDesc(FXObject* sender,FXSelector,void*){
  FXString filename=filelist->getCurrentFile();
  FXLabel *label=(FXLabel*)sender;
  FXFileAssoc *fileassoc=NULL;
  if(FXStat::isDirectory(filename)){
    fileassoc=associations->findDirBinding(filename.text());
    }
  else if(FXStat::isExecutable(filename)){
    fileassoc=associations->findExecBinding(filename.text());
    }
  else{
    fileassoc=associations->findFileBinding(filename.text());
    }
  label->setText(FXPath::name(filename));
  if(fileassoc){
    if(fileassoc->bigicon) fileassoc->bigicon->create();
    label->setIcon(fileassoc->bigicon);
    }
  else{
    label->setIcon(NULL); // FIXME need a default suggestion here
    }
  return 1;
  }


long PathFinderMain::onCmdRotateImage(FXObject*,FXSelector sel,void*){
  FXImage * image=imagepreview->getImage();
  image->rotate((FXSELID(sel)==ID_IMAGE_ROTATE_LEFT)?90:-90);
  imagepreview->setImage(image);
  return 1;
  }

long PathFinderMain::onUpdRotateImage(FXObject* sender,FXSelector,void*){
  sender->handle(this,imagepreview->getImage()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Close image previous panel
long PathFinderMain::onCmdClosePreview(FXObject*,FXSelector,void*){
  closePreview();
  return 1;
  }


// Close image preview
long PathFinderMain::onClickedImagePreview(FXObject*,FXSelector,void *ptr){
  if(((FXEvent*)ptr)->click_count==2) closePreview();
  return 1;
  }


// Update close image previous panel
long PathFinderMain::onUpdClosePreview(FXObject* sender,FXSelector,void*){
  sender->handle(this,switcher->getCurrent()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Load image for preview
FXbool PathFinderMain::previewImage(const FXString& filename){
  FXString ext=FXPath::extension(filename);
  FXImage *img=NULL;
  FXImage *old=NULL;

  // Determine type of image
  if(comparecase(ext,"gif")==0){
    img=new FXGIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"bmp")==0){
    img=new FXBMPImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"xpm")==0){
    img=new FXXPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"pcx")==0){
    img=new FXPCXImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"ico")==0 || comparecase(ext,"cur")==0){
    img=new FXICOImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"tga")==0){
    img=new FXTGAImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"rgb")==0){
    img=new FXRGBImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"pbm")==0 || comparecase(ext,"pgm")==0 || comparecase(ext,"pnm")==0 || comparecase(ext,"ppm")==0){
    img=new FXPPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"xbm")==0){
    img=new FXXBMImage(getApp(),NULL,NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"ppm")==0){
    img=new FXPPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"iff")==0 || comparecase(ext,"lbm")==0){
    img=new FXIFFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
  else if(comparecase(ext,"ras")==0){
    img=new FXRASImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
#ifdef HAVE_PNG_H
  else if(comparecase(ext,"png")==0){
    img=new FXPNGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
#endif
#ifdef HAVE_JPEG_H
  else if(comparecase(ext,"jpg")==0){
    img=new FXJPGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
#endif
#ifdef HAVE_TIFF_H
  else if(comparecase(ext,"tif")==0 || comparecase(ext,"tiff")==0){
    img=new FXTIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
#endif

  // Perhaps failed
  if(img){
    FXFileStream stream;

    // Load image
    if(stream.open(filename,FXStreamLoad)){
      getApp()->beginWaitCursor();
      img->loadPixels(stream);
      stream.close();

      // Blend over background
      if(blending){
        img->blend(imagepreview->getBackColor());
        }

      // Create image
      img->create();

      // Set new image, deleting old
      old=imagepreview->getImage();
      imagepreview->setImage(img);
      delete old;
      getApp()->endWaitCursor();

      // Switch to preview
      switcher->setCurrent(1);
      return TRUE;
      }
    }
  return FALSE;
  }


// Close preview
void PathFinderMain::closePreview(){
  delete imagepreview->getImage();
  imagepreview->setImage(NULL);
  switcher->setCurrent(0);
  }


// Harvest the zombies :-)
long PathFinderMain::onSigHarvest(FXObject*,FXSelector,void*){
#ifndef WIN32
  while(waitpid(-1,NULL,WNOHANG)>0);
#endif
  return 1;
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Create application
  FXApp application("PathFinder",FXString::null);

  // Keep original launch name
  PathFinderMain::pathfindercommand=argv[0];

  // Initialize application
  application.init(argc,argv);

  // Build GUI
  PathFinderMain* window=new PathFinderMain(&application);

  // On unix, we need to catch SIGCHLD to harvest zombie child processes.
#ifndef WIN32
  application.addSignal(SIGCHLD,window,PathFinderMain::ID_HARVEST,TRUE);
#endif

  // Also catch interrupt so we can gracefully terminate
  application.addSignal(SIGINT,window,PathFinderMain::ID_CLOSE);

  // Create window
  application.create();

  // If given, start in indicated directory
  if(argc==2) window->setDirectory(argv[1]);

  // Run the app now...
  return application.run();
  }

