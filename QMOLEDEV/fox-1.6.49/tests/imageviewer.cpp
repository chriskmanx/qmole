/********************************************************************************
*                                                                               *
*                      I m a g e   V i e w e r   D e m o                        *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: imageviewer.cpp,v 1.114 2006/01/22 17:59:01 fox Exp $                    *
********************************************************************************/
#include "fx.h"
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
#include "FXPPMImage.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef WIN32
#include <unistd.h>
#endif


/*******************************************************************************/


// Mini application object
class ImageWindow : public FXMainWindow {
  FXDECLARE(ImageWindow)
protected:
  FXImageView       *imageview;             // Image viewer
  FXRecentFiles      mrufiles;              // Recent files
  FXString           filename;              // File being viewed
  FXMenuBar         *menubar;               // Menu bar
  FXToolBar         *toolbar;               // Tool bar
  FXStatusBar       *statusbar;             // Status bar
  FXSplitter        *splitter;              // Splitter
  FXHorizontalFrame *filebox;               // Box containing directories/files
  FXFileList        *filelist;              // File List
  FXMenuPane        *filemenu;              // File menu
  FXMenuPane        *editmenu;              // Edit menu
  FXMenuPane        *viewmenu;              // View menu
  FXMenuPane        *helpmenu;              // Help menu
  FXMenuPane        *manipmenu;             // Manipulations menu
  FXTextField       *filter;                // Filter for tree list
  FXIcon            *fileopenicon;          // Icons
  FXIcon            *filesaveicon;
  FXIcon            *cuticon;
  FXIcon            *copyicon;
  FXIcon            *pasteicon;
  FXIcon            *uplevelicon;
  FXIcon            *paletteicon;
protected:
  ImageWindow(){}
public:
  long onCmdAbout(FXObject*,FXSelector,void*);
  long onCmdOpen(FXObject*,FXSelector,void*);
  long onCmdSave(FXObject*,FXSelector,void*);
  long onCmdQuit(FXObject*,FXSelector,void*);
  long onUpdTitle(FXObject*,FXSelector,void*);
  long onCmdFileList(FXObject*,FXSelector,void*);
  long onCmdRecentFile(FXObject*,FXSelector,void*);
  long onCmdRotate(FXObject*,FXSelector,void*);
  long onCmdMirror(FXObject*,FXSelector,void*);
  long onCmdScale(FXObject*,FXSelector,void*);
  long onCmdCrop(FXObject*,FXSelector,void*);
  long onUpdImage(FXObject*,FXSelector,void*);
public:
  enum{
    ID_ABOUT=FXMainWindow::ID_LAST,
    ID_OPEN,
    ID_SAVE,
    ID_TITLE,
    ID_QUIT,
    ID_FILELIST,
    ID_RECENTFILE,
    ID_ROTATE_90,
    ID_ROTATE_180,
    ID_ROTATE_270,
    ID_MIRROR_HOR,
    ID_MIRROR_VER,
    ID_MIRROR_BOTH,
    ID_SCALE,
    ID_CROP,
    ID_LAST
    };
public:
  ImageWindow(FXApp* a);
  virtual void create();
  FXbool loadimage(const FXString& file);
  FXbool saveimage(const FXString& file);
  virtual ~ImageWindow();
  };


/*******************************************************************************/


// File Save
const unsigned char filesave[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0x00,0x80,0x80,0x80,0xff,0xff,0xff,0x00,
  0x00,0x00,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x42,0x08,0xaa,0xb1,0xfe,0x4c,0x34,0x06,0xa3,0x90,0x20,0xcc,0x1a,0xae,0xc4,0x95,
  0x75,0x6d,0x8f,0xa6,0x79,0x64,0xa6,0x0d,0x44,0x87,0x9a,0x0c,0xc1,0x9a,0xb4,0x3c,
  0x05,0x33,0xad,0xd7,0x43,0x50,0x0c,0xbf,0x20,0x10,0xb8,0x6a,0x0d,0x85,0x42,0x8d,
  0x11,0x79,0xec,0x65,0x7a,0x4c,0x64,0x23,0xb0,0x6c,0x26,0x55,0xd6,0xe6,0x6d,0xc7,
  0x55,0x24,0x00,0x00,0x3b
  };


// File Open
const unsigned char fileopen[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0x00,0xff,0x00,0xff,0xff,0x00,0x80,0x80,0x80,0xff,0xff,0xff,0x00,
  0x00,0x00,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x45,0x08,0xba,0x1c,0xfe,0x2c,0xae,0x20,0x44,0x90,0x91,0xd6,0x8b,0x95,0xb6,0x1c,
  0x00,0x79,0x95,0x25,0x3e,0x03,0x31,0x3c,0xa5,0x73,0xaa,0xf0,0xda,0xae,0x5e,0xfa,
  0xdc,0xb8,0xa3,0x06,0xc5,0xd0,0xff,0x3e,0x9f,0xc3,0x16,0x04,0x02,0x75,0x81,0xa2,
  0xb2,0x77,0x49,0xf2,0x96,0xc5,0x26,0x21,0x69,0x54,0x36,0xa9,0x50,0xa6,0xe7,0x59,
  0xfd,0x71,0x72,0x60,0x4e,0x02,0x00,0x3b
  };


// Cut
const unsigned char cut[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf1,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0x00,0x00,0x80,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,
  0x10,0x00,0x00,0x02,0x29,0x84,0x8f,0xa9,0x10,0xd7,0xeb,0x22,0x34,0x8f,0x4d,0xda,
  0x42,0x9d,0xf9,0x3a,0xed,0x49,0xa1,0x10,0x08,0x21,0x60,0x0a,0xea,0xa4,0xa6,0x86,
  0xa9,0xc0,0x2e,0xba,0xc8,0xf4,0x9d,0xd8,0x28,0x5c,0xf3,0xe7,0x54,0x00,0x00,0x3b
  };


// Copy
const unsigned char copy[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf1,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0xff,0xff,0xff,0x00,0x00,0x80,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,
  0x10,0x00,0x00,0x02,0x38,0x84,0x8f,0x69,0xc1,0xad,0x18,0x84,0x14,0xec,0xad,0x49,
  0xa9,0x8d,0x8c,0x8e,0x0f,0x42,0x98,0x30,0x60,0xdf,0xc1,0x35,0xe5,0x54,0x0e,0x97,
  0x49,0x45,0xa0,0x9b,0x32,0xeb,0x48,0xe3,0x71,0x43,0x02,0x8d,0x3a,0x92,0xb8,0x14,
  0xa5,0x9f,0xec,0x71,0x33,0x21,0x67,0x33,0x8b,0xf3,0x99,0x28,0x00,0x00,0x3b
  };


// Paste
const unsigned char paste[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0xff,0xff,0x00,0xc0,0xc0,0xc0,0x80,0x80,0x00,0x80,0x80,0x80,0x00,
  0x00,0x80,0xff,0xff,0xff,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x4b,0x08,0xba,0xdc,0x1e,0x30,0x38,0x15,0xa1,0x10,0x55,0x52,0x52,0x48,0xc0,0x5f,
  0xd0,0x15,0x9a,0x48,0x0e,0x68,0x2a,0x7a,0x1b,0x99,0x41,0x05,0x49,0x8d,0x5c,0x4d,
  0x97,0x36,0x61,0xec,0xbc,0x51,0xd2,0x05,0xc3,0x61,0x38,0xdc,0xb5,0x6a,0x42,0xe2,
  0x41,0x68,0x00,0xac,0x46,0xc2,0xc8,0x92,0x17,0xc8,0x25,0x95,0xc5,0x27,0x27,0x9a,
  0x59,0x3a,0x2b,0x57,0x6c,0xb3,0xd1,0x2b,0x8f,0x27,0x93,0x04,0x00,0x3b
  };


// Palette of colors
const unsigned char colorpal[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf2,0x00,0x00,0x80,0x80,0x80,
  0x00,0x00,0x00,0xff,0xff,0x00,0xff,0x00,0xff,0x00,0xff,0x00,0xff,0x00,0x00,0x00,
  0x00,0xff,0x00,0x00,0x00,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,0x00,0x03,
  0x47,0x08,0x0a,0xd1,0xfb,0x30,0x88,0xe9,0xe0,0x92,0x33,0x07,0xcb,0xf2,0xf8,0xd4,
  0xd3,0x64,0xc2,0x37,0x48,0x55,0x83,0x11,0x04,0x83,0x09,0x9b,0x23,0xb1,0x2d,0x49,
  0x6d,0xdd,0x42,0xc6,0x68,0x67,0xdf,0x23,0x58,0xa7,0x40,0x7c,0x05,0x85,0x12,0x62,
  0xe1,0x65,0xc3,0xa1,0x24,0x86,0x68,0x54,0x73,0xc9,0x48,0x0d,0x54,0xd1,0xf1,0x66,
  0x91,0xa9,0x70,0x11,0x30,0x07,0x90,0x00,0x00,0x3b
  };

const unsigned char tbuplevel[]={
  0x47,0x49,0x46,0x38,0x37,0x61,0x10,0x00,0x10,0x00,0xf1,0x00,0x00,0xb2,0xc0,0xdc,
  0x00,0x00,0x00,0xff,0xff,0x00,0xff,0xff,0xff,0x2c,0x00,0x00,0x00,0x00,0x10,0x00,
  0x10,0x00,0x00,0x02,0x32,0x84,0x8f,0x79,0xc1,0xac,0x18,0xc4,0x90,0x22,0x3c,0xd0,
  0xb2,0x76,0x78,0x7a,0xfa,0x0d,0x56,0xe4,0x05,0x21,0x35,0x96,0xcc,0x29,0x62,0x92,
  0x76,0xa6,0x28,0x08,0x8e,0x35,0x5b,0x75,0x28,0xfc,0xba,0xf8,0x27,0xfb,0xf5,0x36,
  0x44,0xce,0xe5,0x88,0x44,0x14,0x00,0x00,0x3b
  };

// Patterns
const FXchar patterns[]=
  "All Files (*)"
  "\nGIF Image (*.gif)"
  "\nBMP Image (*.bmp)"
  "\nXPM Image (*.xpm)"
  "\nPCX Image (*.pcx)"
  "\nICO Image (*.ico)"
  "\nRGB Image  (*.rgb)"
  "\nXBM Image  (*.xbm)"
  "\nTARGA Image  (*.tga)"
  "\nPPM Image  (*.ppm)"
#ifdef HAVE_PNG_H
  "\nPNG Image  (*.png)"
#endif
#ifdef HAVE_JPEG_H
  "\nJPEG Image (*.jpg)"
#endif
#ifdef HAVE_TIFF_H
  "\nTIFF Image (*.tif)"
#endif
  ;

/*******************************************************************************/

// Map
FXDEFMAP(ImageWindow) ImageWindowMap[]={
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_ABOUT,      ImageWindow::onCmdAbout),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_OPEN,       ImageWindow::onCmdOpen),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_SAVE,       ImageWindow::onCmdSave),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_TITLE,      ImageWindow::onUpdTitle),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_QUIT,       ImageWindow::onCmdQuit),
  FXMAPFUNC(SEL_SIGNAL,        ImageWindow::ID_QUIT,       ImageWindow::onCmdQuit),
  FXMAPFUNC(SEL_CLOSE,         ImageWindow::ID_TITLE,      ImageWindow::onCmdQuit),
  FXMAPFUNC(SEL_DOUBLECLICKED, ImageWindow::ID_FILELIST,   ImageWindow::onCmdFileList),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_RECENTFILE, ImageWindow::onCmdRecentFile),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_ROTATE_90,  ImageWindow::onCmdRotate),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_ROTATE_180, ImageWindow::onCmdRotate),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_ROTATE_270, ImageWindow::onCmdRotate),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_MIRROR_HOR, ImageWindow::onCmdMirror),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_MIRROR_VER, ImageWindow::onCmdMirror),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_MIRROR_BOTH,ImageWindow::onCmdMirror),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_SCALE,      ImageWindow::onCmdScale),
  FXMAPFUNC(SEL_COMMAND,       ImageWindow::ID_CROP,       ImageWindow::onCmdCrop),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_ROTATE_90,  ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_ROTATE_180, ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_ROTATE_270, ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_MIRROR_HOR, ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_MIRROR_VER, ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_MIRROR_BOTH,ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_SCALE,      ImageWindow::onUpdImage),
  FXMAPFUNC(SEL_UPDATE,        ImageWindow::ID_CROP,       ImageWindow::onUpdImage),
  };


// Object implementation
FXIMPLEMENT(ImageWindow,FXMainWindow,ImageWindowMap,ARRAYNUMBER(ImageWindowMap))


// Make some windows
ImageWindow::ImageWindow(FXApp* a):FXMainWindow(a,"FOX Image Viewer: - untitled",NULL,NULL,DECOR_ALL,0,0,850,600,0,0){
  setTarget(this);
  setSelector(ID_TITLE);

  // Make some icons
  fileopenicon=new FXGIFIcon(getApp(),fileopen);
  filesaveicon=new FXGIFIcon(getApp(),filesave);
  cuticon=new FXGIFIcon(getApp(),cut);
  copyicon=new FXGIFIcon(getApp(),copy);
  pasteicon=new FXGIFIcon(getApp(),paste);
  uplevelicon=new FXGIFIcon(getApp(),tbuplevel);
  paletteicon=new FXGIFIcon(getApp(),colorpal);

  // Make color dialog
  FXColorDialog *colordlg=new FXColorDialog(this,"Color Dialog");

  // Make menu bar
  menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

  // Tool bar
  toolbar=new FXToolBar(this,LAYOUT_SIDE_TOP|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT|FRAME_RAISED|LAYOUT_FILL_X);

  // Make menu bar
  //menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

  // Tool bar
  //FXHorizontalFrame* toolbarcontainer=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0, 0,0);
  //new FXToolBarTab(toolbarcontainer,NULL,0,FRAME_RAISED);
  //toolbar=new FXToolBar(toolbarcontainer,FRAME_RAISED|PACK_UNIFORM_WIDTH|LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 4,4,4,4, 0,0);

  // Status bar
  statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER);

  // File menu
  filemenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Edit Menu
  editmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Edit",NULL,editmenu);

  // Edit Menu
  manipmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Manipulation",NULL,manipmenu);

  // View menu
  viewmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&View",NULL,viewmenu);

  // Help menu
  helpmenu=new FXMenuPane(this);
  new FXMenuTitle(menubar,"&Help",NULL,helpmenu,LAYOUT_RIGHT);

  // Splitter
  splitter=new FXSplitter(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|SPLITTER_TRACKING|SPLITTER_VERTICAL|SPLITTER_REVERSED);

  // Sunken border for image widget
  FXHorizontalFrame *imagebox=new FXHorizontalFrame(splitter,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Make image widget
  imageview=new FXImageView(imagebox,NULL,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Sunken border for file list
  filebox=new FXHorizontalFrame(splitter,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // Make file list
  FXHorizontalFrame* fileframe=new FXHorizontalFrame(filebox,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
  filelist=new FXFileList(fileframe,this,ID_FILELIST,LAYOUT_FILL_X|LAYOUT_FILL_Y|ICONLIST_MINI_ICONS|ICONLIST_AUTOSIZE);
  new FXButton(filebox,"\tUp one level\tGo up to higher directory.",uplevelicon,filelist,FXFileList::ID_DIRECTORY_UP,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_FILL_Y);

  // Toobar buttons: File manipulation
  new FXButton(toolbar,"&Open\tOpen Image\tOpen image file.",fileopenicon,this,ID_OPEN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);
  new FXButton(toolbar,"&Save\tSave Image\tSave image file.",filesaveicon,this,ID_SAVE,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

  // Toobar buttons: Editing
  new FXButton(toolbar,"Cut\tCut",cuticon,NULL,0,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);
  new FXButton(toolbar,"Copy\tCopy",copyicon,NULL,0,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);
  new FXButton(toolbar,"Paste\tPaste",pasteicon,NULL,0,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

  // Color
  new FXButton(toolbar,"&Colors\tColors\tDisplay color dialog.",paletteicon,colordlg,FXWindow::ID_SHOW,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_RIGHT);

  // File Menu entries
  new FXMenuCommand(filemenu,"&Open...\tCtl-O\tOpen image file.",fileopenicon,this,ID_OPEN);
  new FXMenuCommand(filemenu,"&Save...\tCtl-S\tSave image file.",filesaveicon,this,ID_SAVE);
  new FXMenuCommand(filemenu,"Dump",NULL,getApp(),FXApp::ID_DUMP);

  // Recent file menu; this automatically hides if there are no files
  FXMenuSeparator* sep1=new FXMenuSeparator(filemenu);
  sep1->setTarget(&mrufiles);
  sep1->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_1);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_2);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_3);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_4);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_5);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_6);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_7);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_8);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_9);
  new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_10);
  new FXMenuCommand(filemenu,"&Clear Recent Files",NULL,&mrufiles,FXRecentFiles::ID_CLEAR);
  FXMenuSeparator* sep2=new FXMenuSeparator(filemenu);
  sep2->setTarget(&mrufiles);
  sep2->setSelector(FXRecentFiles::ID_ANYFILES);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,this,ID_QUIT);

  // Edit Menu entries
  new FXMenuCommand(editmenu,"&Undo\tCtl-Z\tUndo last change.");
  new FXMenuCommand(editmenu,"&Redo\tCtl-R\tRedo last undo.");
  new FXMenuCommand(editmenu,"&Copy\tCtl-C\tCopy selection to clipboard.",copyicon,NULL,0);
  new FXMenuCommand(editmenu,"C&ut\tCtl-X\tCut selection to clipboard.",cuticon,NULL,0);
  new FXMenuCommand(editmenu,"&Paste\tCtl-V\tPaste from clipboard.",pasteicon,NULL,0);
  new FXMenuCommand(editmenu,"&Delete\t\tDelete selection.",NULL,NULL,0);

  // Manipulation Menu entries
  new FXMenuCommand(manipmenu,"Rotate 90\t\tRotate 90 degrees.",NULL,this,ID_ROTATE_90);
  new FXMenuCommand(manipmenu,"Rotate 180\t\tRotate 180 degrees.",NULL,this,ID_ROTATE_180);
  new FXMenuCommand(manipmenu,"Rotate -90\t\tRotate -90 degrees.",NULL,this,ID_ROTATE_270);
  new FXMenuCommand(manipmenu,"Mirror Horizontally\t\tMirror Horizontally.",NULL,this,ID_MIRROR_HOR);
  new FXMenuCommand(manipmenu,"Mirror Vertically\t\tMirror Vertically.",NULL,this,ID_MIRROR_VER);
  new FXMenuCommand(manipmenu,"Mirror Both\t\tMirror Both.",NULL,this,ID_MIRROR_BOTH);
  new FXMenuCommand(manipmenu,"Scale...\t\tScale image.",NULL,this,ID_SCALE);
  new FXMenuCommand(manipmenu,"Crop...\t\tCrop image.",NULL,this,ID_CROP);

  // View Menu entries
  new FXMenuCommand(viewmenu,"File list\t\tDisplay file list.",NULL,filebox,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCommand(viewmenu,"Show hidden files\t\tShow hidden files and directories.",NULL,filelist,FXFileList::ID_TOGGLE_HIDDEN);
  new FXMenuCommand(viewmenu,"Show small icons\t\tDisplay directory with small icons.",NULL,filelist,FXFileList::ID_SHOW_MINI_ICONS);
  new FXMenuCommand(viewmenu,"Show big icons\t\tDisplay directory with big icons.",NULL,filelist,FXFileList::ID_SHOW_BIG_ICONS);
  new FXMenuCommand(viewmenu,"Show details view\t\tDisplay detailed directory listing.",NULL,filelist,FXFileList::ID_SHOW_DETAILS);
  new FXMenuCommand(viewmenu,"Rows of icons\t\tView row-wise.",NULL,filelist,FXFileList::ID_ARRANGE_BY_ROWS);
  new FXMenuCommand(viewmenu,"Columns of icons\t\tView column-wise.",NULL,filelist,FXFileList::ID_ARRANGE_BY_COLUMNS);
  new FXMenuCommand(viewmenu,"Toolbar\t\tDisplay toolbar.",NULL,toolbar,FXWindow::ID_TOGGLESHOWN);
  new FXMenuCommand(viewmenu,"Float toolbar\t\tUndock the toolbar.",NULL,toolbar,FXToolBar::ID_DOCK_FLOAT);
  new FXMenuCommand(viewmenu,"Dock toolbar top\t\tDock the toolbar on the top.",NULL,toolbar,FXToolBar::ID_DOCK_TOP);
  new FXMenuCommand(viewmenu,"Dock toolbar left\t\tDock the toolbar on the left.",NULL,toolbar,FXToolBar::ID_DOCK_LEFT);
  new FXMenuCommand(viewmenu,"Dock toolbar right\t\tDock the toolbar on the right.",NULL,toolbar,FXToolBar::ID_DOCK_RIGHT);
  new FXMenuCommand(viewmenu,"Dock toolbar bottom\t\tDock the toolbar on the bottom.",NULL,toolbar,FXToolBar::ID_DOCK_BOTTOM);
  new FXMenuCommand(viewmenu,"Status line\t\tDisplay status line.",NULL,statusbar,FXWindow::ID_TOGGLESHOWN);

  // Help Menu entries
  new FXMenuCommand(helpmenu,"&About FOX...",NULL,this,ID_ABOUT,0);

  // Make a tool tip
  new FXToolTip(getApp(),TOOLTIP_NORMAL);

  // Recent files
  mrufiles.setTarget(this);
  mrufiles.setSelector(ID_RECENTFILE);

  // Initialize file name
  filename="untitled";
  }


// Clean up
ImageWindow::~ImageWindow(){
  delete filemenu;
  delete editmenu;
  delete manipmenu;
  delete helpmenu;
  delete viewmenu;
  delete fileopenicon;
  delete filesaveicon;
  delete cuticon;
  delete copyicon;
  delete pasteicon;
  delete uplevelicon;
  delete paletteicon;
  }


// About box
long ImageWindow::onCmdAbout(FXObject*,FXSelector,void*){
  FXMessageBox about(this,"About Image Viewer","Image Viewer demonstrates the FOX ImageView widget.\n\nUsing the FOX C++ GUI Library (http://www.fox-toolkit.org)\n\nCopyright (C) 2000,2004 Jeroen van der Zijp (jeroen@fox-toolkit.org)",NULL,MBOX_OK|DECOR_TITLE|DECOR_BORDER);
  about.execute();
  return 1;
  }


// Load file
FXbool ImageWindow::loadimage(const FXString& file){
  FXString ext=FXPath::extension(file);
  FXImage *img=NULL;
  FXImage *old;
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
  if(img==NULL){
    FXMessageBox::error(this,MBOX_OK,"Error Loading Image","Unsupported type: %s",ext.text());
    return FALSE;
    }

  // Load it
  FXFileStream stream;
  if(stream.open(file,FXStreamLoad)){
    getApp()->beginWaitCursor();
    img->loadPixels(stream);
    stream.close();

//img->gradient(FXRGB(255,0,0),FXRGB(0,255,0),FXRGB(0,0,255),FXRGB(255,255,255));
//img->gradient(FXRGB(255,0,0),FXRGB(255,255,0),FXRGB(0,0,255),FXRGB(255,0,255));
//img->hgradient(FXRGB(255,0,0),FXRGB(0,0,255));
//img->vgradient(FXRGB(255,0,0),FXRGB(0,0,255));
//img->fill(FXRGB(255,0,0));
//img->fade(FXRGB(255,255,255),128);
//img->blend(getApp()->getBaseColor());
//img->blend(FXRGB(255,128,255));
//img->xshear(-30*256,FXRGB(0,255,128));
//img->yshear(-50*256,FXRGB(0,255,128));


    img->create();
    old=imageview->getImage();
    imageview->setImage(img);
    delete old;
    getApp()->endWaitCursor();
    }
  return TRUE;
  }


// Save file
FXbool ImageWindow::saveimage(const FXString& file){
  FXImage *img;
  FXFileStream stream;
  if(stream.open(file,FXStreamSave)){
    getApp()->beginWaitCursor();
    img=imageview->getImage();///// We may want to save in another format...
    img->savePixels(stream);
    stream.close();
    getApp()->endWaitCursor();
    }
  return TRUE;
  }


// Open
long ImageWindow::onCmdOpen(FXObject*,FXSelector,void*){
  FXFileDialog open(this,"Open Image");
  open.setFilename(filename);
  open.setPatternList(patterns);
  if(open.execute()){
    filename=open.getFilename();
    filelist->setCurrentFile(filename);
    mrufiles.appendFile(filename);
    loadimage(filename);
    }
  return 1;
  }


// Save
long ImageWindow::onCmdSave(FXObject*,FXSelector,void*){
  FXFileDialog savedialog(this,"Save Image");
  savedialog.setFilename(filename);
  if(savedialog.execute()){
    if(FXStat::exists(savedialog.getFilename())){
      if(MBOX_CLICKED_NO==FXMessageBox::question(this,MBOX_YES_NO,"Overwrite Image","Overwrite existing image?")) return 1;
      }
    filename=savedialog.getFilename();
    filelist->setCurrentFile(filename);
    mrufiles.appendFile(filename);
    saveimage(filename);
    }
  return 1;
  }


// Quit
long ImageWindow::onCmdQuit(FXObject*,FXSelector,void*){
  FXTRACE((100,"Quit\n"));

  // Write new window size back to registry
  getApp()->reg().writeIntEntry("SETTINGS","x",getX());
  getApp()->reg().writeIntEntry("SETTINGS","y",getY());
  getApp()->reg().writeIntEntry("SETTINGS","width",getWidth());
  getApp()->reg().writeIntEntry("SETTINGS","height",getHeight());

  // Width of tree
  getApp()->reg().writeIntEntry("SETTINGS","fileheight",filebox->getHeight());

  // Was file box shown
  getApp()->reg().writeIntEntry("SETTINGS","filesshown",filebox->shown());

  FXString dir=filelist->getDirectory();
  getApp()->reg().writeStringEntry("SETTINGS","directory",dir.text());

  // Quit
  getApp()->exit(0);
  return 1;
  }


// Update title
long ImageWindow::onUpdTitle(FXObject* sender,FXSelector,void*){
  FXString caption="FOX Image Viewer:- " + filename;
  FXImage* image=imageview->getImage();
  if(image){ caption+=" (" + FXStringVal(image->getWidth()) + " x " + FXStringVal(image->getHeight()) + ")"; }
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&caption);
  return 1;
  }


// Open recent file
long ImageWindow::onCmdRecentFile(FXObject*,FXSelector,void* ptr){
  filename=(FXchar*)ptr;
  filelist->setCurrentFile(filename);
  loadimage(filename);
  return 1;
  }


// Command from the tree list
long ImageWindow::onCmdFileList(FXObject*,FXSelector,void* ptr){
  FXint index=(FXint)(FXival)ptr;
  if(0<=index){
    if(filelist->isItemDirectory(index)){
      filelist->setDirectory(filelist->getItemPathname(index));
      }
    else if(filelist->isItemFile(index)){
      filename=filelist->getItemPathname(index);
      mrufiles.appendFile(filename);
      loadimage(filename);
      }
    }
  return 1;
  }


// Rotate image
long ImageWindow::onCmdRotate(FXObject*,FXSelector sel,void*){
  FXImage* image=imageview->getImage();
  FXASSERT(image);
  switch(FXSELID(sel)){
    case ID_ROTATE_90: image->rotate(90); break;
    case ID_ROTATE_180: image->rotate(180); break;
    case ID_ROTATE_270: image->rotate(270); break;
    }
  imageview->setImage(image);
  return 1;
  }


// Update image
long ImageWindow::onUpdImage(FXObject* sender,FXSelector,void*){
  if(imageview->getImage())
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
  else
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
  return 1;
  }


// Mirror
long ImageWindow::onCmdMirror(FXObject*,FXSelector sel,void*){
  FXImage* image=imageview->getImage();
  FXASSERT(image);
  switch(FXSELID(sel)){
    case ID_MIRROR_HOR: image->mirror(TRUE,FALSE); break;
    case ID_MIRROR_VER: image->mirror(FALSE,TRUE); break;
    case ID_MIRROR_BOTH: image->mirror(TRUE,TRUE); break;
    }
  imageview->setImage(image);
  return 1;
  }


// Scale
long ImageWindow::onCmdScale(FXObject*,FXSelector,void*){
  FXImage* image=imageview->getImage();
  FXASSERT(image);
  FXint sx=image->getWidth();
  FXint sy=image->getHeight();
  FXint sq=1;                   //: ADDED by Amanda Ross To reflect new feature since V1.1.32
  FXDataTarget sxtarget(sx);
  FXDataTarget sytarget(sy);
  FXDataTarget sqtarget(sq);    //: ADDED by Amanda Ross To reflect new feature since V1.1.32
  FXDialogBox  scalepanel(this,"Scale Image To Size");
  FXHorizontalFrame* frame=new FXHorizontalFrame(&scalepanel,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(frame,"W:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&sxtarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXLabel(frame,"H:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&sytarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXLabel(frame,"Q:",NULL,LAYOUT_CENTER_Y); //: ADDED by Amanda Ross To reflect new feature since V1.1.32
  FXSpinner *spinner=new FXSpinner(frame,2,&sqtarget,FXDataTarget::ID_VALUE,SPIN_NORMAL|FRAME_SUNKEN|FRAME_THICK|LAYOUT_SIDE_TOP);
  spinner->setRange(0,1);
  spinner->setValue(1);
  new FXButton(frame,"Cancel",NULL,&scalepanel,FXDialogBox::ID_CANCEL,LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,4,4);
  new FXButton(frame,"OK",NULL,&scalepanel,FXDialogBox::ID_ACCEPT,LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK,0,0,0,0, 30,30,4,4);
  if(!scalepanel.execute()) return 1;
  if(sx<1 || sy<1) return 1;
  image->scale(sx,sy,FXCLAMP(0,sq,1));
  imageview->setImage(image);
  return 1;
  }


// Crop
long ImageWindow::onCmdCrop(FXObject*,FXSelector,void*){
  FXImage* image=imageview->getImage();
  FXASSERT(image);
  FXint cx=0;
  FXint cy=0;
  FXint cw=image->getWidth();
  FXint ch=image->getHeight();
  FXDataTarget cxtarget(cx);
  FXDataTarget cytarget(cy);
  FXDataTarget cwtarget(cw);
  FXDataTarget chtarget(ch);
  FXDialogBox  croppanel(this,"Crop image");
  FXHorizontalFrame* frame=new FXHorizontalFrame(&croppanel,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  new FXLabel(frame,"X:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&cxtarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXLabel(frame,"Y:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&cytarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXLabel(frame,"W:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&cwtarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXLabel(frame,"H:",NULL,LAYOUT_CENTER_Y);
  new FXTextField(frame,5,&chtarget,FXDataTarget::ID_VALUE,LAYOUT_CENTER_Y|FRAME_SUNKEN|FRAME_THICK|JUSTIFY_RIGHT);
  new FXButton(frame,"Cancel",NULL,&croppanel,FXDialogBox::ID_CANCEL,LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK,0,0,0,0, 20,20,4,4);
  new FXButton(frame,"OK",NULL,&croppanel,FXDialogBox::ID_ACCEPT,LAYOUT_CENTER_Y|FRAME_RAISED|FRAME_THICK,0,0,0,0, 30,30,4,4);
  if(!croppanel.execute()) return 1;
  if(cx+cw<=0 || cy+ch<=0 || cx>=image->getWidth() || cy>=image->getHeight()) return 1;
  image->crop(cx,cy,cw,ch);
  imageview->setImage(image);
  return 1;
  }


// Create and show window
void ImageWindow::create(){
  FXint ww,hh,xx,yy,fh,fs;
  FXString dir;

  // Get size
  xx=getApp()->reg().readIntEntry("SETTINGS","x",600);
  yy=getApp()->reg().readIntEntry("SETTINGS","y",400);
  ww=getApp()->reg().readIntEntry("SETTINGS","width",600);
  hh=getApp()->reg().readIntEntry("SETTINGS","height",400);

  fh=getApp()->reg().readIntEntry("SETTINGS","fileheight",100);
  fs=getApp()->reg().readIntEntry("SETTINGS","filesshown",TRUE);

  dir=getApp()->reg().readStringEntry("SETTINGS","directory","~");
  filelist->setDirectory(dir);

  filebox->setHeight(fh);

  // Hide tree if asked for
  if(!fs) filebox->hide();

  // Reposition window
  position(xx,yy,ww,hh);

  FXMainWindow::create();
  show();
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("ImageViewer","FoxTest");

  // Open display
  application.init(argc,argv);

  // Make window
  ImageWindow* window=new ImageWindow(&application);

  // Handle interrupt to save stuff nicely
  application.addSignal(SIGINT,window,ImageWindow::ID_QUIT);

  // Create it
  application.create();

  // Passed image file?
  if(argc>1) window->loadimage(argv[1]);

  // Run
  return application.run();
  }


