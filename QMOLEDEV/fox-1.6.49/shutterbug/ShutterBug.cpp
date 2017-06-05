/********************************************************************************
*                                                                               *
*                S h u t t e r   B u g   A p p l i c a t i o n                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: ShutterBug.cpp,v 1.49 2006/03/16 04:41:34 fox Exp $                      *
********************************************************************************/
#include "fx.h"
#include "fxkeys.h"
#include "FXPNGImage.h"
#include "FXJPGImage.h"
#include "FXTIFImage.h"
#include "FXICOImage.h"
#include "FXTGAImage.h"
#include "FXRGBImage.h"
#include "icons.h"
#include "ShutterBug.h"
#include "Snapper.h"


/*
  Notes:
  - In fixed size mode you can just drag the snap rectangle around;
    it stays the same size.
  - Now remembers last selected filename and file type.
  - Add feature to cut shot to clipboard.
*/

#define FUDGE         10        // Corner fudge for diagonal dragging
#define MINSIZE       8         // Minimum snap size
#define VERSION_MAJOR 3
#define VERSION_MINOR 0
#define VERSION_PATCH 0

/*******************************************************************************/

// Map
FXDEFMAP(ShutterBug) ShutterBugMap[]={
  FXMAPFUNC(SEL_PAINT,0,ShutterBug::onPaint),
  FXMAPFUNC(SEL_MOTION,0,ShutterBug::onMotion),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,ShutterBug::onBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,ShutterBug::onBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,ShutterBug::onBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,ShutterBug::onBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,ShutterBug::onBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,ShutterBug::onBtnRelease),
  FXMAPFUNC(SEL_KEYPRESS,0,ShutterBug::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,ShutterBug::onKeyRelease),
  FXMAPFUNC(SEL_CLIPBOARD_LOST,0,ShutterBug::onClipboardLost),
  FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,ShutterBug::onClipboardRequest),
  FXMAPFUNC(SEL_TIMEOUT,ShutterBug::ID_SNAPSHOT,ShutterBug::onCmdSnap),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_SNAPSHOT,ShutterBug::onCmdSnap),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_SNAPSHOT_DELAYED,ShutterBug::onCmdSnapDelayed),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_SNAPSHOT_CLIPBOARD,ShutterBug::onCmdSnapClipboard),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_DELAY,ShutterBug::onCmdDelay),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_ABOUT,ShutterBug::onCmdAbout),
  FXMAPFUNC(SEL_UPDATE,ShutterBug::ID_TOGGLE_LASSO,ShutterBug::onUpdLasso),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_TOGGLE_LASSO,ShutterBug::onCmdLasso),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_QUIT,ShutterBug::onCmdQuit),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_COLOR,ShutterBug::onCmdLineColor),
  FXMAPFUNC(SEL_UPDATE,ShutterBug::ID_INSIDE,ShutterBug::onUpdLineInside),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_INSIDE,ShutterBug::onCmdLineInside),
  FXMAPFUNC(SEL_UPDATE,ShutterBug::ID_QUANTIZE,ShutterBug::onUpdQuantize),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_QUANTIZE,ShutterBug::onCmdQuantize),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_SET_COUNT,ShutterBug::onCmdSetCount),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_RESET_COUNT,ShutterBug::onCmdResetCount),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_RECORD_RATE,ShutterBug::onCmdRecordRate),
  FXMAPFUNC(SEL_TIMEOUT,ShutterBug::ID_RECORD_FRAME,ShutterBug::onCmdRecordFrame),
  FXMAPFUNC(SEL_COMMAND,ShutterBug::ID_RECORD_MOVIE,ShutterBug::onCmdRecordMovie),
  FXMAPFUNCS(SEL_LEFTBUTTONPRESS,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onPressSnapper),
  FXMAPFUNCS(SEL_LEFTBUTTONRELEASE,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onReleaseSnapper),
  FXMAPFUNCS(SEL_MIDDLEBUTTONPRESS,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onPressSnapper),
  FXMAPFUNCS(SEL_MIDDLEBUTTONRELEASE,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onReleaseSnapper),
  FXMAPFUNCS(SEL_RIGHTBUTTONPRESS,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onPressSnapper),
  FXMAPFUNCS(SEL_RIGHTBUTTONRELEASE,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onReleaseSnapper),
  FXMAPFUNCS(SEL_MOTION,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onMovedSnapper),
  FXMAPFUNCS(SEL_ENTER,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onEnterSnapper),
  FXMAPFUNCS(SEL_LEAVE,ShutterBug::ID_SNAPPER_0,ShutterBug::ID_SNAPPER_3,ShutterBug::onLeaveSnapper),
  FXMAPFUNCS(SEL_UPDATE,ShutterBug::ID_SIZE_SCREEN,ShutterBug::ID_SIZE_512X512,ShutterBug::onUpdSize),
  FXMAPFUNCS(SEL_COMMAND,ShutterBug::ID_SIZE_SCREEN,ShutterBug::ID_SIZE_512X512,ShutterBug::onCmdSize),
  FXMAPFUNCS(SEL_COMMAND,ShutterBug::ID_WEIGHT_0,ShutterBug::ID_WEIGHT_6,ShutterBug::onCmdLineWeight),
  };


// Implementation
FXIMPLEMENT(ShutterBug,FXShell,ShutterBugMap,ARRAYNUMBER(ShutterBugMap))

// Pattern wildcards
const FXchar patterns[]=
    "GIF Image (*.gif)"
  "\nBMP Image (*.bmp)"
  "\nXPM Image (*.xpm)"
  "\nPCX Image (*.pcx)"
  "\nRGB Image (*.rgb)"
  "\nXBM Image (*.xbm)"
  "\nTARGA Image (*.tga)"
  "\nPPM Image (*.ppm)"
#ifdef HAVE_PNG_H
  "\nPNG Image (*.png)"
#endif
#ifdef HAVE_JPEG_H
  "\nJPEG Image (*.jpg)"
#endif
#ifdef HAVE_TIFF_H
  "\nTIFF Image (*.tif)"
#endif
  "\nRAS Image (*.ras)"
  "\nPS Image (*.ps)"
  ;

// Pattern numbers
enum {
   TYPE_GIF
  ,TYPE_BMP
  ,TYPE_XPM
  ,TYPE_PCX
  ,TYPE_RGB
  ,TYPE_XBM
  ,TYPE_TGA
  ,TYPE_PPM
#ifdef HAVE_PNG_H
  ,TYPE_PNG
#endif
#ifdef HAVE_JPEG_H
  ,TYPE_JPG
#endif
#ifdef HAVE_TIFF_H
  ,TYPE_TIF
#endif
  ,TYPE_RAS
  ,TYPE_PS
  };


// Drag types
FXDragType ShutterBug::dndTypes[7]={0,0,0,0,0,0,0};


/*******************************************************************************/

// ShutterBug main window
ShutterBug::ShutterBug(FXApp* a):FXShell(a,0,0,0,0,0){
  flags|=FLAG_ENABLED;
  rectangle.x=100;
  rectangle.y=100;
  rectangle.w=100;
  rectangle.h=100;
  filename="image.gif";
  fileformat=TYPE_GIF;
  filecount=1;
  clipbuffer=NULL;
  clipwidth=0;
  clipheight=0;
  snapper[0]=new Snapper(getApp(),this,ID_SNAPPER_0);
  snapper[1]=new Snapper(getApp(),this,ID_SNAPPER_1);
  snapper[2]=new Snapper(getApp(),this,ID_SNAPPER_2);
  snapper[3]=new Snapper(getApp(),this,ID_SNAPPER_3);
  bigicon=new FXGIFIcon(getApp(),shutterbug);
  smallicon=new FXGIFIcon(getApp(),tinyshutterbug);
  weighticons[0]=new FXGIFIcon(getApp(),line_1);
  weighticons[1]=new FXGIFIcon(getApp(),line_2);
  weighticons[2]=new FXGIFIcon(getApp(),line_3);
  weighticons[3]=new FXGIFIcon(getApp(),line_4);
  weighticons[4]=new FXGIFIcon(getApp(),line_5);
  weighticons[5]=new FXGIFIcon(getApp(),line_6);
  weight=3;
  size=0;
  delay=3000;
  rate=1000;
  inside=FALSE;
  quantize=TRUE;
  spotx=0;
  spoty=0;
  mode=MODE_NONE;
  }


// Snapper does override-redirect
bool ShutterBug::doesOverrideRedirect() const { return true; }

// Create and show window
void ShutterBug::create(){
  readRegistry();
  FXShell::create();
  dndTypes[0]=getApp()->registerDragType(FXBMPImage::mimeType);
  dndTypes[1]=getApp()->registerDragType(FXGIFImage::mimeType);
  dndTypes[2]=getApp()->registerDragType(FXXPMImage::mimeType);
  dndTypes[3]=getApp()->registerDragType(FXPPMImage::mimeType);
  dndTypes[4]=getApp()->registerDragType(FXJPGImage::mimeType);
  dndTypes[5]=getApp()->registerDragType(FXPNGImage::mimeType);
  dndTypes[6]=getApp()->registerDragType(FXTIFImage::mimeType);
  snapper[0]->create();
  snapper[1]->create();
  snapper[2]->create();
  snapper[3]->create();
  bigicon->create();
  smallicon->create();
  weighticons[0]->create();
  weighticons[1]->create();
  weighticons[2]->create();
  weighticons[3]->create();
  weighticons[4]->create();
  weighticons[5]->create();
  snapper[0]->setBackColor(color);
  snapper[1]->setBackColor(color);
  snapper[2]->setBackColor(color);
  snapper[3]->setBackColor(color);
  moveSnapRectangle(rectangle);
  showSnapRectangle();
  setShape(bigicon);
  show();
  }


// Get default width
FXint ShutterBug::getDefaultWidth(){
  return bigicon->getWidth();
  }


// Get default height
FXint ShutterBug::getDefaultHeight(){
  return bigicon->getHeight();
  }


// Read registry
void ShutterBug::readRegistry(){
  filename=getApp()->reg().readStringEntry("SETTINGS","filename","image.gif");
  fileformat=getApp()->reg().readIntEntry("SETTINGS","fileformat",TYPE_GIF);
  setX(getApp()->reg().readIntEntry("SETTINGS","x",50));
  setY(getApp()->reg().readIntEntry("SETTINGS","y",50));
  weight=getApp()->reg().readIntEntry("SETTINGS","weight",3);
  rectangle.x=getApp()->reg().readIntEntry("SETTINGS","snapx",50);
  rectangle.y=getApp()->reg().readIntEntry("SETTINGS","snapy",50);
  rectangle.w=getApp()->reg().readIntEntry("SETTINGS","snapw",50);
  rectangle.h=getApp()->reg().readIntEntry("SETTINGS","snaph",50);
  delay=getApp()->reg().readUnsignedEntry("SETTINGS","delay",3000);
  rate=getApp()->reg().readUnsignedEntry("SETTINGS","rate",1000);
  filecount=getApp()->reg().readIntEntry("SETTINGS","count",1);
  inside=getApp()->reg().readIntEntry("SETTINGS","inside",FALSE);
  color=getApp()->reg().readColorEntry("SETTINGS","color",FXRGB(255,128,128));
  size=getApp()->reg().readIntEntry("SETTINGS","size",0);
  quantize=getApp()->reg().readIntEntry("SETTINGS","quantize",TRUE);
  if(size){
    if(size<0){
      rectangle.x=0;
      rectangle.y=0;
      rectangle.w=getRoot()->getWidth();
      rectangle.h=getRoot()->getHeight();
      }
    else{
      rectangle.w=size;
      rectangle.h=size;
      }
    }
  }


// Write registry
void ShutterBug::writeRegistry(){
  getApp()->reg().writeStringEntry("SETTINGS","filename",filename.text());
  getApp()->reg().writeIntEntry("SETTINGS","fileformat",fileformat);
  getApp()->reg().writeIntEntry("SETTINGS","x",getX());
  getApp()->reg().writeIntEntry("SETTINGS","y",getY());
  getApp()->reg().writeIntEntry("SETTINGS","weight",weight);
  getApp()->reg().writeIntEntry("SETTINGS","snapx",rectangle.x);
  getApp()->reg().writeIntEntry("SETTINGS","snapy",rectangle.y);
  getApp()->reg().writeIntEntry("SETTINGS","snapw",rectangle.w);
  getApp()->reg().writeIntEntry("SETTINGS","snaph",rectangle.h);
  getApp()->reg().writeUnsignedEntry("SETTINGS","delay",delay);
  getApp()->reg().writeUnsignedEntry("SETTINGS","rate",rate);
  getApp()->reg().writeIntEntry("SETTINGS","count",filecount);
  getApp()->reg().writeIntEntry("SETTINGS","inside",inside);
  getApp()->reg().writeColorEntry("SETTINGS","color",color);
  getApp()->reg().writeIntEntry("SETTINGS","size",size);
  getApp()->reg().writeIntEntry("SETTINGS","quantize",quantize);
  }


// Find out where window was grabbed
FXuchar ShutterBug::where(FXint x,FXint y) const {
  FXuchar code=MODE_NONE;
  if(x<rectangle.x+FUDGE) code|=MODE_LEFT;
  if(rectangle.x+rectangle.w-FUDGE<=x) code|=MODE_RIGHT;
  if(y<rectangle.y+FUDGE) code|=MODE_TOP;
  if(rectangle.y+rectangle.h-FUDGE<=y) code|=MODE_BOTTOM;
  return code;
  }


// Change cursor based on location over window
void ShutterBug::changeCursor(FXint which,FXuchar drag){
  switch(drag){
    case MODE_TOP:
    case MODE_BOTTOM:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_DRAGH_CURSOR));
      break;
    case MODE_LEFT:
    case MODE_RIGHT:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_DRAGV_CURSOR));
      break;
    case MODE_TOPLEFT:
    case MODE_BOTTOMRIGHT:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGTL_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_DRAGTL_CURSOR));
      break;
    case MODE_TOPRIGHT:
    case MODE_BOTTOMLEFT:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_DRAGTR_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_DRAGTR_CURSOR));
      break;
    case MODE_WHOLERECT:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_MOVE_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_MOVE_CURSOR));
      break;
    default:
      snapper[which]->setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
      snapper[which]->setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
      break;
    }
  }


// Handle repaint
long ShutterBug::onPaint(FXObject*,FXSelector,void* ptr){
  register FXEvent *event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,height);
  dc.setForeground(FXRGB(0,0,0));
  dc.drawRectangle(0,0,width-1,height-1);
  dc.drawIcon(bigicon,0,0);
  return 1;
  }


// Mouse motion
long ShutterBug::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  if(flags&FLAG_PRESSED){
    move(event->root_x-spotx,event->root_y-spoty);
    return 1;
    }
  return 0;
  }


// Left button pressed
long ShutterBug::onBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  grab();
  spotx=event->win_x;
  spoty=event->win_y;
  flags|=FLAG_PRESSED;
  return 1;
  }


// Left button released
long ShutterBug::onBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  ungrab();
  flags&=~FLAG_PRESSED;
  if(event->moved) return 1;
  FXMenuPane filemenu(this);
  new FXMenuCaption(&filemenu,"ShutterBug",smallicon);
  new FXMenuSeparator(&filemenu);
  new FXMenuCommand(&filemenu,tr("Snap..."),NULL,this,ID_SNAPSHOT);
  new FXMenuCommand(&filemenu,tr("Snap delayed..."),NULL,this,ID_SNAPSHOT_DELAYED);
  new FXMenuCommand(&filemenu,tr("Snap to clipboard..."),NULL,this,ID_SNAPSHOT_CLIPBOARD);
  new FXMenuCommand(&filemenu,tr("Record movie..."),NULL,this,ID_RECORD_MOVIE);
  new FXMenuCheck(&filemenu,tr("Show lasso"),this,ID_TOGGLE_LASSO);
  new FXMenuCheck(&filemenu,tr("Lines inside"),this,ID_INSIDE);
  new FXMenuCommand(&filemenu,tr("Color..."),NULL,this,ID_COLOR);
  FXMenuPane sizemenu(this);
  new FXMenuCascade(&filemenu,tr("Size"),NULL,&sizemenu);
  new FXMenuRadio(&sizemenu,"8x8",this,ID_SIZE_8X8);
  new FXMenuRadio(&sizemenu,"16x16",this,ID_SIZE_16X16);
  new FXMenuRadio(&sizemenu,"24x24",this,ID_SIZE_24X24);
  new FXMenuRadio(&sizemenu,"32x32",this,ID_SIZE_32X32);
  new FXMenuRadio(&sizemenu,"48x48",this,ID_SIZE_48X48);
  new FXMenuRadio(&sizemenu,"64x64",this,ID_SIZE_64X64);
  new FXMenuRadio(&sizemenu,"128x128",this,ID_SIZE_128X128);
  new FXMenuRadio(&sizemenu,"256x256",this,ID_SIZE_256X256);
  new FXMenuRadio(&sizemenu,"512x512",this,ID_SIZE_512X512);
  new FXMenuRadio(&sizemenu,tr("Screen"),this,ID_SIZE_SCREEN);
  new FXMenuRadio(&sizemenu,tr("Custom"),this,ID_SIZE_CUSTOM);
  FXMenuPane weightmenu(this);
  new FXMenuCascade(&filemenu,tr("Weight"),NULL,&weightmenu);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[0],this,ID_WEIGHT_1);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[1],this,ID_WEIGHT_2);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[2],this,ID_WEIGHT_3);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[3],this,ID_WEIGHT_4);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[4],this,ID_WEIGHT_5);
  new FXMenuCommand(&weightmenu,FXString::null,weighticons[5],this,ID_WEIGHT_6);
  FXMenuPane optionsmenu(this);
  new FXMenuCascade(&filemenu,tr("Options"),NULL,&optionsmenu);
  new FXMenuCommand(&optionsmenu,tr("Delay..."),NULL,this,ID_DELAY);
  new FXMenuCommand(&optionsmenu,tr("Set number..."),NULL,this,ID_SET_COUNT);
  new FXMenuCommand(&optionsmenu,tr("Reset number..."),NULL,this,ID_RESET_COUNT);
  new FXMenuCommand(&optionsmenu,tr("Record Rate..."),NULL,this,ID_RECORD_RATE);
  new FXMenuCheck(&optionsmenu,tr("Fast quantization"),this,ID_QUANTIZE);
  new FXMenuCommand(&filemenu,tr("About..."),NULL,this,ID_ABOUT);
  new FXMenuCommand(&filemenu,tr("Quit"),NULL,this,ID_QUIT);
  filemenu.create();
  filemenu.popup(NULL,event->root_x,event->root_y);
  getApp()->runModalWhileShown(&filemenu);
  return 1;
  }



// Keyboard press
long ShutterBug::onKeyPress(FXObject*,FXSelector,void* ptr){
  if(((FXEvent*)ptr)->code==KEY_q || ((FXEvent*)ptr)->code==KEY_Q){
    FXTRACE((1,"quit\n"));
    writeRegistry();
    getApp()->exit();
    }
  return 1;
  }


// Keyboard release
long ShutterBug::onKeyRelease(FXObject*,FXSelector,void*){
  return 1;
  }


// Quit
long ShutterBug::onCmdQuit(FXObject*,FXSelector,void*){
  FXTRACE((1,"quit\n"));
  writeRegistry();
  getApp()->exit();
  return 1;
  }


// Move snap rectangle
void ShutterBug::moveSnapRectangle(const FXRectangle& r){
  if(inside){
    snapper[0]->position(r.x,r.y,weight,r.h);
    snapper[1]->position(r.x+r.w-weight,r.y,weight,r.h);
    snapper[2]->position(r.x,r.y,r.w,weight);
    snapper[3]->position(r.x,r.y+r.h-weight,r.w,weight);
    }
  else{
    snapper[0]->position(r.x-weight,r.y-weight,weight,r.h+weight+weight);
    snapper[1]->position(r.x+r.w,r.y-weight,weight,r.h+weight+weight);
    snapper[2]->position(r.x-weight,r.y-weight,r.w+weight+weight,weight);
    snapper[3]->position(r.x-weight,r.y+r.h,r.w+weight+weight,weight);
    }
  snapper[0]->raise();
  snapper[1]->raise();
  snapper[2]->raise();
  snapper[3]->raise();
  }


// Show snap rectangle
void ShutterBug::showSnapRectangle(){
  snapper[0]->show();
  snapper[1]->show();
  snapper[2]->show();
  snapper[3]->show();
  snapper[0]->raise();
  snapper[1]->raise();
  snapper[2]->raise();
  snapper[3]->raise();
  getApp()->flush(TRUE);
  }


// Hide snap rectangle
void ShutterBug::hideSnapRectangle(){
  snapper[0]->hide();
  snapper[1]->hide();
  snapper[2]->hide();
  snapper[3]->hide();
  getApp()->flush(TRUE);
  }


// Is snap rectangle shown
FXbool ShutterBug::snapRectangleShown() const {
  return snapper[0]->shown();
  }


// Read pixels from root to image
void ShutterBug::readPixels(FXImage* image,const FXRectangle& r){
  FXDCWindow dc(image);
  dc.clipChildren(FALSE);
  dc.setFunction(BLT_SRC);
  dc.drawArea(getRoot(),r.x,r.y,r.w,r.h,0,0);
  }


// Just snap a rectangle
FXbool ShutterBug::grabRectangle(FXColor*& data,const FXRectangle& r){
  data=NULL;
  if(1<r.w && 1<r.h){
    if(FXCALLOC(&data,FXColor,r.w*r.h)){
      FXImage image(getApp(),data,IMAGE_KEEP,r.w,r.h);
      image.create();
      readPixels(&image,r);
      image.restore();
      return TRUE;
      }
    }
  return FALSE;
  }


// Snapshot rectangle
FXbool ShutterBug::snapRectangle(FXColor*& data,const FXRectangle& r){
  FXbool ok;

  // Hide snap rectangle
  hideSnapRectangle();

  // Hide myself
  hide();

  // Give other apps a chance to repaint
  FXThread::sleep(10000000);

  // Snapshot rectangle
  ok=grabRectangle(data,r);

  // Show snap rectangle
  showSnapRectangle();

  // Restore myself
  show();

  return ok;
  }


// Restore image from off-screen pixmap
long ShutterBug::onCmdSnap(FXObject*,FXSelector,void*){
  FXColor *data=NULL;

  // Try grab pixels
  if(snapRectangle(data,rectangle)){

    // Construct file dialog
    FXFileDialog savedialog(this,tr("Save Image"));
    savedialog.setPatternList(patterns);
    savedialog.setCurrentPattern(fileformat);
    savedialog.setFilename(FXPath::absolute(filename));

    // Run file dialog
    if(savedialog.execute()){
      filename=savedialog.getFilename();
      fileformat=savedialog.getCurrentPattern();
      if(FXStat::exists(filename) && FXMessageBox::question(this,MBOX_YES_NO,tr("Overwrite File"),tr("Overwrite existing image file: %s?"),filename.text())!=MBOX_CLICKED_YES) goto x;
      if(!saveImage(filename,data,rectangle.w,rectangle.h)){
        FXMessageBox::error(this,MBOX_OK,tr("Error Saving Image"),tr("Unable to save image to file: %s."),filename.text());
        }
      }

    // Free pixels
x:  FXFREE(&data);
    }
  return 1;
  }


// Save an image
FXbool ShutterBug::saveImage(const FXString& file,FXColor* data,FXint w,FXint h){
  FXbool ok=FALSE;
  FXFileStream outfile;
  if(outfile.open(file,FXStreamSave)){
    switch(fileformat){
      case TYPE_GIF: ok=fxsaveGIF(outfile,data,w,h,quantize); break;
      case TYPE_BMP: ok=fxsaveBMP(outfile,data,w,h); break;
      case TYPE_XPM: ok=fxsaveXPM(outfile,data,w,h,quantize); break;
      case TYPE_PCX: ok=fxsavePCX(outfile,data,w,h); break;
      case TYPE_RGB: ok=fxsaveRGB(outfile,data,w,h); break;
      case TYPE_XBM: ok=fxsaveXBM(outfile,data,w,h); break;
      case TYPE_TGA: ok=fxsaveTGA(outfile,data,w,h); break;
      case TYPE_PPM: ok=fxsavePPM(outfile,data,w,h); break;
#ifdef HAVE_PNG_H
      case TYPE_PNG: ok=fxsavePNG(outfile,data,w,h); break;
#endif
#ifdef HAVE_JPEG_H
      case TYPE_JPG: ok=fxsaveJPG(outfile,data,w,h,75); break;
#endif
#ifdef HAVE_TIFF_H
      case TYPE_TIF: ok=fxsaveTIF(outfile,data,w,h,0); break;
#endif
      case TYPE_RAS: ok=fxsaveRAS(outfile,data,w,h); break;
      case TYPE_PS: ok=fxsavePS(outfile,data,w,h); break;
      }
    outfile.close();
    }
  return ok;
  }


// We lost the selection somehow
long ShutterBug::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr){
  FXShell::onClipboardLost(sender,sel,ptr);
  FXTRACE((1,"%s::onClipboardLost \n",getClassName()));
  FXFREE(&clipbuffer);
  clipwidth=0;
  clipheight=0;
  return 1;
  }


// Somebody wants our selection
long ShutterBug::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXuval size;
  FXuchar *data;

  FXTRACE((1,"%s::onClipboardRequest \n",getClassName()));

  // Try handling it in base class first
  if(FXShell::onClipboardRequest(sender,sel,ptr)) return 1;

  if(clipbuffer){

    // One of the supported image types?
    if(event->target==dndTypes[0] || event->target==dndTypes[1] || event->target==dndTypes[2] || event->target==dndTypes[3] || event->target==dndTypes[4] || event->target==dndTypes[5] || event->target==dndTypes[6]){
      FXMemoryStream ms;

      // Open memory stream
      ms.open(FXStreamSave,NULL);

      // Render image to memory stream
      if(event->target==dndTypes[0]){
        FXTRACE((1,"Request for bmpType\n"));
        fxsaveBMP(ms,clipbuffer,clipwidth,clipheight);
        }
      else if(event->target==dndTypes[1]){
        FXTRACE((1,"Request for gifType\n"));
        fxsaveGIF(ms,clipbuffer,clipwidth,clipheight);
        }
      else if(event->target==dndTypes[2]){
        FXTRACE((1,"Request for xpmType\n"));
        fxsaveXPM(ms,clipbuffer,clipwidth,clipheight);
        }
      else if(event->target==dndTypes[3]){
        FXTRACE((1,"Request for ppmType\n"));
        fxsavePPM(ms,clipbuffer,clipwidth,clipheight);
        }
      else if(event->target==dndTypes[4]){
        FXTRACE((1,"Request for jpgType\n"));
        fxsaveJPG(ms,clipbuffer,clipwidth,clipheight,75);
        }
      else if(event->target==dndTypes[5]){
        FXTRACE((1,"Request for pngType\n"));
        fxsavePNG(ms,clipbuffer,clipwidth,clipheight);
        }
      else if(event->target==dndTypes[6]){
        FXTRACE((1,"Request for tifType\n"));
        fxsaveTIF(ms,clipbuffer,clipwidth,clipheight,0);
        }
#ifdef WIN32
  //  else if(event->target==imageType){
  //    FXTRACE((1,"Request for imageType\n"));
  //    fxsaveBMP(ms,chart->getData(),chart->getWidth(),chart->getHeight());
  //    }
#endif

      // Grab buffered image
      ms.takeBuffer(data,size);

      // Close memory stream
      ms.close();

      // Set DND data
      setDNDData(FROM_CLIPBOARD,event->target,data,size);
      return 1;
      }
    }
  return 0;
  }


// Snapshot to clipboard
long ShutterBug::onCmdSnapClipboard(FXObject*,FXSelector,void*){
  FXFREE(&clipbuffer);
  clipwidth=0;
  clipheight=0;
  if(acquireClipboard(dndTypes,ARRAYNUMBER(dndTypes))){
    if(snapRectangle(clipbuffer,rectangle)){
      clipwidth=rectangle.w;
      clipheight=rectangle.h;
      }
    }
  return 1;
  }


// Snapshot after small delay
long ShutterBug::onCmdSnapDelayed(FXObject*,FXSelector,void*){
  getApp()->addTimeout(this,ID_SNAPSHOT,delay);
  return 1;
  }


// Get frame number from filename
static FXint frameFromFilename(const FXString& file){
  FXint head=file.rfind(PATHSEP)+1;
  FXint tail=file.find('.',head);
  FXint frame=0;
  if(0<tail){
    FXint wgt=1;
    while(head<tail && Ascii::isDigit(file[tail-1])){
      frame+=wgt*Ascii::digitValue(file[--tail]); wgt*=10;
      }
    }
  return frame;
  }


// Get filename from frame number
static FXString filenameFromFrame(const FXString& file,FXint frame){
  FXString name=file;
  FXint head=name.rfind(PATHSEP)+1;
  FXint tail=name.find('.',head);
  if(0<tail){
    FXint start=tail;
    while(head<start && Ascii::isDigit(name[start-1])) start--;
    if(start<tail){
      name.replace(start,tail-start,FXStringFormat("%0*d",tail-start,frame));
      }
    else{
      name.insert(start,FXStringFormat("%04d",frame));
      }
    }
  return name;
  }


// Start recording a movie
long ShutterBug::onCmdRecordMovie(FXObject*,FXSelector,void*){
  FXFileDialog savedialog(this,tr("Save Image"));
  savedialog.setPatternList(patterns);
  savedialog.setCurrentPattern(fileformat);
  savedialog.setFilename(FXPath::absolute(filenameFromFrame(filename,filecount)));
  if(savedialog.execute()){
    filename=savedialog.getFilename();
    fileformat=savedialog.getCurrentPattern();
    filecount=frameFromFilename(filename);
    FXTRACE((1,"Frame: %s %d\n",filename.text(),filecount));
    hideSnapRectangle();
    hide();
    getApp()->addTimeout(this,ID_RECORD_FRAME,rate);
    }
  return 1;
  }


// Record one frame
long ShutterBug::onCmdRecordFrame(FXObject*,FXSelector,void*){
  FXint curx,cury; FXuint state;
  FXColor *data=NULL;
  FXbool ok=FALSE;
  FXWindow *root=getRoot();
  filename=filenameFromFrame(filename,filecount);
  FXTRACE((1,"Snap Frame: %s\n",filename.text()));
  if(grabRectangle(data,rectangle)){
    ok=saveImage(filename,data,rectangle.w,rectangle.h);
    FXFREE(&data);
    }
  root->getCursorPosition(curx,cury,state);
  if(ok && root->getX()<curx && curx+1<root->getWidth() && root->getY()<cury && cury+1<root->getHeight()){
    getApp()->addTimeout(this,ID_RECORD_FRAME,rate);
    filecount++;
    }
  else{
    showSnapRectangle();
    show();
    }
  return 1;
  }


// Set snapshot delay
long ShutterBug::onCmdDelay(FXObject*,FXSelector,void*){
  FXint time=(FXint)delay;
  if(FXInputDialog::getInteger(time,this,tr("Snap Shot Delay"),tr("Snapshot delay in milliseconds:"),NULL,100,10000)){
    delay=(FXuint)time;
    }
  return 1;
  }


// Set auto-recording rate
long ShutterBug::onCmdRecordRate(FXObject*,FXSelector,void*){
  FXint millisecs=(FXint)rate;
  if(FXInputDialog::getInteger(millisecs,this,tr("Record Rate"),tr("Record one frame every milliseconds:"),NULL,10,10000)){
    rate=(FXuint)millisecs;
    }
  return 1;
  }


// Set file count for auto-recording
long ShutterBug::onCmdSetCount(FXObject*,FXSelector,void*){
  FXint count=filecount;
  if(FXInputDialog::getInteger(count,this,tr("File number"),tr("File number to record next:"),NULL,1,1000000)){
    filecount=count;
    }
  return 1;
  }


// Reset auto-recording file count
long ShutterBug::onCmdResetCount(FXObject*,FXSelector,void*){
  filecount=1;
  return 1;
  }


// Pressed on snapper
long ShutterBug::onPressSnapper(FXObject*,FXSelector sel,void* ptr){
  register FXint which=FXSELID(sel)-ID_SNAPPER_0;
  register FXEvent *event=(FXEvent*)ptr;
  if((event->state&CONTROLMASK) || (event->type!=SEL_LEFTBUTTONPRESS) || size){
    mode=MODE_WHOLERECT;
    spotx=event->root_x-rectangle.x;
    spoty=event->root_y-rectangle.y;
    }
  else{
    mode=where(event->root_x,event->root_y);
    if(mode&MODE_TOP) spoty=event->root_y-rectangle.y;
    else if(mode&MODE_BOTTOM) spoty=event->root_y-rectangle.y-rectangle.h;
    if(mode&MODE_LEFT) spotx=event->root_x-rectangle.x;
    else if(mode&MODE_RIGHT) spotx=event->root_x-rectangle.x-rectangle.w;
    }
  changeCursor(which,mode);
  snapper[0]->raise();
  snapper[1]->raise();
  snapper[2]->raise();
  snapper[3]->raise();
  raise();
  return 1;
  }


// Release on snapper
long ShutterBug::onReleaseSnapper(FXObject*,FXSelector sel,void*){
  register FXint which=FXSELID(sel)-ID_SNAPPER_0;
  mode=MODE_NONE;
  changeCursor(which,mode);
  return 1;
  }


// Moved snapper
long ShutterBug::onMovedSnapper(FXObject*,FXSelector sel,void* ptr){
  register FXint which=FXSELID(sel)-ID_SNAPPER_0;
  register FXEvent *event=(FXEvent*)ptr;
  register FXuchar m;
  FXint t;
  FXTRACE((1,"%s::onMovedSnapper %d,%d\n",getClassName(),((FXEvent*)ptr)->win_x,((FXEvent*)ptr)->win_y));
  if(mode!=MODE_NONE){

    // Move whole rectangle
    if(mode&MODE_WHOLERECT){
      rectangle.x=event->root_x-spotx;
      rectangle.y=event->root_y-spoty;
      }

    // Move corner of rectangle
    else{

      // Vertical
      if(mode&MODE_TOP){
        t=rectangle.y+rectangle.h-event->root_y+spoty;
        if(t>=MINSIZE){ rectangle.h=t; rectangle.y=event->root_y-spoty; }
        }
      else if(mode&MODE_BOTTOM){
        t=event->root_y-spoty-rectangle.y;
        if(t>=MINSIZE){ rectangle.h=t; }
        }

      // Horizontal
      if(mode&MODE_LEFT){
        t=rectangle.x+rectangle.w-event->root_x+spotx;
        if(t>=MINSIZE){ rectangle.w=t; rectangle.x=event->root_x-spotx; }
        }
      else if(mode&MODE_RIGHT){
        t=event->root_x-spotx-rectangle.x;
        if(t>=MINSIZE){ rectangle.w=t; }
        }
      }

    // Update rectangle
    moveSnapRectangle(rectangle);
    m=mode;
    }
  else{
    if((event->state&CONTROLMASK) || size)
      m=MODE_WHOLERECT;
    else
      m=where(event->root_x,event->root_y);
    }
  changeCursor(which,m);
  return 1;
  }


// Entered snapper window
long ShutterBug::onEnterSnapper(FXObject*,FXSelector sel,void* ptr){
  register FXuchar m=MODE_WHOLERECT;
  if(!(((FXEvent*)ptr)->state&CONTROLMASK) && (0==size)){
    m=where(((FXEvent*)ptr)->root_x,((FXEvent*)ptr)->root_y);
    }
  changeCursor(FXSELID(sel)-ID_SNAPPER_0,m);
  return 1;
  }


// Left snapper window
long ShutterBug::onLeaveSnapper(FXObject*,FXSelector,void*){
  return 1;
  }


// About box
long ShutterBug::onCmdAbout(FXObject*,FXSelector,void*){
  FXDialogBox about(this,tr("About ShutterBug"),DECOR_TITLE|DECOR_BORDER,0,0,0,0, 10,10,0,0, 0,0);
  new FXLabel(&about,FXString::null,bigicon,FRAME_GROOVE|LAYOUT_SIDE_LEFT|LAYOUT_CENTER_Y|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y);
  FXVerticalFrame* side=new FXVerticalFrame(&about,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 10,10,10,10, 0,0);
  new FXLabel(side,"ShutterBug",NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_FILL_X);
  new FXHorizontalSeparator(side,SEPARATOR_LINE|LAYOUT_FILL_X);
  new FXLabel(side,FXStringFormat(tr("\nFOX Screenshot Utility, version %d.%d.%d.\nShutterBug uses the FOX Toolkit version %d.%d.%d.\nCopyright (C) 2003,2005 Jeroen van der Zijp (jeroen@fox-toolkit.org).\n "),VERSION_MAJOR,VERSION_MINOR,VERSION_PATCH,FOX_MAJOR,FOX_MINOR,FOX_LEVEL),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  FXButton *button=new FXButton(side,tr("&OK"),NULL,&about,FXDialogBox::ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,32,32,2,2);
  button->setFocus();
  about.execute();
  return 1;
  }


// Toggle lasso display
long ShutterBug::onCmdLasso(FXObject*,FXSelector,void*){
  if(snapRectangleShown()) hideSnapRectangle(); else showSnapRectangle();
  return 1;
  }


// Update toggle lasso
long ShutterBug::onUpdLasso(FXObject* sender,FXSelector,void*){
  sender->handle(this,snapRectangleShown()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change size mode
long ShutterBug::onCmdSize(FXObject*,FXSelector sel,void*){
  size=FXSELID(sel)-ID_SIZE_CUSTOM;
  if(size){
    if(size<0){
      rectangle.x=0;
      rectangle.y=0;
      rectangle.w=getRoot()->getDefaultWidth();
      rectangle.h=getRoot()->getDefaultHeight();
      }
    else{
      if(rectangle.x>getRoot()->getDefaultWidth()-size){ rectangle.x=getRoot()->getDefaultWidth()-size; }
      if(rectangle.y>getRoot()->getDefaultHeight()-size){ rectangle.y=getRoot()->getDefaultHeight()-size; }
      if(rectangle.x<0){ rectangle.x=0; }
      if(rectangle.y<0){ rectangle.y=0; }
      rectangle.w=size;
      rectangle.h=size;
      }
    FXTRACE((1,"x=%d y=%d w=%d h=%d\n",rectangle.x,rectangle.y,rectangle.w,rectangle.h));
    moveSnapRectangle(rectangle);
    }
  return 1;
  }


// Update change size mode
long ShutterBug::onUpdSize(FXObject* sender,FXSelector sel,void*){
  sender->handle(this,size==FXSELID(sel)-ID_SIZE_CUSTOM?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Change line weigth
long ShutterBug::onCmdLineWeight(FXObject*,FXSelector sel,void*){
  weight=FXSELID(sel)-ID_WEIGHT_0;
  moveSnapRectangle(rectangle);
  return 1;
  }


// Change line color
long ShutterBug::onCmdLineColor(FXObject*,FXSelector,void*){
  FXColorDialog colordialog(this,tr("Line Color"));
  colordialog.setOpaqueOnly(TRUE);
  colordialog.setRGBA(color);
  if(colordialog.execute()){
    color=colordialog.getRGBA();
    snapper[0]->setBackColor(color);
    snapper[1]->setBackColor(color);
    snapper[2]->setBackColor(color);
    snapper[3]->setBackColor(color);
    }
  return 1;
  }


// Lines inside area
long ShutterBug::onCmdLineInside(FXObject*,FXSelector,void* ptr){
  inside=(FXbool)(FXuval)ptr;
  moveSnapRectangle(rectangle);
  return 1;
  }


// Update lines inside area
long ShutterBug::onUpdLineInside(FXObject* sender,FXSelector,void*){
  sender->handle(this,inside?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Quantization
long ShutterBug::onCmdQuantize(FXObject*,FXSelector,void* ptr){
  quantize=(FXbool)(FXuval)ptr;
  return 1;
  }


// Update quantization
long ShutterBug::onUpdQuantize(FXObject* sender,FXSelector,void*){
  sender->handle(this,quantize?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Destroy main window
ShutterBug::~ShutterBug(){
  getApp()->removeTimeout(this,ID_RECORD_FRAME);
  getApp()->removeTimeout(this,ID_SNAPSHOT);
  FXFREE(&clipbuffer);
  delete snapper[0];
  delete snapper[1];
  delete snapper[2];
  delete snapper[3];
  delete bigicon;
  delete smallicon;
  delete weighticons[0];
  delete weighticons[1];
  delete weighticons[2];
  delete weighticons[3];
  delete weighticons[4];
  delete weighticons[5];
  }

/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("ShutterBug",FXString::null);

  // Open display
  application.init(argc,argv);

  // Main window
  new ShutterBug(&application);

  // Create app
  application.create();

  // Run
  return application.run();
  }
