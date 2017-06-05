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
* $Id: ShutterBug.h,v 1.25 2006/01/22 17:58:15 fox Exp $                        *
********************************************************************************/
#ifndef SHUTTERBUG_H
#define SHUTTERBUG_H


/*******************************************************************************/

class Snapper;

// Mini application object
class ShutterBug : public FXShell {
  FXDECLARE(ShutterBug)
protected:
  FXString          filename;        // Filename last used
  FXint             fileformat;      // File format last chosen
  FXint             filecount;       // File count for numbered files
  FXColor          *clipbuffer;      // Clipped image buffer
  FXint             clipwidth;       // Clipped image width
  FXint             clipheight;      // Clipped image height
  Snapper          *snapper[4];      // Sides of area
  FXIcon           *bigicon;         // Big application icon
  FXIcon           *smallicon;       // Small application icon
  FXIcon           *weighticons[6];  // Line weight icons
  FXRectangle       rectangle;       // Rectangle to snap
  FXColor           color;           // Color of the snapshot lines
  FXint             weight;          // Weight of the snapshot lines
  FXint             size;            // Fixed size if not 0
  FXuint            delay;           // Timer delay in ms
  FXuint            rate;            // Record rate in ms per frame
  FXbool            inside;          // Lines are inside
  FXbool            quantize;        // Quantization mode
  FXint             spotx;           // Grab-spot of mouse on rectangle
  FXint             spoty;
  FXuchar           mode;            // Dragging mode
protected:
  static FXDragType dndTypes[7];     // DND Types
protected:
  ShutterBug(){}
  FXuchar where(FXint x,FXint y) const;
  void changeCursor(FXint which,FXuchar drag);
  void showSnapRectangle();
  void hideSnapRectangle();
  FXbool snapRectangleShown() const;
  void moveSnapRectangle(const FXRectangle& r);
  FXbool saveImage(const FXString& file,FXColor* data,FXint w,FXint h);
  FXbool snapRectangle(FXColor*& data,const FXRectangle& r);
  FXbool grabRectangle(FXColor*& data,const FXRectangle& r);
  void readPixels(FXImage* image,const FXRectangle& rectangle);
  virtual bool doesOverrideRedirect() const;
protected:
  enum {
    MODE_NONE        = 0,
    MODE_TOP         = 1,
    MODE_BOTTOM      = 2,
    MODE_LEFT        = 4,
    MODE_RIGHT       = 8,
    MODE_TOPLEFT     = (MODE_TOP|MODE_LEFT),
    MODE_TOPRIGHT    = (MODE_TOP|MODE_RIGHT),
    MODE_BOTTOMLEFT  = (MODE_BOTTOM|MODE_LEFT),
    MODE_BOTTOMRIGHT = (MODE_BOTTOM|MODE_RIGHT),
    MODE_WHOLERECT   = 16
    };
private:
  ShutterBug(const ShutterBug&);
  ShutterBug &operator=(const ShutterBug&);
public:
  long onPaint(FXObject*,FXSelector,void*);
  long onMotion(FXObject*,FXSelector,void*);
  long onBtnPress(FXObject*,FXSelector,void*);
  long onBtnRelease(FXObject*,FXSelector,void*);
  long onKeyPress(FXObject*,FXSelector,void*);
  long onKeyRelease(FXObject*,FXSelector,void*);
  long onPressSnapper(FXObject*,FXSelector,void*);
  long onReleaseSnapper(FXObject*,FXSelector,void*);
  long onMovedSnapper(FXObject*,FXSelector,void*);
  long onEnterSnapper(FXObject*,FXSelector,void*);
  long onLeaveSnapper(FXObject*,FXSelector,void*);
  long onCmdSnap(FXObject*,FXSelector,void*);
  long onCmdSnapDelayed(FXObject*,FXSelector,void*);
  long onCmdSnapClipboard(FXObject*,FXSelector,void*);
  long onCmdDelay(FXObject*,FXSelector,void*);
  long onCmdAbout(FXObject*,FXSelector,void*);
  long onCmdLasso(FXObject*,FXSelector,void*);
  long onUpdLasso(FXObject*,FXSelector,void*);
  long onCmdSize(FXObject*,FXSelector,void*);
  long onUpdSize(FXObject*,FXSelector,void*);
  long onCmdQuit(FXObject*,FXSelector,void*);
  long onCmdLineWeight(FXObject*,FXSelector,void*);
  long onCmdLineColor(FXObject*,FXSelector,void*);
  long onCmdLineInside(FXObject*,FXSelector,void*);
  long onUpdLineInside(FXObject*,FXSelector,void*);
  long onClipboardLost(FXObject*,FXSelector,void*);
  long onClipboardRequest(FXObject*,FXSelector,void*);
  long onCmdQuantize(FXObject*,FXSelector,void*);
  long onUpdQuantize(FXObject*,FXSelector,void*);
  long onCmdSetCount(FXObject*,FXSelector,void*);
  long onCmdResetCount(FXObject*,FXSelector,void*);
  long onCmdRecordRate(FXObject*,FXSelector,void*);
  long onCmdRecordFrame(FXObject*,FXSelector,void*);
  long onCmdRecordMovie(FXObject*,FXSelector,void*);
public:
  enum {
    ID_SNAPSHOT=FXMainWindow::ID_LAST,
    ID_SNAPSHOT_DELAYED,
    ID_SNAPSHOT_CLIPBOARD,
    ID_RECORD_FRAME,
    ID_RECORD_MOVIE,
    ID_ABOUT,
    ID_TOGGLE_LASSO,
    ID_SNAPPER_0,
    ID_SNAPPER_1,
    ID_SNAPPER_2,
    ID_SNAPPER_3,
    ID_SIZE_SCREEN,
    ID_SIZE_CUSTOM,
    ID_SIZE_8X8=ID_SIZE_CUSTOM+8,
    ID_SIZE_16X16=ID_SIZE_CUSTOM+16,
    ID_SIZE_24X24=ID_SIZE_CUSTOM+24,
    ID_SIZE_32X32=ID_SIZE_CUSTOM+32,
    ID_SIZE_48X48=ID_SIZE_CUSTOM+48,
    ID_SIZE_64X64=ID_SIZE_CUSTOM+64,
    ID_SIZE_128X128=ID_SIZE_CUSTOM+128,
    ID_SIZE_256X256=ID_SIZE_CUSTOM+256,
    ID_SIZE_512X512=ID_SIZE_CUSTOM+512,
    ID_WEIGHT_0,
    ID_WEIGHT_1,
    ID_WEIGHT_2,
    ID_WEIGHT_3,
    ID_WEIGHT_4,
    ID_WEIGHT_5,
    ID_WEIGHT_6,
    ID_COLOR,
    ID_DELAY,
    ID_INSIDE,
    ID_QUANTIZE,
    ID_SET_COUNT,
    ID_RESET_COUNT,
    ID_RECORD_RATE,
    ID_QUIT,
    ID_LAST
    };
public:

  /// Construct calculator dialog
  ShutterBug(FXApp* a);

  /// Create
  virtual void create();

  /// Return the default width of this window
  virtual FXint getDefaultWidth();

  /// Return the default height of this window
  virtual FXint getDefaultHeight();

  /// Read/write registry
  void readRegistry();
  void writeRegistry();

  /// Destroy calculator
  virtual ~ShutterBug();
  };

#endif
