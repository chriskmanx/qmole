/********************************************************************************
*                                                                               *
*                         C o l o r W e l l   C l a s s                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXColorWell.cpp,v 1.68 2006/01/22 17:58:20 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObject.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXLabel.h"
#include "FXColorWell.h"
#include "FXColorDialog.h"

/*
  Notes:
  - Is there any reason why one wouldn't ^C ^V on the clipboard colors same
    as text?  Maybe ^A should claim selection?
  - Single-click should send SEL_COMMAND to target.
  - Perhaps change of color should send SEL_CHANGED.
  - Do not start drag operation unless moving a little bit.
  - Drive from keyboard.
  - Yes, you can now drag a color into a text widget, or drag the name of
    a color into the well, as well as pasting a color into a text widget or
    vice versa!
  - KDE/Qt has color drag and drop wrong:- should multiply by 257, not 255,
    so get full range of [0 - 65535].
*/

#define WELLSIZE    12              // Minimum well size
#define FOCUSBORDER 3               // Focus border

using namespace FX;

/*******************************************************************************/

namespace FX {

// Map
FXDEFMAP(FXColorWell) FXColorWellMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXColorWell::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXColorWell::onMotion),
  FXMAPFUNC(SEL_DRAGGED,0,FXColorWell::onDragged),
  FXMAPFUNC(SEL_DND_MOTION,0,FXColorWell::onDNDMotion),
  FXMAPFUNC(SEL_DND_ENTER,0,FXColorWell::onDNDEnter),
  FXMAPFUNC(SEL_DND_LEAVE,0,FXColorWell::onDNDLeave),
  FXMAPFUNC(SEL_DND_DROP,0,FXColorWell::onDNDDrop),
  FXMAPFUNC(SEL_DND_REQUEST,0,FXColorWell::onDNDRequest),
  FXMAPFUNC(SEL_FOCUSIN,0,FXColorWell::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXColorWell::onFocusOut),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXColorWell::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXColorWell::onLeftBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXColorWell::onMiddleBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXColorWell::onMiddleBtnRelease),
  FXMAPFUNC(SEL_CLICKED,0,FXColorWell::onClicked),
  FXMAPFUNC(SEL_DOUBLECLICKED,0,FXColorWell::onDoubleClicked),
  FXMAPFUNC(SEL_KEYPRESS,0,FXColorWell::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXColorWell::onKeyRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXColorWell::onUngrabbed),
  FXMAPFUNC(SEL_BEGINDRAG,0,FXColorWell::onBeginDrag),
  FXMAPFUNC(SEL_ENDDRAG,0,FXColorWell::onEndDrag),
  FXMAPFUNC(SEL_SELECTION_LOST,0,FXColorWell::onSelectionLost),
  FXMAPFUNC(SEL_SELECTION_GAINED,0,FXColorWell::onSelectionGained),
  FXMAPFUNC(SEL_SELECTION_REQUEST,0,FXColorWell::onSelectionRequest),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXColorWell::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXColorWell::onQueryHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_SETVALUE,FXColorWell::onCmdSetValue),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_SETINTVALUE,FXColorWell::onCmdSetIntValue),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_GETINTVALUE,FXColorWell::onCmdGetIntValue),
  FXMAPFUNC(SEL_CHANGED,FXColorWell::ID_COLORDIALOG,FXColorWell::onChgColorWell),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_COLORDIALOG,FXColorWell::onCmdColorWell),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_SETHELPSTRING,FXColorWell::onCmdSetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_GETHELPSTRING,FXColorWell::onCmdGetHelp),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_SETTIPSTRING,FXColorWell::onCmdSetTip),
  FXMAPFUNC(SEL_COMMAND,FXColorWell::ID_GETTIPSTRING,FXColorWell::onCmdGetTip),
  };


// Object implementation
FXIMPLEMENT(FXColorWell,FXFrame,FXColorWellMap,ARRAYNUMBER(FXColorWellMap))



/*******************************************************************************/


// Init
FXColorWell::FXColorWell(){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  rgba=0;
  wellColor[0]=0;
  wellColor[1]=0;
  }


// Make a color well
FXColorWell::FXColorWell(FXComposite* p,FXColor clr,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  target=tgt;
  message=sel;
  rgba=clr;
  wellColor[0]=rgbaoverwhite(rgba);
  wellColor[1]=rgbaoverblack(rgba);
  }


// Create window
void FXColorWell::create(){
  FXFrame::create();
  if(!colorType){ colorType=getApp()->registerDragType(colorTypeName); }
  if(!textType){ textType=getApp()->registerDragType(textTypeName); }
  if(!utf8Type){ utf8Type=getApp()->registerDragType(utf8TypeName); }
  }


// Detach window
void FXColorWell::detach(){
  FXFrame::detach();
  colorType=0;
  textType=0;
  }


// If window can have focus
bool FXColorWell::canFocus() const { return true; }


// Into focus chain
void FXColorWell::setFocus(){
  FXFrame::setFocus();
  setDefault(TRUE);
  }


// Out of focus chain
void FXColorWell::killFocus(){
  FXFrame::killFocus();
  setDefault(MAYBE);
  }


// Compute color over black
FXColor FXColorWell::rgbaoverblack(FXColor clr){
  FXint r,g,b,mul=FXALPHAVAL(clr);
  r=(FXREDVAL(clr)*mul+127)/255;
  g=(FXGREENVAL(clr)*mul+127)/255;
  b=(FXBLUEVAL(clr)*mul+127)/255;
  return FXRGB(r,g,b);
  }


// Compute color over white
FXColor FXColorWell::rgbaoverwhite(FXColor clr){
  FXint r,g,b,mul=FXALPHAVAL(clr),lum=(255-mul);
  r=(lum*255+FXREDVAL(clr)*mul+127)/255;
  g=(lum*255+FXGREENVAL(clr)*mul+127)/255;
  b=(lum*255+FXBLUEVAL(clr)*mul+127)/255;
  return FXRGB(r,g,b);
  }


// Get default size
FXint FXColorWell::getDefaultWidth(){
  return WELLSIZE+FOCUSBORDER+padleft+padright+4;
  }


FXint FXColorWell::getDefaultHeight(){
  return WELLSIZE+FOCUSBORDER+padtop+padbottom+4;
  }


// Handle repaint
long FXColorWell::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent *ev=(FXEvent*)ptr;
  FXDCWindow dc(this,ev);
  FXPoint points[3];
  dc.setForeground(backColor);
  dc.fillRectangle(0,0,width,padtop+FOCUSBORDER);
  dc.fillRectangle(0,padtop+FOCUSBORDER,padleft+FOCUSBORDER,height-padtop-padbottom-(FOCUSBORDER<<1));
  dc.fillRectangle(width-padright-FOCUSBORDER,padtop+FOCUSBORDER,padright+FOCUSBORDER,height-padtop-padbottom-(FOCUSBORDER<<1));
  dc.fillRectangle(0,height-padbottom-FOCUSBORDER,width,padbottom+FOCUSBORDER);
  if(hasSelection()){
    dc.setForeground(borderColor);
    dc.drawRectangle(padleft+1,padtop+1,width-padright-padleft-3,height-padbottom-padtop-3);
    }
  dc.setForeground(wellColor[0]);
  points[0].x=points[1].x=padleft+FOCUSBORDER+2;
  points[2].x=width-padright-FOCUSBORDER-2;
  points[0].y=points[2].y=padtop+FOCUSBORDER+2;
  points[1].y=height-padbottom-FOCUSBORDER-2;
  dc.fillPolygon(points,3);
  dc.setForeground(wellColor[1]);
  points[0].x=padleft+FOCUSBORDER+2;
  points[1].x=points[2].x=width-padright-FOCUSBORDER-2;
  points[0].y=points[1].y=height-padbottom-FOCUSBORDER-2;
  points[2].y=padtop+FOCUSBORDER+2;
  dc.fillPolygon(points,3);
  drawDoubleSunkenRectangle(dc,padleft+FOCUSBORDER,padtop+FOCUSBORDER,width-padright-padleft-(FOCUSBORDER<<1),height-padbottom-padtop-(FOCUSBORDER<<1));
  if(hasFocus()){
    dc.drawFocusRectangle(padleft,padtop,width-padright-padleft,height-padbottom-padtop);
    }
  return 1;
  }


// Gained focus
long FXColorWell::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onFocusIn(sender,sel,ptr);
  update();
  return 1;
  }


// Lost focus
long FXColorWell::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onFocusOut(sender,sel,ptr);
  update();
  return 1;
  }


// Dragging something over well; save old color and block GUI updates
long FXColorWell::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onDNDEnter(sender,sel,ptr)) return 1;
  flags&=~FLAG_UPDATE;
  return 1;
  }


// Dragged out of well, so restore old color and reenable GUI updates
long FXColorWell::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onDNDLeave(sender,sel,ptr)) return 1;
  flags|=FLAG_UPDATE;
  return 1;
  }


// Handle drag-and-drop motion
long FXColorWell::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr){

  // Handle base class first
  if(FXFrame::onDNDMotion(sender,sel,ptr)) return 1;

  // No more messages while inside
  setDragRectangle(0,0,width,height,FALSE);

  // Is it a color being dropped?
  if(offeredDNDType(FROM_DRAGNDROP,colorType)){
    acceptDrop(DRAG_ACCEPT);
    return 1;
    }

  // Is it a name of a color being dropped?
  if(offeredDNDType(FROM_DRAGNDROP,textType)){
    acceptDrop(DRAG_ACCEPT);
    return 1;
    }
  return 0;
  }


// Handle drag-and-drop drop
long FXColorWell::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr){
  FXuchar *pointer;
  FXuint   length;
  FXColor  color;

  // Enable updating
  flags|=FLAG_UPDATE;

  // Try handling it in base class first
  if(FXFrame::onDNDDrop(sender,sel,ptr)) return 1;

  // Try and obtain the color first
  if(getDNDData(FROM_DRAGNDROP,colorType,pointer,length)){
    color=FXRGBA((((FXushort*)pointer)[0]+128)/257,(((FXushort*)pointer)[1]+128)/257,(((FXushort*)pointer)[2]+128)/257,(((FXushort*)pointer)[3]+128)/257);
    FXFREE(&pointer);
    setRGBA(color,TRUE);
    return 1;
    }

  // Maybe its the name of a color
  if(getDNDData(FROM_DRAGNDROP,textType,pointer,length)){
    FXRESIZE(&pointer,FXuchar,length+1); pointer[length]='\0';
    color=fxcolorfromname((const FXchar*)pointer);
    FXFREE(&pointer);

    // Accept the drop only if it was a valid color name
    if(color!=FXRGBA(0,0,0,0)){
      setRGBA(color,TRUE);
      return 1;
      }
    }

  return 0;
  }


// Service requested DND data
long FXColorWell::onDNDRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr; FXushort *clr; FXchar *str;

  // Try handling it in base class first
  if(FXFrame::onDNDRequest(sender,sel,ptr)) return 1;

  // Requested as a color
  if(event->target==colorType){
    FXMALLOC(&clr,FXushort,4);
    clr[0]=257*FXREDVAL(rgba);
    clr[1]=257*FXGREENVAL(rgba);
    clr[2]=257*FXBLUEVAL(rgba);
    clr[3]=257*FXALPHAVAL(rgba);
    setDNDData(FROM_DRAGNDROP,colorType,(FXuchar*)clr,sizeof(FXushort)*4);
    return 1;
    }

  // Requested as a color name
  if(event->target==textType){
    FXMALLOC(&str,FXchar,50);
    fxnamefromcolor(str,rgba);
    setDNDData(FROM_DRAGNDROP,textType,(FXuchar*)str,strlen(str));
    return 1;
    }
  return 0;
  }


// We now really do have the selection; repaint the text field
long FXColorWell::onSelectionGained(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onSelectionGained(sender,sel,ptr);
  update();
  return 1;
  }


// We lost the selection somehow; repaint the text field
long FXColorWell::onSelectionLost(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onSelectionLost(sender,sel,ptr);
  update();
  return 1;
  }


// Somebody wants our selection
long FXColorWell::onSelectionRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Try handling it in base class first
  if(FXFrame::onSelectionRequest(sender,sel,ptr)) return 1;

  // Requested as a color
  if(event->target==colorType){
    FXushort *color;
    FXMALLOC(&color,FXushort,4);
    color[0]=257*FXREDVAL(rgba);
    color[1]=257*FXGREENVAL(rgba);
    color[2]=257*FXBLUEVAL(rgba);
    color[3]=257*FXALPHAVAL(rgba);
    setDNDData(FROM_SELECTION,colorType,(FXuchar*)color,sizeof(FXushort)*4);
    return 1;
    }

  // Requested as a color name
  if(event->target==stringType || event->target==textType){
    FXchar *data;
    FXCALLOC(&data,FXchar,50);
    fxnamefromcolor(data,rgba);
#ifndef WIN32
    setDNDData(FROM_SELECTION,event->target,(FXuchar*)data,strlen(data));
#else
    setDNDData(FROM_SELECTION,event->target,(FXuchar*)data,strlen(data)+1);
#endif
    return 1;
    }
  return 0;
  }


// Start a drag operation
long FXColorWell::onBeginDrag(FXObject* sender,FXSelector sel,void* ptr){
  FXDragType types[2];
  if(FXFrame::onBeginDrag(sender,sel,ptr)) return 1;
  types[0]=colorType;
  types[1]=textType;
  beginDrag(types,2);
  setDragCursor(getApp()->getDefaultCursor(DEF_SWATCH_CURSOR));
  return 1;
  }


// End drag operation
long FXColorWell::onEndDrag(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onEndDrag(sender,sel,ptr)) return 1;
  endDrag(didAccept()==DRAG_COPY);
  setDragCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
  return 1;
  }


// Dragged stuff around
long FXColorWell::onDragged(FXObject* sender,FXSelector sel,void* ptr){
  if(FXFrame::onDragged(sender,sel,ptr)) return 1;
  handleDrag(((FXEvent*)ptr)->root_x,((FXEvent*)ptr)->root_y,DRAG_COPY);
  if(didAccept()==DRAG_COPY){
    setDragCursor(getApp()->getDefaultCursor(DEF_SWATCH_CURSOR));
    }
  else{
    setDragCursor(getApp()->getDefaultCursor(DEF_DNDSTOP_CURSOR));
    }
  return 1;
  }


// Moving
long FXColorWell::onMotion(FXObject*,FXSelector,void* ptr){
  if(flags&FLAG_DODRAG){
    handle(this,FXSEL(SEL_DRAGGED,0),ptr);
    return 1;
    }
  if((flags&FLAG_TRYDRAG) && ((FXEvent*)ptr)->moved){
    if(handle(this,FXSEL(SEL_BEGINDRAG,0),ptr)) flags|=FLAG_DODRAG;
    flags&=~FLAG_TRYDRAG;
    return 1;
    }
  return 0;
  }


// Drag start
long FXColorWell::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    if(event->click_count==1){
      flags&=~FLAG_UPDATE;
      flags|=FLAG_TRYDRAG;
      }
    }
  return 1;
  }


// Drop
long FXColorWell::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXuint flgs=flags;
  if(isEnabled()){
    ungrab();
    flags|=FLAG_UPDATE;
    flags&=~(FLAG_TRYDRAG|FLAG_DODRAG);
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    if(flgs&FLAG_DODRAG){handle(this,FXSEL(SEL_ENDDRAG,0),ptr);}
    if(event->click_count==1){
      handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXuval)rgba);
      if(!event->moved && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)rgba);
      }
    else if(event->click_count==2){
      handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXuval)rgba);
      }
    return 1;
    }
  return 1;
  }


// Pressed middle button to paste
long FXColorWell::onMiddleBtnPress(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONPRESS,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// Released middle button causes paste of selection
long FXColorWell::onMiddleBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuchar *pointer;
  FXuint   length;
  FXColor  color;
  if(isEnabled()){
    ungrab();
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONRELEASE,message),ptr)) return 1;
    if(getDNDData(FROM_SELECTION,colorType,pointer,length)){
      color=FXRGBA((((FXushort*)pointer)[0]+128)/257,(((FXushort*)pointer)[1]+128)/257,(((FXushort*)pointer)[2]+128)/257,(((FXushort*)pointer)[3]+128)/257);
      FXFREE(&color);
      setRGBA(color,TRUE);
      return 1;
      }
    if(getDNDData(FROM_SELECTION,stringType,pointer,length)){
      FXRESIZE(&pointer,FXuchar,length+1); pointer[length]='\0';
      color=fxcolorfromname((const FXchar*)pointer);
      FXFREE(&pointer);
      setRGBA(color,TRUE);
      return 1;
      }
    }
  return 0;
  }


// Key Press
long FXColorWell::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    switch(event->code){
      case KEY_space:
      case KEY_KP_Enter:
      case KEY_Return:
        flags&=~FLAG_UPDATE;
        return 1;
      }
    }
  return 0;
  }


// Key Release
long FXColorWell::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    flags|=FLAG_UPDATE;
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_space:
        handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXuval)rgba);
        if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)rgba);
        return 1;
      case KEY_KP_Enter:
      case KEY_Return:
        handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXuval)rgba);
        return 1;
      }
    }
  return 0;
  }


// Clicked in the well
long FXColorWell::onClicked(FXObject*,FXSelector,void*){
  FXDragType types[3];
  if(target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),(void*)(FXuval)rgba)) return 1;
  if(!hasSelection()){
    types[0]=stringType;
    types[1]=colorType;
    types[2]=textType;
    acquireSelection(types,3);
    }
  return 1;
  }


// Double clicked in well; normally pops the color dialog
// except when COLORWELL_SOURCEONLY is passed in which case
// editing by the dialog is not allowed (used for wells inside the
// color dialog itself for example).
// The well follows the editing via the dialog; when the dialog
// is closed by cancelling it it will revert to the old color
long FXColorWell::onDoubleClicked(FXObject*,FXSelector,void*){
  if(target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),(void*)(FXuval)rgba)) return 1;
  if(options&COLORWELL_SOURCEONLY) return 1;
  FXColorDialog colordialog(this,tr("Color Dialog"));
  FXColor oldcolor=getRGBA();
  colordialog.setTarget(this);
  colordialog.setSelector(ID_COLORDIALOG);
  colordialog.setRGBA(oldcolor);
  colordialog.setOpaqueOnly(isOpaqueOnly());
  if(!colordialog.execute()){
    setRGBA(oldcolor,TRUE);
    }
  return 1;
  }


// The widget lost the grab for some reason
long FXColorWell::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXFrame::onUngrabbed(sender,sel,ptr);
  flags&=~(FLAG_TRYDRAG|FLAG_DODRAG);
  flags|=FLAG_UPDATE;
  endDrag(FALSE);
  return 1;
  }


// Change from another Color Well
long FXColorWell::onChgColorWell(FXObject*,FXSelector,void* ptr){
  flags&=~FLAG_UPDATE;
  setRGBA((FXColor)(FXuval)ptr);
  if(target) target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXuval)rgba);
  return 1;
  }


// Command from another Color Well
long FXColorWell::onCmdColorWell(FXObject*,FXSelector,void* ptr){
  flags|=FLAG_UPDATE;
  setRGBA((FXColor)(FXuval)ptr);
  if(target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)rgba);
  return 1;
  }


// Set help using a message
long FXColorWell::onCmdSetHelp(FXObject*,FXSelector,void* ptr){
  setHelpText(*((FXString*)ptr));
  return 1;
  }


// Get help using a message
long FXColorWell::onCmdGetHelp(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getHelpText();
  return 1;
  }


// Set tip using a message
long FXColorWell::onCmdSetTip(FXObject*,FXSelector,void* ptr){
  setTipText(*((FXString*)ptr));
  return 1;
  }


// Get tip using a message
long FXColorWell::onCmdGetTip(FXObject*,FXSelector,void* ptr){
  *((FXString*)ptr)=getTipText();
  return 1;
  }


// We were asked about tip text
long FXColorWell::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXColorWell::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Change RGBA color
void FXColorWell::setRGBA(FXColor clr,FXbool notify){
  if(options&COLORWELL_OPAQUEONLY) clr|=FXRGBA(0,0,0,255);
  if(clr!=rgba){
    rgba=clr;
    wellColor[0]=rgbaoverwhite(rgba);
    wellColor[1]=rgbaoverblack(rgba);
    update();
    if(notify && target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)(FXuval)rgba);
    }
  }


// Set color
long FXColorWell::onCmdSetValue(FXObject*,FXSelector,void* ptr){
  setRGBA((FXColor)(FXuval)ptr);
  return 1;
  }


// Update well from a message
long FXColorWell::onCmdSetIntValue(FXObject*,FXSelector,void* ptr){
  setRGBA(*((FXColor*)ptr));
  return 1;
  }


// Obtain value from well
long FXColorWell::onCmdGetIntValue(FXObject*,FXSelector,void* ptr){
  *((FXColor*)ptr)=getRGBA();
  return 1;
  }


// Return true if only opaque colors allowed
FXbool FXColorWell::isOpaqueOnly() const {
  return (options&COLORWELL_OPAQUEONLY)!=0;
  }


// Change opaque only mode
void FXColorWell::setOpaqueOnly(FXbool opaque){
  if(opaque){
    options|=COLORWELL_OPAQUEONLY;
    setRGBA(rgba);
    }
  else{
    options&=~COLORWELL_OPAQUEONLY;
    }
  }


// Save data
void FXColorWell::save(FXStream& store) const {
  FXFrame::save(store);
  store << wellColor[0] << wellColor[1];
  store << rgba;
  store << tip;
  store << help;
  }


// Load data
void FXColorWell::load(FXStream& store){
  FXFrame::load(store);
  store >> wellColor[0] >> wellColor[1];
  store >> rgba;
  store >> tip;
  store >> help;
  }


// Destroy
FXColorWell::~FXColorWell(){
  }


// {
//   FXEvent *ev=(FXEvent*)ptr;
//   XEvent se;
//   FXint tox,toy;
//   Window tmp;
//   Window www=getWindowAt(ev->root_x,ev->root_y);
//
//   XTranslateCoordinates(getDisplay(),XDefaultRootWindow(getDisplay()),www,ev->root_x,ev->root_y,&tox,&toy,&tmp);
//
//   se.xbutton.type=ButtonPress;
//   se.xbutton.serial=0;
//   se.xbutton.send_event=1;
//   se.xbutton.display=getDisplay();
//   se.xbutton.window=www;
//   se.xbutton.root=XDefaultRootWindow(getDisplay());
//   se.xbutton.subwindow=None;
//   se.xbutton.time=ev->time;
//   se.xbutton.x=tox;
//   se.xbutton.y=toy;
//   se.xbutton.x_root=ev->root_x;
//   se.xbutton.y_root=ev->root_y;
//   se.xbutton.state=0;
//   se.xbutton.button=2;
//   se.xbutton.same_screen=TRUE;
// fprintf(stderr,"SendEvent to window %d\n",se.xbutton.window);
//   XSendEvent(getDisplay(),se.xbutton.window,True,NoEventMask,&se);
//
//   se.xbutton.type=ButtonRelease;
//   se.xbutton.serial=0;
//   se.xbutton.send_event=1;
//   se.xbutton.display=getDisplay();
//   se.xbutton.window=www;
//   se.xbutton.root=XDefaultRootWindow(getDisplay());
//   se.xbutton.subwindow=None;
//   se.xbutton.time=ev->time+1;
//   se.xbutton.x=tox;
//   se.xbutton.y=toy;
//   se.xbutton.x_root=ev->root_x;
//   se.xbutton.y_root=ev->root_y;
//   se.xbutton.state=Button2Mask;
//   se.xbutton.button=2;
//   se.xbutton.same_screen=TRUE;
// fprintf(stderr,"SendEvent to window %d\n",se.xbutton.window);
//   XSendEvent(getDisplay(),se.xbutton.window,True,NoEventMask,&se);
// }

}
