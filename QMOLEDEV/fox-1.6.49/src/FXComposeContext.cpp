/********************************************************************************
*                                                                               *
*                         C o m p o s e - C o n t e x t                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2005,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: FXComposeContext.cpp,v 1.12 2006/01/22 17:58:21 fox Exp $                *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXSettings.h"
#include "FXRegistry.h"
#include "FXApp.h"
#include "FXId.h"
#include "FXWindow.h"
#include "FXComposeContext.h"
#include "FXException.h"


/*
  Notes:
  - In Asian languages, a text entry widget that's being edited may have
    an input method editor.  During the composition process, the system
    needs to keep track of the state of the composition until it is
    committed as an input to the widget.  This class represents that
    state.
  - Each text entry widget may have a compositon context, while it
    has the focus.
  - The composition context is deleted when the focus is moved to another
    widget away from the entry widget.
*/

#define DISPLAY(app)     ((Display*)((app)->display))


using namespace FX;

/*******************************************************************************/

namespace FX {


// Object implementation
FXIMPLEMENT(FXComposeContext,FXId,NULL,0)


#ifdef WIN32   //////////////////////////  MS-Windows ///////////////////////////


// Deserialization
FXComposeContext::FXComposeContext():window(NULL),message(0){
  FXTRACE((1,"FXComposeContext::FXComposeContext %p\n",this));
  }


// Create input context
FXComposeContext::FXComposeContext(FXApp* a,FXWindow* win,FXSelector sel):FXId(a),window(win),message(sel){
  FXTRACE((1,"FXComposeContext::FXComposeContext %p\n",this));
  }


// Realize the input context
void FXComposeContext::create(){
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));
      ///////
      }
    }
  }


// Unrealize the input context
void FXComposeContext::destroy(){
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
      ///////
      }
    }
  }


// Set focus to it
void FXComposeContext::focusIn(){
  if(xid){
    ///////
    }
  }


// Kill focus to it
void FXComposeContext::focusOut(){
  if(xid){
    ///////
    }
  }


// Set the spot
void FXComposeContext::setSpot(FXint x,FXint y){
  if(xid){
    ///////
    }
  }


// Set the area
void FXComposeContext::setArea(FXint x,FXint y,FXint w,FXint h){
  if(xid){
    ///////
    }
  }


// Translate key event
FXString FXComposeContext::translateEvent(FXRawEvent& event){
  FXString result;
  if(xid){
    ///////
    }
  return result;
  }


// Delete input context
FXComposeContext::~FXComposeContext(){
  FXTRACE((1,"FXComposeContext::~FXComposeContext %p\n",this));
  }


#else   //////////////////////////////  X-Windows ///////////////////////////////


// Deserialization
FXComposeContext::FXComposeContext():window(NULL),message(0){
  FXTRACE((1,"FXComposeContext::FXComposeContext %p\n",this));
  }


// Create input context
FXComposeContext::FXComposeContext(FXApp* a,FXWindow* win,FXSelector sel):FXId(a),window(win),message(sel){
  FXTRACE((1,"FXComposeContext::FXComposeContext %p\n",this));
  }


/*

    // Determine list of input styles
    XIMStyles *ximstyles=NULL;
    XGetIMValues((XIM)xim,XNQueryInputStyle,&ximstyles,NULL);
    if(ximstyles){
      FXuint s;

      // Try preferred input style
      for(s=0; s<ximstyles->count_styles; s++){
        if(ximstyles->supported_styles[s]==inputstyle) goto match;
        }

      // Try root input style
      inputstyle=XIMPreeditNothing|XIMStatusNothing;
      for(s=0; s<ximstyles->count_styles; s++){
        if(ximstyles->supported_styles[s]==inputstyle) goto match;
        }

      // Try none style
      inputstyle=XIMPreeditNone|XIMStatusNone;
      for(s=0; s<ximstyles->count_styles; s++){
        if(ximstyles->supported_styles[s]==inputstyle) goto match;
        }

      // No style at all
      inputstyle=0;

      // Free list
match:XFree(ximstyles);
*/


/*
bool isIMRunning(Display *display){
  const FXchar *p=XSetLocaleModifiers(NULL);
  if(p){
    FXTRACE((1,"XSetLocaleModifiers=%s\n",p));
    FXString server("@server=");
    server.append(p+4);         // skip "@im="
    FXint pos=server.find('@',1);
    if(0<pos) server.trunc(pos);
    Atom atom=XInternAtom(display,server.text(),False);
    Window win=XGetSelectionOwner(display,atom);
    return win!=None;
    }
  return false;
  }
*/


// Realize the input context
void FXComposeContext::create(){
  if(!xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::create %p\n",getClassName(),this));
#ifndef NO_XIM
      XIMCallback statusStartStruct;
      XIMCallback statusDoneStruct;
      XIMCallback statusDrawStruct;
      XIMCallback editStartStruct;
      XIMCallback editDoneStruct;
      XIMCallback editDrawStruct;
      XIMCallback editCaretStruct;
      XVaNestedList editAttr;
      XVaNestedList statusAttr;
      XIMStyles *ximstyles=NULL;
      XRectangle rect;
      XPoint spot;
      FXuint style,s;

      // Check if input methods are available
      if(!getApp()->hasInputMethod()){ fxerror("FXComposeContext: no input methods\n"); }

      // We must have a window
      if(!window || !window->id()){ fxerror("FXComposeContext: illegal window parameter\n"); }

      // Get input style
      if(comparecase(getApp()->inputstyle,"onthespot")==0)
        style=XIMPreeditCallbacks|XIMStatusNothing;
      else if(comparecase(getApp()->inputstyle,"overthespot")==0)
        style=XIMPreeditPosition|XIMStatusNothing;
      else if(comparecase(getApp()->inputstyle,"offthespot")==0)
        style=XIMPreeditArea|XIMStatusArea;
      else if(comparecase(getApp()->inputstyle,"root")==0)
        style=XIMPreeditNothing|XIMStatusNothing;
      else
        style=XIMPreeditNone|XIMStatusNone;

      // Determine list of input styles
      XGetIMValues((XIM)getApp()->xim,XNQueryInputStyle,&ximstyles,NULL);
      if(ximstyles){

        // Try preferred input style
        for(s=0; s<ximstyles->count_styles; s++){
          if(ximstyles->supported_styles[s]==style) goto m;
          }

        // Try root input style
        style=XIMPreeditNothing|XIMStatusNothing;
        for(s=0; s<ximstyles->count_styles; s++){
          if(ximstyles->supported_styles[s]==style) goto m;
          }

        // Try none style
        style=XIMPreeditNone|XIMStatusNone;
        for(s=0; s<ximstyles->count_styles; s++){
          if(ximstyles->supported_styles[s]==style) goto m;
          }

        // Pick first
        if(ximstyles->count_styles){
          style=ximstyles->supported_styles[0];
          }

        // Free list
m:      XFree(ximstyles);
        }

      // On the spot method
      if(style&XIMPreeditCallbacks){
        editStartStruct.client_data=(XPointer)this;
        editStartStruct.callback=(XIMProc)editStartCallback;
        editDoneStruct.client_data=(XPointer)this;
        editDoneStruct.callback=(XIMProc)editDoneCallback;
        editDrawStruct.client_data=(XPointer)this;
        editDrawStruct.callback=(XIMProc)editDrawCallback;
        editCaretStruct.client_data=(XPointer)this;
        editCaretStruct.callback=(XIMProc)editCaretCallback;
        editAttr=XVaCreateNestedList(0,XNPreeditStartCallback,&editStartStruct,XNPreeditDrawCallback,&editDrawStruct,XNPreeditDoneCallback,&editDoneStruct,XNPreeditCaretCallback,&editCaretStruct,NULL);

        // Have status callbacks
        if(style&XIMStatusCallbacks){
          FXTRACE((1,"On the Spot/Status\n"));
          statusStartStruct.client_data=(XPointer)this;
          statusStartStruct.callback=(XIMProc)statusStartCallback;
          statusDoneStruct.client_data=(XPointer)this;
          statusDoneStruct.callback=(XIMProc)statusDoneCallback;
          statusDrawStruct.client_data=(XPointer)this;
          statusDrawStruct.callback=(XIMProc)statusDrawCallback;
          statusAttr=XVaCreateNestedList(0,XNStatusStartCallback,&statusStartStruct,XNStatusDoneCallback,&statusDoneStruct,XNStatusDrawCallback,&statusDrawStruct,NULL);
          xid=(FXID)XCreateIC((XIM)getApp()->xim,XNInputStyle,XIMPreeditCallbacks|XIMStatusCallbacks,XNClientWindow,window->id(),XNPreeditAttributes,editAttr,XNStatusAttributes,statusAttr,NULL);
          XFree(statusAttr);
          }

        // No status callbacks
        else{
          FXTRACE((1,"On the Spot\n"));
          xid=(FXID)XCreateIC((XIM)getApp()->xim,XNInputStyle,XIMPreeditCallbacks|XIMStatusNothing,XNClientWindow,window->id(),XNPreeditAttributes,editAttr,NULL);
          }
        XFree(editAttr);
        }

      // Off the spot method
      else if(style&XIMPreeditArea){
        FXTRACE((1,"Off the Spot\n"));
        rect.x=0;
        rect.y=0;
        rect.width=window->getWidth();
        rect.height=window->getHeight();
        editAttr=XVaCreateNestedList(0,XNArea,&rect,NULL);
        xid=(FXID)XCreateIC((XIM)getApp()->xim,XNInputStyle,XIMPreeditArea|XIMStatusArea,XNClientWindow,window->id(),XNPreeditAttributes,editAttr,NULL);
        XFree(editAttr);
        }

      // Over the spot method
      else if(style&XIMPreeditPosition){
        FXTRACE((1,"Over the Spot\n"));
        spot.x=1;
        spot.y=1;
        editAttr=XVaCreateNestedList(0,XNSpotLocation,&spot,NULL);
        xid=(FXID)XCreateIC((XIM)getApp()->xim,XNInputStyle,XIMPreeditPosition|XIMStatusNothing,XNClientWindow,window->id(),XNPreeditAttributes,editAttr,NULL);
        XFree(editAttr);
        }

      // Root method
      else{
        FXTRACE((1,"Root\n"));
        xid=(FXID)XCreateIC((XIM)getApp()->xim,XNInputStyle,XIMPreeditNothing|XIMStatusNothing,XNClientWindow,window->id(),NULL);
        }

      // Reset context
      if(xid){
        //long filterevents=0;
        //XGetICValues((XIC)xid,XNFilterEvents,&filterevents,NULL);
        //XSelectInput((Display*)getApp()->getDisplay(),window->id(),BASIC_EVENT_MASK|ENABLED_EVENT_MASK|filterevents);
        XmbResetIC((XIC)xid);
        }
#endif
      }
    }
  }


// Unrealize the input context
void FXComposeContext::destroy(){
  if(xid){
    if(getApp()->isInitialized()){
      FXTRACE((100,"%s::destroy %p\n",getClassName(),this));
#ifndef NO_XIM
      XDestroyIC((XIC)xid);
#endif
      }
    }
  }


// Set focus to it
void FXComposeContext::focusIn(){
#ifndef NO_XIM
  if(xid){
    XSetICFocus((XIC)xid);
    }
#endif
  }


// Kill focus to it
void FXComposeContext::focusOut(){
#ifndef NO_XIM
  if(xid){
    XUnsetICFocus((XIC)xid);
    }
#endif
  }


// Set the spot
void FXComposeContext::setSpot(FXint x,FXint y){
#ifndef NO_XIM
  if(xid){
    XVaNestedList editAttr;
    XPoint spot;
    spot.x=x;
    spot.y=y;
    editAttr=XVaCreateNestedList(0,XNSpotLocation,&spot,NULL);
    XSetICValues((XIC)xid,XNPreeditAttributes,editAttr,NULL);
    XFree(editAttr);
    }
#endif
  }


// Set the area
void FXComposeContext::setArea(FXint x,FXint y,FXint w,FXint h){
#ifndef NO_XIM
  if(xid){
    XVaNestedList editAttr;
    XRectangle rect;
    rect.x=x;
    rect.y=y;
    rect.width=w;
    rect.height=h;
    editAttr=XVaCreateNestedList(0,XNArea,&rect,NULL);
    XSetICValues((XIC)xid,XNPreeditAttributes,editAttr,NULL);
    XFree(editAttr);
    }
#endif
  }


// Translate key event
FXString FXComposeContext::translateEvent(FXRawEvent& event){
  FXString result;
#ifndef NO_XIM
  if(xid){
    char buffer[40]; KeySym sym; Status s; int n;
    n=XmbLookupString((XIC)xid,&event.xkey,buffer,sizeof(buffer),&sym,&s);
    if(s!=XLookupChars && s!=XLookupBoth) n=0;
    // FIXME decode buffer based on XLocaleOfIM(XIMOfIC((XIC)xid))
    buffer[n]=0;
    FXTRACE((100,"XLocaleOfIM=%s\n",XLocaleOfIM(XIMOfIC((XIC)xid))));
    result.assign(buffer,n);
    }
#endif
  return result;
  }


int FXComposeContext::editStartCallback(void*,FXComposeContext* cc,void*){
  FXTRACE((1,"editStartCallback\n"));
  return -1;			// No length limit
  }


void FXComposeContext::editDoneCallback(void*,FXComposeContext* cc,void*){
  FXTRACE((1,"editDoneCallback\n"));
  }


void FXComposeContext::editDrawCallback(void*,FXComposeContext* cc,void* ptr){
#ifndef NO_XIM
  XIMPreeditDrawCallbackStruct *drawstruct=(XIMPreeditDrawCallbackStruct*)ptr;
  XIMText *ximtext=drawstruct->text;
  FXTRACE((1,"editDrawCallback caret=%d first=%d len=%d\n",drawstruct->caret,drawstruct->chg_first,drawstruct->chg_length));
#endif
  }


void FXComposeContext::editCaretCallback(void*,FXComposeContext* cc,void* ptr){
#ifndef NO_XIM
  XIMPreeditCaretCallbackStruct *caretstruct=(XIMPreeditCaretCallbackStruct*)ptr;
  FXTRACE((1,"editCaretCallback position=%d direction=%d style=%d\n",caretstruct->position,caretstruct->direction,caretstruct->style));
#endif
  }


void FXComposeContext::statusStartCallback(void*,FXComposeContext* cc,void*){
  FXTRACE((1,"statusStartCallback\n"));
  }


void FXComposeContext::statusDoneCallback(void*,FXComposeContext* cc,void*){
  FXTRACE((1,"statusDoneCallback\n"));
  }


void FXComposeContext::statusDrawCallback(void*,FXComposeContext* cc,void* ptr){
#ifndef NO_XIM
  XIMStatusDrawCallbackStruct* drawstruct=(XIMStatusDrawCallbackStruct*)ptr;
  FXTRACE((1,"statusDrawCallback\n"));
#endif
  }


// Delete input context
FXComposeContext::~FXComposeContext(){
  FXTRACE((1,"FXComposeContext::~FXComposeContext %p\n",this));
  destroy();
  window=(FXWindow*)-1L;
  }

#endif  /////////////////////////////////////////////////////////////////////////


}
