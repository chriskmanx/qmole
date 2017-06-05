///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidget.H>
#include <MSGUI/MSWidgetCursor.H>

unsigned long MSDefaultBackground=0x0;  // runtime default: pixel value for default bg color
unsigned long MSDefaultForeground=0x0;  // runtime default: pixel value for default fg color
Font MSDefaultFontID=0x0;               // runtime default: fontID for default font

const char *MSDefaultBackgroundColorSpec="light slate grey";  
const char *MSDefaultForegroundColorSpec="white";
const char *MSDefaultFont               ="lucidasanstypewriter-bold-14";
const char *MSAltDefaultFont            ="fixed";   
const int MSDefaultBorderWidth          =0;
const int MSDefaultWindowBorderWidth    =0;
const int MSDefaultWidth                =10;
const int MSDefaultHeight               =10;
const int MSDefaultX                    =0;
const int MSDefaultY                    =0;

MSWidget *MSWidget::_focusWindow=0;

class MSDefaultCallback : public MSCallback
{
private:
   MSWidget                 *_widget;
   MSWidgetCallbackFunction  _function;
   void                     *_clientData;
public:
   MSDefaultCallback(MSWidget *widget_,MSWidgetCallbackFunction func_,void *clientData_=0)
   :_widget(widget_),_function(func_),_clientData(clientData_) {}
   virtual void process(void);
};

void MSDefaultCallback::process(void)
{_function(_widget,_clientData);}

class MSDefaultWidgetIterator : public MSWidgetIterator
{
private:
   MSWidgetIteratorFunction  _function;
   void                     *_clientData;
public:
   MSDefaultWidgetIterator(MSWidgetIteratorFunction func_,void *clientData_=0)
   :_function(func_),_clientData(clientData_){}
   virtual MSBoolean applyTo(MSWidget *widget_);
};

MSBoolean MSDefaultWidgetIterator::applyTo(MSWidget *widget_)
{return (_function(widget_,_clientData));}

#ifdef MS_NO_INLINES
#include <MSGUI/MSWidgetInlines.C>
#endif

MSWidget::MSWidget(void) :
MSRect(0,0,MSDefaultWidth,MSDefaultHeight)
{
  _owner=0,_server=MSDisplayServer::defaultDisplayServer();
  init();
}

MSWidget::MSWidget(MSDisplayServer *server_,int x_,int y_,int w_,int h_) : 
MSRect(x_,y_,w_,h_)
{
  _owner=0,_server=server_;
  init();
}

MSWidget::MSWidget(MSWidget *owner_,int x_,int y_,int w_,int h_) : 
MSRect(x_,y_,w_,h_)
{
  _owner=owner_,_server=owner_->_server;
  init();
}

MSWidget::~MSWidget(void)
{
  prepareForDestruction();
  if (_window!=0) XDestroyWindow(display(),_window);
  // setting these to zero helps debugging - i.e. one can easily tell if ~ was run
  _owner=0,_window=0,_server=0,_bg=0,_fg=0,_fontID=0;
}

void MSWidget::safeDestroy(MSWidget *pWidget_)
{
  if (pWidget_!=0) pWidget_->destroy();
}

// allowDelete is MSTrue when the queue is processing -
// thus if we are deleting a parent widget (one that has
// children) from the process method of the deleteQueue,
// then the children will be deleted and not put on the
// queue, which would be deadly
void MSWidget::destroy(void)
{
  extern void applicationAddToWidgetDestructionQueue(MSWidget *);
  extern MSBoolean applicationAllowWidgetDestruction(void);

  if (applicationAllowWidgetDestruction()==MSTrue) delete this;
  else
   {
     applicationAddToWidgetDestructionQueue(this);
     decoupleAllWidgets();
     prepareForDestruction();
   }
}

// this method makes the widget appear to the toolkit as if it has been
// destructed - all that remains to do is destroy the window and
// recover the memory for the widget (delete widget).
// this method is primarily used from MSWidget::destroy to allow
// callback safe destruction of widgets
void MSWidget::prepareForDestruction(void)
{
  freeze();
  MSWidget *pTopWidget=top();
  if (_window!=0) _server->widgetHashTable()->remove(_window); // no more events 
  if (pTopWidget!=0) pTopWidget->removeFromFocusList(this);
  if (pTopWidget!=0&&pTopWidget->inputFocus()==this) (void)pTopWidget->traverseFocus(0);
  if (focusWindow()==this) _focusWindow=0;
  childDestroyNotify();
  activateCallback(MSWidgetCallback::destroy);
  removeAllCallbacks();
}

// guarantee that no more events will be delivered to either
// this widget or any of the widgets in its hierarchy
// this includes both screen (XEvent) and Model events.
void MSWidget::decoupleAllWidgets(void)
{
  MSWidgetCursor aCursor(this,MSBreadthFirst);
  for (aCursor.setToFirst();aCursor.isValid();aCursor.setToNext())
   {
     MSWidget *pWidget=aCursor.widget();
     Window aWindow=pWidget->_window;
     // decouple from possible model
     pWidget->decoupleWidget();
     // disable X event dispatching to the widget
     if (aWindow!=0) _server->widgetHashTable()->remove(aWindow);
   }
}

void MSWidget::init(void)
{
  if (_owner==0)
   {
     if (_server!=0)
      {
	_bg=_server->defaultBackground();
	_fg=_server->defaultForeground();
	_fontID=_server->defaultFont();
      }
     else
      {
	_bg=0;
	_fg=0;
	_fontID=0;
      }
   }     
  else
   {
     _bg=_owner->background();
     _fg=_owner->foreground();
     _fontID=_owner->font();
   }
  _window=0;
  _visible=MSFalse;
  _resizeConstraints=0;
  _sensitive=MSTrue;
  _readOnly=MSFalse;
  _acceptFocus=MSTrue;
  _acceptTab=MSFalse;
  _mapped=MSFalse;
  _dynamic=MSFalse;
  _firstMap=MSFalse;
  _freezeStatus=MSFalse;
  _eventMask=0;
  _eventHandler=0;
  _eventHandlerData=0;
  _eventOverride=0;
}

const MSString &MSWidget::instanceName(void) const
{ return _instanceName; }

void MSWidget::instanceName(const MSString &instanceName_)
{ _instanceName=instanceName_; }

MSString MSWidget::instanceFullname(void) const
{
  MSString fullname(_instanceName);
  if (fullname.length()>0)
   {
     const MSWidget *pWidget=_owner;
     while (pWidget!=0)
      {
	if (pWidget->instanceName().length()>0)
	 {
	   fullname.insert(".");
	   fullname.insert(pWidget->instanceName());
	 }
	pWidget=pWidget->_owner;
      }
   }
  return fullname;
}

// this method allows a help string to be obtained virtually
// which can help greatly when dealing with windowless widgets
// like table columns or graph traces
const MSString& MSWidget::virtualHelpString(int,int)
{ return _helpString; }

// the following 2 methods are used to support an application builder
void MSWidget::eventHandler(long mask_,EventHandler eventHandler_,void *clientData_)
{
  selectInput(mask_);
  _eventHandler=eventHandler_;
  _eventHandlerData=clientData_;
}

void MSWidget::eventOverride(EventOverride eventOverride_)
{ _eventOverride=eventOverride_; }

// this will go away once RTTI is available, only a few widgets
// will actually override this method

const MSSymbol& MSWidget::widgetType(void) const
{
  static const MSSymbol widgetSymbol("MSWidget");
  return widgetSymbol;
}

const MSSymbol& MSWidget::parentWidgetType(void) const
{ return (_owner!=0)?_owner->widgetType():MSSymbol::nullSymbol(); }

void MSWidget::selectInput(long eventMask_)
{
  if (_window!=0&&eventMask_!=eventMask())
   {
     eventMask(eventMask_);
     XSelectInput(display(),_window,eventMask());
   }
}

void MSWidget::selectInput(void)  
{ selectInput((long)NoEventMask); }

void MSWidget::event(const XEvent *pEvent_)
{
 if (!_eventOverride||!(*_eventOverride)(pEvent_->type))
  switch (pEvent_->type)
   {
   case Expose:           expose(pEvent_);             break;
   case NoExpose:         noExpose(pEvent_);           break;
   case GraphicsExpose:   graphicsExpose(pEvent_);     break;
   case VisibilityNotify: visibilityNotify(pEvent_);   break;
   case EnterNotify:      if (_server->eventGrabbed(pEvent_,this)==MSTrue) enterNotify(pEvent_);  break;
   case LeaveNotify:      leaveNotify(pEvent_);        break;
   case ConfigureNotify:  configureNotify(pEvent_);    break;
   case CreateNotify:     createNotify(pEvent_);       break;
   case DestroyNotify:    destroyNotify(pEvent_);      break;
   case ReparentNotify:   reparentNotify(pEvent_);     break;
   case MapNotify:        mapNotify(pEvent_);          break;
   case UnmapNotify:      unmapNotify(pEvent_);        break;
   case PropertyNotify:   propertyNotify(pEvent_);     break;
   case SelectionNotify:  selectionNotify(pEvent_);    break;
   case SelectionClear:   selectionClear(pEvent_);     break;
   case SelectionRequest: selectionRequest(pEvent_);   break;
   case ClientMessage:    clientMessage(pEvent_);      break;
   case MotionNotify:     if (_server->eventGrabbed(pEvent_,this)==MSTrue) motionNotify(pEvent_);  break;   
   case ButtonRelease:    if (_server->eventGrabbed(pEvent_,this)==MSTrue) buttonRelease(pEvent_); break;
   case ButtonPress:      if (_server->eventGrabbed(pEvent_,this)==MSTrue) buttonPress(pEvent_);
                          else _server->bell();                                                    break;
   case KeyRelease:       if (_server->eventGrabbed(pEvent_,this)==MSTrue) keyRelease(pEvent_);    break;  
   case KeyPress:         if (_server->eventGrabbed(pEvent_,this)==MSTrue) keyPressEvent(pEvent_);      
                          else _server->bell();                                                    break;
   case FocusIn:          focusInEventNotify(pEvent_);  break;
   case FocusOut:         focusOutEventNotify(pEvent_); break;
   default:                                                                                         break;
   }
  // this event handler is intended to support the mstk builder
  // i.e. it is easier to get events this way rather than
  // subclass every widget and override the event methods
  if (_eventHandler!=0)	(*_eventHandler)(this,pEvent_,_eventHandlerData);
}

MSWidget *MSWidget::widget(Window window_)
{
  MSWidget *pWidget=(MSWidget *)_server->widgetHashTable()->lookup(window_);   
  return ((unsigned long)pWidget==_server->widgetHashTable()->notFound())?0:pWidget;
}

const MSWidget *MSWidget::widget(Window window_) const
{
  MSWidget *pWidget=(MSWidget *)_server->widgetHashTable()->lookup(window_);   
  return ((unsigned long)pWidget==_server->widgetHashTable()->notFound())?0:pWidget;
}

int MSWidget::offsetX(void) const
{ return 0; }
int MSWidget::offsetY(void) const                  
{ return 0; }

int MSWidget::rootX(void) const
{
  int xx,yy;
  rootXY(xx,yy);
  return xx;
}

int MSWidget::rootY(void) const
{
  int xx,yy;
  rootXY(xx,yy);
  return yy;
}

void MSWidget::rootXY(int& x_,int& y_) const
{
  MSWidget *pWidget=(MSWidget *)this;
  int xx=0,yy=0;
  for (;pWidget!=0;pWidget=pWidget->_owner)
   {
     if (pWidget->_owner==0)
      {
        xx+=pWidget->offsetX();
	yy+=pWidget->offsetY();
      }
     xx+=pWidget->x_origin();
     yy+=pWidget->y_origin();
   }
  x_=xx;
  y_=yy;
}

void MSWidget::pointerXY(int& x_,int& y_) const
{
  unsigned int keys;
  int ix=0,iy=0;
  int rx=0,ry=0;
  Window root,child;
  if (_window!=0) XQueryPointer(display(),_window,&root,&child,&rx,&ry,&ix,&iy,&keys);
  else XQueryPointer(display(),top()->_window,&root,&child,&rx,&ry,&ix,&iy,&keys);
  x_=rx;
  y_=ry;
}

MSWidget *MSWidget::top(void)
{
  MSWidget *pTopWidget=(MSWidget *)this;
  while (pTopWidget->_owner!=0) pTopWidget=pTopWidget->_owner;
  return pTopWidget;
}

const MSWidget *MSWidget::top(void) const
{
  MSWidget *pTopWidget=(MSWidget *)this;
  while (pTopWidget->_owner!=0) pTopWidget=pTopWidget->_owner;
  return pTopWidget;
}

void MSWidget::warpTo(int x_,int y_)
{ if (_window!=0) XWarpPointer(display(),_window,_window,0,0,0,0,x_,y_); }

void MSWidget::moveTo(int x_,int y_)
{
  if (x_origin()!=x_||y_origin()!=y_)
   {
     MSRect::x(x_);
     MSRect::y(y_);
     if (_window!=0) XMoveWindow(display(),_window,x_,y_);
     childMoveNotify();
   }
}

void MSWidget::move(int dx_,int dy_) 
{ moveTo(x_origin()+dx_,y_origin()+dy_); }
void MSWidget::width(int w_)         
{ if (width()!=w_) resize(w_,height()); }
void MSWidget::height(int h_)        
{ if (height()!=h_) resize(width(),h_); }

void MSWidget::resize(int w_,int h_)
{
  if (width()!=w_||height()!=h_)
   {     
     if (w_<=0) w_=1;
     if (h_<=0) h_=1;
     if (w_>=USHRT_MAX) w_=width();
     if (h_>=USHRT_MAX) h_=height();     
     if (w_>0) MSRect::width(w_);
     if (h_>0) MSRect::height(h_);

     if (_window!=0) XResizeWindow(display(),_window,width(),height());
     configure();
     childConfigureNotify();
   }
}

void MSWidget::naturalSize(void) 
{ computeSize(); }     

void MSWidget::sensitive(MSBoolean sensitive_) 
{ 
  if (sensitive()!=sensitive_)
   {
     _sensitive=sensitive_; 
     updateSensitivity();
   }
}

MSBoolean MSWidget::isProtected(void) const
{ return (readOnly()==MSTrue || sensitive()==MSFalse)?MSTrue:MSFalse;}

void MSWidget::readOnly(MSBoolean readOnly_) 
{ 
  if(_readOnly != readOnly_)
    {
      _readOnly = readOnly_;
      updateReadOnly();
    }
}
void MSWidget::freeze(void)                    
{ freezeStatus(MSTrue); }
void MSWidget::unfreeze(void)                  
{ freezeStatus(MSFalse); }

void MSWidget::backingStore(int backingStore_)
{
  if (_window!=0)
   {
     XSetWindowAttributes attrs;
     attrs.backing_store=backingStore_;
     XChangeWindowAttributes(display(),_window,CWBackingStore,&attrs);
   }
}

void MSWidget::saveUnder(MSBoolean saveUnder_)
{
  if (_window!=0)
   {
     XSetWindowAttributes attrs;
     attrs.save_under=saveUnder_;
     XChangeWindowAttributes(display(),_window,CWSaveUnder,&attrs);
   }
}

void MSWidget::overrideRedirect(MSBoolean redirect_)
{
  if (_window!=0)
   {
     XSetWindowAttributes attrs;
     attrs.override_redirect=(int)redirect_;
     XChangeWindowAttributes(display(),_window,CWOverrideRedirect,&attrs);
   }
}

void MSWidget::resizeConstraints(const char *constraints_)
{ 
  At a(0,0,1,1,resizeConstraints());
  a.constraints(constraints_);
  resizeConstraints(a.constraints());
}
void MSWidget::resizeConstraints(unsigned long constraints_)
{ 
  if (constraints_!=resizeConstraints()) 
   { 
     _resizeConstraints=constraints_; 
     childResizeConstraintsNotify();
   }
}

At MSWidget::childPosition(MSWidget *pWidget_) 
{ return At(0,0,1,1,pWidget_->resizeConstraints()); }

void MSWidget::at(const At& at_)
{ if (_owner!=0) _owner->childPosition(this,at_); }
At MSWidget::at(void) 
{ 
  if (_owner!=0) return _owner->childPosition(this);
  else return At(0,0,1,1,resizeConstraints()); 
}

void MSWidget::firstMap(MSBoolean firstMap_) 
{ 
  if (firstMap()!=firstMap_) 
   {
     _firstMap=firstMap_; 
     if (firstMap()==MSTrue) firstMapNotify(); 
   } 
}

void MSWidget::map(void)
{
  if (mapped()==MSFalse)
   {
     _mapped=MSTrue;
     if (firstMap()==MSFalse) firstMap(MSTrue);
     childMapNotify();	
     if (_window!=0) XMapWindow(display(),_window);
     
     MSWidget *p=_owner;
     MSBoolean unobscured=MSTrue;
     for (;p!=0;p=p->_owner)
      {
	if (p->mapped()==MSFalse) 
	 {
	   unobscured=MSFalse;
	   break;
	 }
      } 
     if (unobscured==MSTrue) visibilityUnobscured();
   }
}

void MSWidget::unmap(void)
{
  _mapped=MSFalse;
  if (_window!=0) XUnmapWindow(display(),_window);
  childUnmapNotify();	
  visibilityObscured();
}

void MSWidget::show(void) 
{ map(); }
void MSWidget::hide(void) 
{ unmap(); }
void MSWidget::raise(void)
{ if (_window!=0) XRaiseWindow(display(),_window); }
void MSWidget::lower(void)
{ if (_window!=0) XLowerWindow(display(),_window); }
void MSWidget::clear(void)
{ if (_window!=0) XClearWindow(display(),_window); }

void MSWidget::background(const char *pString_)
{ background(_server->pixel(pString_)); }
void MSWidget::foreground(const char *pString_)
{ foreground(_server->pixel(pString_)); }
void MSWidget::font(const char *pString_)
{ font(_server->fontID(pString_)); }

void MSWidget::background(unsigned long pixel_)
{
  if (pixel_!=background())
   {
     unsigned long old=background();
     _bg=pixel_;
     if (_window!=0) XSetWindowBackground(display(),_window,pixel_); 
     updateBackground(old);
   }
}

void MSWidget::foreground(unsigned long pixel_)
{ 
  if (pixel_!=foreground()) 
   {
     unsigned long old=foreground();
     _fg=pixel_;
     updateForeground(old);
   }
}

void MSWidget::font(Font font_)
{ 
  if (font_!=font()) 
   {
     Font old=font();
     _fontID=font_;
     updateFont(old);
   }
}

void MSWidget::updateFont(Font) {}
void MSWidget::updateForeground(unsigned long) {}
void MSWidget::updateBackground(unsigned long) {}
void MSWidget::updateSensitivity(void) {}
void MSWidget::updateReadOnly(void) {}

// if a window is reparented to another screen - i.e. different server, we will
// have problems, widgets are storing fg,bg,fonts,and gcs which will not work
// we need a generic reparent notification
void MSWidget::reparent(MSWidget *owner_)
{
  if (_owner!=owner_)
   {
     MSWidget *oldOwner=_owner;
     if (_window!=0) XReparentWindow(display(),_window,
                                      owner_->_window,x_origin(),y_origin());
     _owner=owner_;
     _server=oldOwner->_server;
     if (oldOwner!=0) oldOwner->childRemove(this);
     if (_owner!=0)
      {
        _owner->childInsert(this);
        // If new owner's visibility is different than mine,
        // then I need to change my visibility accordingly.
        if (_owner->visible()!=visible())
         {
           if (_owner->visible()==MSTrue) visibilityUnobscured();
           else visibilityObscured();
         }
      }
     if (oldOwner->_owner==0&&_owner->_owner==0)
      {
        MSWidget *pFocusWidget=oldOwner->inputFocus();
	if (pFocusWidget!=0) _owner->traverseFocus(pFocusWidget);
      }
   }
}

Window MSWidget::parent(void) const
{ return (_owner==0)?_server->root():_owner->_window; }

// Input focus routines
// Only TopLevel Windows actually get Focus 
// focusWindow returns the top level window
// which currently has focus as managed by
// the window manager

// this is the window where keyboard events get delivered to -
// can either be a primitive or manager
MSWidget *MSWidget::inputFocus(void)
{
  return (MSWidget::focusWindow()!=0&&MSWidget::focusWindow()!=this)?
          MSWidget::focusWindow()->inputFocus():0;
}

const MSWidget *MSWidget::inputFocus(void) const
{
  return (MSWidget::focusWindow()!=0&&MSWidget::focusWindow()!=this)?
          MSWidget::focusWindow()->inputFocus():0;
}

MSBoolean MSWidget::setFocus(MSWidget *pWidget_)
{
  if (pWidget_->_owner!=0&&pWidget_->sensitive()==MSTrue&&pWidget_->acceptFocus()==MSTrue)
   {
     MSWidget *pTopWidget=pWidget_->top();
     MSWidget::_focusWindow=pTopWidget;
     return pTopWidget->traverseFocus(pWidget_);
   }
  return MSFalse;
}

MSBoolean MSWidget::releaseFocus(void)
{
  MSWidget *temp=MSWidget::focusWindow();
  if (temp!=0)
   {
     if (temp->loseFocus()==MSTrue)
      {
	MSWidget::_focusWindow=0;
	return MSTrue;
      }
     else return MSFalse;
   }
  else return MSTrue;
}

MSBoolean MSWidget::traverseFocus(MSWidget *pWidget_)
{
  if (pWidget_==inputFocus()) return MSTrue;
  else if (pWidget_!=0)
   {
     if (pWidget_->_owner==0)
      {
	pWidget_->warpTo();
	return MSTrue;
      }
     else if (pWidget_->sensitive()==MSTrue&&pWidget_->acceptFocus()==MSTrue)
      {
	return setFocus(pWidget_);
      }
   }
  else if (pWidget_==0) return releaseFocus();
  return MSFalse;
}

// if this widget is a topLevel widget (i.e. _owner==0
// then we will warp the cursor to the toplevel, which
// let the window manager give it focus. otherwise, we
// do a normal traversal - the topLevel widget that contains
// this widget will actually do the traversal.
MSBoolean MSWidget::obtainFocus(void) 
{
  if (_owner==0)
   {
     warpTo();
     return MSTrue;
   }
  return traverseFocus(this);
}

MSBoolean MSWidget::traverseToNext(void)
{
  MSWidget *pTopWidget=top();
  return (pTopWidget!=0)?pTopWidget->traverseToNext():MSFalse;
}

MSBoolean MSWidget::traverseToPrevious(void)
{
  MSWidget *pTopWidget=top();
  return (pTopWidget!=0)?pTopWidget->traverseToPrevious():MSFalse;
}

void MSWidget::addToFocusList(void)
{
  MSWidget *pTopWidget=top();
  if (pTopWidget!=0) pTopWidget->addToFocusList(this);
}
void MSWidget::removeFromFocusList(void)
{
  MSWidget *pTopWidget=top();
  if (pTopWidget!=0) pTopWidget->removeFromFocusList(this);
}

MSBoolean MSWidget::isTraversable(void) const
{
  if (mapped()==MSTrue&&sensitive()==MSTrue&&acceptFocus()==MSTrue)
   {
     // do not trust visible, because of server sync problems
     MSWidget *pWidget=(MSWidget *)_owner;
     for (;pWidget!=0;pWidget=pWidget->_owner)
      {
	if (pWidget->mapped()==MSFalse) return MSFalse;
      } 
     return MSTrue;
   }
  return MSFalse;
}

MSBoolean MSWidget::isTraversable(MSWidget *pWidget_) const
{
  if (pWidget_!=0) return pWidget_->isTraversable();
  return MSFalse;
}

void MSWidget::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     XEvent aEvent;
     while (XCheckWindowEvent(display(),_window,ExposureMask,&aEvent)==True);
     redraw();
   }
}

// compress motion Notify events
XEvent *MSWidget::compressMotion(const XEvent *pEvent_)
{
  XEvent nextEvent;
  XEvent *pEvent=(XEvent *)pEvent_;

  while (XPending(pEvent_->xmotion.display)) 
   {
     XPeekEvent(pEvent_->xmotion.display,&nextEvent);
     if (nextEvent.type==MotionNotify&&pEvent_->xmotion.window==nextEvent.xmotion.window)
      {
	// replace the current event with the next one 
	XNextEvent(pEvent_->xmotion.display,pEvent);
      } 
     else break;
   }
  return pEvent;
}

void MSWidget::busyOn(void) 
{ 
// use MSApplicationBusy
}

void MSWidget::busyOff(void) 
{
// use MSApplicationBusy  
}

void MSWidget::disownSelection(Atom selection_)
{
  if (selection_==XA_PRIMARY)
   {
     if (server()->primarySelectionOwner()==this)
      {
        XSetSelectionOwner(display(),selection_,None,CurrentTime);
        server()->primarySelectionOwner(0);
      }
   }
  else XSetSelectionOwner(display(),selection_,None,CurrentTime);
}

MSStatus MSWidget::ownSelection(Atom selection_)
{
  if (selection_==XA_PRIMARY)
   {
     if (server()->primarySelectionOwner()!=this)
      {
        //We need to actively notify the current primary owner if it happens to be a widget
        //within the same application.  This is because the X Server does not send a
        //selectionClear if the current primary owner and the window that is trying
        //to acquire primary ownership are created by the same app.
        if (server()->primarySelectionOwner()!=0)
         {
           if ((unsigned long)(server()->widgetHashTable()->lookup(server()->primarySelectionOwner()->window()))!=
               server()->widgetHashTable()->notFound())
            {
              selectionClearNotify(server()->primarySelectionOwner(),0);
            }
         }
        server()->primarySelectionOwner(0);
        XSetSelectionOwner(display(),selection_,_window,CurrentTime);
        Window win=XGetSelectionOwner(display(),selection_);
        if (window()==win)
	  {
           server()->primarySelectionOwner(this);
#ifdef MS_WINDOWS
	   selectionRequest(0);
#endif
           return MSSuccess;
	  }
        else
	  {
#ifdef MS_WINDOWS
	    selectionRequest(0);
	    return MSSuccess;
#else
	    return MSFailure;
#endif
	  }
      }
     else
       {
#ifdef MS_WINDOWS
	 selectionRequest(0);
#endif
	 return MSSuccess;
       }
   }
  else
   {
     XSetSelectionOwner(display(),selection_,_window,CurrentTime);
     Window win=XGetSelectionOwner(display(),selection_);
     if (window()==win) return MSSuccess;
     else return MSFailure;
   }
}

MSStatus MSWidget::copyPrimary(const char *data_,int itemCount_)
{
  if (ownSelection(XA_PRIMARY)==MSSuccess)
   {
     MSString aString(data_,itemCount_);
     _server->copyBuffer(aString);
     return MSSuccess;
   }
  return MSFailure;  
}

void MSWidget::selectionRequest(const XEvent *pEvent_)
{
#ifndef MS_WINDOWS
  if (pEvent_->xselectionrequest.selection==XA_PRIMARY&&
      pEvent_->xselectionrequest.owner==_window)
   {
     Atom target=convertTarget(pEvent_->xselectionrequest.target);
     if (target==XA_STRING)
      {
	MSString buffer;
	int len=0;
	const char *cp = getPrimarySelection(buffer, len);
	
	if(cp !=0)
	  {
	    Atom property;
	    if (pEvent_->xselectionrequest.property==None) property=XA_PRIMARY;
	    else property=pEvent_->xselectionrequest.property;
	    
	    XChangeProperty(display(),pEvent_->xselectionrequest.requestor,
			    property,target,propertyFormat(target),
			    PropModeAppend,(unsigned const char *)cp,len);

	    XSelectionEvent selectEvent;
	    selectEvent.type=SelectionNotify;
	    selectEvent.property=pEvent_->xselectionrequest.property;
	    selectEvent.display=display();
	    selectEvent.requestor=pEvent_->xselectionrequest.requestor;
	    selectEvent.selection=pEvent_->xselectionrequest.selection;
	    selectEvent.target=pEvent_->xselectionrequest.target;
	    selectEvent.time=pEvent_->xselectionrequest.time;
	    XSendEvent(display(),pEvent_->xselectionrequest.requestor,
		       False,0L,(XEvent *)&selectEvent);
	  }
      }
   }
#else
  MSString buffer;
  int len=0;
  const char *cp = getPrimarySelection(buffer, len);
  XStoreBytes(display(),cp,len);
#endif
}

void MSWidget::selectionNotify(const XEvent *pEvent_)
{
#ifndef MS_WINDOWS
  if (pEvent_->xselection.selection==XA_PRIMARY&&pEvent_->xselection.requestor==_window&&
      pEvent_->xselection.property!=(Atom)None&&pEvent_->xselection.target!=(Atom)None)
   {
     if(propertyData(pEvent_->xselection.property,pEvent_->xselection.target) ==MSSuccess &&
	server()->pasteBuffer().length()>0) 
       {
	 insertPrimarySelection();
       }
   }
#else
  insertPrimarySelection();
#endif
}

const char* MSWidget::getPrimarySelection(MSString& buffer_, int& len_)
{
  buffer_= _server->copyBuffer();
  len_=buffer_.length();
  return buffer_.string();
}

void MSWidget::insertPrimarySelection(void)
{
  activateCallback(MSWidgetCallback::selectionnotify);  
}

void MSWidget::selectionClear(const XEvent *)
{
  _server->copyBuffer("Selection Cleared");
  activateCallback(MSWidgetCallback::selectionclear);
}

void MSWidget::convertSelection(void)
{
#ifndef MS_WINDOWS
  XConvertSelection(display(),XA_PRIMARY,XA_STRING,
		    _server->atom(MSAtomTable::MStk),_window,CurrentTime);  
#else
  int n;
  char *buffer=XFetchBytes(display(),&n);
  if(n!=0)
    {
      _server->pasteBuffer(buffer);
      XFree(buffer);
    }
  selectionNotify(0);
#endif
}

MSStatus MSWidget::propertyData(Atom prop_,Atom target_)
{
  MSStatus r=MSFailure;
  Atom actualTarget=target_;
  int actualFormat;
  int status;
  unsigned long itemCount;
  unsigned long bytesRemaining;
  unsigned char *data=0;

  status=XGetWindowProperty(display(),_window,prop_,
                            0L,MSDisplayServer::propertyFullLength(),True,
                            convertTarget(actualTarget),
                            &actualTarget,&actualFormat,&itemCount,
                            &bytesRemaining,&data);
  if (status==Success)
   {
     propertyToString(actualTarget,(char *)data,(int)itemCount);
     r=MSSuccess;
   }  
  if (data!=0) XFree((char *)data);
  return r;
}

Atom MSWidget::convertTarget(Atom target_)
{
  Atom r=XA_STRING;
  if (target_==XA_INTEGER||target_==XA_WINDOW||target_==XA_ATOM) r=target_;
  else if (target_==_server->atom(MSAtomTable::Targets)) r=XA_ATOM;
  else if (target_==_server->atom(MSAtomTable::ClientWindow)) r=XA_WINDOW;
  else if (target_==_server->atom(MSAtomTable::TimeStamp)) r=XA_INTEGER;
  else if (target_==_server->atom(MSAtomTable::Length)) r=XA_INTEGER;
  else if (target_==_server->atom(MSAtomTable::CharacterPosition)) 
    r=_server->atom(MSAtomTable::Span);
  return r;
}

int MSWidget::propertyFormat(Atom target_) 
{ return (target_==XA_STRING)?8:32; }

void MSWidget::propertyToString(Atom target_,char *data_,int itemCount_)
{
  if (target_==XA_ATOM) atomsFromData(data_,itemCount_);
  else if (target_==XA_STRING) 
   {
     MSString aString(data_,itemCount_);
     _server->pasteBuffer(aString);
   }
}

void MSWidget::atomsFromData(char *,int) {}
void MSWidget::print(const char *)                
{ redraw(); }
void MSWidget::visibilityObscured(void)   
{ visible(MSFalse); }
void MSWidget::visibilityUnobscured(void) 
{ visible(MSTrue); }

void MSWidget::takeFocus(void) 
{ focusIn(); }

MSBoolean MSWidget::loseFocus(void) 
{ return focusOut(),MSTrue; }

void MSWidget::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *)
{
  MSKeyPress keyPress( keysym_,state_);
  keyTranslate(keyPress); 
}
 
void MSWidget::keyRelease(const XEvent *pEvent_,KeySym,unsigned int,const char *)
{ keyRelease(pEvent_); }

void MSWidget::childMapNotify(void)
{ if (_owner!=0) _owner->childMap(this); }
void MSWidget::childUnmapNotify(void)
{ if (_owner!=0) _owner->childUnmap(this); }
void MSWidget::childConfigureNotify(void)
{ if (_owner!=0) _owner->childConfigure(this); }
void MSWidget::childMoveNotify(void)
{ if (_owner!=0) _owner->childMove(this); }
void MSWidget::childResizeConstraintsNotify(void)
{ if (_owner!=0) _owner->childResizeConstraints(this); }
void MSWidget::childDestroyNotify(void)
{ if (_owner!=0) _owner->childDestroy(this); }
void MSWidget::childCreateNotify(void)
{ if (_owner!=0) _owner->childCreate(this); }
void MSWidget::childInsertNotify(void)
{ if (_owner!=0) _owner->childInsert(this); }
void MSWidget::childRemoveNotify(void)
{ if (_owner!=0) _owner->childRemove(this); }

void MSWidget::focusInNotify(MSWidget *pWidget_)
{ if (pWidget_!=0) pWidget_->focusIn(); }
void MSWidget::focusOutNotify(MSWidget *pWidget_)
{ if (pWidget_!=0) pWidget_->focusOut(); }
void MSWidget::takeFocusNotify(MSWidget *pWidget_)
{ if (pWidget_!=0) pWidget_->takeFocus(); }
MSBoolean MSWidget::loseFocusNotify(MSWidget *pWidget_)
{ return (pWidget_!=0)?pWidget_->loseFocus():MSFalse; }
void MSWidget::selectionClearNotify(MSWidget *pWidget_,const XEvent *event_)
{ if (pWidget_!=0) pWidget_->selectionClear(event_);}


void MSWidget::visibilityObscuredNotify(MSWidget *pWidget_)
{ if (pWidget_!=0) pWidget_->visibilityObscured(); }
void MSWidget::visibilityUnobscuredNotify(MSWidget *pWidget_)
{ if (pWidget_!=0) pWidget_->visibilityUnobscured(); }

void MSWidget::buttonMotionNotify(MSWidget *pWidget_,const XEvent *pEvent_)
{ if (pWidget_!=0) pWidget_->motionNotify(pEvent_); }

void MSWidget::buttonPressNotify(MSWidget *pWidget_,const XEvent *pEvent_)
{
  if (pEvent_->xbutton.button==Button1)      pWidget_->button1Press(pEvent_);
  else if (pEvent_->xbutton.button==Button2) pWidget_->button2Press(pEvent_);
  else if (pEvent_->xbutton.button==Button3) pWidget_->button3Press(pEvent_);
}

void MSWidget::buttonReleaseNotify(MSWidget *pWidget_,const XEvent *pEvent_)
{
  if (pEvent_->xbutton.button==Button1)      pWidget_->button1Release(pEvent_);
  else if (pEvent_->xbutton.button==Button2) pWidget_->button2Release(pEvent_);
  else if (pEvent_->xbutton.button==Button3) pWidget_->button3Release(pEvent_);
}

void MSWidget::keyPressNotify(MSWidget *pWidget_,const XEvent *pEvent_,
                               KeySym keysym_,unsigned int state_,const char *pString_)
{ if (pWidget_!=0) pWidget_->keyPress(pEvent_,keysym_,state_,pString_); }

void MSWidget::keyReleaseNotify(MSWidget *pWidget_,const XEvent *pEvent_,
                                 KeySym keysym_,unsigned int state_,const char *pString_)
{ if (pWidget_!=0) pWidget_->keyRelease(pEvent_,keysym_,state_,pString_); }

void MSWidget::keyTranslateNotify(MSWidget *pWidget_,const XEvent *pEvent_,
                                  KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_,state_);
  if (pWidget_!=0)
   {
     if ( pWidget_->keyTranslate(keyPress) == MSTrue); 
     else keyPressNotify(pWidget_,pEvent_,keysym_,state_,pString_);  
   }
}

void MSWidget::keyPressEvent(const XEvent *pEvent_) 
{  
  char    buf[16];
  KeySym  keysym;
  int     len=XLookupString((XKeyEvent *)pEvent_,buf,8,&keysym,NULL);
  buf[len]='\0';

  #ifdef MS_KEYPAD_BUG
  server()->correctKeypadKeys(pEvent_,keysym,pEvent_->xkey.state,buf);
  #endif
  
  keyPress(pEvent_,keysym,pEvent_->xkey.state,buf);
}

MSBoolean MSWidget::keyTranslate(const MSKeyPress& keyPress_) 
{  return  keyTranslationTable()->translate(keyPress_,this); }

MSCallback *MSWidget::callback(const MSSymbol& name_)
{ return MSCallbackBehavior::callback(name_); }

void MSWidget::callback(const MSSymbol& name_,MSCallback *callback_)
{ MSCallbackBehavior::callback(name_,callback_); }

void MSWidget::callback(const MSSymbol& name_,MSWidgetCallbackFunction function_,void *clientData_)
{ MSCallbackBehavior::callback(name_,new MSDefaultCallback(this,function_,clientData_)); }

MSBoolean MSWidget::activateCallback(const MSSymbol& name_)
{ return MSCallbackBehavior::activateCallback(name_);}

MSBoolean MSWidget::activateCallback(MSWidget *pWidget_,const MSSymbol& name_)
{ return (pWidget_!=0)?pWidget_->activateCallback(name_):MSFalse; }

//#############################################################################################
// set and get methods to support the application builder

void MSWidget::set(MSAttrValueList& avList_)
{
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="foreground")
      foreground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="background")
      background(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="font")
      font(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="acceptFocus")
      acceptFocus(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="dynamic")
      dynamic(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="sensitive")
      sensitive(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="readOnly")
      readOnly(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="at")
      at(At(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="resizeConstraints")
      resizeConstraints(avList_[i].value()),index<<i;
   } 
  avList_.remove(index);
}

MSAttrValueList& MSWidget::get(MSAttrValueList& avList_)
{
  MSStringVector aStringVector("MSFalse\nMSTrue");
  
  avList_<<MSAttrValue("foreground",_server->colorName(foreground()),MSAttrValue::Color);
  avList_<<MSAttrValue("background",_server->colorName(background()),MSAttrValue::Color);  
  avList_<<MSAttrValue("font",_server->fontName(font()),MSAttrValue::Font); 
  avList_<<MSAttrValue("acceptFocus",aStringVector(acceptFocus()),aStringVector);
  avList_<<MSAttrValue("sensitive",aStringVector(sensitive()),aStringVector);
  avList_<<MSAttrValue("readOnly",aStringVector(readOnly()),aStringVector);
  avList_<<MSAttrValue("dynamic",aStringVector(dynamic()),aStringVector);
  At aAt=at();
  avList_<<MSAttrValue("resizeConstraints",aAt.parsedConstraints(),MSAttrValue::String);  
  avList_<<MSAttrValue("at",aAt.asString(),MSAttrValue::String);
  avList_<<MSAttrValue("destroy","",MSAttrValue::Callback);
  avList_<<MSAttrValue("takefocus","",MSAttrValue::Callback);  
  return avList_;
}

MSWidgetVector MSWidget::children(void)
{ return MSWidgetVector(); }

MSBoolean MSWidget::allWidgetsDo(MSWidgetIteratorFunction func_,
				 void *clientData_,MSWidgetIterationType type_)
{
  MSDefaultWidgetIterator defaultIterator(func_,clientData_);
  if (type_==MSBreadthFirst) 
   {
     MSWidgetVector vector(this);
     return (breadthFirstIteration(vector,defaultIterator));
   }
  else return (depthFirstIteration(defaultIterator));
}

MSBoolean MSWidget::allWidgetsDo(MSWidgetIterator &iterator_,MSWidgetIterationType type_)
{
  if (type_==MSBreadthFirst)
   {
     MSWidgetVector vector(this);
     return (breadthFirstIteration(vector,iterator_));
   }
  else return (depthFirstIteration(iterator_));
}

MSBoolean MSWidget::depthFirstIteration(MSWidgetIterator &iterator_)
{      
  // Depth First Iteration
  MSWidgetVector vector=children();
  unsigned len=vector.length();
  for (unsigned i=0;i<len;i++)
   {
     if (vector(i)->depthFirstIteration(iterator_)==MSFalse) return MSFalse;
   }
  return (iterator_.applyTo(this));
}

MSBoolean MSWidget::breadthFirstIteration(MSWidgetVector &aWidgetVector_,
					  MSWidgetIterator &iterator_)
{      
  unsigned i,len=aWidgetVector_.length();
  for (i=0;i<len;i++) if (iterator_.applyTo(aWidgetVector_(i))==MSFalse) return MSFalse;
  MSWidgetVector breadthVector;
  for (i=0;i<len;i++)
   {
     MSWidgetVector vector=aWidgetVector_(i)->children();
     breadthVector.append(vector);
   }
  if (breadthVector.length()) return (breadthFirstIteration(breadthVector,iterator_));
  else return MSTrue;
}

void MSWidget::acceptFocus(MSBoolean b_)     
{ _acceptFocus=b_; }
void MSWidget::dynamic(MSBoolean b_)                   
{ _dynamic=b_; }
void MSWidget::acceptTab(MSBoolean b_)    
{ _acceptTab=b_; }
void MSWidget::freezeStatus(MSBoolean b_) 
{ _freezeStatus=b_; }
void MSWidget::visible(MSBoolean b_)      
{ _visible=b_; }
void MSWidget::eventMask(long eventMask_)
{ _eventMask=eventMask_; }
void MSWidget::focusWindow(MSWidget *widget_) 
{ _focusWindow=widget_; }

const MSString& MSWidget::helpString(void) const
{ return _helpString; }
  
void MSWidget::helpString(const MSString& aString_)
{ _helpString=aString_; }

void MSWidget::childPosition(MSWidget *pWidget_,const At& at_) 
{
  pWidget_->resizeConstraints(at_.constraints());
}


// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################
  
void MSWidget::redraw(void) {}
void MSWidget::computeSize(void) {}
void MSWidget::decoupleWidget(void) {}  
void MSWidget::addToFocusList(MSWidget *) {}
void MSWidget::removeFromFocusList(MSWidget *) {}
void MSWidget::noExpose(const XEvent *) {}
void MSWidget::graphicsExpose(const XEvent *) {}
void MSWidget::visibilityNotify(const XEvent *) {}
void MSWidget::keyRelease(const XEvent *) {}
void MSWidget::buttonPress(const XEvent *pEvent_)
{ buttonPressNotify(this,pEvent_); }
void MSWidget::buttonRelease(const XEvent *pEvent_)
{ buttonReleaseNotify(this,pEvent_); }
void MSWidget::motionNotify(const XEvent *) {}
void MSWidget::mapNotify(const XEvent *) {}
void MSWidget::unmapNotify(const XEvent *) {}
void MSWidget::configureNotify(const XEvent *) {}
void MSWidget::createNotify(const XEvent *) {}
void MSWidget::destroyNotify(const XEvent *) {}
void MSWidget::enterNotify(const XEvent *) {}
void MSWidget::leaveNotify(const XEvent *) {}
void MSWidget::focusInEventNotify(const XEvent *) {}
void MSWidget::focusOutEventNotify(const XEvent *) {}
void MSWidget::reparentNotify(const XEvent *) {}
void MSWidget::propertyNotify(const XEvent *) {}
void MSWidget::clientMessage(const XEvent *) {}
void MSWidget::errorEvent(const XEvent *) {}

void MSWidget::configure(void) {}
void MSWidget::firstMapNotify(void) {}
void MSWidget::enter(void) {}
void MSWidget::leave(void) {}
void MSWidget::focusIn(void) {}
void MSWidget::focusOut(void) {}

void MSWidget::button1Press(const XEvent *) {}
void MSWidget::button2Press(const XEvent *) {}
void MSWidget::button3Press(const XEvent *) {}
void MSWidget::button1Release(const XEvent *) {}
void MSWidget::button2Release(const XEvent *) {}
void MSWidget::button3Release(const XEvent *) {}
void MSWidget::key(KeySym,unsigned int,const char *) {}

void MSWidget::childMap(MSWidget *) {}
void MSWidget::childUnmap(MSWidget *) {}
void MSWidget::childConfigure(MSWidget *) {}
void MSWidget::childMove(MSWidget *) {}
void MSWidget::childResizeConstraints(MSWidget *) {}
void MSWidget::childDestroy(MSWidget *) {}
void MSWidget::childCreate(MSWidget *) {}
void MSWidget::childInsert(MSWidget *) {}
void MSWidget::childRemove(MSWidget *) {}



