///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#if HAVE_FSTREAM
#include <fstream>
#else
#include <fstream.h>
#endif
#include <MSGUI/MSShell.H>
#include <MSGUI/MSWidgetCursor.H>

#ifdef MS_WINDOWS
#include <MSX11.H>

static const unsigned long MSShellEventMask=(FocusChangeMask|
					     StructureNotifyMask|KeyPressMask|
					     PropertyChangeMask);
#else
static const unsigned long MSShellEventMask=(LeaveWindowMask|EnterWindowMask|
					     StructureNotifyMask|KeyPressMask|
					     PropertyChangeMask);
#endif

MSShell *MSShell::_defaultLeader=0;
MSWidgetVector MSShell::_shellList;

extern MSString applicationArgumentString(void);

MSShell::Follower::Follower(MSShell *pShell_) : 
_pShell(pShell_),
_remap(pShell_->mapped()) 
{}

MSShell::Follower::~Follower(void)
{}

MSShell::GroupList::GroupList(void)
{}
MSShell::GroupList::~GroupList(void)
{}

void MSShell::GroupList::removeAll(void)
{_groupList.removeAll();}

void MSShell::GroupList::removeAt(unsigned index_)
{_groupList.removeAt(index_);}

void MSShell::GroupList::append(Follower *pFollower_)
{_groupList.append((unsigned long)pFollower_);}

unsigned MSShell::GroupList::numberOfFollowers(void) const
{return _groupList.length();}

MSShell::Follower *MSShell::GroupList::operator()(unsigned index_) const 
{return (MSShell::Follower *)_groupList(index_);}

MSShell::Follower *MSShell::GroupList::operator()(unsigned index_)       
{return (MSShell::Follower *)_groupList(index_);}

MSShell::MSShell(const char *windowTitle_) :
MSTopLevel(windowTitle_)
{ init(); }

MSShell::MSShell(MSDisplayServer *server_,const char *windowTitle_) :
MSTopLevel(server_,windowTitle_) 
{ init(); }

MSShell::~MSShell(void)
{
  // do not want excess traffic-will get many PropertyNotify events
  // when the window is destroyed
  selectInput();  
                  
  // remove all the widgets in the focus list before destroying children
  // which will eliminate unneeded removeFromFocusList activity
  _traversalList.removeAll();
  // set the _focusWidget to null before destroying the children
  // which will eliminate unneeded traversal processing
  traverseFocus(0);
  
  if (_child!=0) safeDestroy(_child);
  _child=0;
  if (_shellList.length()>0)
   {
     unsigned index=_shellList.indexOf((unsigned long)this,0);
     if (index<_shellList.length()) _shellList.removeAt(index);
   }

  if (_leader!=0) _leader->removeFollower(this);
  if (_defaultLeader==this) _defaultLeader=0;

  for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
   {
     _groupList(i)->_pShell->leader(_defaultLeader);  
     if (_defaultLeader!=0) _defaultLeader->addFollower(_groupList(i)->_pShell);
     delete _groupList(i);
   }
  _groupList.removeAll();
}

void MSShell::init(void)
{
  if (_shellList.length()==0) 
   {
     if (windowManagerCommand().length()==0)
      {
	windowManagerCommand(applicationArgumentString());
      }
     setWMClientMachine();
     setWMSaveYourself();
     setWMCommand();
     if (_server->isCDERunning()==MSTrue)
      {
        _server->workspaceChangedFunction(&MSShell::updateCurrentWorkspace);
      }
   }

  _inCurrentWorkspace=MSFalse;
  _busyCount=0;
  _focusWidget=0;
  _leader=0;
  _child=0;
  _created=MSFalse;
  _header=MSTrue;
  _footer=MSFalse;
  _resizeable=MSTrue;
  _offsetX=0;
  _offsetY=0;
  _nestedTraversal=MSFalse;
    
  _shellList.append(this);
  if (_defaultLeader==0) defaultLeader(this);

  // We don't want to propagate the following events
  XSetWindowAttributes attributes;
  attributes.do_not_propagate_mask=(ButtonPressMask|ButtonReleaseMask|KeyPressMask
				    |KeyReleaseMask|PointerMotionMask);
  XChangeWindowAttributes(display(),_window,CWDontPropagate,&attributes);
  
  selectInput(MSShellEventMask);
}

int MSShell::offsetX(void) const
{ return _offsetX; }
int MSShell::offsetY(void) const        
{ return _offsetY; }

MSWidgetVector MSShell::children(void)
{
  MSWidgetVector vector;
  if (_child!=0) vector.append(_child);
  return vector;
}

MSWidget *MSShell::inputFocus(void)
{ return _focusWidget; }
const MSWidget *MSShell::inputFocus(void) const
{ return _focusWidget; }

void MSShell::print(const char *file_)
{ if (_child!=0) _child->print(file_); }

int MSShell::childWidth(void) const
{ return (_child!=0)?_child->width():0; }
int MSShell::childHeight(void) const
{ return (_child!=0)?_child->height():0; }

int MSShell::idealShellWidth(void) const
{ return childWidth(); }
int MSShell::idealShellHeight(void) const
{ return childHeight(); }

void MSShell::childCreate(MSWidget *pWidget_)
{
  if (_child==0)
   {
     _child=pWidget_;
     adjustChildPosition();
   }
}

// a child has been destroyed-check child
void MSShell::childDestroy(MSWidget *pWidget_)
{ if (pWidget_==_child) _child=0; }

// remove child-it has been reparented
void MSShell::childRemove(MSWidget *pWidget_) 
{ childDestroy(pWidget_); }
// insert child-it has been reparented to this shell
void MSShell::childInsert(MSWidget *pWidget_)
{
  if (_child==0)
   {
     _child=pWidget_;
     adjustChildPosition();
     adjustSize();
   }       
}

void MSShell::childConfigure(MSWidget *pWidget_)
{ if (pWidget_==_child&&_child!=0) adjustSize(); }

void MSShell::configure(void) 
{ adjustChildSize(); }

void MSShell::adjustChildPosition(void) 
{ if (_child!=0) _child->moveTo(0,0); }

void MSShell::adjustChildSize(void) 
{ if (_child!=0) _child->resize(width(),height()); }
 
void MSShell::adjustSize(void)
{ resize(idealShellWidth(),idealShellHeight()); }

void MSShell::naturalSize(void)
{ if (_child!=0) _child->naturalSize(); }

void MSShell::resize(int w_,int h_)
{
  if (MSRect::width()!=w_||MSRect::height()!=h_)
   {     
     if (w_>0) MSRect::width(w_);
     if (h_>0) MSRect::height(h_);
     setWMSize();
     configure();
   }
}

void MSShell::moveToCenter(void)
{
  int x=(_server->width()-width())/2;
  int y=(_server->height()-height())/2;
  moveTo(x,y);
}

void MSShell::moveTo(int x_,int y_)
{
  if (MSRect::x()!=x_||MSRect::y()!=y_)
   {
     MSRect::x(x_);
     MSRect::y(y_);
     setWMPosition();
   }
}

void MSShell::showChildren(void)
{
  if (_child!=0) 
   {
     adjustChildPosition();
     _child->show();
   }
}

void MSShell::showCentered(void)
{
  showChildren();
  moveToCenter();
  map();
}

void MSShell::show(void)
{
  showChildren();
  map();
}

void MSShell::loadAndShow(const char *fileName_)
{
  showChildren();
  loadStateFrom(fileName_);
  map();
}

void MSShell::loadAndShow(const MSWidgetState &widgetState_)
{
  showChildren();
  loadStateFrom(widgetState_);
  map();
}

void MSShell::loadAndShow(istream &is_)
{
  showChildren();
  loadStateFrom(is_);
  map();
}

void MSShell::map(void)
{
  if (mapped()==MSFalse)
   {
     if (_child!=0) adjustSize();
     else setWMSize();
     if (firstMap()==MSFalse) setWMPosition();
     MSWidget::map();
     mapFollowers();
     XFlush(display());
   }
}

void MSShell::unmap(void)
{
  if (mapped()==MSTrue)
   {
     unmapFollowers();
     iconify();
   }
}

void MSShell::iconized(void)
{ activateCallback(MSWidgetCallback::iconized); }

void MSShell::deiconized(void)
{ activateCallback(MSWidgetCallback::deiconized); }

void MSShell::visibilityObscured(void)
{
  visible(MSFalse);
  visibilityObscuredNotify(_child);
}

void MSShell::visibilityUnobscured(void)
{
  visible(MSTrue);
  visibilityUnobscuredNotify(_child);
}

void MSShell::firstMapNotify(void)
{ setDefaultTraversal(); }

void MSShell::mapNotify(const XEvent *pEvent_)
{ if (_created==MSFalse&&pEvent_->xmap.window==_window) setOffsets(); }

void MSShell::enterNotify(const XEvent *pEvent_)
{ if (pEvent_->xcrossing.mode==NotifyNormal) processFocusIn(); }

void MSShell::leaveNotify(const XEvent *pEvent_)
{ if (pEvent_->xcrossing.mode==NotifyNormal) processFocusOut(); }

void MSShell::focusInEventNotify(const XEvent *)
{ processFocusIn();}

void MSShell::focusOutEventNotify(const XEvent *)
{ processFocusOut();}

void MSShell::processFocusIn(void)
{
  if (_focusWidget!=0&&this!=MSWidget::_focusWindow) 
    {
      if (_focusWidget->sensitive()==MSTrue&&_focusWidget->acceptFocus()==MSTrue)
	{
	  MSWidget *old=MSWidget::_focusWindow;
	  focusOutNotify(old);
	  MSWidget::_focusWindow=this;
	  focusIn();
	}
      else focusWidget(0);
    }
  else if (_focusWidget!=0&&this==MSWidget::_focusWindow) focusIn();
  else if (_focusWidget==0&&MSWidget::_focusWindow==0) 
    {
      MSWidget::_focusWindow=this;        
      setDefaultFocus();
    }
}

void MSShell::processFocusOut(void)
{
  MSWidget::_focusWindow=0;  
  focusOut();
}


void MSShell::configureNotify(const XEvent *pEvent_)
{
  if (pEvent_->xconfigure.window==_window&&pEvent_->xconfigure.event==_window)
   {
     if (pEvent_->xconfigure.send_event==True)
      { 
	if (_created==MSTrue)
	 {
	   if (pEvent_->xconfigure.x+pEvent_->xconfigure.width>=0&&
	       pEvent_->xconfigure.y+pEvent_->xconfigure.height>=0&&
	       pEvent_->xconfigure.x<=_server->width()&&
	       pEvent_->xconfigure.y<=_server->height()) 
	    {
    	      MSRect::x(pEvent_->xconfigure.x-offsetX());
	      MSRect::y(pEvent_->xconfigure.y-offsetY());	
	      if (pEvent_->xconfigure.width!=width()||pEvent_->xconfigure.height!=height())
	       {
		 MSRect::width(pEvent_->xconfigure.width);
		 MSRect::height(pEvent_->xconfigure.height);
		 configure();
	       }
	    }
	 }
      }
     else if (_created==MSTrue)
      {
	if (pEvent_->xconfigure.x+pEvent_->xconfigure.width>=0&&
	    pEvent_->xconfigure.y+pEvent_->xconfigure.height>=0&&
	    pEvent_->xconfigure.x<=_server->width()&&
	    pEvent_->xconfigure.y<=_server->height()) 
	 {
	   // We can't rely on the (x,y) received in the Event Structure
	   // So let's actively pursue it
	   int x,y;
	   Window child;
	   XTranslateCoordinates(display(),_window,XRootWindowOfScreen(screen()),
				 0,0,&x,&y,&child);
	   MSRect::x(x-offsetX());
	   MSRect::y(y-offsetY());	

	   if (pEvent_->xconfigure.width!=width()||pEvent_->xconfigure.height!=height())
	    {
	      MSRect::width(pEvent_->xconfigure.width);
	      MSRect::height(pEvent_->xconfigure.height);
              configure();
	    }
	 }
      }
   }
}

void MSShell::keyPress(const XEvent *pEvent_,KeySym keysym_,
		       unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_, state_);
  if (keyTranslate(keyPress) == MSFalse)
   {
     if (keysym_!=XK_Tab&&state_==Mod1Mask&&isprint(pString_[0]))
      {
        if (processAltKey(pEvent_,keysym_,state_,pString_)==MSTrue) return;
      }
     if (_focusWidget!=0)
      {
        if (keysym_==XK_Tab)
         {
           unsigned int state=(state_&(ShiftMask|ControlMask|Mod1Mask));
           if (state==ControlMask) traverseToNextShell();
           else if (state==(ShiftMask+ControlMask)) traverseToPreviousShell();
           else if (_focusWidget->acceptTab()==MSFalse)
            {
              if (state==ShiftMask) shiftTab();
              else tab();
            }
           else 
            {
              if(state==Mod1Mask) tab();           
              else if (state==(ShiftMask+Mod1Mask)) shiftTab();
              else keyPressNotify(_focusWidget,pEvent_,keysym_,state_,pString_);
            }
         }
        else if ((keysym_>=XK_F1&&keysym_<=XK_F10)||keysym_==XK_F11||keysym_==XK_F12)
         {
           // check for function key callback	
           if (processFunctionKey(pEvent_,keysym_,state_,pString_)==MSFalse) 
            {
              keyPressNotify(_focusWidget,pEvent_,keysym_,state_,pString_);	   
            }
         }
        else keyPressNotify(_focusWidget,pEvent_,keysym_,state_,pString_);
      }
   }
}

void MSShell::keyRelease(const XEvent *pEvent_,KeySym keysym_,
			 unsigned int state_,const char *pString_)
{ keyReleaseNotify(_focusWidget,pEvent_,keysym_,state_,pString_); }

void MSShell::keyRelease(const XEvent *pEvent_) 
{ 
  char    buf[16];
  KeySym  keysym;
  int     len=XLookupString((XKeyEvent *)pEvent_,buf,8,&keysym,NULL);

  #ifdef MS_KEYPAD_BUG
  server()->correctKeypadKeys(pEvent_,keysym,pEvent_->xkey.state,buf);
  #endif
  
  buf[len]='\0';
  keyRelease(pEvent_,keysym,pEvent_->xkey.state,buf);
}

MSBoolean MSShell::processAltKey(const XEvent *,KeySym,unsigned int,const char *)
{ return MSFalse; }

MSBoolean MSShell::processFunctionKey(const XEvent *,KeySym keysym_,unsigned int,const char *)
{
  static MSSymbol syms[12]={MSWidgetCallback::f1,MSWidgetCallback::f2,
		            MSWidgetCallback::f3,MSWidgetCallback::f4,
		            MSWidgetCallback::f5,MSWidgetCallback::f6,
		            MSWidgetCallback::f7,MSWidgetCallback::f8,
		            MSWidgetCallback::f9,MSWidgetCallback::f10,
		            MSWidgetCallback::f11,MSWidgetCallback::f12};  
  int num=(int)(keysym_-XK_F1);  
  if (num>=0&&num<12)
   {
     MSWidget *pWidget=_focusWidget;
     while (pWidget!=0)
      {
	if (activateCallback(pWidget,syms[num])==MSTrue) return MSTrue;
	else pWidget=pWidget->owner();
      }
   }
  return MSFalse;
}

void MSShell::propertyNotify(const XEvent *pEvent_)
{
  Atom wmStateAtom=_server->atom(MSAtomTable::WMState);
  if (pEvent_->xproperty.atom==wmStateAtom) updateWMState();
  else
   {
     Atom workspacePresenceAtom=XInternAtom(display(),"_DT_WORKSPACE_PRESENCE",False);
     if (pEvent_->xproperty.atom==workspacePresenceAtom)
      {
        updateWorkspacePresence();
        updateCurrentWorkspaceState(_server->currentWorkspaceAtom());
      }
   }
}

void MSShell::updateWMState(void)
{
  Atom wmStateAtom=_server->atom(MSAtomTable::WMState);
  Atom actualTarget;
  int actualFormat;
  unsigned long itemCount;
  unsigned long bytesRemaining;
  unsigned char *actData;
  int *data=0;
  int state=WithdrawnState;
  long len=((sizeof(int)+3)/4+(sizeof(Window)+3)/4);
     
  int status=XGetWindowProperty(display(),_window,wmStateAtom,
                                0L,len,False,wmStateAtom,
                                &actualTarget,&actualFormat,&itemCount,
                                &bytesRemaining,&actData);
  
  if (status==Success&&wmStateAtom==actualTarget&&actualFormat==32&&itemCount>0)
   {
     data=(int *)actData;
     state=data[0];
   }  
  if (data!=0) XFree((char *)actData);
  if (state==IconicState)
   {
     visibilityObscured();
     if (MSWidget::mapped()==MSTrue) 
      {
        unmapFollowers();
        // We must set the state after calling unmapFollowers, not before
        _mapped=MSFalse;  
      }
     iconized();
   }
  else if (state==NormalState)
   {
     visibilityUnobscured();
     if (MSWidget::mapped()==MSFalse) 
      {
        _mapped=MSTrue;
        mapFollowers();
      }
     deiconized();
   }
}

// first send event from olvwm is incorrect - it is the x,y offsets
void MSShell::setOffsets(void)
{
  Window win;
  int x,y;
  unsigned int w,h,bw,d;
  
  XGetGeometry(display(),_window,&win,&x,&y,&w,&h,&bw,&d);
  // if the offsets turned out to be zeros, we are probably using mwm
  // or ted. The following is used to find offsets in that situation.
  if(x==0&&y==0)
   {
     _adjustWMPosition=MSTrue;
     XTranslateCoordinates(display(),_window,XRootWindowOfScreen(screen()),
			   0,0,&x,&y,&win);
     offsetX(x-MSRect::x());
     offsetY(y-MSRect::y());
   }
  else
   {
     offsetX(x);
     offsetY(y);
   }
  _created=MSTrue;
}

//###################################################################################
// leader/follower/window group methods

MSWidgetVector MSShell::followerList(void)
{
  MSWidgetVector aWidgetVector;
  for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
   {
     aWidgetVector.append(_groupList(i)->_pShell);
   }
  return aWidgetVector;
}

void MSShell::addFollower(MSShell *pShell_)
{
  if (pShell_!=0&&pShell_!=this)
   {
     for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
      {
	if (_groupList(i)->_pShell==pShell_) return;        
      }
     _groupList.append(new Follower(pShell_));
   }  
}

void MSShell::removeFollower(MSShell *pShell_)
{
  if (pShell_!=0&&pShell_!=this)
   {
     for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
      {
	if (_groupList(i)->_pShell==pShell_)
	 {
	   delete _groupList(i);
	   _groupList.removeAt(i);
	 }
      }
   }  
}

void MSShell::mapFollowers(void)
{
  for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
   {
     if (_groupList(i)->_remap==MSTrue) _groupList(i)->_pShell->map();
   }
}

void MSShell::unmapFollowers(void)
{
  for (unsigned i=0;i<_groupList.numberOfFollowers();i++)
   {
     _groupList(i)->_remap=_groupList(i)->_pShell->mapped();
     _groupList(i)->_pShell->unmap();	   
   }
}

// leader_ is the proposed new leader of this,we first need to check
// leader_ is not already a follower of this.
MSBoolean MSShell::windowGroup(MSShell *leader_)
{
  if (leader_!=this)
   {
     if (_leader!=leader_)
      {
	if (doesLeaderFollowThis(leader_)==MSFalse)
	 {
	   if (_leader!=0) _leader->removeFollower(this);
	   if (leader_!=0) leader_->addFollower(this);
	   leader(leader_);
	   return MSTrue;
	 }
      }
     else return MSTrue;
   }
  return MSFalse;
}

// need to check that leader_ is not already a follower of this,
// otherwise we can get a cycle
MSBoolean MSShell::doesLeaderFollowThis(MSShell *leader_)
{
  MSBoolean r=MSFalse;
  if (leader_!=0)
   {
     for (unsigned i=0;i<_groupList.numberOfFollowers()&&r==MSFalse;i++)
      {
	if (_groupList(i)->_pShell==leader_) r=MSTrue;
	else r=_groupList(i)->_pShell->doesLeaderFollowThis(leader_);
      }
   }
  return r;  
}

void MSShell::defaultLeader(MSShell *defaultLeader_)
{
  MSShell *oldDefault=_defaultLeader;
  _defaultLeader=defaultLeader_;

  // remove all followers from the old default and add them
  // to the new default
  if (oldDefault!=0&&oldDefault!=defaultLeader_)
   {
     GroupList& oldList=oldDefault->_groupList;
     for (unsigned i=0;i<oldList.numberOfFollowers();i++)
      {
        if (_defaultLeader!=0) _defaultLeader->addFollower(oldList(i)->_pShell);
        oldList(i)->_pShell->leader(_defaultLeader);
	delete oldList(i);
      }
     oldList.removeAll();
   }
}

//###################################################################################
// focus and traversal methods

MSBoolean MSShell::loseFocus(void) 
{ return (_focusWidget!=0)?loseFocusNotify(_focusWidget):MSTrue; }
void MSShell::takeFocus(void)    
{ takeFocusNotify(_focusWidget); }
void MSShell::focusOut(void)     
{ focusOutNotify(_focusWidget); }
void MSShell::focusIn(void)      
{ focusInNotify(_focusWidget); }

void MSShell::traverse(void) 
{ tab(); }
void MSShell::tab(void)      
{ traverseFocusToNext(); }
void MSShell::shiftTab(void) 
{ traverseFocusToPrevious(); }

MSBoolean MSShell::traverseFocus(MSWidget *newFocusWidget_)
{
  MSBoolean lf=MSTrue;
  if (newFocusWidget_!=this)
   {
     if (_nestedTraversal==MSFalse)
      {
	nestedTraversal(MSTrue);
	if (_focusWidget!=0&&_focusWidget!=newFocusWidget_) lf=loseFocusNotify(_focusWidget);
	if (lf==MSTrue)
	 {
	   focusWidget(newFocusWidget_);
	   takeFocusNotify(_focusWidget);
#ifdef MS_WINDOWS
	   MSXSetInputFocus(_focusWidget==0?0:_focusWidget->window());
#endif
	   activateCallback(_focusWidget,MSWidgetCallback::takefocus);
	   nestedTraversal(MSFalse);
	   return MSTrue;
	 }
	nestedTraversal(MSFalse);
      }
   }
  return MSFalse;
}

void MSShell::addToFocusList(MSWidget *pWidget_)      
{
  if (_traversalList.indexOf((unsigned long)pWidget_,0)==_traversalList.length())
   {
     _traversalList.append(pWidget_);
   }
}
  
void MSShell::removeFromFocusList(MSWidget *pWidget_) 
{ 
  unsigned index=_traversalList.indexOf((unsigned long)pWidget_,0);
  if (index<_traversalList.length()) _traversalList.removeAt(index);
}

// get the entry after focus
MSWidget *MSShell::getNextFocus(void)
{
  if (_traversalList.length()>0)
   {
     MSWidget *pWidget=0;
     unsigned i,index=_traversalList.indexOf((unsigned long)_focusWidget,0);
     if (index==_traversalList.length()) index=0;
     for (i=index+1;i<_traversalList.length();i++)
      {
	pWidget=_traversalList(i);
        if (isTraversable(pWidget)==MSTrue) return pWidget;
      }
     for (i=0;i<index;i++)
      {
	pWidget=_traversalList(i);
        if (isTraversable(pWidget)==MSTrue) return pWidget;
      }
   }
  return 0;
}

// get the entry before focus
MSWidget *MSShell::getPreviousFocus(void)
{
  if (_traversalList.length()>0)
   {
     MSWidget *pWidget=0;
     unsigned i,index=_traversalList.indexOf((unsigned long)_focusWidget,0);
     if (index==_traversalList.length()) index=0;
     for (i=index-1;i<_traversalList.length();i--)
      {
	pWidget=_traversalList(i);
        if (isTraversable(pWidget)==MSTrue) return pWidget;
      }
     for (i=_traversalList.length()-1;i>index;i--)
      {
	pWidget=_traversalList(i);
        if (isTraversable(pWidget)==MSTrue) return pWidget;	
      }
   }
  return 0;
}

MSBoolean MSShell::traverseFocusToNext(void)
{
  MSWidget *pWidget=getNextFocus();
  return (pWidget!=0)?traverseFocus(pWidget):MSFalse;
}

MSBoolean MSShell::traverseFocusToPrevious(void)  
{
  MSWidget *pWidget=getPreviousFocus();
  return (pWidget!=0)?traverseFocus(pWidget):MSFalse;
}

MSBoolean MSShell::traverseToNext(void)   
{ return traverseFocusToNext(); }
MSBoolean MSShell::traverseToPrevious(void) 
{ return traverseFocusToPrevious(); }  

static int widgetCoordCompare(MSWidget *aWidget_,MSWidget *bWidget_)
{
  int aX,aY;
  int bX,bY;
  aWidget_->rootXY(aX,aY);
  bWidget_->rootXY(bX,bY);  
  if (aY==bY)
   {
     if (aX==bX) return 0;
     else if (aX<bX) return -1;
     else return 1;
   }
  else if (aY<bY) return -1;
  else return 1;
}

static void widgetSort(MSWidgetVector& aWidgetVector_)
{
  unsigned i,j,min,n=aWidgetVector_.length();
  int comp;
  for (i=0;i<n;i++)
   {
     min=i;
     for (j=i+1;j<n;j++) 
      {
	comp=widgetCoordCompare(aWidgetVector_(j),aWidgetVector_(min));
        if (comp<0) min=j;
      }
     aWidgetVector_.exchange(min,i);
   }
}

void MSShell::setDefaultTraversal(void)
{
  MSWidgetVector aWidgetVector(_traversalList);
  widgetSort(aWidgetVector);
  traversalList(aWidgetVector);
}

void MSShell::setDefaultFocus(void)
{
  MSWidget *pWidget=0;
  for (unsigned i=0;i<_traversalList.length();i++)
   {
     pWidget=_traversalList(i);
     if (pWidget!=0&&pWidget->sensitive()==MSTrue&&pWidget->acceptFocus()==MSTrue) break;
     else pWidget=0;
   }
  if (pWidget!=0&&pWidget!=this)
   {
     MSBoolean lf=MSTrue;
     if (_focusWidget!=0) lf=loseFocusNotify(_focusWidget);
     if (lf==MSTrue)
      {
        focusWidget(pWidget);
        takeFocusNotify(_focusWidget);     
      }
   }
}

void MSShell::traverseToNextShell(void)
{
  if (_shellList.length()>0)
   {
     MSWidget *pWidget=0;
     unsigned i,index=_shellList.indexOf((unsigned long)this,0);
     if (index==_shellList.length()) index=0;
     
     for (i=index+1;i<_shellList.length();i++)
      {
	pWidget=_shellList(i);
        if (pWidget!=0&&pWidget!=this&&pWidget->mapped()==MSTrue&&
	    server()->name()==pWidget->server()->name()) 
         { 
           XWarpPointer(display(),_window,pWidget->window(),0,0,0,0,10,10);
           return;
         }
      }
     for (i=0;i<index;i++)
      {
	pWidget=_shellList(i);
        if (pWidget!=0&&pWidget!=this&&pWidget->mapped()==MSTrue&&
	    server()->name()==pWidget->server()->name()) 
         {
           XWarpPointer(display(),_window,pWidget->window(),0,0,0,0,10,10);
           return;
	 }
      }
   }
}

void MSShell::traverseToPreviousShell(void)
{
  if (_shellList.length()>0)
   {
     MSWidget *pWidget=0;
     unsigned i,index=_shellList.indexOf((unsigned long)this,0);
     if (index==_shellList.length()) index=0;
     for (i=index-1;i<_shellList.length();i--)
      {
	pWidget=_shellList(i);
        if (pWidget!=0&&pWidget!=this&&pWidget->mapped()==MSTrue&&
	    server()->name()==pWidget->server()->name()) 
         { 
           XWarpPointer(display(),_window,pWidget->window(),0,0,0,0,10,10);
           return;
         }
      }
     for (i=_shellList.length()-1;i>index;i--)
      {
	pWidget=_shellList(i);
        if (pWidget!=0&&pWidget!=this&&pWidget->mapped()==MSTrue&&
	    server()->name()==pWidget->server()->name()) 
         { 
           XWarpPointer(display(),_window,pWidget->window(),0,0,0,0,10,10);
           return;
         }
      }
   }
}

void changeBusyState(MSShell *pShell_,MSBoolean state_) 
{ MSShell::changeBusyState(pShell_,state_); }

void MSShell::changeBusyState(MSShell *pShell_,MSBoolean state_) 
{
  if (_shellList.length()>0)
   { 
     unsigned index=_shellList.indexOf((unsigned long)pShell_);
     if (index<_shellList.length())
      {
        pShell_->setBusyState(state_);
	XFlush(pShell_->display());	
      }
   }
}

void changeBusyState(MSBoolean state_) 
{ MSShell::changeBusyState(state_); }

void MSShell::changeBusyState(MSBoolean state_) 
{
  if (_shellList.length()>0)
   { 
     MSShell *pShell;
     Display *dpy=0;
     for (unsigned i=0;i<_shellList.length();i++)
      {
        pShell=(MSShell *)_shellList(i);
	pShell->setBusyState(state_);
	dpy=pShell->display();
      }
     if (dpy!=0) XFlush(dpy);
   }
}

void MSShell::setBusyState(MSBoolean state_)
{
  if (state_==MSTrue)
   {
     if (busyCount()==0) XDefineCursor(display(),_window,_server->watchCursor());
     _busyCount++;
   }
  else
   {
     if (busyCount()==1) XUndefineCursor(display(),_window);
     if (busyCount()>0) _busyCount--;
   }
}

// ####################################################################
// inline methods
// ####################################################################

void MSShell::offsetX(int x_)
{ _offsetX=x_; }
void MSShell::offsetY(int y_)
{ _offsetY=y_; }

int MSShell::busyCount(void)             
{ return _busyCount; }
void MSShell::busyCount(int count_)      
{ _busyCount=count_; }

void MSShell::nestedTraversal(MSBoolean nestedTraversal_)
{_nestedTraversal=nestedTraversal_;}

const MSWidgetVector& MSShell::traversalList(void)
{ return _traversalList; }
const MSWidgetVector& MSShell::traversalList(void) const
{ return _traversalList; }
void MSShell::traversalList(const MSWidgetVector& aWidgetVector_)
{ _traversalList=aWidgetVector_; }

const MSWidgetVector& MSShell::topLevelList(void)
{ return _shellList; }
MSWidgetVector& MSShell::shellList(void) 
{ return _shellList; }

void MSShell::leader(MSShell *leader_)
{ _leader=leader_; }
void MSShell::focusWidget(MSWidget *widget_) 
{ _focusWidget=widget_; }

MSWidget *MSShell::child(void)
{ return _child; }
const MSWidget *MSShell::child(void) const
{ return _child; }

MSShell *MSShell::defaultLeader(void) 
{ return _defaultLeader; }
MSShell *MSShell::leader(void)
{ return _leader; }
const MSShell *MSShell::leader(void) const
{ return _leader; }
MSShell *MSShell::windowGroup(void)
{ return _leader; }
const MSShell *MSShell::windowGroup(void) const
{ return _leader; }
MSWidget *MSShell::focusWidget(void) 
{ return _focusWidget; }
const MSWidget *MSShell::focusWidget(void) const
{ return _focusWidget; }

void MSShell::set(MSAttrValueList& avList_)
{
  MSTopLevel::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="x") moveTo(avList_[i].value().asInt(),y_origin()),index<<i;
     else if (avList_[i].attribute()=="y") moveTo(x_origin(),avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="width") width(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="height") height(avList_[i].value().asInt()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSShell::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("x",MSString(x_origin()));
  avList_<<MSAttrValue("y",MSString(y_origin()));
  avList_<<MSAttrValue("width",MSString(width()));
  avList_<<MSAttrValue("height",MSString(height()));
  avList_<<MSAttrValue("iconized","",MSAttrValue::Callback);
  avList_<<MSAttrValue("deiconized","",MSAttrValue::Callback);  
  avList_<<MSAttrValue("incurrentworkspace","",MSAttrValue::Callback);
  avList_<<MSAttrValue("outofcurrentworkspace","",MSAttrValue::Callback);
  MSString value;
  for(int i=1;i<=12;i++)
   {
     value="f"+MSString(i);
     avList_<<MSAttrValue(value,"",MSAttrValue::Callback);
   }
  
  return MSTopLevel::get(avList_);
}

void MSShell::loadStateFrom(const MSWidgetState &widgetState_)
{
  MSWidgetCursor cursor(this);
  for (cursor.setToFirst();cursor.isValid()==MSTrue;cursor.setToNext())
   {
     MSWidget *child=cursor.widget();
     if (child->instanceName().length()>0)
      {
	MSAttrValueList *list;
	if ((list=(MSAttrValueList *)widgetState_.lookup(child->instanceFullname()))!=
	    (MSAttrValueList *)widgetState_.notFound())
	 {
	   child->set(*list);
	 }
      }
   }
}

void MSShell::loadStateFrom(istream &is_)
{
  if (!is_) return;
  MSWidgetState widgetState(is_);
  loadStateFrom(widgetState);
}

void MSShell::loadStateFrom(const char *fileName_)
{
  ifstream ifs(fileName_);
  loadStateFrom(ifs);
}

void MSShell::saveStateTo(const char *fileName_)
{
  ofstream ofs(fileName_);
  saveStateTo(ofs);
}

void MSShell::saveStateTo(ostream& aStream_)
{
  if (!aStream_) return;
  MSWidgetCursor cursor(this,MSDepthFirst);
  for (cursor.setToFirst();cursor.isValid()==MSTrue;cursor.setToNext())
   {
     MSWidget *child=cursor.widget();
     if (child->instanceName().length()>0)
      {
	MSAttrValueList avList;
	child->get(avList);
	if (avList.length()>0)
	 {
           MSString childFullName=child->instanceFullname();
           unsigned n=avList.length();
	   for (unsigned j=0;j<n;j++)
	    {
	      if ((avList[j].valueType()&MSAttrValue::Control)==0&&avList[j].value().length()>0)
	       {
		 aStream_<<childFullName<<".has.";
		 aStream_<<avList[j].attribute()<<"(";
		 aStream_<<avList[j].value()<<")"<<endl;
	       }
	    }
	 }
      }
   }
}

ostream& operator<<(ostream& aStream_,MSShell& aShell_)
{
  aShell_.saveStateTo(aStream_);
  return aStream_;
}

istream& operator>>(istream& aStream_,MSShell& aShell_)
{
  aShell_.loadStateFrom(aStream_);
  return aStream_;
}

void MSShell::inCurrentWorkspaceNotify(void)
{ activateCallback(MSWidgetCallback::incurrentworkspace); }

void MSShell::outOfCurrentWorkspaceNotify(void)
{ activateCallback(MSWidgetCallback::outofcurrentworkspace); }

void MSShell::updateCurrentWorkspaceState(Atom currentWorkspaceAtom_)
{
  if (inWorkspace(currentWorkspaceAtom_)==MSTrue)
   {
     if (_inCurrentWorkspace==MSFalse)
      {
        _inCurrentWorkspace=MSTrue;
        inCurrentWorkspaceNotify();
      }
   }
  else
   {
     if (_inCurrentWorkspace==MSTrue)
      {
        _inCurrentWorkspace=MSFalse;
        outOfCurrentWorkspaceNotify();        
      }
   }
}

// this static funtion will be called whenever the Current Workspace
// is changed on a CDE desktop
void MSShell::updateCurrentWorkspace(Atom currentWorkspaceAtom_)
{
  unsigned n=_shellList.length();
  MSShell *pShell;     
  for (unsigned i=0;i<n;i++)
   {
     pShell=(MSShell *)_shellList(i);
     pShell->updateCurrentWorkspaceState(currentWorkspaceAtom_);
   }
}
