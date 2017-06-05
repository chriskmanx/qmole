///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenuShell.H>

MSMenuShell::MSMenuShell(const char *windowTitle_) :
MSShell(windowTitle_)
{ init(); }

MSMenuShell::MSMenuShell(MSDisplayServer *server_,const char *windowTitle_) :
MSShell(server_,windowTitle_) 
{ init(); }

MSMenuShell::~MSMenuShell(void) 
{
  if (menuBar()!=0) safeDestroy(menuBar());
  _menuBar=0;
}

void MSMenuShell::init(void)
{
  _menuBar=0;
  _child=(MSWidget *)0x1;
  _menuBar=new MSMenuBar(this);
  menuBar()->moveTo(0,0);
  _child=0;
  _placementFlag=MSFalse;
}

void MSMenuShell::childConfigure(MSWidget *widget_)
{ 
  if (frozen()==MSFalse&&placementFlag()==MSFalse)
   {
     if (widget_==menuBar()&&menuBar()!=0)
      { 
	adjustChildPosition(); 
	adjustSize();
      }
     else if (widget_==child()&&child()!=0) adjustSize();
     adjustChildSize();
   }
}

// a child has been destroyed-check child
void MSMenuShell::childDestroy(MSWidget *widget_)
{
  if (widget_==menuBar()) _menuBar=0;
  else MSShell::childDestroy(widget_);
}

int MSMenuShell::menuBarHeight(void) const
{ return (menuBar()!=0)?menuBar()->height():0; }
int MSMenuShell::menuBarWidth(void) const
{ return (menuBar()!=0)?menuBar()->width():0; }

int MSMenuShell::idealShellHeight(void) const
{ return childHeight()+menuBarHeight(); }

int MSMenuShell::idealShellWidth(void) const
{ return (childWidth()<menuBarWidth())?menuBarWidth():childWidth(); }

void MSMenuShell::adjustChildPosition(void) 
{ if (child()!=0) child()->moveTo(0,menuBarHeight()); }

void MSMenuShell::adjustChildSize(void) 
{ 
  _placementFlag=MSTrue;
  if (menuBar()!=0) menuBar()->width(width());
  if (child()!=0) child()->resize(width(),height()-menuBarHeight());
  _placementFlag=MSFalse;
}

void MSMenuShell::showChildren(void)
{
  if (menuBar()!=0) menuBar()->map();
  MSShell::showChildren();
}

MSBoolean MSMenuShell::processAltKey(const XEvent *pEvent_,KeySym keysym_,
				     unsigned int state_,const char *pString_)
{
  if (menuBar()!=0)
   {
     keyPressNotify(menuBar(),pEvent_,keysym_,state_,pString_);
     return MSTrue;
   }
  else return MSShell::processAltKey(pEvent_,keysym_,state_,pString_);
}

MSBoolean MSMenuShell::processFunctionKey(const XEvent *pEvent_,KeySym keysym_,
					  unsigned int state_,const char *pString_)
{
  if (menuBar()!=0&&keysym_==XK_F10)
   {
     if (menuBar()->obtainFocus()==MSTrue)
      {
	menuBar()->grabAndSelect();
	return MSTrue;
      }
     return MSFalse;
   }
  else return MSShell::processFunctionKey(pEvent_,keysym_,state_,pString_);
}

MSWidgetVector MSMenuShell::children(void)
{
  MSWidgetVector vector;
  if (child()!=0) vector.append(child());
  if (menuBar()!=0) vector.append(menuBar());
  return vector;
}
