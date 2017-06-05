///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusShell.H>
#include <AplusGUI/AplusDisplayServer.H>

static const char *_XA_VUE_WORKSPACE_HINTS    ="_DT_WORKSPACE_HINTS";

extern long dbg_tmstk;
MSBoolean AplusShell::_allowNestedTraversal = MSFalse;

AplusShell::AplusShell(void) : MSShell()
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}
AplusShell::AplusShell(MSDisplayServer *server_) : MSShell(server_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}
AplusShell::~AplusShell(void)
{
  if (MSShell::defaultLeader()==(MSShell *)this)
   {
     MSShell::defaultLeader(0);
   }
}

void AplusShell::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusShell::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusShell"  << endl;
     redraw();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusShell"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}

void AplusShell::decoupleWidget(void)
{
  if (model())
   {
     delete _model;
     _model=0;
   }
}

MSWidgetView *AplusShell::getNextFocusAfter(MSWidgetView *pWidgetView_)
{
  if (traversalList().length()>0)
   {
     MSWidget *pWidget=0;
     unsigned i,index=traversalList().indexOf((unsigned long)pWidgetView_,0);
     if (index==traversalList().length()) index=0;
     for (i=index+1;i<traversalList().length();i++)
      {
	pWidget=traversalList()(i);
        if (isTraversable(pWidget)==MSTrue) return (MSWidgetView *)pWidget;
      }
     for (i=0;i<index;i++)
      {
	pWidget=traversalList()(i);
        if (isTraversable(pWidget)==MSTrue) return (MSWidgetView *)pWidget;
      }
   }
  return 0;
}

void AplusShell::insertFocusAfter(MSWidgetView *from_, MSWidgetView *to_)
{
  unsigned fromIndex = _traversalList.indexOf((unsigned long) from_);
  unsigned toIndex = _traversalList.indexOf((unsigned long) to_);

  if (fromIndex != _traversalList.length() &&
      toIndex != _traversalList.length())
   {
     _traversalList.removeAt(toIndex);
     _traversalList.insertAt(fromIndex+1, (unsigned long) to_);
   }
}

// This code is almost the same as traverseFocus in Shell
// except it allows nesting. (identical code in AplusPopup)

MSBoolean AplusShell::traverseFocus(MSWidget *newFocusWidget_)
{
  static MSSymbol takeFocusSymbol("takefocus");
  if (AplusShell::allowNestedTraversal())
   {
     MSBoolean lf=MSTrue;
     if (focusWidget()!=0&&focusWidget()!=newFocusWidget_) lf=loseFocusNotify(focusWidget());
     if (lf==MSTrue)
      {
	focusWidget(newFocusWidget_);
	takeFocusNotify(focusWidget());
	activateCallback(focusWidget(),takeFocusSymbol);
	return MSTrue;
      }
     return MSFalse;
   }
  else return MSShell::traverseFocus(newFocusWidget_);
}

void AplusShell::showAndWaitForMap(void)
{
  if (mapped()==MSFalse)
  {
    show();
    // Waiting for MapNotify Event;
    while (1)
    {
      XEvent ev;
      XPeekEvent(display(),&ev);
      server()->processOneEvent();
      if (ev.type==MapNotify&&ev.xmap.window==window())
      {
	server()->flush();
	return;
      }
    }
  }
  else show();
}


void AplusShell::save(void)
{
  MSShell::save();	// this will activate the "saveyourself" callback
  windowManagerCommand(getenv("AW_WM_COMMAND"));
}


void AplusShell::virtualScreen(A screen_)
{
  if (server()->isCDERunning()==MSTrue)
    {
      if (!QA(screen_) || screen_->t!=It)
	{
	  return;
	}

      int n=(int)screen_->n;
      unsigned long wsCount=server()->numberOfWorkspaces();

      if (n>0 && wsCount>0)
	{
	  Atom *wsAtoms=((AplusDisplayServer *)server())->workspaceAtoms();
	  if (wsAtoms==0)
	    {
	      return;
	    }

	  Atom *atoms=new Atom[n];
	  
	  int wsNum;

	  for (unsigned int i=0; i<n; ++i)
	    {
	      wsNum = (int)screen_->p[i]-1;  // workspace numbers are 1-based
	      if (wsNum<wsCount)   // if it's a valid workspace number
		{
		  atoms[i] = wsAtoms[wsNum];
		}
	      else	// the workspace number is invalid
		{
		  atoms[i] = 0;
		}
	    }

	  Atom property=XInternAtom(display(),_XA_VUE_WORKSPACE_HINTS,False);
	  XChangeProperty(display(),window(), 
			  property,property,32,
			  PropModeReplace, 
			  (unsigned char *)atoms, 
			  n);
	  XFlush (display());
	  delete [] atoms;
	  delete [] wsAtoms;  // wsAtoms were allocated by server()->workspaceAtoms() method
	}
    }
  else	// CDE is not running
    {
      Atom atom = XInternAtom(display(), "WM_VIRTUAL_SCREEN", False);
      XChangeProperty(display(),window(),atom,XA_INTEGER,32,PropModeReplace,
		      (unsigned char *)screen_->p,1);
    }
}


A AplusShell::virtualScreen(void)
{
  if (server()->isCDERunning()==MSTrue)
    {
      unsigned long wsCount=server()->numberOfWorkspaces();
      if (wsCount==0)
	{
	  return gi(-1);  // this should never happen
	}

      Atom *wsAtoms=((AplusDisplayServer *)server())->workspaceAtoms();
      if (wsAtoms==0)
	{
	  return gi(-1);  // this should never happen
	}

      A presence=gv(It,_workspacePresenceCount);

	  unsigned i ;
      for (i=0; i<_workspacePresenceCount; ++i)
	{
	  unsigned j ;
	  for (j=0; j<wsCount; ++j)
	    {
	      if (_workspacePresenceAtoms[i]==wsAtoms[j])
		{
		  presence->p[i]=j+1; // workspace numbers are 1-based
		  break;
		}
	    }
	  
	  if (j==wsCount)	// i-th atom could not be found -> it is invalid
	    {
	      presence->p[i]=0;
	    }
	}

      delete [] wsAtoms;  // wsAtoms were allocated by server()->workspaceAtoms() method
      return presence;
    }
  else	// CDE is not running
    {
      return gi(((AplusDisplayServer *)server())->virtualScreen(window()));
    }
}


const MSSymbol& AplusShell::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusShell::symbol(void)
{
  static MSSymbol sym("AplusShell");
  return sym;
}
