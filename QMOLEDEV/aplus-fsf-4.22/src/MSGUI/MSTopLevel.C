///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSGUI/MSTopLevel.H>
#include <MSGUI/MSPixmap.H>
#include <MSTypes/MSMessageLog.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#if defined(MS_HAS_SYSTEMINFO)
#include <sys/systeminfo.h>
#elif defined(MS_UNISTD_HAS_GETHOSTNAME)
#include <unistd.h>
#elif defined(HAVE_UNISTD_H)
#include <unistd.h>
#else
extern "C" int gethostname(char *,int);
#endif

static const char *_XA_VUE_WORKSPACE_HINTS    ="_DT_WORKSPACE_HINTS";
static const char *_XA_VUE_WORKSPACE_PRESENCE ="_DT_WORKSPACE_PRESENCE";
static const char *_XA_MWM_HINTS              ="_MOTIF_WM_HINTS";

extern MSString applicationArgumentString(void);
extern void applicationQuit(void);

MSString MSTopLevel::_windowManagerCommand;

MSTopLevel::MSTopLevel(const char *windowTitle_) 
{ init(windowTitle_); }

MSTopLevel::MSTopLevel(MSDisplayServer *server_,const char *windowTitle_) :
MSWidgetCommon(server_) 
{ init(windowTitle_); }

MSTopLevel::~MSTopLevel(void)
{
  if (_iconPixmap!=0) delete _iconPixmap;
  delete [] _workspacePresenceAtoms;
}

void MSTopLevel::init(const char *windowTitle_)
{
  _iconPixmap=0;
  _adjustWMPosition=MSFalse;
  _mwmHints._flags=0;
  _mwmHints._functions=0;
  _mwmHints._decorations=0;
  _mwmHints._inputMode=0;
  _workspacePresenceAtoms=0;
  _workspacePresenceCount=0;  
  
  windowTitle((windowTitle_!=0)?windowTitle_:"");

  setWMHints();
  setWMProtocols();
  setWMNormalHints();
}

MSStringVector MSTopLevel::workspacePresence(void) const
{ 
  if (server()->isCDERunning()==MSTrue)
   {
     const MSDisplayServer *srv=server();
     MSStringVector presence(_workspacePresenceCount);

     for (unsigned long i=0;i<_workspacePresenceCount;i++)
      {
	presence.replaceAt(i,srv->workspaceName(_workspacePresenceAtoms[i]));
      }

     return presence;
   }
  else
    {
      return MSStringVector();
    }
}

void MSTopLevel::workspacePresence(const MSString& workspace_)
{ workspacePresence(MSStringVector(1,workspace_)); }

void MSTopLevel::workspacePresence(const MSStringVector& newPresence_)
{
  unsigned n=newPresence_.length();
  if (n>0)
   {
     Atom *atoms=new Atom[n];
     
     for (unsigned i=0;i<n;i++)
      {
        atoms[i]=server()->workspaceAtom(newPresence_(i));
      }

     Atom property=XInternAtom(display(),_XA_VUE_WORKSPACE_HINTS,False);
     XChangeProperty(display(),window(), 
                     property,property,32,
                     PropModeReplace, 
                     (unsigned char *)atoms, 
                     n);
     XFlush (display());
     delete [] atoms;
   }
}
  
void MSTopLevel::workspacePresenceChangeNotify(void)
{ activateCallback(MSWidgetCallback::workspacepresencechange); }

void MSTopLevel::updateWorkspacePresence(void)
{
  if (server()->isCDERunning()==MSTrue)
    {
      Atom          actualType;
      int           actualFormat;
      unsigned long numItems;
      unsigned long leftover;
      Atom          *pWsAtoms=0;
      
      Atom property=XInternAtom(display(),_XA_VUE_WORKSPACE_PRESENCE,False);
      int status=XGetWindowProperty(display(),window(),
				    property,0L,(long)BUFSIZ,
				    False,property,
				    &actualType,&actualFormat,
				    &numItems,&leftover,
				    (unsigned char**)&pWsAtoms);
      
      if (status==Success&&actualType==property)
	{
	  if (numItems!=_workspacePresenceCount) // if the presence count was changed
	    {
	      // reallocate the presence atoms list
	      //
	      _workspacePresenceCount = numItems;
	      delete [] _workspacePresenceAtoms;
	      if (_workspacePresenceCount>0)
		{
		  _workspacePresenceAtoms = new Atom[_workspacePresenceCount];
		}
	      else
		{
		  _workspacePresenceAtoms = 0;
		}
	    }
	  
	  // copy the new atoms
	  //
	  memcpy(_workspacePresenceAtoms,pWsAtoms,(int)_workspacePresenceCount*sizeof(Atom));
	  
	  // Notify about the change
	  workspacePresenceChangeNotify();
	}
      
      if (pWsAtoms!=0) XFree((char*)pWsAtoms);
    }
}

MSBoolean MSTopLevel::inWorkspace(Atom workspaceAtom_) const
{
  if (_workspacePresenceAtoms!=0)
   {
     unsigned n=_workspacePresenceCount;
     for (unsigned i=0;i<n;i++)
      {
        if (_workspacePresenceAtoms[i]==workspaceAtom_) return MSTrue;
      }
   }
  return MSFalse;
}

void MSTopLevel::setWMPosition(void)
{
  XWindowChanges values;
  unsigned int mask=CWX|CWY;
  values.x=MSRect::x(); 
  values.y=MSRect::y(); 
  if(_adjustWMPosition==MSTrue)
    {
      values.x+=offsetX();
      values.y+=offsetY();
    }
  XReconfigureWMWindow(display(),window(),DefaultScreen(display()),mask,&values);
}

void MSTopLevel::setWMSize(void)
{
  XWindowChanges values;
  unsigned int mask=CWWidth|CWHeight;
  values.width=width();
  values.height=height();
  XReconfigureWMWindow(display(),window(),DefaultScreen(display()),mask,&values);
}

void MSTopLevel::setWMProtocols(void)
{
  Atom proto=server()->atom(MSAtomTable::WMDeleteWindow);
  XSetWMProtocols(display(),window(),&proto,1);  
}

void MSTopLevel::setWMSaveYourself(void)
{
  Atom proto[2];
  proto[0]=server()->atom(MSAtomTable::WMDeleteWindow);
  proto[1]=server()->atom(MSAtomTable::WMSaveYourself);
  XSetWMProtocols(display(),window(),proto,2);
  if (windowManagerCommand().length()==0)
  {
    windowManagerCommand(applicationArgumentString());
  }
}

void MSTopLevel::setWMClientMachine(void)
{
  char buf[80];
  buf[0]='\0';
#if defined(MS_HAS_SYSTEMINFO)
  if (sysinfo(SI_HOSTNAME,buf,80)<80)
#else
  if (0==gethostname(buf,80))
#endif
   {
     XTextProperty prop;
     prop.value=(unsigned char *)buf;
     prop.encoding=XA_STRING;
     prop.format=8;
     prop.nitems=strlen(buf);
     XSetWMClientMachine(display(),window(),&prop);  
   } 
}

void MSTopLevel::setWMHints(void)
{
  XWMHints *wmHints=XAllocWMHints();
  wmHints->flags=InputHint|StateHint;
  wmHints->initial_state=NormalState;
  wmHints->input=True;

  XSetWMHints(display(),window(),wmHints);
  XFree((char *)wmHints);
}

void MSTopLevel::setWMNormalHints(void)
{
  XSizeHints *sh=XAllocSizeHints();
  sh->x=MSRect::x();
  sh->y=MSRect::y();  
  sh->width=width();
  sh->height=height();
  sh->base_width=width();
  sh->base_height=height();
  sh->min_width=10;
  sh->min_height=10;
  sh->flags=USPosition|USSize|PBaseSize|PMinSize;
  
  XSetWMNormalHints(display(),window(),sh);
  XFree((char *)sh);
}

// adding of decorations/functions is not supported
// mwm seems to ignore dynamic changes, it takes
// a snapshot when the window is mapped, therefore
// one should only remove decorations
void MSTopLevel::addWMDecorations(unsigned long)
{}
void MSTopLevel::addWMFunctions(unsigned long)
{}
void MSTopLevel::removeWMDecorations(unsigned long decor_)
{ setWMDecorations(decor_,MSFalse); }
void MSTopLevel::removeWMFunctions(unsigned long functions_)
{ setWMFunctions(functions_,MSFalse); }

void MSTopLevel::setWMDecorations(unsigned long decor_,MSBoolean add_)
{
  if (firstMap()==MSFalse)
   {
     if (server()->isCDERunning()==MSTrue)
      {
	_mwmHints._flags|=MWMHints::DecorationsFlag;
	_mwmHints._decorations=decor_;
	if (add_==MSFalse) _mwmHints._decorations|=MWMHints::DAll;
	Atom mwmHintsAtom=XInternAtom(display(),_XA_MWM_HINTS,False);
	XChangeProperty(display(),window(),mwmHintsAtom,mwmHintsAtom, 
			32,PropModeReplace,(unsigned char *)&_mwmHints,4);
      }
   }
  else
   {
     MSMessageLog::warningMessage("Application Error: Unable to set Window Manager Decorations after TopLevel Window is mapped");
   }
}

void MSTopLevel::setWMFunctions(unsigned long functions_,MSBoolean add_)
{
  if (firstMap()==MSFalse)
   {
     if (server()->isCDERunning()==MSTrue)
      {
	_mwmHints._flags|=MWMHints::FunctionsFlag;
	_mwmHints._functions=functions_;
	if (add_==MSFalse) _mwmHints._functions|=MWMHints::FAll;
	Atom mwmHintsAtom=XInternAtom(display(),_XA_MWM_HINTS,False);
	XChangeProperty(display(),window(),mwmHintsAtom,mwmHintsAtom, 
			32,PropModeReplace,(unsigned char *)&_mwmHints,4);
      }
   }
  else
   {
     MSMessageLog::warningMessage("Application Error: Unable to set Window Manager Functions after TopLevel Window is mapped");
   }
}

void MSTopLevel::minimumSize(int width_,int height_)
{
  XSizeHints *sh=XAllocSizeHints();
  sh->min_width=width_;
  sh->min_height=height_;
  sh->flags=PMinSize;
  XSetWMNormalHints(display(),window(),sh);
  XFree((char *)sh);
}

void MSTopLevel::windowTitle(const char *name_)
{
  if (name_!=0&&window()!=0)
   {
     XTextProperty prop;
     prop.value=(unsigned char *)name_;
     prop.format=8;
     prop.encoding=XA_STRING;
     prop.nitems=strlen(name_);
     XSetWMName(display(),window(),&prop);
   }
}

MSString MSTopLevel::windowTitle(void) const
{
  XTextProperty prop;
  prop.value=0;
  if (window()!=0) XGetWMName(display(),window(),&prop);
  MSString aString((const char *)prop.value);
  if (prop.value!=0) XFree((char *)prop.value);
  return aString;
}

void MSTopLevel::iconTitle(const char *name_)
{
  if (name_!=0&&window()!=0)
   {
     XTextProperty prop;
     prop.value=(unsigned char *)name_;
     prop.format=8;
     prop.encoding=XA_STRING;
     prop.nitems=strlen(name_);
     XSetWMIconName(display(),window(),&prop);
   }
}

MSString MSTopLevel::iconTitle(void) const
{
  XTextProperty prop;
  prop.value=0;
  if (window()!=0) XGetWMIconName(display(),window(),&prop);
  MSString n((const char *)prop.value);
  if (prop.value!=0) XFree((char *)prop.value);
  return n;
}

void MSTopLevel::iconPixmap(const MSPixmap &pixmap_)
{
  XWMHints *wmhints=XGetWMHints(display(),window());
  if (wmhints==0) wmhints=XAllocWMHints();
  wmhints->flags|=IconPixmapHint;
  wmhints->icon_pixmap=pixmap_.pixmap();
  if (pixmap_.clipMask()!=0)
   {
     wmhints->icon_mask=pixmap_.clipMask();
     wmhints->flags|=IconMaskHint;
   }
  XSetWMHints(display(),window(),wmhints);
  XFree((char *)wmhints);
  if (iconPixmap()!=0) delete iconPixmap();
  _iconPixmap=new MSPixmap(pixmap_);
}

void MSTopLevel::windowManagerCommand(const char *cmd_)
{ _windowManagerCommand=cmd_; }

void MSTopLevel::setWMCommand(void)
{
  if (windowManagerCommand().length()>0)
   {
     XChangeProperty(display(),window(),XA_WM_COMMAND,XA_STRING,8,
                     PropModeReplace,(unsigned char *)windowManagerCommand().string(),
		     windowManagerCommand().length());
   }
  else // zero length append
   {
     XChangeProperty(display(),window(),XA_WM_COMMAND,
		     XA_STRING,8,PropModeAppend,(unsigned char *)0,0);
   }
}

void MSTopLevel::resizeable(MSBoolean resizeable_)
{
  if (resizeable()!=resizeable_&&firstMap()==MSFalse)
   {
     _resizeable=resizeable_;
     if (server()->isCDERunning()==MSTrue)
      {
	if (resizeable()==MSFalse)
         {
           removeWMDecorations(MWMHints::DResizeHandles);
           removeWMFunctions(MWMHints::FResize);
         }
      }
     else
      {
	Atom decor=server()->atom(MSAtomTable::DecorResize);
	Atom atom=server()->atom((resizeable()==MSTrue)?
				 MSAtomTable::DecorAdd:MSAtomTable::DecorDel);
	XChangeProperty(display(),window(),atom,
			XA_ATOM,32,PropModeReplace,(unsigned char *)&decor,1);
      }
   }
}

void MSTopLevel::footer(MSBoolean footer_)
{
  if (footer()!=footer_)
   {
     _footer=footer_;
     if (server()->isCDERunning()==MSFalse)
      {
	Atom decor=server()->atom(MSAtomTable::DecorFooter);
	Atom atom=server()->atom((footer()==MSTrue)?
				 MSAtomTable::DecorAdd:MSAtomTable::DecorDel);
	XChangeProperty(display(),window(),atom,
			XA_ATOM,32,PropModeReplace,(unsigned char *)&decor,1);
      }
   }
}

void MSTopLevel::header(MSBoolean header_)
{
  if (header()!=header_)
   {
     _header=header_;
     if (server()->isCDERunning()==MSTrue)
      {
	if (header()==MSFalse) removeWMDecorations(MWMHints::DTitle);
      }
     else
      {
	Atom decor=server()->atom(MSAtomTable::DecorHeader);
	Atom atom=server()->atom((header()==MSTrue)?
				 MSAtomTable::DecorAdd:MSAtomTable::DecorDel);
	XChangeProperty(display(),window(),atom,
			XA_ATOM,32,PropModeReplace,(unsigned char *)&decor,1);
      }
   }
}

void MSTopLevel::iconify(void)
{
  if (mapped()==MSTrue)
   {
     _mapped=MSFalse;
     XIconifyWindow(display(),window(),DefaultScreen(display()));
   }
}

void MSTopLevel::clientMessage(const XEvent *event_)
{
  if (event_->xclient.message_type==server()->atom(MSAtomTable::WMProtocols))
   {
     if (event_->xclient.data.l[0]==server()->atom(MSAtomTable::WMDeleteWindow)) quit();
     else if (event_->xclient.data.l[0]==server()->atom(MSAtomTable::WMSaveYourself)) 
      { 
        save();
	setWMCommand();
      }
   }
}

void MSTopLevel::quit(void) 
{
  if (activateCallback(MSWidgetCallback::quit)==MSFalse)
   {
     applicationQuit();
   }
}

void MSTopLevel::save(void) 
{ activateCallback(MSWidgetCallback::saveyourself); }

void MSTopLevel::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="windowTitle") windowTitle(avList_[i].value()),index<<i;  
   }
  avList_.remove(index);
}

MSAttrValueList& MSTopLevel::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("windowTitle",windowTitle(),MSAttrValue::String);
  avList_<<MSAttrValue("workspacepresencechange","",MSAttrValue::Callback);
  avList_<<MSAttrValue("saveyourself","",MSAttrValue::Callback);
  avList_<<MSAttrValue("quit","",MSAttrValue::Callback);
  return MSWidgetCommon::get(avList_);
}

// ####################################################################
// inline methods
// ####################################################################

const MSString& MSTopLevel::windowManagerCommand(void) const
{ return _windowManagerCommand; }
MSBoolean MSTopLevel::footer(void) const
{ return _footer; }
MSBoolean MSTopLevel::header(void) const         
{ return _header; }
MSBoolean MSTopLevel::resizeable(void) const         
{ return _resizeable; }
MSPixmap *MSTopLevel::iconPixmap(void) 
{ return _iconPixmap; }

