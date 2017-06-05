///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSGUI/MSWidget.H>
#include <MSGUI/MSDisplayServer.H>
#include <MSGUI/MSColorManager.H>
#include <MSGUI/MSFontManager.H>
#include <MSGUI/MSDisplayCursor.H>
#include <MSGUI/MSToolTip.H>
#include <MSIPC/MSChannel.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSStringVector.H>

static const char *_XA_DT_WM_REQUEST         ="_DT_WM_REQUEST";
static const char *_XA_MWM_INFO              ="_MOTIF_WM_INFO";
static const char *_XA_DT_WORKSPACE_LIST     ="_DT_WORKSPACE_LIST";
static const char *_XA_DT_WORKSPACE_CURRENT  ="_DT_WORKSPACE_CURRENT";
static const char *_XA_DT_WORKSPACE_INFO     ="_DT_WORKSPACE_INFO_";

static const int MSWidgetHashTableSize=2048;
static const int MSShadowHashTableSize=64;
static const int MSToolTipHashTableSize=128;
static const int MSPropertyFullLength=8192;
static const int numErrors=18;
static const int numOpCodes=120;
static const int XErrorBufSize=256;

static const char *MSWidgetOutputDefaultHighlightColor="yellow";

MSDisplayServer *MSDisplayServer::_defaultDisplayServer=0;
MSWorkspaceChangedFunction MSDisplayServer::_workspaceChangedFunction=0;

static char *errorCodes[numErrors]=
{
  "Success","BadRequest","BadValue","BadWindow","BadPixmap",
  "BadAtom","BadCursor","BadFont","BadMatch","BadDrawable",
  "BadAccess","BadAlloc","BadGC","BadIDChoice","BadName",
  "BadLength","BadImplementation"
};

static char *opCodeNames[numOpCodes]=
{
  "","CreateWindow","ChangeWindowAttributes","GetWindowAttributes",
  "DestroyWindow","DestroySubwindows","ChangeSaveSet","ReparentWindow",
  "MapWindow","MapSubwindows","UnmapWindow","UnmapSubwindows",
  "ConfigureWindow","CirculateWindow","GetGeometry","QueryTree",
  "InternAtom","GetAtomName","ChangeProperty","DeleteProperty",
  "GetProperty","ListProperties","SetSelectionOwner","GetSelectionOwner",
  "ConvertSelection","SendEvent","GrabPointer","UngrabPointer","GrabButton",
  "UngrabButton","ChangeActivePointerGrab","GrabKeyboard","UngrabKeyboard",
  "GrabKey","UngrabKey","AllowEvents","GrabServer","UngrabServer","QueryPointer",
  "GetMotionEvents","TranslateCoords","WarpPointer","SetInputFocus","GetInputFocus",
  "QueryKeymap","OpenFont","CloseFont","QueryFont","QueryTextExtents","ListFonts",
  "ListFontsWithInfo","SetFontPath","GetFontPath","CreatePixmap","FreePixmap",
  "CreateGC","ChangeGC","CopyGC","SetDashes","SetClipRectangles","FreeGC",
  "ClearArea","CopyArea","CopyPlane","PolyPoint","PolyLine","PolySegment","PolyRectangle",
  "PolyArc","FillPoly","PolyFillRectangle","PolyFillArc","PutImage","GetImage","PolyText8",
  "PolyText16","ImageText8","ImageText16","CreateColormap","FreeColormap",
  "CopyColormapAndFree","InstallColormap","UninstallColormap","ListInstalledColormaps",
  "AllocColor","AllocNamedColor","AllocColorCells","AllocColorPlanes","FreeColors",
  "StoreColors","StoreNamedColor","QueryColors","LookupColor","CreateCursor",
  "CreateGlyphCursor","FreeCursor","RecolorCursor","QueryBestSize","QueryExtension",
  "ListExtensions","ChangeKeyboardMapping","GetKeyboardMapping","ChangeKeyboardControl",
  "GetKeyboardControl","Bell","ChangePointerControl","GetPointerControl","SetScreenSaver",
  "GetScreenSaver","ChangeHosts","ListHosts","SetAccessControl","SetCloseDownMode",
  "KillClient","RotateProperties","ForceScreenSaver","SetPointerMapping",
  "GetPointerMapping","SetModifierMapping","GetModifierMapping"
};

#ifdef MS_KEYPAD_BUG

struct KeypadInfo
{
  KeySym keysym;
  char* string;
};

static KeypadInfo keypad_table[] = {
{ XK_KP_0, "0" },
{ XK_KP_1, "1"},
{ XK_KP_2, "2"},
{ XK_KP_3, "3"},
{ XK_KP_4, "4"},
{ XK_KP_5, "5"},
{ XK_KP_6, "6"},
{ XK_KP_7, "7"},
{ XK_KP_8, "8"},
{ XK_KP_9, "9"},
{ XK_KP_Decimal, "." },
{ XK_KP_Equal ,"="},
{ XK_KP_Multiply,"*"},
{ XK_KP_Add, "+"},
{ XK_KP_Separator, ""},
{ XK_KP_Subtract, "-"},
{ XK_KP_Divide, "/"},
{ XK_KP_Enter, "" }
};

static const int numKeypadKeys = 18;
static const int keypadDualModeElements = 11;
#endif

#ifdef MS_NO_INLINES
#include <MSGUI/MSDisplayServerInlines.C>
#endif

extern MSGUIExport MSBoolean applicationAddServer(MSDisplayServer *);
extern MSGUIExport MSBoolean applicationRemoveServer(MSDisplayServer *);
extern MSGUIExport MSDisplayServer *applicationDisplayServer(Display *);
extern MSGUIExport void applicationExit(void);

class MSDisplayServerChannel : public MSChannel
{
protected:
  MSDisplayServer *_pServer;
public:
  MSDisplayServerChannel(MSDisplayServer *);
  ~MSDisplayServerChannel(void);
  virtual void process(void);
};

class KeyboardGrabber
{
public:
  Window  _grabWindow;
  int     _ownerEvents;
  int     _pointerMode;
  int     _keyboardMode;
  Time    _time;

  KeyboardGrabber(Window grabWindow_,int ownerEvents_,int pointerMode_,
                  int keyboardMode_,Time time_);
};

KeyboardGrabber::KeyboardGrabber(Window grabWindow_,int ownerEvents_,
                                 int pointerMode_,int keyboardMode_,Time time_) :
_grabWindow(grabWindow_),
_ownerEvents(ownerEvents_),
_pointerMode(pointerMode_),
_keyboardMode(keyboardMode_),
_time(time_)
{}

class PointerGrabber
{
public:
  Window       _grabWindow;
  int          _ownerEvents;
  unsigned int _eventMask;
  int          _pointerMode;
  int          _keyboardMode;
  Window       _confineTo;
  Cursor       _cursor;
  Time         _time;

  PointerGrabber(Window grabWindow_,int ownerEvents_,unsigned int eventMask_,int pointerMode_,
                 int keyboardMode_,Window confineTo_,Cursor cursor_,Time time_);
};

PointerGrabber::PointerGrabber(Window grabWindow_,int ownerEvents_,
                               unsigned int eventMask_,int pointerMode_,
                               int keyboardMode_,Window confineTo_,Cursor cursor_,Time time_) :
_grabWindow(grabWindow_),
_ownerEvents(ownerEvents_),
_eventMask(eventMask_),
_pointerMode(pointerMode_),
_keyboardMode(keyboardMode_),
_confineTo(confineTo_),
_cursor(cursor_),
_time(time_)
{}

MSAtomTable::MSAtomTable(void) 
{ _array=new Atom[MSAtomTable::LastAtom]; }
MSAtomTable::~MSAtomTable(void) 
{ delete [] _array; }

void MSAtomTable::add(int index_,Atom atom_)
{ if (index_<LastAtom) _array[index_]=atom_; }

Atom MSAtomTable::atom(int index_) const
{ return _array[index_]; }

MSDisplayServerChannel::MSDisplayServerChannel(MSDisplayServer *pServer_):
MSChannel("MSDisplayServer",pServer_->connection(),MSChannel::High,MSChannel::Read) 
{ _pServer=pServer_; }

MSDisplayServerChannel::~MSDisplayServerChannel(void)
{}
void MSDisplayServerChannel::process(void) 
{ _pServer->service(); }

int MSDisplayServer::ioErrorHandler(void)
{
  fprintf(stderr, "XIO:  fatal IO error on X server %s\n",DisplayString(_dpy));
  fprintf(stderr, "      after %d", NextRequest(_dpy)-1);
  fprintf(stderr, " requests %d", LastKnownRequestProcessed(_dpy));
  fprintf(stderr, " known processed with %d", QLength(_dpy));
  fprintf(stderr, " events remaining.\n");
  fprintf(stderr, "      The connection was probably broken by ");
  fprintf(stderr, "a server shutdown or KillClient.\n");
  return (MSTrue)?exitOnError(),0:0;
}

int MSDisplayServer::errorHandler(const XErrorEvent *event_)
{
  char *errorText=new char[XErrorBufSize+1];
  XGetErrorText(_dpy,event_->error_code,errorText,XErrorBufSize);

  fprintf(stderr, "X Error on display:           \t%s\n",_name.string());
  fprintf(stderr, "Resource ID of failed request:\t%p\n",(void *)event_->resourceid);
  fprintf(stderr, errorText);
  
  delete [] errorText;
  
  fprintf(stderr, "Op Code:         \t%d",(int)event_->request_code);
  fprintf(stderr, ".%d\n",(int)event_->minor_code);
  if (event_->request_code>0&&event_->request_code<numOpCodes)
   { fprintf(stderr, "Protocol Name:   \t%s\n",opCodeNames[event_->request_code]); } 
  fprintf(stderr, "Error Code:      \t%d\n",(int)event_->error_code);
  if (event_->error_code>0&&event_->error_code<numErrors)
   { fprintf(stderr, "Error Name:      \t%s\n",errorCodes[event_->error_code]); }
  
  if ((int)event_->error_code==BadAlloc)
   {
     fprintf(stderr, "\nThe XServer does not have enough memory to accommodate this request.");
     exitOnError();
   }
  return 0;
}

void MSDisplayServer::exitOnError(void)
{
  fprintf(stderr, "Exiting Application ... ");
  fprintf(stderr, "from MSDisplayServer::exitOnError\n");
  applicationExit();
}

static int XIOErrHndlr(Display *dpy_)
{
  MSDisplayServer *s=MSDisplayServer::serverOfDisplay(dpy_);
  if (s!=0) s->ioErrorHandler();
  else 
   {
     fprintf(stderr, "Fatal IO error on X connection: exiting in XIOErrHandlr\n");
     MSDisplayServer::exitOnError();
   }
  return 1;
}

static int XErrHndlr(Display *dpy_,XErrorEvent *event_)
{
  MSDisplayServer *s=MSDisplayServer::serverOfDisplay(dpy_);
  return (s!=0)?s->errorHandler(event_):1;
}

// ##############################################################################
// CDE WindowManager functionality
// ##############################################################################

class MWMWidget : public MSWidget
{
public:
  MWMWidget(MSDisplayServer*,Window);
  ~MWMWidget(void);

  Window               _mwmWindow;
  Atom                 _workspaceCurrentAtom;
  Atom                 _workspaceListAtom;
  Atom                 _wmStateAtom;  
  unsigned long        _workspaceCount;
  MSStringVector       _workspaceNames;
  Atom                *_workspaceInfoAtoms;
  Atom                *_workspaceAtoms;
  Atom                 _currentWorkspaceAtom;
  MSString             _currentWorkspaceName;  

  virtual void propertyNotify(const XEvent *);
  
  void updateWorkspaceList(void);
  void updateWorkspaceInfo(Atom workspaceInfoAtom_);  
  void updateCurrentWorkspaceName(void);  

  MSString getWorkspaceName(Atom workspaceAtom_) const;
  MSStringVector getWorkspaceNames(void);

  Atom getWorkspaceAtom(const MSString& workspaceName_) const;
  
  unsigned long numberOfWorkspaces(void) const {return _workspaceCount;}
  Atom currentWorkspaceAtom(void) const {return _currentWorkspaceAtom;}  
  const MSString& currentWorkspaceName(void) const {return _currentWorkspaceName;}
  const MSStringVector& workspaceNames(void) const {return _workspaceNames;}
  MSString workspaceName(Atom) const;
  Atom workspaceAtom(const MSString&) const;
};

// ##############################################################################
// MSDisplayServer
// ##############################################################################

MSDisplayServer::MSDisplayServer(void)
{
  _dpy=(Display *)XOpenDisplay((char *)0);
  if(_dpy==0) _name=getenv("DISPLAY");
  else _name=DisplayString(_dpy);
  init();
}

MSDisplayServer::MSDisplayServer(const char *dpyName_) :
_name(dpyName_)
{
  _dpy=(Display *)XOpenDisplay((char *)dpyName_);
  init();
}

void MSDisplayServer::init(void)
{
  if (_dpy==0)
   {
     fprintf(stderr, "Unable to connect to Server: ");
     fprintf(stderr, "%s\n",_name.string());
     fprintf(stderr, "Check that your 'DISPLAY' environment variable is set correctly.\n");
     fprintf(stderr, "Use the following UNIX command to set:\n");
     fprintf(stderr, "\t1. Korn Shell (ksh):    $export DISPLAY=hostname:0.x\n");
     fprintf(stderr, "\t2. Command Shell (csh): $setenv DISPLAY \"hostname:0.x\"\n\n"); 
     fprintf(stderr, "where x is the screen number (usually 0 or 1).\n\n");
     fprintf(stderr, "If exporting DISPLAY, check that access to client machine X server is allowed\n");
     fprintf(stderr, "Use xhost + to allow access from remote machine\n");
     MSDisplayServer::exitOnError();     
   }
  else
   {
     (void)XSetIOErrorHandler(XIOErrHndlr);
     (void)XSetErrorHandler(XErrHndlr);

     _colorManager=0;
     _channel=0;
     _watchCursor=0;
     _shadowHashTable=new MSHashTable(MSShadowHashTableSize);
     _widgetHashTable=new MSHashTable(MSWidgetHashTableSize);
     _toolTipHashTable=new MSHashTable(MSToolTipHashTableSize);
     _toolTipHashTable->notFound((unsigned long) new MSStringVector("No Tip !"));
     _fontManager=new MSFontManager(*this);
     _colorManager=new MSColorManager(*this);
     _copyBuffer="Selection Cleared";
     _pasteBuffer="";
     _toolTip=0;
     _pMWMWidget=0;
     
     initAtoms();

     if (_defaultDisplayServer==0) _defaultDisplayServer=this;

     // add to server to global application object
     applicationAddServer(this); 
     _status=QueueOk;
     _channel=new MSDisplayServerChannel(this);
     _channel->enable();
     _watchCursor=new MSDisplayCursor(this,XC_watch,pixel("black"),pixel("white"));
     _defaultFg=_colorManager->defaultFg();
     _defaultBg=_colorManager->defaultBg();
     _defaultFont=_fontManager->defaultFontID();
     _keyboardGrabList=new MSUnsignedLongVector;
     _pointerGrabList=new MSUnsignedLongVector; 
     _passiveGrabList=new MSWidgetVector;
     _keyboardGrabber=0;
     _pointerGrabber=0;

     _defaultValueBg =0;
     _defaultHighlightColor=pixel(MSWidgetOutputDefaultHighlightColor);
     _primarySelectionOwner=0;
     _menuGrabber=0;
     _menuBar=0;
     _menuGrabCursor=0;
     _scrollBarMenu=0;
     setWindowManager();
     
     #ifdef MS_KEYPAD_BUG
     setKeypadCodes();
     #endif
   }
}

#ifdef MS_KEYPAD_BUG
void MSDisplayServer::setKeypadCodes(void)
{
  _keypadKeycodes = new KeyCode[numKeypadKeys];
  for (int i=0;i<numKeypadKeys;i++)
   {
     _keypadKeycodes[i] = ::XKeysymToKeycode(display(), keypad_table[i].keysym);
   }
  
  KeyCode numLockKeycode = ::XKeysymToKeycode(display(),XK_Num_Lock);
  int numLockModifier ;
  XModifierKeymap *modMap = XGetModifierMapping(display());
  MSBoolean found =MSFalse;
  for ( int j=ShiftMapIndex; j <=Mod5MapIndex; j++)
   {
     for ( int k=0; k < modMap->max_keypermod && found==MSFalse; k++)
      {
        if (modMap->modifiermap[j*modMap->max_keypermod+k] ==numLockKeycode)
         {
           numLockModifier = j;
           found = MSTrue;
           break;
         }
      }
   }
  _numLockModifier = (1<<numLockModifier);
  XFreeModifiermap(modMap);
}
#endif
MSDisplayServer::~MSDisplayServer(void)
{
  if (_scrollBarMenu!=0)     _scrollBarMenu->destroy();
  if (_pMWMWidget!=0)        delete _pMWMWidget;
  if (_toolTip!=0)           delete _toolTip;
  if (_colorManager!=0)      delete _colorManager;
  if (_fontManager!=0)       delete _fontManager;
  if (_shadowHashTable!=0)   delete _shadowHashTable; 
  if (_widgetHashTable!=0)   delete _widgetHashTable; 
  if (_toolTipHashTable!=0)
   {
     MSStringVector *vector=(MSStringVector *)_toolTipHashTable->notFound();
     delete vector;
     delete _toolTipHashTable;
   }
  if (_watchCursor!=0) delete _watchCursor;
  if (_menuGrabCursor!=0) delete _menuGrabCursor;
  if (_defaultDisplayServer==this) _defaultDisplayServer=0;
  if (_channel!=0) delete _channel;
  _channel=0;
  #ifdef MS_KEYPAD_BUG
  if (_keypadKeycodes!=0) delete[] _keypadKeycodes;
  #endif
  
  int i;
  MSUnsignedLongVector *pGrabList;

  pGrabList=(MSUnsignedLongVector *)_keyboardGrabList;
  for (i=0;i<pGrabList->length();i++)
   {
     KeyboardGrabber *pGrabber=(KeyboardGrabber *)((*pGrabList)(i));
     delete pGrabber;
   }
  delete pGrabList;
  
  pGrabList=(MSUnsignedLongVector *)_pointerGrabList;
  for (i=0;i<pGrabList->length();i++)
   {
     PointerGrabber *pGrabber=(PointerGrabber *)((*pGrabList)(i));
     delete pGrabber;
   }
  delete pGrabList;
  
  delete _passiveGrabList;
  
  XCloseDisplay(_dpy);
  // remove the server from the global application object
  applicationRemoveServer(this);
  _dpy=0;
  _colorManager=0;
  _fontManager=0;
  _watchCursor=0;
  _shadowHashTable=0;
  _widgetHashTable=0;
  _keyboardGrabList=0;
  _pointerGrabList=0;
  _passiveGrabList=0;
}

void MSDisplayServer::bell(void)  
{ XBell(_dpy,0); }

void MSDisplayServer::dispatch(void) 
{ 
  XSync(_dpy,False); 
  (void)process(); 
}

MSBoolean MSDisplayServer::flush(void) 
{ return process(); }

#ifdef MS_WINDOWS
#include <MSGUI/MSApplication.H>
#endif

void MSDisplayServer::dispatchEvent(XEvent& aEvent_)
{
  if (aEvent_.xany.type==MappingNotify)
   {
     if (aEvent_.xmapping.request==MappingModifier||
         aEvent_.xmapping.request==MappingKeyboard)
      {
        XRefreshKeyboardMapping(&(aEvent_.xmapping));
      }
   }
  else
   {
#ifdef MS_WINDOWS
     MSWidget *pWidget=MSApplication::findWidget(aEvent_.xany.window);
#else
     MSWidget *pWidget=(MSWidget *)_widgetHashTable->lookup(aEvent_.xany.window);
#endif
     if ((unsigned long)pWidget!=_widgetHashTable->notFound()) pWidget->event(&aEvent_);
   }
}

void MSDisplayServer::service(void)
{
  if (XEventsQueued(_dpy,QueuedAfterReading)||(_status==NoQueueAfterSelect))
   {
     _status=QueueOk;
     processXEvents();     
   }
  else // if connect is broken then XNextEvent will exit 
   {
     XEvent aEvent;
     _status=NoQueueAfterSelect;
     XNextEvent(_dpy,&aEvent);
     dispatchEvent(aEvent);
   }  
}

void MSDisplayServer::processOneEvent(void)
{
  XEvent aEvent;
  XNextEvent(_dpy,&aEvent);
  dispatchEvent(aEvent);
}

void MSDisplayServer::processXEvents(void)
{
  if (XEventsQueued(_dpy,QueuedAfterReading)>0)
   {
     XEvent aEvent;
     while (XQLength(_dpy)>0)     
      {
	XNextEvent(_dpy,&aEvent);
        dispatchEvent(aEvent);
      }
   }
}

MSBoolean MSDisplayServer::flush(Display *dpy_) 
{ return serverOfDisplay(dpy_)->process(); }

// This flush and event processing loop end when the displays has been 
// examined without processing incoming events. d counts the
// consecutive zero event calls.  
MSBoolean MSDisplayServer::process(void)
{
  XEvent     aEvent;
  MSBoolean  didWork=MSFalse;
  int        d=0;
  
  while (d!=1)
   {
     if (XEventsQueued(_dpy,QueuedAfterFlush)==0)
      {
        d=1;
	continue;
      }
     d=0;  // bummer,we have to go around again 
     while (XQLength(_dpy)>0)
      {
        didWork=MSTrue;
	XNextEvent(_dpy,&aEvent);
        dispatchEvent(aEvent);
      }
   }
  return didWork;
}

void MSDisplayServer::initAtoms(void)
{
  Display *d=_dpy;
  
  _atomTable.add(MSAtomTable::WMState,XInternAtom(d,"WM_STATE",False));
  _atomTable.add(MSAtomTable::WMDeleteWindow,XInternAtom(d,"WM_DELETE_WINDOW",False));
  _atomTable.add(MSAtomTable::WMSaveYourself,XInternAtom(d,"WM_SAVE_YOURSELF",False));
  _atomTable.add(MSAtomTable::WMProtocols,XInternAtom(d,"WM_PROTOCOLS",False));
  _atomTable.add(MSAtomTable::MStk,XInternAtom(d,"MStk",False));

  _atomTable.add(MSAtomTable::WinAttr,XInternAtom(d,"_OL_WIN_ATTR",False));
  _atomTable.add(MSAtomTable::WTBase,XInternAtom(d,"_OL_WT_BASE",False));
  _atomTable.add(MSAtomTable::WTNotice,XInternAtom(d,"_OL_WT_NOTICE",False));
  _atomTable.add(MSAtomTable::WTCmd,XInternAtom(d,"_OL_WT_CMD",False));
  _atomTable.add(MSAtomTable::WTOther,XInternAtom(d,"_OL_WT_OTHER",False));
  _atomTable.add(MSAtomTable::PushpinState,XInternAtom(d,"_OL_PIN_STATE",False));
  _atomTable.add(MSAtomTable::PinOut,XInternAtom(d,"_OL_PIN_OUT",False));
  _atomTable.add(MSAtomTable::PinIn,XInternAtom(d,"_OL_PIN_IN",False));
  _atomTable.add(MSAtomTable::WindowBusy,XInternAtom(d,"_OL_WIN_BUSY",False));
  _atomTable.add(MSAtomTable::LeftFooter,XInternAtom(d,"_OL_WINMSG_ERROR",False));
  _atomTable.add(MSAtomTable::RightFooter,XInternAtom(d,"_OL_WINMSG_STATE",False));
  _atomTable.add(MSAtomTable::DecorResize,XInternAtom(d,"_OL_DECOR_RESIZE",False));
  _atomTable.add(MSAtomTable::DecorPin,XInternAtom(d,"_OL_DECOR_PIN",False));
  _atomTable.add(MSAtomTable::DecorFooter,XInternAtom(d,"_OL_DECOR_FOOTER",False));
  _atomTable.add(MSAtomTable::DecorHeader,XInternAtom(d,"_OL_DECOR_HEADER",False));
  _atomTable.add(MSAtomTable::DecorAdd,XInternAtom(d,"_OL_DECOR_ADD",False));
  _atomTable.add(MSAtomTable::DecorDel,XInternAtom(d,"_OL_DECOR_DEL",False));
  _atomTable.add(MSAtomTable::MenuFull,XInternAtom(d,"_OL_MENU_FULL",False));
  _atomTable.add(MSAtomTable::MenuLimited,XInternAtom(d,"_OL_MENU_LIMITED",False));

  _atomTable.add(MSAtomTable::Targets,XInternAtom(d,"TARGETS",False));
  _atomTable.add(MSAtomTable::ClientWindow,XInternAtom(d,"CLIENT_WINDOW",False));
  _atomTable.add(MSAtomTable::TimeStamp,XInternAtom(d,"TIMESTAMP",False));
  _atomTable.add(MSAtomTable::Length,XInternAtom(d,"LENGTH",False));
  _atomTable.add(MSAtomTable::CharacterPosition,XInternAtom(d,"CHARACTER_POSITION",False));
  _atomTable.add(MSAtomTable::Span,XInternAtom(d,"SPAN",False));
}

void MSDisplayServer::copyBuffer(const MSString& aString_)
{ _copyBuffer=aString_; }
void MSDisplayServer::pasteBuffer(const MSString& aString_)
{ _pasteBuffer=aString_; }

void MSDisplayServer::defaultFont(const char *font_)
{_defaultFont=fontID(font_);}
void MSDisplayServer::defaultForeground(const char *fg_)
{_defaultFg=pixel(fg_);}
void MSDisplayServer::defaultBackground(const char *bg_)
{_defaultBg=pixel(bg_);}

void MSDisplayServer::defaultValueBackground(const char *bg_)
{_defaultValueBg=pixel(bg_);}
void MSDisplayServer::defaultHighlightColor(const char *c_)
{_defaultHighlightColor=pixel(c_);}

unsigned long MSDisplayServer::pixel(const char *color_)     
{return _colorManager->pixel(color_);}
const char *MSDisplayServer::colorName(unsigned long pixel_) const
{return _colorManager->colorName(pixel_);}
Colormap MSDisplayServer::colormap(void) const
{return _colorManager->colormap();}
Font MSDisplayServer::fontID(const char *font_)              
{return _fontManager->fontID(font_);}
const char *MSDisplayServer::fontName(Font fid_) const
{return _fontManager->fontName(fid_);}
const XFontStruct *MSDisplayServer::fontStruct(Font fid_) const
{return _fontManager->fontStruct(fid_);}
Cursor MSDisplayServer::watchCursor(void) const
{return _watchCursor->cursor();}

MSToolTip *MSDisplayServer::toolTip(void)
{
  if (_toolTip==0) _toolTip=new MSToolTip(this);
  return _toolTip;
}

int MSDisplayServer::propertyFullLength(void)
{ return MSPropertyFullLength; }

Display *MSDisplayServer::defaultDisplay(void)
{
  if (_defaultDisplayServer==0)
   {
     fprintf(stderr, "No Default Server Established: exit in DefaultDisplay()\n");
     exitOnError();   
   }
  return _defaultDisplayServer->_dpy;
}

MSDisplayServer *MSDisplayServer::defaultDisplayServer(void)
{
  if (_defaultDisplayServer==0)
   {
     fprintf(stderr, "No Default Server Established: exit in defaultDisplayServer()\n");
     exitOnError();        
   }
  return _defaultDisplayServer;
}

MSDisplayServer *MSDisplayServer::serverOfDisplay(Display *dpy_)
{ return ((MSDisplayServer *)applicationDisplayServer(dpy_)); }

int serverConnection(MSDisplayServer *pServer_)
{
  if (pServer_==0) pServer_=MSDisplayServer::defaultDisplayServer();   
  return pServer_->connection();
}  

MSWidget *MSDisplayServer::grabWidget(void) const
{
  if (_passiveGrabList->length()>0)
   {
     return (MSWidget *)((const MSWidgetVector *)_passiveGrabList)->lastElement();
   }
  else return (MSWidget *)0;
}

void MSDisplayServer::addPassiveGrab(MSWidget *pWidget_)
{
  if (pWidget_!= 0)
   {
     unsigned index=_passiveGrabList->indexOf((unsigned long)pWidget_);
     if (index==_passiveGrabList->length())   _passiveGrabList->append(pWidget_);
   }
}

void MSDisplayServer::removePassiveGrab(MSWidget *pWidget_)
{
  unsigned index=_passiveGrabList->indexOf((unsigned long)pWidget_);
  if (index!=_passiveGrabList->length()) _passiveGrabList->removeAt(index);
}

MSBoolean MSDisplayServer::eventGrabbed(const XEvent *event_,MSWidget *pWidget_) const
{
  if (event_->xany.window==_pointerGrabber) return MSTrue;
  if (event_->xany.window==_keyboardGrabber) return MSTrue;
  if (_passiveGrabList->length()>0)
   {
     MSWidget *topWidget=pWidget_->top();
     MSWidget *pWidget=(MSWidget *)((const MSWidgetVector *)_passiveGrabList)->lastElement();
     if (pWidget==topWidget) return MSTrue;
     else return MSFalse;
   }
  else return MSTrue;
}

int MSDisplayServer::grabPointer(Window grabWindow_,int ownerEvent_,
				 unsigned eventMask_,int pointerMode_,int keyboardMode_,
				 Window confineTo_,Cursor cursor_,Time time_,MSBoolean revertBack_)
{
  int ret=XGrabPointer(_dpy,grabWindow_,ownerEvent_,eventMask_,pointerMode_,keyboardMode_,
                       confineTo_,cursor_,time_);
  if (ret==GrabSuccess)
   {
     _pointerGrabber=grabWindow_;
     if (revertBack_==MSTrue)
      {
	PointerGrabber *pGrabber=
          new PointerGrabber(grabWindow_,ownerEvent_,eventMask_,pointerMode_,
                             keyboardMode_,confineTo_,cursor_,time_);

	MSBoolean found=MSFalse;
        MSUnsignedLongVector *pGrabList=(MSUnsignedLongVector *)_pointerGrabList;
        
        for (int i=0;i<pGrabList->length();i++)
         {
           PointerGrabber *pGrabWidget=(PointerGrabber *) ((*pGrabList)(i));
           if (pGrabWidget->_grabWindow==grabWindow_)
            {
              found=MSTrue;
              delete pGrabWidget;
              pGrabList->replaceAt(i,(unsigned long)pGrabber);
              break;
            }
         }
        if (found==MSFalse)
	 {
           pGrabList->append((unsigned long)pGrabber);
	 }
      }
   }
  return ret;
}

int MSDisplayServer::grabKeyboard(Window grabWindow_,int ownerEvent_,int pointerMode_,
				  int keyboardMode_,Time time_,MSBoolean revertBack_)
{
  int ret=XGrabKeyboard(_dpy,grabWindow_,ownerEvent_,pointerMode_,keyboardMode_,time_);
  if (ret==GrabSuccess)
   {
     _keyboardGrabber=grabWindow_;
     if (revertBack_==MSTrue)
      {
	KeyboardGrabber *pGrabber=
          new KeyboardGrabber(grabWindow_,ownerEvent_,pointerMode_,
                              keyboardMode_,time_);
        
	MSBoolean found=MSFalse;
        MSUnsignedLongVector *pGrabList=(MSUnsignedLongVector *)_keyboardGrabList;
        
        for(int i=0;i<pGrabList->length();i++)
         {
           KeyboardGrabber *pGrabWidget=(KeyboardGrabber *) ((*pGrabList)(i));
           if (pGrabWidget->_grabWindow==grabWindow_)
            {
              found=MSTrue;
              delete pGrabWidget;
              pGrabList->replaceAt(i,(unsigned long)pGrabber);
              break;
            }
         }
	if (found==MSFalse)
	 {
           pGrabList->append((unsigned long)pGrabber);
	 }
      }
   }
  return ret;
}

void MSDisplayServer::ungrabPointer(Window window_,Time time)
{
  MSUnsignedLongVector *pGrabList=(MSUnsignedLongVector *)_pointerGrabList;
  if (pGrabList->length()>0)
   {
     MSBoolean found=MSFalse;
     int index=pGrabList->length()-1;
     while (found==MSFalse&&index>=0)
      {
	PointerGrabber *pGrabWidget=(PointerGrabber *)((*pGrabList)(index));
        
	// If the widget asking for ungrab is in the grab list, we will
	// revert the grab to the next one in the list, and remove all
	// the entries that come before the asking widget.
	if (pGrabWidget->_grabWindow==window_)
	 {
	   found=MSTrue;
           if (index-1>=0)
	    {
	      PointerGrabber *pGrabber=(PointerGrabber *)((*pGrabList)(index-1));
	      int ret=XGrabPointer(_dpy,pGrabber->_grabWindow,pGrabber->_ownerEvents,
				   pGrabber->_eventMask,pGrabber->_pointerMode,
                                   pGrabber->_keyboardMode,
				   pGrabber->_confineTo,pGrabber->_cursor,pGrabber->_time);
	      if (ret!=GrabSuccess)
	       {
		 fprintf(stderr, "Warning : Unable to revert pointer grab to Window %ld\n",pGrabber->_grabWindow);
		 _pointerGrabber=0;
	       }
	      else _pointerGrabber=pGrabber->_grabWindow;
	    }
           else
	    {
	      XUngrabPointer(_dpy,time);
	      _pointerGrabber=0;
	    }
           for(int j=index;j<pGrabList->length();j++)
            {
	      PointerGrabber *pGrabber=(PointerGrabber *)((*pGrabList)(j));
	      delete pGrabber;
            }
           pGrabList->removeAt(index,pGrabList->length()-index);
	 }
        index--;
      }
     // If we can't find matching window in the grab list, we'll just
     // revert back to the first one on the list
     if (found==MSFalse)
      {
	PointerGrabber *pGrabber=(PointerGrabber *)
          ((const MSWidgetVector *)pGrabList)->lastElement();
	int ret=XGrabPointer(_dpy,pGrabber->_grabWindow,pGrabber->_ownerEvents,
			     pGrabber->_eventMask,pGrabber->_pointerMode,pGrabber->_keyboardMode,
			     pGrabber->_confineTo,pGrabber->_cursor,pGrabber->_time);
	if (ret!=GrabSuccess)
	 {
	   fprintf(stderr, "Warning : Unable to revert pointer grab to Window %ld\n",pGrabber->_grabWindow);
	   _pointerGrabber=0;
	 }
	else _pointerGrabber=pGrabber->_grabWindow;
      }
   }
  else
   {
     XUngrabPointer(_dpy,time);
     _pointerGrabber=0;
   }
}

void MSDisplayServer::ungrabKeyboard(Window window_,Time time)
{
  MSUnsignedLongVector *pGrabList=(MSUnsignedLongVector *)_keyboardGrabList;
  if (pGrabList->length()>0)
   {
     int index=pGrabList->length()-1;
     MSBoolean found=MSFalse;
     
     while (found==MSFalse&&index>=0)
      {
	KeyboardGrabber *pGrabWidget=(KeyboardGrabber *)((*pGrabList)(index));
	// If the widget asking for ungrab is in the grab list, we will
	// revert the grab to the next one in the list, and remove all
	// the entries that come before the asking widget.
	if (pGrabWidget->_grabWindow==window_)
	 {
	   found=MSTrue;
	   if (index-1>=0)
	    {
	      KeyboardGrabber *pGrabber=(KeyboardGrabber *)((*pGrabList)(index-1));
	      int ret=XGrabKeyboard(_dpy,pGrabber->_grabWindow,pGrabber->_ownerEvents,
				    pGrabber->_pointerMode,pGrabber->_keyboardMode,pGrabber->_time);
	      if (ret!=GrabSuccess)
	       {
		 fprintf(stderr, "Warning : Unable to revert keyboard grab to Window %ld\n",pGrabber->_grabWindow);
		 _keyboardGrabber=0;
	       }
	      else _keyboardGrabber=pGrabber->_grabWindow;
	    }
	   else
	    {
	      XUngrabKeyboard(_dpy,time);
	      _keyboardGrabber=0;
	    }

           for(int j=index;j<pGrabList->length();j++)
            {
	      KeyboardGrabber *pGrabber=(KeyboardGrabber *)((*pGrabList)(j));
	      delete pGrabber;
            }
           pGrabList->removeAt(index, pGrabList->length()-index);
	 }
        index--;
      }
     // If we can't find matching window in the grab list, we'll just
     // revert back to the first one on the list
     if (found==MSFalse)
      {
	KeyboardGrabber *pGrabber=(KeyboardGrabber *)
          ((const MSWidgetVector *)pGrabList)->lastElement();
	int ret=XGrabKeyboard(_dpy,pGrabber->_grabWindow,pGrabber->_ownerEvents,
			      pGrabber->_pointerMode,pGrabber->_keyboardMode,pGrabber->_time);
	if (ret!=GrabSuccess)
	 {
	   fprintf(stderr, "Warning : Unable to revert keyboard grab to Window %ld\n",pGrabber->_grabWindow);
	   _keyboardGrabber=0;
	 }
	else _keyboardGrabber=pGrabber->_grabWindow;
      }
   }
  else
   {
     XUngrabKeyboard(_dpy,time);
     _keyboardGrabber=0;
   }
} 

// MSDisplayServer access methods

int MSDisplayServer::screenNum(void) const   
{ return DefaultScreen(_dpy); }
Screen *MSDisplayServer::screen(void) const   
{ return DefaultScreenOfDisplay(_dpy); }
int MSDisplayServer::depth(void) const   
{ return DefaultDepthOfScreen(screen()); }
Visual *MSDisplayServer::visual(void) const   
{ return DefaultVisual(_dpy,DefaultScreen(_dpy)); }
Window MSDisplayServer::root(void) const     
{ return RootWindow(_dpy,DefaultScreen(_dpy)); }
int MSDisplayServer::connection(void) const  
{ return ConnectionNumber(_dpy); }
int MSDisplayServer::width(void) const       
{ return DisplayWidth(_dpy,DefaultScreen(_dpy)); }
int MSDisplayServer::widthMM(void) const     
{ return DisplayWidthMM(_dpy,DefaultScreen(_dpy)); }
int MSDisplayServer::height(void) const      
{ return DisplayHeight(_dpy,DefaultScreen(_dpy)); }
int MSDisplayServer::heightMM(void) const    
{ return DisplayHeightMM(_dpy,DefaultScreen(_dpy)); }
Atom MSDisplayServer::atom(int index_) const
{ return _atomTable.atom(index_); }

// ##############################################################################
// CDE WindowManager functionality
// ##############################################################################

void MSDisplayServer::workspaceChangedFunction(MSWorkspaceChangedFunction pFunction_)
{ _workspaceChangedFunction=pFunction_; }

void MSDisplayServer::currentWorkspaceChangedNotify(Atom currentWorkspaceAtom_)
{ if (_workspaceChangedFunction!=0) _workspaceChangedFunction(currentWorkspaceAtom_); }

MSBoolean MSDisplayServer::isCDERunning(void) const
{
  if (_windowManager==MWM&&_mwmWindow!=0) return MSTrue;
  return MSFalse;
}

MSStringVector MSDisplayServer::workspaceNames(void) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->workspaceNames();
  return MSStringVector();
}

MSString MSDisplayServer::currentWorkspaceName(void) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->currentWorkspaceName();
  return MSString();
}

Atom MSDisplayServer::currentWorkspaceAtom(void) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->currentWorkspaceAtom();
  return 0;
}


unsigned long MSDisplayServer::numberOfWorkspaces(void) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->numberOfWorkspaces();
  return 0;
}


MSString MSDisplayServer::workspaceName(Atom wsAtom_) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->workspaceName(wsAtom_);
  return MSString();
}


Atom MSDisplayServer::workspaceAtom(const MSString& wsName_) const
{
  if (isCDERunning()==MSTrue) return _pMWMWidget->workspaceAtom(wsName_);
  return 0;
}


MSBoolean MSDisplayServer::changeWorkspaceTo(const MSString& workspaceName_)
{
  if (isCDERunning()==MSTrue)
   {
     MSString theRequest("f.goto_workspace \"");
     theRequest+=workspaceName_;
     theRequest+='\"';

     Atom property=XInternAtom(display(),_XA_DT_WM_REQUEST,False);
     // Make the request by appending the request
     // name to the _VUE_WM_REQUEST property
     XChangeProperty(display(),_mwmWindow, 
                     property,XA_STRING,8, 
                     PropModeAppend, 
                     (unsigned char *)theRequest.string(), 
                     theRequest.length()+1);
     XFlush(display());
     return MSTrue;
   }
  return MSFalse;
}

void MSDisplayServer::setWindowManager(void)
{
  Atom	         actualType;
  int		 actualFormat;
  unsigned long  numItems,bytesAfter;
  MWMInfo       *pMWMInfo=0;
  
  Atom mwmInfoAtom=XInternAtom(_dpy,_XA_MWM_INFO,False);
  XGetWindowProperty(_dpy,root(),mwmInfoAtom,0,2,False,mwmInfoAtom,
		     &actualType,&actualFormat,&numItems,&bytesAfter,
                     (unsigned char **)&pMWMInfo);
  
  if ((actualType!=mwmInfoAtom)||(actualFormat!=32)||(numItems<2)) _windowManager=Other;
  else
   {
     Window   	  top,parent,*pChildren=0;
     unsigned int numChildren;

     // set the motif window manager special window
     _mwmWindow=(Window)pMWMInfo->_wmWindow;
     
     if (XQueryTree(_dpy,root(),&top,&parent,&pChildren,&numChildren))
      {
	unsigned i;
	for (i=0;i<numChildren&&pChildren[i]!=_mwmWindow;i++);
	_windowManager=(i==numChildren)?Other:MWM;
      }
     else _windowManager=Other;
     if (pChildren!=0) XFree((char *)pChildren);
   }
  if (pMWMInfo!=0) XFree((char *)pMWMInfo);
  if (isCDERunning()==MSTrue) _pMWMWidget=new MWMWidget(this,_mwmWindow);
}	

void MSDisplayServer::menuGrabCursor(MSDisplayCursor* cursor_)
{
  if(_menuGrabCursor!=0) delete _menuGrabCursor;
  _menuGrabCursor=cursor_;
}

// ####################################################################
// MWMWidget implementation
// allows us to get VUE window manager notification
// ####################################################################
MWMWidget::MWMWidget(MSDisplayServer *pDisplayServer_,Window window_) :
MSWidget(pDisplayServer_), _workspaceInfoAtoms(0), _workspaceAtoms(0), _workspaceCount(0)
{
  if (window_!=0)
   {
     _mwmWindow=window_;
     _workspaceCurrentAtom=XInternAtom(display(),_XA_DT_WORKSPACE_CURRENT,False);
     _workspaceListAtom=XInternAtom(display(),_XA_DT_WORKSPACE_LIST,False);
     _wmStateAtom=XInternAtom(display(),"WM_STATE",False);     
     server()->widgetHashTable()->add(window_,this);
     XSelectInput(display(),window_,PropertyChangeMask);
     updateWorkspaceList();
     updateCurrentWorkspaceName();
   }
}

MWMWidget::~MWMWidget(void)
{
  delete [] _workspaceInfoAtoms;
  delete [] _workspaceAtoms;
}

void MWMWidget::updateWorkspaceInfo(Atom workspaceInfoAtom_)
{
  unsigned n=numberOfWorkspaces();
  for (unsigned i=0;i<n;i++)
   {
     if (_workspaceInfoAtoms[i]==workspaceInfoAtom_)
      {
        _workspaceNames[i]=getWorkspaceName(_workspaceAtoms[i]);
        break;
      }
   }
}

void MWMWidget::updateWorkspaceList(void)
{
  Atom          *pWsAtoms=0;
  Atom           actualType;
  int            actualFormat;
  unsigned long  lcount;
  unsigned long  leftover;
  
  int status=XGetWindowProperty(display(),_mwmWindow,
				_workspaceListAtom,0L,(long)BUFSIZ,
				False,XA_ATOM,
				&actualType,&actualFormat,
				&lcount,&leftover,
				(unsigned char**)&pWsAtoms);
      
  if (status==Success && actualType==XA_ATOM)
    {
      if (lcount!=_workspaceCount)   // if the number of workspaces was changed
	{
	  // reallocate the workspace atoms list and the workspace info atoms list
	  //
	  _workspaceCount = lcount;
	  delete [] _workspaceAtoms;
	  delete [] _workspaceInfoAtoms;
	  if (_workspaceCount>0)
	    {
	      _workspaceAtoms = new Atom[_workspaceCount];
	      _workspaceInfoAtoms = new Atom[_workspaceCount];
	    }
	  else
	    {
	      _workspaceAtoms = 0;
	      _workspaceInfoAtoms = 0;
	    }
	}

      memcpy(_workspaceAtoms,pWsAtoms,(int)_workspaceCount*sizeof(Atom)); // copy the new atoms
      //
      // copy the new workspace info atoms
      //
      MSString wsPropName;
      char *name;
      for (unsigned i=0;i<_workspaceCount;i++)
	{
	  wsPropName=_XA_DT_WORKSPACE_INFO;
	  name=XGetAtomName(display(),_workspaceAtoms[i]);
	  wsPropName+=name;
	  if(name!=0) XFree(name);
	  _workspaceInfoAtoms[i]=XInternAtom(display(),(char *)wsPropName.string(),False);
	}
    }

  if (pWsAtoms!=0)
    {
      XFree((char*)pWsAtoms);
    }

  _workspaceNames=getWorkspaceNames();
}

void MWMWidget::updateCurrentWorkspaceName(void)
{
  if (_mwmWindow!=0)
   {
     Atom	   actualType;
     int	   actualFormat;
     unsigned long numItems,bytesAfter;
     Atom          *pWorkspaceAtom=0;

     int status=XGetWindowProperty(display(),_mwmWindow,_workspaceCurrentAtom,
                                   0,1,False,XA_ATOM,
                                   &actualType,&actualFormat,&numItems,&bytesAfter,
                                   (unsigned char **)&pWorkspaceAtom);
     if (status==Success)
      {
        if (actualType==XA_ATOM&&numItems==1&&actualFormat==32)
         {
           Atom workspaceAtom=pWorkspaceAtom[0];
           if (pWorkspaceAtom!=0) XFree((char *)pWorkspaceAtom);
           _currentWorkspaceAtom=workspaceAtom;           
           _currentWorkspaceName=getWorkspaceName(_currentWorkspaceAtom);
           server()->currentWorkspaceChangedNotify(_currentWorkspaceAtom);           
         }
      }
   }
}

MSStringVector MWMWidget::getWorkspaceNames(void)
{
  if (_mwmWindow!=0 && _workspaceCount>0)
   {
     MSStringVector names(_workspaceCount);
     for (unsigned i=0; i<_workspaceCount; ++i)
       {
	 names.replaceAt(i,getWorkspaceName(_workspaceAtoms[i]));
       }
     return names;
   }
  else
    {
      return MSStringVector();
    }
}

MSString MWMWidget::getWorkspaceName(Atom workspaceAtom_) const
{
  MSString theName;
  if (_mwmWindow!=0)
   {
     Atom	   actualType;
     int	   actualFormat;
     unsigned long numItems,bytesAfter;
     char *pString=XGetAtomName(display(),workspaceAtom_);
     MSString wsPropName=_XA_DT_WORKSPACE_INFO;
     wsPropName+=pString;
     Atom wsAtom=XInternAtom(display(),(char *)wsPropName.string(),False);
     unsigned char *data=0;
     int status=XGetWindowProperty(display(),_mwmWindow,wsAtom,
                                   0,MSDisplayServer::propertyFullLength(),
                                   False,XA_STRING,
                                   &actualType,&actualFormat,
                                   &numItems,&bytesAfter,&data);
     if (status==Success&&actualType==XA_STRING) theName=(char *)data;
     if (data!=0) XFree((char *)data);
     if (pString!=0) XFree(pString);
   }
  return theName;
}

void MWMWidget::propertyNotify(const XEvent *pEvent_)
{
  Atom property=pEvent_->xproperty.atom;
  if (property!=_wmStateAtom)
   {
     if (property==_workspaceCurrentAtom) updateCurrentWorkspaceName();
     else if (property==_workspaceListAtom) updateWorkspaceList();
     else updateWorkspaceInfo(property);
   }
}


MSString MWMWidget::workspaceName(Atom wsAtom_) const
{
  for (unsigned long i=0; i<_workspaceCount; ++i)
    {
      if (_workspaceAtoms[i]==wsAtom_)
	{
	  return _workspaceNames(i);
	}
    }

  return MSString();
}


Atom MWMWidget::workspaceAtom(const MSString& wsName_) const
{
  unsigned index=_workspaceNames.indexOf(wsName_);
  if (index<_workspaceCount)
    {
      return _workspaceAtoms[index];
    }
  else	// wsName_ is not one of alias names; possibly, an internal name (e.g.,"ws0")
    {
      return XInternAtom(display(),wsName_.string(),False);
    }
}

#ifdef MS_KEYPAD_BUG
void MSDisplayServer::correctKeypadKeys(const XEvent *pEvent_,KeySym &keysym_,unsigned int state_,char * pString_)
{
  KeyCode keycode = ((XKeyEvent*)pEvent_)->keycode;
  
  if (state_ == _numLockModifier)
   {
     for (int i=0;i<keypadDualModeElements
;i++)
      {
        if ( _keypadKeycodes[i] == keycode)
         {
           if( keysym_ != keypad_table[i].keysym)
            {
              strcpy(pString_, keypad_table[i].string);
              keysym_ = keypad_table[i].keysym;
            }
           return;
         }
      }
   }
  
  for (int i=keypadDualModeElements;i<numKeypadKeys;i++)
   {
     if ( _keypadKeycodes[i] == keycode)
      {
        if( keysym_ != keypad_table[i].keysym)
         {
           strcpy(pString_, keypad_table[i].string);
           keysym_ = keypad_table[i].keysym;
         }
        return;
      }
   }  
}

#endif
