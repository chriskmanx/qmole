#ifndef MSDisplayServerHEADER
#define MSDisplayServerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSDefines.H>

class MSDisplayCursor;
class MSFontManager;
class MSColorManager;
class MSWidget;
class MSToolTip;
class MSChannel;
class MSVector;
class MSWidgetVector;
class MWMWidget;

template <class Type> class MSTypeVector;
typedef MSTypeVector<MSString> MSStringVector;

class MSGUIExport MSAtomTable 
{
protected:
  Atom  *_array;
public:
  enum AtomName {WMState=0,WMDeleteWindow=1,WMProtocols=2,MStk=3,
                 DecorAdd=5,DecorDel=6,DecorFooter=7,DecorHeader=8,DecorPin=9,
                 DecorResize=10,LeftFooter=11,RightFooter=12,MenuFull=13,MenuLimited=14,
                 PushpinState=15,PinIn=16,PinOut=17,WindowBusy=18,WinAttr=19,
                 WTBase=20,WTCmd=21,WTNotice=22,WTOther=23,
                 Targets=25,ClientWindow=26,TimeStamp=27,Length=28,
                 CharacterPosition=29,Span=30,WMSaveYourself=31,LastAtom=32};

  MSAtomTable(void); 
  ~MSAtomTable(void); 
  
  void add(int index_,Atom atom_);
  Atom atom(int index_) const;
};

typedef void (*MSWorkspaceChangedFunction)(Atom);

class MSGUIExport MSDisplayServer 
{
private:
friend class MSDisplayServerChannel;
friend class MWMWidget;
public:
  enum QueueStatus {NoQueueAfterSelect=0,QueueOk=1};

  MSDisplayServer(void);
  MSDisplayServer(const char *name_);
  ~MSDisplayServer(void);

  Display *display(void) const;
  int screenNum(void) const;
  int depth(void) const;
  Screen *screen(void) const;
  Visual *visual(void) const;
  Window root(void) const;
  int connection(void) const;
  int width(void) const;
  int widthMM(void) const;
  int height(void) const;
  int heightMM(void) const;

  const char *name(void) const;
  MSColorManager *colorManager(void) const;
  MSFontManager *fontManager(void) const;
  MSAtomTable& atomTable(void);
  const MSAtomTable& atomTable(void) const;
  MSHashTable *shadowHashTable(void) const;
  MSHashTable *widgetHashTable(void) const;
  const MSString& copyBuffer(void) const;
  const MSString& pasteBuffer(void) const;
  Atom atom(int index_) const;
  MSDisplayCursor *watch(void) const;
  Font defaultFont(void) const;
  unsigned long defaultForeground(void) const;
  unsigned long defaultBackground(void) const;
  Colormap colormap(void) const;
  Cursor watchCursor(void) const;
  MSHashTable *toolTipHashTable(void) const;
  MSToolTip *toolTip(void);
  
  void processOneEvent(void);
  void dispatch(void);
  MSBoolean flush(void);
  void bell(void);
  static MSBoolean flush(Display *);

  void defaultFont(const char *);
  void defaultForeground(const char *);
  void defaultBackground(const char *);
  void copyBuffer(const MSString&);
  void pasteBuffer(const MSString&);

  unsigned long defaultHighlightColor(void) const;
  void defaultHighlightColor(const char *);
  
  unsigned long defaultValueBackground(void) const;
  void defaultValueBackground(const char *);
  
  void addPassiveGrab(MSWidget *);
  void removePassiveGrab(MSWidget *);
  MSWidget *grabWidget(void) const;
  MSBoolean eventGrabbed(const XEvent *,MSWidget *) const;

  int grabPointer(Window,int,unsigned,int,int,Window,Cursor,Time,MSBoolean revertBack_=MSFalse);
  int grabKeyboard(Window,int,int,int,Time,MSBoolean revertBack_=MSFalse);
  void ungrabPointer(Window,Time);
  void ungrabKeyboard(Window,Time);
  Window keyboardGrabber(void) const;
  Window pointerGrabber(void) const;

  MSBoolean isCDERunning(void) const;
  MSStringVector workspaceNames(void) const;
  MSString currentWorkspaceName(void) const;
  Atom currentWorkspaceAtom(void) const;
  MSBoolean changeWorkspaceTo(const MSString& workspaceName_);
  unsigned long numberOfWorkspaces(void) const;
  MSString workspaceName(Atom) const;
  Atom workspaceAtom(const MSString&) const;

  static void workspaceChangedFunction(MSWorkspaceChangedFunction);

  unsigned long pixel(const char *colorString_);
  const char *colorName(unsigned long pixel_) const;
  Font fontID(const char *fontString_);
  const char *fontName(Font fontID_) const;
  const XFontStruct *fontStruct(Font fontID_) const;

  int ioErrorHandler(void);
  int errorHandler(const XErrorEvent *);

  const MSWidget *primarySelectionOwner(void) const;
  MSWidget *primarySelectionOwner(void);
  void primarySelectionOwner(MSWidget *);

  static void exitOnError(void);
  static Display *defaultDisplay(void);
  static MSDisplayServer *defaultDisplayServer(void);
  static MSDisplayServer* serverOfDisplay(Display *);
  static int propertyFullLength(void);

  MSWidget* menuGrabber(void);
  void menuGrabber(MSWidget*);
  MSWidget *menuBar(void);
  void menuBar(MSWidget*);
  MSDisplayCursor *menuGrabCursor(void);
  void menuGrabCursor(MSDisplayCursor*);
  
  MSWidget *scrollBarMenu(void);
  void scrollBarMenu(MSWidget*);
#ifdef MS_KEYPAD_BUG
  void correctKeypadKeys(const XEvent *,KeySym&,unsigned int,char *);
#endif
public:
  // this should be protected, but lcc has a problem
  enum WindowManager {Unset=0,MWM=1,Other=2}; 
protected:

  struct MWMInfo
  {
    enum Flags {StartupStandard=1,StartupCustom=2};
  
    long   _flags;
    Window _wmWindow;
  };

  static MSDisplayServer            *_defaultDisplayServer;
  static MSWorkspaceChangedFunction  _workspaceChangedFunction;
  
  Display                      *_dpy;
  MSColorManager               *_colorManager;     // holds hashTable of colors in use
  MSFontManager                *_fontManager;      // holds hashTable of fonts in use
  MSDisplayCursor              *_watchCursor;      // default watch cursor for this connection 
  MSHashTable                  *_shadowHashTable;  // hashTable of shadow objects - keys are bg color
  MSHashTable                  *_widgetHashTable;  // hashTable of widgets - keys are window ids
  MSHashTable                  *_toolTipHashTable; // hashTable of toolTips - keys are widget pointer
  MSChannel                    *_channel;
  MSAtomTable                   _atomTable;
  MSString                      _copyBuffer;       // buffer to hold primary selection for copying to
  MSString                      _pasteBuffer;      // buffer to hold primary selection for pasting 
  MSString                      _name;
  unsigned long                 _defaultFg;
  unsigned long                 _defaultBg;
  Font                          _defaultFont;
  MSDisplayServer::QueueStatus  _status;
  MSVector                     *_keyboardGrabList;
  MSVector                     *_pointerGrabList;
  MSWidgetVector               *_passiveGrabList;
  Window                        _keyboardGrabber;
  Window                        _pointerGrabber;
  MSToolTip                    *_toolTip;
  WindowManager                 _windowManager;
  Window                        _mwmWindow;
  MWMWidget                    *_pMWMWidget;
  unsigned long                 _defaultHighlightColor;
  unsigned long                 _defaultValueBg;
  MSWidget                     *_primarySelectionOwner;
  MSDisplayCursor              *_menuGrabCursor;
  MSWidget                     *_menuBar;
  MSWidget                     *_menuGrabber;
  MSWidget                     *_scrollBarMenu;

#ifdef MS_KEYPAD_BUG
  KeyCode                      *_keypadKeycodes;
  unsigned int                  _numLockModifier;
  void setKeypadCodes(void);
#endif
  
  void init(void);
  void initAtoms(void);
  void processXEvents(void);
  MSBoolean process(void);
  void service(void);
  void dispatchEvent(XEvent&);
  void setWindowManager(void);
  void currentWorkspaceChangedNotify(Atom);
};

#ifndef MS_NO_INLINES
#ifndef MSDisplayServerINLINES
#include <MSGUI/MSDisplayServerInlines.C>
#endif
#endif

#endif







