#ifndef MSWidgetHEADER
#define MSWidgetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


// this is a big list, but we will build a precompiled
// header for MSWidget.H and then having these here is
// a bonus - i.e. they will all be pre-compiled.

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSCallback.H>
#include <MSTypes/MSCallbackBehavior.H>
#include <MSGUI/MSKeysym.H>
#include <MSGUI/MSRect.H>
#include <MSGUI/MSDisplayServer.H>
#include <MSGUI/MSDisplayCursor.H>
#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSDefaults.H>
#include <MSGUI/MSWidgetCallback.H>
#include <MSGUI/MSWidgetVector.H>
#include <MSGUI/MSWidgetIterator.H>
#include <MSGUI/At.H>  
#include <MSGUI/MSAttrValue.H>
#include <MSGUI/MSKeyTranslationTable.H>
#include <MSGUI/MSKeyCallback.H>

typedef void (*MSWidgetCallbackFunction)(MSWidget*, void*);

class MSGUIExport MSWidget : public MSRect, public MSCallbackBehavior
{
friend class MSDisplayServer;
public:
  MSWidget(void);
  MSWidget(MSDisplayServer *server_,int x_=0,int y_=0,int w_=10,int h_=10);
  MSWidget(MSWidget *owner_,int x_=0,int y_=0,int w_=10,int h_=10);
  ~MSWidget(void);
  
  // Mutator Methods
  void move(int,int);             // move by delta x,y
  virtual void moveTo(int,int);   // move to absolute x,y
  virtual void resize(int,int);   // resize the widget
  void warpTo(int x_=0,int y_=0); // warp the cursor to x,y relative to this window
  void width(int);
  void height(int);
  void reparent(MSWidget *);
  void background(unsigned long);  
  void foreground(unsigned long);
  void font(Font);
  void background(const char *);  
  void foreground(const char *);
  void font(const char *);
  void sensitive(MSBoolean);
  void readOnly(MSBoolean);
  virtual void freeze(void);
  virtual void unfreeze(void);
  void acceptFocus(MSBoolean);
  void dynamic(MSBoolean);
  void helpString(const MSString&);

  // Position/Constraint Management Methods
  void resizeConstraints(const char *opts_);
  void resizeConstraints(unsigned long);
  void at(const At& at_);
  At at(void);

  // Widget Instance Name
  const MSString &instanceName(void) const;
  void instanceName(const MSString &);
  MSString instanceFullname(void) const;
  
  // Access Methods
  MSWidget *owner(void);
  const MSWidget *owner(void) const;  
  MSDisplayServer *server(void);
  const MSDisplayServer *server(void) const;
  Display *display(void) const;
  Window window(void) const;   
  Colormap colormap(void) const;              
  MSColorManager *colorManager(void);
  const MSColorManager *colorManager(void) const;
  MSFontManager *fontManager(void);
  const MSFontManager *fontManager(void) const;
  int x_origin(void) const;
  int y_origin(void) const;              
  int width(void) const;              
  int height(void) const;                 
  unsigned long background(void) const;
  unsigned long foreground(void) const;  
  Font font(void) const;  
  MSBoolean sensitive(void) const;
  MSBoolean readOnly(void) const;
  MSBoolean acceptFocus(void) const;       
  MSBoolean acceptTab(void) const;     
  MSBoolean dynamic(void) const;       
  MSBoolean visible(void) const;         
  MSBoolean frozen(void) const;         
  MSBoolean firstMap(void) const;          
  MSBoolean mapped(void) const;        
  unsigned long resizeConstraints(void) const;
  long eventMask(void) const;  
  Screen *screen(int) const;
  Screen *screen(void) const;             
  int depth(void) const;            
  Window parent(void) const;
  MSWidget *widget(Window);
  MSWidget *top(void);
  const MSWidget *widget(Window) const;
  const MSWidget *top(void) const;
  const MSString& helpString(void) const;
  virtual const MSString& virtualHelpString(int x_,int y_);
  
  // Position Access Methods
  int rootX(void) const;
  int rootY(void) const;
  void rootXY(int& x,int& y) const;    // get the root coords of this window
  void pointerXY(int& x,int& y) const; // get the pointer x,y in root coords

  // Window methods
  virtual void redraw(void);
  virtual void print(const char *file_=0);
  virtual void map(void);
  virtual void unmap(void);
  virtual void show(void);
  virtual void hide(void);
  virtual void raise(void);
  virtual void lower(void);
  virtual void clear(void);
  virtual void naturalSize(void);
  virtual void destroy(void);
  virtual void safeDestroy(MSWidget *);

  // Focus/Traverse Methods
  void addToFocusList(void);
  void removeFromFocusList(void);
  virtual MSWidget *inputFocus(void);
  virtual const MSWidget *inputFocus(void) const;
  virtual MSBoolean traverseFocus(MSWidget *);
  virtual MSBoolean traverseToNext(void);
  virtual MSBoolean traverseToPrevious(void);  
  MSBoolean obtainFocus(void);
  MSBoolean isTraversable(void) const;
  MSBoolean isTraversable(MSWidget *) const;

  // Callback methods
  MSCallback *callback(const MSSymbol& name_);
  void callback(const MSSymbol& name_,MSCallback *callback_);
  void callback(const MSSymbol& name_,MSWidgetCallbackFunction function_,void *clientData_=0);

  // keyCallback methods
  unsigned long addKeyCallback( const char*, MSKeyCallback*);
  void removeKeyCallback(unsigned long);   
  void removeKeyCallback(const char*); 

  // busy cursor routines
  static void busyOn(void);
  static void busyOff(void);

  //children list and Iteration methods
  virtual MSWidgetVector children(void);
  MSBoolean allWidgetsDo(MSWidgetIterator& iterator_,
			 MSWidgetIterationType type_=MSBreadthFirst);
  MSBoolean allWidgetsDo(MSWidgetIteratorFunction function_, void *clientData_=0, 
			 MSWidgetIterationType type_=MSBreadthFirst);

  // methods for builder support
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  typedef void (*EventHandler)(MSWidget *widget_,const XEvent *event_,void *clientData_);
  void eventHandler(long mask_,EventHandler eventHandler_,void *clientData_=0);

  typedef MSBoolean (*EventOverride)(int eventType);
  void eventOverride(EventOverride);
  
  // will be removed when dynamic_cast (RTTI) becomes available
  const MSSymbol& parentWidgetType(void) const; 
  virtual const MSSymbol& widgetType(void) const;

  // X11 Selection Buffer Functions
  MSStatus copyPrimary(const char *data_,int itemCount_);
  void convertSelection(void);

  // focusWindow returns the shell that currently has the focus
  static MSWidget *focusWindow(void);
  static void focusWindow(MSWidget *);

  //Can this widget be modified by user.
  virtual MSBoolean isProtected(void) const;

protected:
  MSWidget              *_owner;
  Window                 _window;
  MSDisplayServer       *_server;
  unsigned long          _bg;
  unsigned long          _fg;
  Font                   _fontID;
  MSBoolean              _sensitive;     // is this widget greyed-out and sensitive to button/key events 
  MSBoolean              _readOnly;      // is this widget responsive to button/key events
  MSBoolean              _acceptFocus;   // will this widget accept focus
  MSBoolean              _acceptTab;     // does this widget accept a tab
  MSBoolean              _mapped;        // set on a map,unset on an unmap
  MSBoolean              _firstMap;      // set the first time a widget is mapped
  MSBoolean              _visible;       // is the widget visible
  MSBoolean              _dynamic;       // is the resize behavior dynamic?
  MSBoolean              _freezeStatus;  // is the widget diabled
  unsigned long          _resizeConstraints;
  long                   _eventMask;
  MSKeyTranslationTable  _keyTranslationTable;
  static MSWidget       *_focusWindow;
  EventHandler           _eventHandler;  // instance variables for builder support
  void                  *_eventHandlerData;
  EventOverride          _eventOverride;
  MSString               _helpString;
  MSString               _instanceName;
  
  MSKeyTranslationTable *keyTranslationTable(void);

  virtual int offsetX(void) const;
  virtual int offsetY(void) const;

  void init(void);
  virtual void computeSize(void);

  void eventMask(long);  
  void selectInput(long);
  void selectInput(void);
  void backingStore(int);
  void overrideRedirect(MSBoolean);
  void saveUnder(MSBoolean);
  void acceptTab(MSBoolean);
  void freezeStatus(MSBoolean);
  void visible(MSBoolean);
  void firstMap(MSBoolean);

  MSBoolean activateCallback(const MSSymbol&);
  MSBoolean activateCallback(MSWidget *,const MSSymbol&);

  virtual MSBoolean keyTranslate(const MSKeyPress&);
  MSBoolean hasKeyCallback(const MSKeyPress&) ;
  void removeAllKeyCallbacks();
  
  void prepareForDestruction(void);
  void decoupleAllWidgets(void);
  virtual void decoupleWidget(void);
  
  virtual void childPosition(MSWidget *,const At&);
  virtual At childPosition(MSWidget *);

  virtual void addToFocusList(MSWidget *);
  virtual void removeFromFocusList(MSWidget *);

  static MSBoolean setFocus(MSWidget *);
  static MSBoolean releaseFocus(void);

  // property methods
  virtual MSStatus ownSelection(Atom);
  virtual void disownSelection(Atom);
  virtual MSStatus propertyData(Atom prop_,Atom target_);
  virtual Atom convertTarget(Atom target_);
  virtual int propertyFormat(Atom target_);
  virtual void propertyToString(Atom target_,char *data_,int itemCount_);
  virtual void atomsFromData(char *data_,int itemCount_);

  XEvent *compressMotion(const XEvent *);

public:
  // update notification methods - i.e. notify the widget that its font has changed
  // the old value of the attribute is passed to the function
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateSensitivity(void);
  virtual void updateReadOnly(void);

protected:
  // X event methods
  virtual void expose(const XEvent *event_);
  virtual void noExpose(const XEvent *);
  virtual void graphicsExpose(const XEvent *);
  virtual void visibilityNotify(const XEvent *);
  virtual void keyPressEvent(const XEvent *);
  virtual void keyRelease(const XEvent *);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void motionNotify(const XEvent *);
  virtual void mapNotify(const XEvent *);
  virtual void unmapNotify(const XEvent *);
  virtual void configureNotify(const XEvent *);
  virtual void createNotify(const XEvent *);
  virtual void destroyNotify(const XEvent *);
  virtual void enterNotify(const XEvent *);
  virtual void leaveNotify(const XEvent *);
  virtual void focusInEventNotify(const XEvent *);
  virtual void focusOutEventNotify(const XEvent *);
  virtual void reparentNotify(const XEvent *);
  virtual void propertyNotify(const XEvent *);
  virtual void clientMessage(const XEvent *);
  virtual void selectionNotify(const XEvent *);
  virtual void selectionClear(const XEvent *);
  virtual void selectionRequest(const XEvent *);
  virtual void errorEvent(const XEvent *);
  virtual void event(const XEvent *);

  // synthetic events 
  virtual void configure(void);
  virtual void firstMapNotify(void);
  virtual void enter(void);
  virtual void leave(void);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void takeFocus(void);
  virtual MSBoolean loseFocus(void);

  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  virtual void button1Release(const XEvent *);
  virtual void button2Release(const XEvent *);
  virtual void button3Release(const XEvent *);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *,KeySym,unsigned int,const char *);
  virtual void key(KeySym,unsigned int,const char *);

  virtual void childMap(MSWidget *);
  virtual void childUnmap(MSWidget *);
  virtual void childConfigure(MSWidget *);
  virtual void childMove(MSWidget *);
  virtual void childResizeConstraints(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);

  // synthetic event generation
  // these events are delivered to the owner of 'this'
  void childMapNotify(void);
  void childUnmapNotify(void);
  void childConfigureNotify(void);
  void childMoveNotify(void);
  void childResizeConstraintsNotify(void);
  void childDestroyNotify(void);
  void childCreateNotify(void);
  void childInsertNotify(void);
  void childRemoveNotify(void);

  // these events are forwarded to the widget passed on the stack
  void visibilityObscuredNotify(MSWidget *);
  void visibilityUnobscuredNotify(MSWidget *);
  void buttonMotionNotify(MSWidget *,const XEvent *);
  void buttonPressNotify(MSWidget *,const XEvent *);
  void buttonReleaseNotify(MSWidget *,const XEvent *);
  void keyTranslateNotify(MSWidget *,const XEvent *,KeySym,unsigned int,const char *);
  void keyPressNotify(MSWidget *,const XEvent *,KeySym,unsigned int,const char *);
  void keyReleaseNotify(MSWidget *,const XEvent *,KeySym,unsigned int,const char *);
  void focusInNotify(MSWidget *);
  void focusOutNotify(MSWidget *);
  void takeFocusNotify(MSWidget *);
  MSBoolean loseFocusNotify(MSWidget *);
  void selectionClearNotify(MSWidget *,const XEvent *);

  // Selection notification
  virtual const char * getPrimarySelection(MSString&, int&);
  virtual void insertPrimarySelection(void);

  // Iteration methods
  MSBoolean breadthFirstIteration(MSWidgetVector&,MSWidgetIterator&);
  MSBoolean depthFirstIteration(MSWidgetIterator&);
};

#ifndef MS_NO_INLINES
#ifndef MSWidgetINLINES
#include <MSGUI/MSWidgetInlines.C>
#endif
#endif

#endif
