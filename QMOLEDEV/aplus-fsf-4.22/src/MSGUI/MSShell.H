#ifndef MSShellHEADER
#define MSShellHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTopLevel.H>
#include <MSGUI/MSWidgetState.H>

class MSShellBusy;
class MSApplicationBusy;

class MSGUIExport MSShell : public MSTopLevel
{
friend class MSShellBusy;
friend class MSApplicationBusy;
friend MSGUIExport void changeBusyState(MSShell *,MSBoolean);   
friend MSGUIExport void changeBusyState(MSBoolean); 

public:
  MSShell(const char *windowTitle_=0);
  MSShell(MSDisplayServer *,const char *windowTitle_=0);
  ~MSShell(void);

  static MSShell *defaultLeader(void);
  static void defaultLeader(MSShell *);  
  static const MSWidgetVector& topLevelList(void);
  
  MSWidgetVector followerList(void);
  const MSWidgetVector& traversalList(void) const;
  const MSWidgetVector& traversalList(void);
  void traversalList(const MSWidgetVector&);
  
  MSWidget *child(void);
  MSShell *leader(void);
  MSShell *windowGroup(void);
  MSWidget *focusWidget(void);
  virtual MSWidget *inputFocus(void);

  const MSWidget *child(void) const;
  const MSShell *leader(void) const;
  const MSShell *windowGroup(void) const;
  const MSWidget *focusWidget(void) const;
  virtual const MSWidget *inputFocus(void) const;
  
  virtual MSBoolean traverseFocus(MSWidget *);
  virtual MSBoolean traverseToNext(void);
  virtual MSBoolean traverseToPrevious(void);
  virtual void traverse(void);

  MSBoolean windowGroup(MSShell *);
  void leader(MSShell *);

  void moveToCenter(void);
  virtual void naturalSize(void);
  virtual void resize(int,int);
  virtual void moveTo(int,int);
  virtual void print(const char *file_=0);
  virtual void unmap(void);
  virtual void map(void);
  virtual void show(void);
  virtual void showCentered(void);  

  virtual MSWidgetVector children(void);

  virtual void loadAndShow(const char *fileName_);
  virtual void loadAndShow(const MSWidgetState &);
  virtual void loadAndShow(istream &);
  
  virtual void loadStateFrom(const MSWidgetState &);
  virtual void loadStateFrom(const char *fileName_);
  virtual void loadStateFrom(istream &);

  virtual void saveStateTo(const char *fileName_);
  virtual void saveStateTo(ostream &);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  friend MSGUIExport ostream &operator<<(ostream &,MSShell &);
  friend MSGUIExport istream &operator>>(istream &,MSShell &);
  
public:
  class MSGUIExport Follower
  {
  public:
    MSShell   *_pShell;
    MSBoolean  _remap;

    Follower(MSShell *shell_);
    ~Follower(void);
  };

  class MSGUIExport GroupList
  {
  public:
    MSUnsignedLongVector _groupList;

    GroupList(void);
    ~GroupList(void);    

    void removeAll(void);
    void removeAt(unsigned);
    void append(Follower *);
    unsigned numberOfFollowers(void) const;
    Follower *operator()(unsigned index_) const;
    Follower *operator()(unsigned index_);
  };

protected:
  static MSWidgetVector  _shellList;
  static MSShell        *_defaultLeader;
  int                    _busyCount;    // ref counting for busy cursor
  GroupList              _groupList;
  MSWidgetVector         _traversalList;
  MSWidget              *_focusWidget;
  MSShell               *_leader;
  MSWidget              *_child;
  MSBoolean              _created;      // set after the first mapping event is received
  int                    _offsetX;      // window manager x offset
  int                    _offsetY;      // window manager y offset
  MSBoolean              _nestedTraversal;
  MSBoolean              _inCurrentWorkspace;  // is this window in the current workspace
  
  void init(void);
  static MSWidgetVector& shellList(void);

  virtual int offsetX(void) const;
  virtual int offsetY(void) const;
  int childWidth(void) const;
  int childHeight(void) const;
  virtual int idealShellWidth(void) const;
  virtual int idealShellHeight(void) const;

  virtual void showChildren(void);  
  virtual void adjustChildPosition(void);
  virtual void adjustChildSize(void);
  virtual void adjustSize(void);

  MSBoolean doesLeaderFollowThis(MSShell *);
  void addFollower(MSShell *);
  void removeFollower(MSShell *);
  void unmapFollowers(void);
  void mapFollowers(void);

  void focusWidget(MSWidget *);
  void nestedTraversal(MSBoolean nestedTraversal_);

  MSWidget *getNextFocus(void);
  MSWidget *getPreviousFocus(void);
  virtual MSBoolean traverseFocusToNext(void);
  virtual MSBoolean traverseFocusToPrevious(void);  

  virtual void addToFocusList(MSWidget *);
  virtual void removeFromFocusList(MSWidget *);

  void setDefaultFocus(void);
  void setDefaultTraversal(void);  

  virtual MSBoolean processAltKey(const XEvent *,KeySym,unsigned int,const char *);
  virtual MSBoolean processFunctionKey(const XEvent *,KeySym,unsigned int,const char *);
  virtual void traverseToNextShell(void);
  virtual void traverseToPreviousShell(void);

  virtual void firstMapNotify(void);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void configure(void);
  virtual void childCreate(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childConfigure(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void configureNotify(const XEvent *);
  virtual void mapNotify(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *);
  virtual void enterNotify(const XEvent *);
  virtual void leaveNotify(const XEvent *);
  virtual void propertyNotify(const XEvent *);

  virtual void focusInEventNotify(const XEvent *);
  virtual void focusOutEventNotify(const XEvent *);
 
  void processFocusIn(void);
  void processFocusOut(void);

  virtual void iconized(void);
  virtual void deiconized(void);  
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void takeFocus(void);
  virtual MSBoolean loseFocus(void);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);
  virtual void inCurrentWorkspaceNotify(void);
  virtual void outOfCurrentWorkspaceNotify(void);
  
  static void updateCurrentWorkspace(Atom);
  void updateCurrentWorkspaceState(Atom);  
  void updateWMState(void);
  
  static void changeBusyState(MSShell *,MSBoolean);   
  static void changeBusyState(MSBoolean); 
  void setBusyState(MSBoolean);
  int busyCount(void);
  void busyCount(int);

private:
  void setOffsets(void);
  void offsetX(int);
  void offsetY(int);
};

#endif
