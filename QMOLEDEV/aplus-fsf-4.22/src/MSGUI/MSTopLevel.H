#ifndef MSTopLevelHEADER
#define MSTopLevelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>

class MSPixmap;
class MSGUIExport MSTopLevel : public MSWidgetCommon
{
public:
  enum {WindowBorder=2,ResizeHandles=4,TitleBar=8,
	Menu=16,IconifyButton=32,MaximizeButton=64};
 
  enum {Resize=2,Move=4,Minimize=8,Maximize=16,Close=32};
  
public:
  MSTopLevel(const char *windowTitle_=0);
  MSTopLevel(MSDisplayServer *,const char *windowTitle_=0);
  ~MSTopLevel(void);

  MSBoolean footer(void) const;
  MSBoolean header(void) const;
  MSBoolean resizeable(void) const;
  const MSString& windowManagerCommand(void) const;
  MSString windowTitle(void) const;
  MSString iconTitle(void) const;

  void minimumSize(int width_,int height_);
  void resizeable(MSBoolean);
  void footer(MSBoolean);
  void header(MSBoolean);
  void windowManagerCommand(const char *); 
  void windowTitle(const char *);  
  void iconTitle(const char *);  
  void iconPixmap(const MSPixmap&);

  void addWMDecorations(unsigned long);
  void addWMFunctions(unsigned long);

  void removeWMDecorations(unsigned long);
  void removeWMFunctions(unsigned long);

  void workspacePresence(const MSString&);
  void workspacePresence(const MSStringVector&);
  MSStringVector workspacePresence(void) const;

  virtual void iconify(void);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  struct MWMHints
  {
    enum Flags       {FunctionsFlag=1,DecorationsFlag=2,InputModeFlag=4};
    enum Functions   {FAll=1,FResize=2,FMove=4,FMinimize=8,FMaximize=16,FClose=32};
    enum Decorations {DAll=1,DBorder=2,DResizeHandles=4,DTitle=8,
		      DMenu=16,DMinimize=32,DMaximize=64};
    enum InputMode   {IModeless=0,IPrimaryApplicationModal=1,
		      ISystemModal=2,IFullApplicationModal=3};
  
    long  _flags;
    long  _functions;
    long  _decorations;
    int   _inputMode;
  };
  
  static MSString      _windowManagerCommand;
  unsigned long        _workspacePresenceCount;
  Atom                *_workspacePresenceAtoms;
  MSBoolean            _footer;
  MSBoolean            _header;
  MSBoolean            _resizeable;
  MSPixmap            *_iconPixmap;
  MWMHints             _mwmHints;
  MSBoolean            _adjustWMPosition;
  
  MSPixmap *iconPixmap(void);
  
  void init(const char *windowTitle_);

  MSBoolean inWorkspace(Atom) const;
  void updateWorkspacePresence(void);
  
  virtual void workspacePresenceChangeNotify(void);
  
  void setWMSize(void);
  void setWMPosition(void);
  void setWMProtocols(void);
  void setWMSaveYourself(void);
  void setWMClientMachine(void);
  void setWMHints(void);
  void setWMNormalHints(void);
  void setWMCommand(void);
  void setWMDecorations(unsigned long,MSBoolean);    
  void setWMFunctions(unsigned long,MSBoolean);    

  virtual void quit(void);
  virtual void save(void);
  virtual void clientMessage(const XEvent *);
};

#endif







