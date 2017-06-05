#ifndef MSPopupHEADER
#define MSPopupHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSShell.H>

class MSGUIExport MSPopup : public MSShell
{
public:
  MSPopup(const char *windowTitle_=0);
  MSPopup(MSDisplayServer *,const char *windowTitle_=0);
  MSPopup(MSShell *,const char *windowTitle_=0);
  ~MSPopup(void);

  MSBoolean pushPinState(void) const;

  void pinIn(void);
  void pinOut(void);

  void transientFor(MSShell *);

  void modal(MSBoolean);
  MSBoolean modal(void) const; 

  virtual void map(void);
  virtual void unmap(void); 

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
  
private:
  MSBoolean _pushPinState;
  MSBoolean _modal;

  void init(void);
  void pushPinState(MSBoolean);

  virtual void setWinAttr(void);
  virtual void dismiss(void);
  virtual void clientMessage(const XEvent *event_);
  virtual void propertyNotify(const XEvent *event_);
};

inline MSBoolean MSPopup::pushPinState(void) const
{ return _pushPinState; }
inline void MSPopup::pushPinState(MSBoolean s_) 
{ _pushPinState=s_; }
inline MSBoolean MSPopup::modal(void) const
{ return _modal; }

#endif




