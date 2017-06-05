#ifndef MSMenuBarItemHEADER
#define MSMenuBarItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuItem.H>

class MSGUIExport MSMenuBarItem : public MSMenuItem 
{
friend class MSPulldownMenu;
protected:
  MSPulldownMenu  *_pulldownMenu;

  void init(void);
  virtual int computeYCoord(void);
  virtual MSMenu* cascadedMenu(void);
  virtual const MSMenu* cascadedMenu(void) const;
  void pulldownMenu(MSPulldownMenu *);
  void menuDestroy(MSPulldownMenu *);

  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  virtual void menushow(void);
  virtual MSAttrValueList& get(MSAttrValueList&);

public:
  MSMenuBarItem(MSMenuBar *);
  MSMenuBarItem(MSMenuBar *,const char *,char mnemonic_=0,int tag_=0);
  MSMenuBarItem(MSMenuBar *,const MSString&,char mnemonic_=0,int tag_=0);
  MSMenuBarItem(MSMenuBar *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_=0);
  ~MSMenuBarItem(void);

  MSPulldownMenu *pulldownMenu(void)             { return _pulldownMenu; }
  const MSPulldownMenu *pulldownMenu(void) const { return _pulldownMenu; }

  virtual void grab(void);
  virtual void arm(void);
  virtual void disarm(void);
};

#endif
