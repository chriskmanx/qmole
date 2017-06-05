#ifndef MSManagerHEADER
#define MSManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>
#include <MSTypes/MSNodeList.H>

class MSGUIExport MSManager : public MSWidgetCommon
{
public:
  MSManager(MSWidget *);
  ~MSManager(void);

  int childCount(void) const { return _childCount; }

  virtual void show(void);
  virtual void print(const char *file_=0);
  virtual void redraw(void);

  virtual MSWidgetVector children(void);

protected:
  MSNodeItem        _childListHead;
  int               _childCount;
  
  MSNodeItem *childListHead(void)             { return &_childListHead; }
  const MSNodeItem *childListHead(void) const { return &_childListHead; }

  void init(void);

  virtual void updateBackground(unsigned long);

  virtual void visibilityUnobscured(void);
  virtual void visibilityObscured(void);

  virtual void placement(void);
  virtual void configure(void);

  virtual void childDestroy(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
};

#endif











