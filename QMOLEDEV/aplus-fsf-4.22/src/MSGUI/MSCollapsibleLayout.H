#ifndef MSCollapsibleLayoutHEADER
#define MSCollapsibleLayoutHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetCommon.H>
#include <MSTypes/MSNodeList.H>

class MSPixmap;
class MSCollapsibleEntry;

class MSGUIExport MSCollapsibleLayout : public MSWidgetCommon
{
public:
  MSCollapsibleLayout(MSWidget *);
  ~MSCollapsibleLayout(void);
  
  int childCount(void) const { return _childCount; }

  virtual MSWidgetVector children(void);

  void permuteWidgets(const MSWidgetVector &);
  
  void close(MSWidget*);
  void open(MSWidget*);
  MSBoolean isOpened(MSWidget*) const;
  MSBoolean isClosed(MSWidget*) const;

  void handleBackground(unsigned long);
  void handleBackground(const char *);
  unsigned long  handleBackground() const;

  void handleForeground(unsigned long);
  void handleForeground(const char *);
  unsigned long  handleForeground() const;

  void handleSize(int) ;
  int handleSize() const { return _handleSize; } 

  void handleToolTip(MSWidget* , const MSStringVector &);
  const MSStringVector &handleToolTip(MSWidget*) const;

 
  virtual void show(void);
  virtual void print(const char *file_=0);
  virtual void unfreeze();
  
  void set(MSAttrValueList&);
  MSAttrValueList& get(MSAttrValueList&);

protected:
  virtual void visibilityUnobscured(void);
  virtual void visibilityObscured(void);
  virtual void placement(void);
  virtual void configure(void);
  virtual void childDestroy(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childMap(MSWidget *);
  virtual void childUnmap(MSWidget *);
  virtual void childConfigure(MSWidget *);
  virtual void firstMapNotify(void);
  virtual void naturalSize(void);
  virtual void childResizeConstraints(MSWidget *);
  virtual void computeSize();

  virtual void entryOpened();
  virtual void entryClosed();

private:
  MSNodeItem        _childListHead;
  int               _childCount;
  MSBoolean        _internalEvent;
  
  MSBoolean         naturalSizing;
  MSPixmap *       _tile;
  unsigned long    _handleBg; 
  unsigned long    _handleFg; 
  int              _handleSize;

  friend class MSCollapsibleEntry;
  
  void init(void);
  MSNodeItem *childListHead(void)       { return &_childListHead; }
  const MSNodeItem *childListHead(void) const { return &_childListHead; }
  MSCollapsibleEntry *getEntry(MSWidget *) const;

  void natural(int&,int&,int&,int&);
  void doShownPlacement(int&,int&,int&);
  void doHiddenPlacement();
  void internalEvent(MSBoolean);
  MSBoolean internalEvent(void);
  void updateHandleTile();
  void hideConfigure(MSCollapsibleEntry*); 
};


#endif











