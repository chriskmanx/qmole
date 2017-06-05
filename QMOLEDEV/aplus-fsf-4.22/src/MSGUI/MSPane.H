#ifndef MSPaneHEADER
#define MSPaneHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLayout.H>
#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSGC.H>

class MSPane;

class MSGUIExport MSSash : public MSWidgetCommon
{
protected:
  MSDisplayCursor *_crossCursor;
  MSGC             _drawGC;
  int              _row;
  int              _column;
  int              _min;
  int              _max;   

  GC gc(void) const;
  virtual void redraw(void);
  virtual void drawSash(void);
  virtual void drawSeparator(void);
  virtual void size(int);
  virtual void moveSash(int);
  virtual void buttonPress(const XEvent *);

  virtual void updateBackground(unsigned long);

public:
  MSSash(MSPane *);
  ~MSSash(void);

  int row(void)    const {return _row;}
  int column(void) const {return _column;}
  int min(void)    const {return _min;}
  int max(void)    const {return _max;}

  void row(int row_)    {_row=row_;}
  void column(int col_) {_column=col_;}
  void min(int min_)    {_min=min_;}
  void max(int max_)    {_max=max_;}
};

class MSGUIExport MSVSash : public MSSash
{
protected:
  virtual void drawSash(void);
  virtual void drawSeparator(void);
  virtual void size(int size_);
  virtual void moveSash(int delta_);

  virtual void button1Press(const XEvent *);

public:
  MSVSash(MSPane *);
  ~MSVSash(void);
};

class MSGUIExport MSHSash : public MSSash
{
protected:
  virtual void drawSash(void);
  virtual void drawSeparator(void);
  virtual void size(int size_);
  virtual void moveSash(int delta_);

  virtual void button1Press(const XEvent *);

public:
  MSHSash(MSPane *);
  ~MSHSash(void);
};

class MSGUIExport MSPane : public MSLayout
{
friend class MSSash;
friend class MSHSash;
friend class MSVSash;

public:
  MSPane(MSWidget *,const char *title_=0);
  MSPane(MSWidget *,const MSStringVector&);
  ~MSPane(void);

  MSBoolean ignoreResizeConstraints(void) const { return _ignoreResizeConstraints; }
  void ignoreResizeConstraints(MSBoolean);
  
  
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  virtual void set(MSAttrValueList& avList_);

protected:
  MSWidgetVector _sashList;
  MSBoolean      _sashAction;   // true when a sash action is under way-create,map etc..
  MSBoolean      _ignoreResizeConstraints;

  MSWidgetVector& sashList(void)             {return _sashList;}
  const MSWidgetVector& sashList(void) const {return _sashList;}  
  MSBoolean sashAction(void)           const {return _sashAction;}

  void sashAction(MSBoolean action_)   { _sashAction=action_; }

  void init(void);
  void removeSash(int row_,int col_);
  MSSash *sash(int row_,int col_);
  int numSashes(void);
  virtual void removeAllSashes(void);
  virtual void removeExtraSashes(void);  
  virtual void placeSashes(void);
 
  void adjustRowHeight(int row_,int height_);
  void adjustColumnWidth(int col_,int width_);
  virtual void placement(void);

  virtual At childPosition(MSWidget *);
  virtual void childPosition(MSWidget *,const At&);
  virtual void childResizeConstraints(MSWidget *);

  virtual void childConfigure(MSWidget *);
  virtual void childMap(MSWidget *);
  virtual void childUnmap(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);

  virtual void updateBackground(unsigned long);
  virtual void updateOrientation(void);
};

#endif







