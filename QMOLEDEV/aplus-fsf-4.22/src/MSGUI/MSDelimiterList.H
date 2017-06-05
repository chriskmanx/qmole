#ifndef MSDelimiterListHEADER
#define MSDelimiterListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSList.H>

class MSGUIExport MSDelimiterList : public MSList
{
protected:
  MSGC                   _segmentGC;
  MSGC                   _delimiterGC;
  MSGC                   _trackGC;
  MSPixmap              *_stipple;
  unsigned long          _delimiterColor;
  unsigned long          _delimiterHighlightColor;
  unsigned long          _delimiterSelectionBackground;
  unsigned long          _delimiterTitleForeground;
  MSString               _delimiterTitle;
  MSIndexVector          _delimiterVector;
  int                    _selectedDelimiter;
  MSBoolean              _listEdit;
  MSBoolean              _delimiterEdit;
  MSBoolean              _delimiterSelection;
  
  MSGC& segmentGC(void)   {return _segmentGC;}
  MSGC& delimiterGC(void) {return _delimiterGC;}
  MSGC& trackGC(void)     {return _trackGC;}
  MSPixmap *stipple(void) {return _stipple;}

  void init(void);
  void drawDelimiters(Window);
  void drawDelimiters(Window,int);
  void drawDelimiterHeaders(Window);
  void highlightDelimiter(int);

  virtual void addDelimiter(int);
  virtual void removeDelimiter(int);
  virtual void dragDelimiter(const XEvent *);
  virtual void selectDelimiter(const XEvent *);
  virtual void delimiterChangeNotify(void);
  virtual void delimiterSelectionNotify(void);
  
  virtual int computeDelimiterXCoord(int);
  virtual int xToColumn(int);
  virtual void updateFont(Font);
  virtual void updateBackground(unsigned long);
  virtual void redrawImmediately(void);

  virtual void drawActualRow(Window,int row_,const char *string_,int len_,
                             const MSPixmap *,unsigned long fg_,unsigned long bg_,
                             Font font_,MSBoolean selected_,MSBoolean selectOutline_);

  virtual void updateData(void);

  virtual void headingAreaSelection(const XEvent *);
  virtual void dataAreaSelection(const XEvent *);
  virtual void delimiterButton1Event(const XEvent *);
  virtual void delimiterButton2Event(const XEvent *);
  
  virtual void calculateHeadingsHeight(void);
  
public:
  MSDelimiterList(MSWidget *,const char *title_=0);
  MSDelimiterList(MSWidget *,const MSStringVector &title_);
  ~MSDelimiterList(void);
  
  void delimiterVector(const MSIndexVector &delimiterVector_,int selectedDelimiter_);
  void delimiterVector(const MSIndexVector &delimiterVector_);
  const MSIndexVector &delimiterVector(void) const {return _delimiterVector;}

  void selectedDelimiter(int);
  int selectedDelimiter(void) const {return _selectedDelimiter;}

  void delimiterTitle(const MSString &);
  const MSString &delimiterTitle(void) const {return _delimiterTitle;}

  void delimiterEdit(MSBoolean delimiterEdit_);
  MSBoolean delimiterEdit(void) const {return _delimiterEdit;}

  void listEdit(MSBoolean listEdit_) {_listEdit=listEdit_;}
  MSBoolean listEdit(void) const {return _listEdit;}

  void delimiterSelection(MSBoolean delimiterSelection_) {_delimiterSelection=delimiterSelection_;}
  MSBoolean delimiterSelection(void) const {return _delimiterSelection;}

  void delimiterColor(const char *);
  void delimiterColor(unsigned long);
  unsigned long delimiterColor(void) const {return _delimiterColor;}

  void delimiterHighlightColor(const char *);
  void delimiterHighlightColor(unsigned long);
  unsigned long delimiterHighlightColor(void) const {return _delimiterHighlightColor;}

  void delimiterSelectionBackground(const char *);
  void delimiterSelectionBackground(unsigned long);
  unsigned long delimiterSelectionBackground(void) const {return _delimiterSelectionBackground;}

  void delimiterTitleForeground(const char *);
  void delimiterTitleForeground(unsigned long);
  unsigned long delimiterTitleForeground(void) const {return _delimiterTitleForeground;}

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

};


#endif
