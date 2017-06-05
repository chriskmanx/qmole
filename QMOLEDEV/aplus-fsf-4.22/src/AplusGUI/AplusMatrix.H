#ifndef AplusMatrixHEADER
#define AplusMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSGC.H>
#include <MSTypes/MSInt.H>
#include <AplusGUI/AplusArray.H>

class ASpaceFunction : public AFunction
{
public:
  ASpaceFunction(void) { _func=0;_arg=0; }
  ASpaceFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~ASpaceFunction(void) {}
  
  A callFunc(V v_,A a_)
   { A r=(func()!=0)?(A)(*func())(arg(),a_,aplus_nl,aplus_nl,v_):aplus_nl;
     if (a_!=0) dc((A)a_);
     return r;
   }
  A invoke(V v_,A value_) { return (A)callFunc(v_,(A)ic(value_)); } 
};

class  AplusMatrix : public AplusArray
{
public:
  AplusMatrix(MSWidget *);
  ~AplusMatrix(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  void rowIndex(A);
  void colIndex(A);
  void cornerIndex(MSBoolean);
  A rowIndexVector(void)      	    { return (A)ic(rowIndex()); }
  A colIndexVector(void)      	    { return (A)ic(colIndex()); }
  MSBoolean cornerIndex(void) 	    { return _cornerIndex; }
  unsigned long rowIndexBg(void)    { return _rowIndexBg; }
  unsigned long colIndexBg(void)    { return _colIndexBg; }
  unsigned long cornerIndexBg(void) { return _cornerIndexBg; }
  void rowIndexBg(unsigned long);
  void colIndexBg(unsigned long);
  void cornerIndexBg(unsigned long);

  ASpaceFunction *spaceFunc(void) { return &_spaceFunc; }
  void spaceFunc(AFunc func_,AClientData *arg_);

  A colSpace(void)      { return (A)ic(spaceVector()); }
  int numHeadings(void) { return _numHeadings; }

  void colSpace(A);
  void numHeadings(int);
  
  A heading(int);
  A rowLabel(int);
  unsigned long labelColor(int);

  virtual unsigned columnLength(unsigned);

  virtual void firstMapNotify(void);
  virtual void updateTitle(void);

protected:
  int            _labelLength;
  int            _numHeadings;
  A              _spaceVector;
  ASpaceFunction _spaceFunc;
  unsigned long  _rowIndexBg;
  unsigned long  _colIndexBg;
  unsigned long  _cornerIndexBg;
  A              _rowIndex;
  A              _colIndex;
  MSBoolean      _cornerIndex;
  MSGC           *_rowIndexXGC;
  MSGC           *_colIndexXGC;
  MSGC           *_cornerIndexXGC;

  A spaceVector(void)            { return _spaceVector; }
  A rowIndex(void)               { return _rowIndex; }
  A colIndex(void)               { return _colIndex; }
  MSGC *rowIndexXGC(void)        { return _rowIndexXGC; }
  MSGC *colIndexXGC(void)        { return _colIndexXGC; }
  MSGC *cornerIndexXGC(void)     { return _cornerIndexXGC; }
  GC rowIndexGC(void)            { return rowIndexXGC()->gc(); }
  GC colIndexGC(void)            { return colIndexXGC()->gc(); }
  GC cornerIndexGC(void)         { return cornerIndexXGC()->gc(); }
  int labelLength(void)          { return _labelLength; }
  void createRowIndexXGC(void);
  void createColIndexXGC(void);
  void createCornerIndexXGC(void);

  virtual int labelWidth(void)       { return labelLength()*charWidth()+(columnSpacing()<<1); }
  virtual int headingsHeight( void ) { return numHeadings()*rowHeight(); }

  // We have to override redrawImmediately() to ensure that the corner label gets drawn
  // every time the window is redrawn.
  //
  virtual void redrawImmediately(void);

  // Draw corner Label
  virtual void drawLabel() { drawLabel(panner()->window()); }
  virtual void drawLabel(Window);

  // Draw row labels
  virtual void drawLabels(int rs_,int re_) { drawLabels(panner()->window(),rs_,re_); }
  virtual void drawLabels(Window,int,int);

  // Draw column labels
  virtual void drawHeadings(Window win_) { drawHeadings(win_, firstColumn(), lastColumn()); }
  virtual void drawHeadings(Window,int,int);
  virtual void drawFieldHeadings(Window xid_,int cs_,int ce_);
  virtual void drawFieldHeading(Window,A,unsigned long,Font,MSRect&);

  virtual void shapeUpdate(void);
  virtual void updateColSpace(void);

  Font headingFont(int col_);
  unsigned long headingColor(int col_);
  Font labelFont(int row_);

  MSBoolean isColIndexed(int);
  MSBoolean isRowIndexed(int);

  void sizeUpdate(void);

  STANDARD_WIDGET_METHODS_H
};

#endif  //AplusMatrixHEADER
