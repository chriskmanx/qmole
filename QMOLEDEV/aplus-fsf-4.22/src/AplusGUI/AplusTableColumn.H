#ifndef AplusTableColumnHEADER
#define AplusTableColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTableColumn.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusReportAlgo.H>
#include <AplusGUI/AplusEnumConverter.H>

class ABreakFunction : public AOutFunction
{
public:
  ABreakFunction(void) { _func=0; _arg=0; }
  ABreakFunction(AFunc func_, AClientData *arg_=0) : AOutFunction(func_, arg_) {}
  ~ABreakFunction(void) {}

  virtual A callFunc(V v_, A a_, int row_, int col_,A p_=aplus_nl);
};

class AFormatBreakFunction : public ABreakFunction
{
public:
  AFormatBreakFunction(void) {}
  ~AFormatBreakFunction(void) {}
};

class AplusTable;

class AplusTableColumn : public MSTableColumn
{
  friend class AplusTable;
  
 public:
  AplusTableColumn(AplusTable *);
  ~AplusTableColumn(void);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);
  
  AplusTable *table(void) const { return (AplusTable *)MSTableColumn::table(); }
  
  AOutFunction *breakOffsetFunc(void)         { return &_breakOffsetFunc; }
  AOutFunction *breakFontFunc(void)           { return &_breakFontFunc; }
  AOutFunction *breakStyleFunc(void)          { return &_breakStyleFunc; }
  AOutFunction *breakLeadingFunc(void)        { return &_breakLeadingFunc; }
  AOutFunction *fgGrayScaleFunc(void)         { return &_fgGrayScaleFunc; }
  AOutFunction *bgGrayScaleFunc(void)         { return &_bgGrayScaleFunc; }
  AOutFunction *breakFgGrayScaleFunc(void)    { return &_breakFgGrayScaleFunc; }
  AOutFunction *breakBgGrayScaleFunc(void)    { return &_breakBgGrayScaleFunc; }
  AOutFunction *reportFontFunc(void)          { return &_reportFontFunc; }
  AOutFunction *styleFunc(void)               { return &_styleFunc; }
  AOutFunction *breakCriteriaFunc(void)       { return &_breakCriteriaFunc; }
  ABreakFunction *breakProcessFunc(void)      { return &_breakProcessFunc; }
  AFormatBreakFunction *formatBreakFunc(void) { return &_formatBreakFunc; }

  void breakOffsetFunc(AFunc func_, AClientData *arg_)      { breakOffsetFunc()->set(func_,arg_); }
  void breakFontFunc(AFunc func_, AClientData *arg_)        { breakFontFunc()->set(func_,arg_); }
  void breakStyleFunc(AFunc func_, AClientData *arg_)       { breakStyleFunc()->set(func_,arg_); }
  void breakLeadingFunc(AFunc func_, AClientData *arg_)     { breakLeadingFunc()->set(func_,arg_); }
  void fgGrayScaleFunc(AFunc func_, AClientData *arg_)      { fgGrayScaleFunc()->set(func_,arg_); }
  void bgGrayScaleFunc(AFunc func_, AClientData *arg_)      { bgGrayScaleFunc()->set(func_,arg_); }
  void breakFgGrayScaleFunc(AFunc func_, AClientData *arg_) { breakFgGrayScaleFunc()->set(func_,arg_); }
  void breakBgGrayScaleFunc(AFunc func_, AClientData *arg_) { breakBgGrayScaleFunc()->set(func_,arg_); }
  void reportFontFunc(AFunc func_, AClientData *arg_)       { reportFontFunc()->set(func_,arg_); }
  void styleFunc(AFunc func_, AClientData *arg_)            { styleFunc()->set(func_,arg_); }
  void breakCriteriaFunc(AFunc func_, AClientData *arg_)    { breakCriteriaFunc()->set(func_,arg_); }
  void formatBreakFunc(AFunc func_, AClientData *arg_)      { formatBreakFunc()->set(func_,arg_);   }
  void breakProcessFunc(AFunc func_, AClientData *arg_)
    { breakProcessFunc()->set(func_,arg_); breakString().removeAll(); }

  void style(unsigned long style_)      { MSTableColumn::style(style_); }
  void reportFont(const char *font_)    { MSTableColumn::reportFont(font_); }
  void fgGrayScale(double fg_)          { MSTableColumn::fgGrayScale(fg_); }
  void bgGrayScale(double bg_)          { MSTableColumn::bgGrayScale(bg_); }
  void breakFgGrayScale(double fg_)     { MSTableColumn::breakFgGrayScale(fg_); }
  void breakBgGrayScale(double bg_)     { MSTableColumn::breakBgGrayScale(bg_); }
  void breakStyle(unsigned long style_) { MSTableColumn::breakStyle(style_); }
  void breakFont(const char *font_)     { MSTableColumn::breakFont(font_); }
  void breakOffset(int offset_)         { MSTableColumn::breakOffset((long)offset_); }
  void breakLeading(int leading_)       { MSTableColumn::breakLeading((long)leading_); }

  unsigned long style(void)	 { return MSTableColumn::style(); }
  const char *reportFont(void)	 { return MSTableColumn::reportFont(); }
  double fgGrayScale(void)	 { return MSTableColumn::fgGrayScale(); }
  double bgGrayScale(void)	 { return MSTableColumn::bgGrayScale(); }
  double breakFgGrayScale(void)	 { return MSTableColumn::breakFgGrayScale(); }
  double breakBgGrayScale(void)	 { return MSTableColumn::breakBgGrayScale(); }
  unsigned long breakStyle(void) { return MSTableColumn::breakStyle(); }
  const char *breakFont(void)	 { return MSTableColumn::breakFont(); }
  int breakOffset(void)		 { return MSTableColumn::breakOffset(); }
  int breakLeading(void)	 { return MSTableColumn::breakLeading(); }

  A breakText(void) {return _breakText;}
  void breakText(A breakText_) {_breakText = breakText_;}
  
  unsigned compMode(void) {return _compMode;}
  void compMode(unsigned);
  
 protected:
  virtual MSBoolean verifyData(V,A);
  AVariableData *getVarData(void);
  void setBg(unsigned long);
  void setFont(Font);
  virtual A defaultInFunc(V v_,const char *string);
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);
  virtual const char *formatOutput(MSString&, unsigned);
  virtual MSBoolean validate(const char *,unsigned);
  virtual MSBoolean validate(V v_, const char *string_, unsigned row_);
  void forceEval(void) const;
  virtual unsigned numRows(void) const;
  virtual unsigned numColumns(void) const;

  virtual void update(V,A,A,I);
  virtual void update(V,int,int,UpdateType);
  virtual void update(const MSIndexVector &v_);
  virtual void updateForeground(unsigned long);
  virtual unsigned long cellForeground(unsigned);
  virtual unsigned long cellBackground(unsigned);
  virtual Font cellFont(unsigned);
  virtual MSBoolean isCellProtected(unsigned);
  Font titleFont(void);
  MSUnsignedLongVector getCycleColors(int) const;
  virtual void updateFont(Font);

  // Report Methods
  virtual MSBoolean breakCriteria(unsigned);
  virtual MSBoolean isDuplicate(unsigned);

  virtual unsigned long style(unsigned);
  virtual const char *reportFont(unsigned);
  virtual double fgGrayScale(unsigned);
  virtual double bgGrayScale(unsigned);
  virtual double breakFgGrayScale(unsigned);
  virtual double breakBgGrayScale(unsigned);

  virtual void breakProcess(MSIndexVector &);
  virtual const char *formatBreak(MSString &, unsigned, unsigned);
  virtual unsigned long breakStyle(unsigned);
  virtual int breakOffset(unsigned);
  virtual int breakLeading(unsigned);
  virtual const char *breakFont(unsigned);

  virtual double computeIndexedSum(MSIndexVector &);
  virtual double computeColumnTotal(void);

  MSBoolean isNumericColumn(void);
  void invokeFunction(AOutFunction *, unsigned);
  void invokeFunction(AOutFunction *, unsigned, unsigned);
  void invokeFunction(AOutFunction *, unsigned, unsigned, I *);
  void invokeFunction(AFormatBreakFunction *, unsigned, unsigned, A);

  void columnResizeCB(void);

  void setClipMode(void);
    
private:
  AOutFunction     	 _breakOffsetFunc;
  ACharStrFunction 	 _breakFontFunc;
  AOutFunction 	   	 _breakStyleFunc;
  AOutFunction 	   	 _breakLeadingFunc;
  AOutFunction 	   	 _fgGrayScaleFunc;
  AOutFunction 	   	 _bgGrayScaleFunc;
  AOutFunction 	   	 _breakFgGrayScaleFunc;
  AOutFunction 	   	 _breakBgGrayScaleFunc;
  ACharStrFunction 	 _reportFontFunc;
  AOutFunction 	   	 _styleFunc;
  ABreakFunction   	 _breakProcessFunc;
  AFormatBreakFunction 	 _formatBreakFunc;
  AOutFunction 	   	 _breakCriteriaFunc;
  A        	   	 _outStr;
  MSString         	 _breakProcessStringBuffer;
  MSString 	   	 _reportFontStringBuffer;
  MSString 	   	 _breakFontStringBuffer;
  A                	 _breakText;
  unsigned         	 _compMode;
  AplusReportAlgorithm  *_algorithm;

  STANDARD_WIDGET_METHODS_H
};


inline AVariableData *AplusTableColumn::getVarData(void)
{
  return (model()&&((AplusModel*)model())->aplusVar()!=0) ?
    (AVariableData *)(((AplusModel*)model())->pAVarData()) : 0;
}


#endif // AplusTableColumnHEADER
