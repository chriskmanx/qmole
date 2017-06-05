#ifndef MSStringVectorTraceSetHEADER
#define MSStringVectorTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSTraceSet.H>

class MSGUIExport MSStringVectorTraceSet:public MSTraceSet 
{
public:
  MSStringVectorTraceSet(MSGraph *,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringVectorTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringVectorTraceSet(MSGraph *,MSStringVector&,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSStringVectorTraceSet(MSGraph *,MSStringVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSStringVectorTraceSet(void);

  virtual void moveTo(int,int);
  virtual void moveTo(double,double);
  virtual void xOrigin(double);
  virtual void yOrigin(double);

  void model(MSStringVector&);
  void model(const MSStringVector&);

  MSStringVector& viewModel(void)             { return *((MSStringVector*)_model); }
  const MSStringVector& viewModel(void) const { return *((MSStringVector*)_model); }
  virtual MSStringVector& text(void);
  virtual const MSStringVector& text(void) const;
  virtual unsigned textLength(void) const;

  MSStringVectorTraceSet& operator=(const MSStringVector& vector_)  {vector(vector_); return *this;}
  operator MSStringVector&() const {return *((MSStringVector *)_model);} 
  virtual unsigned long style(unsigned) const;
  virtual void style(Style);
  virtual void style(unsigned long);
  // the trace style mutator methods are disabled as
  // they are not applicable to this trace set
  virtual void style(unsigned long,unsigned);
  virtual void style(const MSUnsignedLongVector&);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
protected:
  void init(void);
  MSStringVector& vector(void)                {return *((MSStringVector *)_model); }
  const MSStringVector& vector(void)    const {return *((MSStringVector *)_model); }
  void vector(const MSStringVector& v_)       {if (MSView::model()!=0) vector()=v_;}
  virtual int dataCount(void) const;
  virtual double x(int) const;
  virtual double y(int,int) const;
  virtual void validate(int,int,double,double);

private:
  double _x;
  double _y;
};

#endif
