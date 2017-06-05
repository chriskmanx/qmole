#ifndef MSIntVectorTraceSetHEADER
#define MSIntVectorTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSIntVector.H>
#include <MSGUI/MSTraceSet.H>

class MSGUIExport MSIntVectorTraceSet:public MSTraceSet
{
public:
  MSIntVectorTraceSet(MSGraph *,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntVectorTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntVectorTraceSet(MSGraph *,MSIntVector&,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntVectorTraceSet(MSGraph *,MSIntVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSIntVectorTraceSet(void);

  void model(MSIntVector&);
  void model(const MSIntVector&);

  MSIntVector& viewModel(void)             { return *((MSIntVector*)_model); }
  const MSIntVector& viewModel(void) const { return *((MSIntVector*)_model); }
  virtual void selectable(MSBoolean);
  virtual MSBoolean selectable(void) const;
  virtual void constraint(unsigned long);
  virtual unsigned long constraint(void) const;
  MSIntVectorTraceSet& operator=(const MSIntVector& vector_)    {vector(vector_); return *this;}
  operator MSIntVector&() const {return *((MSIntVector *)_model);} 

protected:
  MSIntVector& vector(void)                  {return (MSIntVector&)*_model;}
  const MSIntVector& vector(void)      const {return (MSIntVector&)*_model;}  
  void vector(const MSIntVector& v_)         {if (MSView::model()!=0) vector()=v_;}
  virtual int dataCount(void) const;
  virtual double y(int,int) const;
  virtual void validate(int,int,double,double);
  virtual MSBoolean moveTraceValidate(double,double);
  virtual MSTraceSet* copyTraceValidate(MSGraph*,const char*, const MSSymbol&);
  virtual MSFloatMatrix asFloatMatrix(void) const;
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
};

#endif
