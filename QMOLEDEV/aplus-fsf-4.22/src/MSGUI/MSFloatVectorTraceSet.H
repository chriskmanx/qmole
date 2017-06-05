#ifndef MSFloatVectorTraceSetHEADER
#define MSFloatVectorTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFloatVector.H>
#include <MSGUI/MSTraceSet.H>

class MSGUIExport MSFloatVectorTraceSet:public MSTraceSet
{
public:
  MSFloatVectorTraceSet(MSGraph *,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatVectorTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatVectorTraceSet(MSGraph *,MSFloatVector&,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatVectorTraceSet(MSGraph *,MSFloatVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSFloatVectorTraceSet(void);

  void model(MSFloatVector&);
  void model(const MSFloatVector&);

  MSFloatVector& viewModel(void)             { return *((MSFloatVector*)_model); }
  const MSFloatVector& viewModel(void) const { return *((MSFloatVector*)_model); }
  virtual void selectable(MSBoolean);
  virtual MSBoolean selectable(void) const;
  virtual void constraint(unsigned long);
  virtual unsigned long constraint(void) const;
  MSFloatVectorTraceSet& operator=(const MSFloatVector& vector_)  {vector(vector_); return *this;}
  operator MSFloatVector&() const {return *((MSFloatVector *)_model);} 

protected:
  MSFloatVector& vector(void)                 {return (MSFloatVector&)*_model;}
  const MSFloatVector& vector(void)     const {return (MSFloatVector&)*_model;}  
  void vector(const MSFloatVector& v_)        {if (MSView::model()!=0) vector()=v_;}
  virtual int dataCount(void) const;
  virtual double y(int,int) const;
  virtual void validate(int,int,double,double);
  virtual MSBoolean moveTraceValidate(double,double);
  virtual MSTraceSet* copyTraceValidate(MSGraph*,const char*, const MSSymbol&);
  virtual MSFloatMatrix asFloatMatrix(void) const;
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
};

#endif







