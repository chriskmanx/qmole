#ifndef MSFloatMatrixTraceSetHEADER
#define MSFloatMatrixTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFloatMatrix.H>
#include <MSGUI/MSTraceSet.H>

class MSGUIExport MSFloatMatrixTraceSet:public MSTraceSet 
{
public:
  MSFloatMatrixTraceSet(MSGraph *,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatMatrixTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatMatrixTraceSet(MSGraph *,MSFloatMatrix&,const char *legends_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatMatrixTraceSet(MSGraph *,MSFloatMatrix&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSFloatMatrixTraceSet(void);

  void model(MSFloatMatrix&);
  void model(const MSFloatMatrix&);

  MSFloatMatrix& viewModel(void)             { return *((MSFloatMatrix*)_model); }
  const MSFloatMatrix& viewModel(void) const { return *((MSFloatMatrix*)_model); }
  MSFloatMatrixTraceSet& operator=(const MSFloatMatrix& matrix_)  {matrix(matrix_); return *this;}
  operator MSFloatMatrix&() const {return *((MSFloatMatrix *)_model);} 

protected:
  MSFloatMatrix& matrix(void)                 {return *((MSFloatMatrix *)_model); }
  const MSFloatMatrix& matrix(void)     const {return *((MSFloatMatrix *)_model); }
  void matrix(const MSFloatMatrix& v_)        {if (MSView::model()!=0) matrix()=v_;}
  virtual int dataCount(void) const;
  virtual double x(int) const;
  virtual double y(int,int) const;
  virtual int numColumns(void) const;
  virtual void validate(int,int,double,double);
  virtual MSBoolean moveTraceValidate(double,double);
  virtual MSFloatMatrix asFloatMatrix(void) const;
  virtual MSTraceSet* copyTraceValidate(MSGraph*,const char*, const MSSymbol&);
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
};

#endif
