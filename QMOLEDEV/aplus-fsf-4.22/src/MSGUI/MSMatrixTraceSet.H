#ifndef MSMatrixTraceSetHEADER
#define MSMatrixTraceSetHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSTypeMatrix.H>
#include <MSGUI/MSTraceSet.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSMatrixTraceSet.C")
#endif

template<class Type>
class MSMatrixTraceSet : public MSTraceSet
{
public:
  MSMatrixTraceSet(MSGraph *,const char *title_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSMatrixTraceSet(MSGraph *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSMatrixTraceSet(MSGraph *,MSTypeMatrix<Type>&,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSMatrixTraceSet(MSGraph *,MSTypeMatrix<Type>&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSMatrixTraceSet(void);
  
  void model(MSTypeMatrix<Type>&);
  void model(const MSTypeMatrix<Type>&);

  MSTypeMatrix<Type>& viewModel(void)             { return *((MSTypeMatrix<Type>*)_model); }
  const MSTypeMatrix<Type>& viewModel(void) const { return *((MSTypeMatrix<Type>*)_model); }
  
protected:
  MSTypeMatrix<Type>& matrix(void);
  const MSTypeMatrix<Type>& matrix(void) const;
  void matrix(const MSTypeMatrix<Type>&);

  virtual int dataCount(void) const;
  virtual double x(int) const;
  virtual double y(int,int) const;
  virtual int numColumns(void) const;   
  virtual void validate(int,int,double,double);
  virtual const char *formatOutput(MSString &,unsigned,unsigned);

  MSMatrixTraceSet<Type>& operator=(const MSTypeMatrix<Type>& aMatrix_);
  operator MSTypeMatrix<Type>&() const { return matrix(); }
};

#endif
