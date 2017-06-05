#ifndef MSVectorTableColumnHEADER
#define MSVectorTableColumnHEADER

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

#include <MSGUI/MSTableColumn.H>
#include <MSTypes/MSBuiltinVector.H>
#include <MSTypes/MSTypeVector.H>

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSVectorTableColumn.C")
#endif

template<class Type>
class MSVectorTableColumn : public MSTableColumn
{
public:
  MSVectorTableColumn(MSReportTable *,const char *title_=0,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTableColumn(MSReportTable *,const MSStringVector&,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTableColumn(MSReportTable *,MSTypeVector<Type>&,const char *title_=0,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSVectorTableColumn(MSReportTable *,MSTypeVector<Type>&,const MSStringVector&,
		      const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSVectorTableColumn(void);

  void model(MSTypeVector<Type>&);
  void model(const MSTypeVector<Type>&);

  MSTypeVector<Type>& viewModel(void)             { return *((MSTypeVector<Type>*)_model); }
  const MSTypeVector<Type>& viewModel(void) const { return *((MSTypeVector<Type>*)_model); }
  MSVectorTableColumn<Type>& operator=(const MSTypeVector<Type>& vector_);

  // This member function now inlined, because of bug in Visual C++
  operator MSTypeVector<Type>&() const
  { return *((MSTypeVector<Type> *)_model); }

  virtual MSIndexVector gradeUp(void) const;
  virtual MSIndexVector gradeDown(void) const;
  virtual void permute(const MSIndexVector &);
  virtual void range(MSIndexVector &start_,MSIndexVector &end_);
  virtual MSIndexVector rangeGradeUp(const MSIndexVector &start_,const MSIndexVector &end_);
  virtual MSIndexVector rangeGradeDown(const MSIndexVector &start_,const MSIndexVector &end_);
  virtual unsigned numRows(void) const;
  virtual MSBoolean isDuplicate(unsigned);
  virtual MSBoolean breakCriteria(unsigned);
  virtual const char *formatOutput(MSString &,unsigned);
  virtual const char *formatBreak(MSString &,unsigned,unsigned);

protected:
  void init(void);
  virtual void breakProcess(MSIndexVector&);
  virtual MSBoolean validate(const char *,unsigned);
  virtual void moveRow(int, int);

  inline MSTypeVector<Type>& vector(void);
  inline const MSTypeVector<Type>& vector(void) const;  
  void vector(const MSTypeVector<Type>&);

  static const Type& getType(void);
};

template<class Type>
inline MSTypeVector<Type>& MSVectorTableColumn<Type>::vector(void)
{ return (MSTypeVector<Type>&)*_model; }

template<class Type>
inline const MSTypeVector<Type>& MSVectorTableColumn<Type>::vector(void) const
{ return (MSTypeVector<Type>&)*_model; }

#endif
