#ifndef MSIntTableColumnHEADER
#define MSIntTableColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTableColumn.H>
#include <MSTypes/MSIntVector.H>

class MSGUIExport MSIntTableColumn : public MSTableColumn
{
public:
  MSIntTableColumn(MSReportTable *,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntTableColumn(MSReportTable *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntTableColumn(MSReportTable *,MSIntVector&,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSIntTableColumn(MSReportTable *,MSIntVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSIntTableColumn(void);

  void model(MSIntVector&);
  void model(const MSIntVector&);

  MSIntVector& viewModel(void)             { return (MSIntVector&)*_model; }
  const MSIntVector& viewModel(void) const { return (MSIntVector&)*_model; }

  MSIntTableColumn& operator=(const MSIntVector& vector_);
  operator MSIntVector&() const;

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
  virtual void breakProcess(MSIndexVector&);
  virtual MSBoolean validate(const char *,unsigned);
  virtual void moveRow(int, int);

  MSIntVector& vector(void) { return (MSIntVector&)*_model; }
  const MSIntVector& vector(void) const { return (MSIntVector&)*_model; }  
  void vector(const MSIntVector&);
};

inline MSIntTableColumn& MSIntTableColumn::operator=(const MSIntVector& vector_)
{ vector(vector_); return *this; }
inline MSIntTableColumn::operator MSIntVector&() const
{ return *((MSIntVector *)_model); } 

#endif
