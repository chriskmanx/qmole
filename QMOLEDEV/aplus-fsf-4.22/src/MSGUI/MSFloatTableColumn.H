#ifndef MSFloatTableColumnHEADER
#define MSFloatTableColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTableColumn.H>
#include <MSTypes/MSFloatVector.H>

class MSGUIExport MSFloatTableColumn : public MSTableColumn
{
public:
  MSFloatTableColumn(MSReportTable *,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatTableColumn(MSReportTable *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatTableColumn(MSReportTable *,MSFloatVector&,const char *heading_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSFloatTableColumn(MSReportTable *,MSFloatVector&,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSFloatTableColumn(void);

  void model(MSFloatVector&);
  void model(const MSFloatVector&);

  MSFloatVector& viewModel(void)             { return (MSFloatVector&)*_model; }
  const MSFloatVector& viewModel(void) const { return (MSFloatVector&)*_model; }

  MSFloatTableColumn& operator=(const MSFloatVector& vector_);
  operator MSFloatVector&() const;
  
  
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

  virtual void moveRow(int from_,int to_);
  
  MSFloatVector& vector(void) { return (MSFloatVector&)*_model; }
  const MSFloatVector& vector(void) const { return (MSFloatVector&)*_model; }  
  void vector(const MSFloatVector&);
};

inline MSFloatTableColumn& MSFloatTableColumn::operator=(const MSFloatVector& vector_)
{ vector(vector_); return *this; }
inline MSFloatTableColumn::operator MSFloatVector&() const
{ return *((MSFloatVector *)_model); } 

#endif
