///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringTableColumn.H>
#include <MSGUI/MSTable.H>

MSStringTableColumn::MSStringTableColumn(MSReportTable *owner_,const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{ init(); }

MSStringTableColumn::MSStringTableColumn(MSReportTable *owner_,const MSStringVector& heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{ init(); }

MSStringTableColumn::MSStringTableColumn(MSReportTable *owner_,MSStringVector& model_,
					 const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  init();
  model(model_);
}

MSStringTableColumn::MSStringTableColumn(MSReportTable *owner_,MSStringVector& model_,
					 const MSStringVector& heading_,const MSSymbol& tag_):
MSTableColumn(owner_,heading_,tag_)
{
  init();
  model(model_);
}

MSStringTableColumn::~MSStringTableColumn(void)
{}

void MSStringTableColumn::init(void)
{ _columnAlignment=MSLeft; }

void MSStringTableColumn::model(MSStringVector& model_)
{ couple(&model_); }

void MSStringTableColumn::model(const MSStringVector& model_)
{ constCouple(&model_); }

void MSStringTableColumn::vector(const MSStringVector& aVector_)
{ if (MSView::model()!=0) vector()=aVector_; }

unsigned MSStringTableColumn::numRows(void) const
{ return (MSView::model()!=0)?vector().length():0; }

// report methods
MSBoolean MSStringTableColumn::isDuplicate(unsigned row_)
{ return (MSView::model()!=0&&row_!=0)?vector()(row_)==vector()(row_-1)?MSTrue:MSFalse:MSFalse;}

MSBoolean MSStringTableColumn::breakCriteria(unsigned row_)
{
  return (MSView::model()!=0&&row_!=0)?row_==vector().length()||
  vector()(row_)!=vector()(row_-1)?MSTrue:MSFalse:MSFalse;
}

MSBoolean MSStringTableColumn::validate(const char *pString_,unsigned row_)
{ return (MSView::model()!=0)?((vector().set(row_,pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

const char *MSStringTableColumn::formatOutput(MSString &buffer_,unsigned row_) 
{
  if (MSView::model()!=0) buffer_=vector()(row_);
  return buffer_.string();
}

const char *MSStringTableColumn::formatBreak(MSString &buffer_,unsigned row_,unsigned col_) 
{
  unsigned index=reportTable()->breakIndex()(row_);
  if (col_==column()&&breakString().length()>0&&breakIndex().indexOf(index)<breakIndex().length())
   {
     int i=row_<breakString().length()?row_:breakString().length()-1;
     buffer_=breakString()(i);
   }
  return buffer_.string();
}

MSIndexVector MSStringTableColumn::gradeUp(void) const
{
  if (MSView::model()!=0) return ((MSStringVector *)_model)->gradeUp();
  else return MSIndexVector::nullVector();
}

MSIndexVector MSStringTableColumn::gradeDown(void) const
{
  if (MSView::model()!=0) return ((MSStringVector *)_model)->gradeDown();
  else return MSIndexVector::nullVector();
}

void MSStringTableColumn::permute(const MSIndexVector &indexVector_)
{
  if (MSView::model()!=0) ((MSStringVector *)_model)->permute(indexVector_);
}

void MSStringTableColumn::range(MSIndexVector &start_,MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSIndexVector Start=start_,End=end_;
     MSStringVector &vector=*(MSStringVector *)_model;
     start_.removeAll();
     end_.removeAll();
     for (unsigned i=0;i<Start.length();i++)
      {
	// Get a subVector
	MSIndexVector subIndex;
	subIndex.series(End(i)-Start(i)+1,Start(i));
	MSStringVector subVector=MSStringVector::select(vector,subIndex);

	// Then get a unique vector
	MSIndexVector firstOccurrences=subVector.indicesOf(subVector);
	MSIndexVector integers;
	integers.series(firstOccurrences.length());
	MSBinaryVector aMaskVector=firstOccurrences.binaryCompare(integers,MSEqualTo);
	MSStringVector unique=MSStringVector::compress(subVector,aMaskVector);

	// Then append the range of each unique element
	for (unsigned j=0;j<unique.length();j++)
	 {
	   start_.append(Start(i)+subVector.indexOf(unique(j)));
	   end_.append(Start(i)+subVector.lastIndexOf(unique(j)));
	 }
      }
   }
}

MSIndexVector MSStringTableColumn::rangeGradeUp(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSStringVector &vector=*(MSStringVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSStringVector subVector=MSStringVector::select(vector,subIndex);
	MSIndexVector sortedIndex=subVector.gradeUp();
	unsigned startIndex=start_(i);
	for (unsigned j=0;j<sortedIndex.length();j++)
	 {
	   index.replaceAt(startIndex+j,sortedIndex(j)+startIndex);
	 }
      }
     return index;
   }
  else return MSIndexVector::nullVector();
}

MSIndexVector MSStringTableColumn::rangeGradeDown(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSStringVector &vector=*(MSStringVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSStringVector subVector=MSStringVector::select(vector,subIndex);
	MSIndexVector sortedIndex=subVector.gradeDown();
	unsigned startIndex=start_(i);
	for (unsigned j=0;j<sortedIndex.length();j++)
	 {
	   index.replaceAt(startIndex+j,sortedIndex(j)+startIndex);
	 }
      }
     return index;
   }
  else return MSIndexVector::nullVector();
}
void MSStringTableColumn::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSStringVector& vectorModel = vector();
     MSString item = vectorModel(from_);
     vectorModel.removeAt(from_);
     if(from_<to_)
      {
        if (to_>=vectorModel.length()) vectorModel.append(item);
        else vectorModel.insertAt(to_,item);
      }
     else
      {
        vectorModel.insertAt(to_,item);
      }
   }
}

void MSStringTableColumn::set(MSAttrValueList& avList_)
{
  MSTableColumn::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
    if (avList_[i].attribute()=="breakString")
      {
        breakString(MSAttrValue::stringToStringVector(avList_[i].value()));
	index<<i;
      }
   }
  avList_.remove(index);
}


MSAttrValueList& MSStringTableColumn::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("breakString",MSAttrValue::stringVectorToString(breakString()));
  return MSTableColumn::get(avList_);
}
