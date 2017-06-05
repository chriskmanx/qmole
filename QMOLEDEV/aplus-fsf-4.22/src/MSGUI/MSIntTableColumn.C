///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIndexedFunctions.H>
#include <MSGUI/MSIntTableColumn.H>
#include <MSGUI/MSTable.H>

MSIntTableColumn::MSIntTableColumn(MSReportTable *owner_,const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSInt::WithoutCommas);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
}

MSIntTableColumn::MSIntTableColumn(MSReportTable *owner_,const MSStringVector& heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSInt::WithoutCommas);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
}

MSIntTableColumn::MSIntTableColumn(MSReportTable *owner_,MSIntVector& model_,const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSInt::WithoutCommas);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
  model(model_);
}

MSIntTableColumn::MSIntTableColumn(MSReportTable *owner_,MSIntVector& model_,const MSStringVector& heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSInt::WithoutCommas);     
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
  model(model_);
}

MSIntTableColumn::~MSIntTableColumn(void)
{}

void MSIntTableColumn::model(MSIntVector& model_)
{ couple(&model_); }

void MSIntTableColumn::model(const MSIntVector& model_)
{ constCouple(&model_); }

void MSIntTableColumn::vector(const MSIntVector& aVector_)
{ if (MSView::model()!=0) vector()=aVector_; }

unsigned MSIntTableColumn::numRows(void) const
{ return (MSView::model()!=0)?vector().length():0; }

MSBoolean MSIntTableColumn::isDuplicate(unsigned row_)
{ return (MSView::model()!=0&&row_!=0)?vector()(row_)==vector()(row_-1)?MSTrue:MSFalse:MSFalse;}

MSBoolean MSIntTableColumn::breakCriteria(unsigned row_)
{
  return (MSView::model()!=0&&row_!=0)?row_==vector().length()||
  vector()(row_)!=vector()(row_-1)?MSTrue:MSFalse:MSFalse;
}

MSBoolean MSIntTableColumn::validate(const char *pString_,unsigned row_)
{
  if (MSView::model()!=0)
   {
     MSInt aInt;
     if (aInt.set(pString_)==MSError::MSSuccess)
      {
#if defined(MS_OPERATOR_CONVERSION_BUG)
	vector().set(row_,(int)aInt);
#else        
	vector().set(row_,aInt);
#endif        
	return MSTrue;
      }
   }
  return MSFalse;
}

const char *MSIntTableColumn::formatOutput(MSString &buffer_,unsigned row_) 
{
  if (MSView::model()!=0)
   {
     MSInt aInt(vector()(row_));
     aInt.format(buffer_,format());
   }
  return buffer_.string();
}

// report methods
void MSIntTableColumn::breakProcess(MSIndexVector& i_)
{
  MSString buffer;
  MSInt aInt((int)(MSIndexedFunctions::computeIndexedFunction(vector(),weights(),i_,breakProcessMode())));
  breakString()<<aInt.format(buffer,format());
}

const char *MSIntTableColumn::formatBreak(MSString &buffer_,unsigned row_,unsigned) 
{
  if (row_<breakString().length()) buffer_=breakString()(row_);
  return buffer_.string();
}

MSIndexVector MSIntTableColumn::gradeUp(void) const
{
  if (MSView::model()!=0) return ((MSIntVector *)_model)->gradeUp();
  else return MSIndexVector::nullVector();
}

MSIndexVector MSIntTableColumn::gradeDown(void) const
{
  if (MSView::model()!=0) return ((MSIntVector *)_model)->gradeDown();
  else return MSIndexVector::nullVector();
}

void MSIntTableColumn::permute(const MSIndexVector &indexVector_)
{
  if (MSView::model()!=0) ((MSIntVector *)_model)->permute(indexVector_);
}

void MSIntTableColumn::range(MSIndexVector &start_,MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSIndexVector Start=start_,End=end_;
     MSIntVector &vector=*(MSIntVector *)_model;
     start_.removeAll();
     end_.removeAll();
     for (unsigned i=0;i<Start.length();i++)
      {
	// Get a subVector
	MSIndexVector subIndex;
	subIndex.series(End(i)-Start(i)+1,Start(i));
	MSIntVector subVector=MSIntVector::select(vector,subIndex);

	// Then get a unique vector
	MSIndexVector firstOccurrences=subVector.indicesOf(subVector);
	MSIndexVector integers;
	integers.series(firstOccurrences.length());
	MSBinaryVector aMaskVector=firstOccurrences.binaryCompare(integers,MSEqualTo);
	MSIntVector unique=MSIntVector::compress(subVector,aMaskVector);

	// Then append the range of each unique element
	for (unsigned j=0;j<unique.length();j++)
	 {
	   start_.append(Start(i)+subVector.indexOf(unique(j)));
	   end_.append(Start(i)+subVector.lastIndexOf(unique(j)));
	 }
      }
   }
}

MSIndexVector MSIntTableColumn::rangeGradeUp(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSIntVector &vector=*(MSIntVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSIntVector subVector=MSIntVector::select(vector,subIndex);
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

MSIndexVector MSIntTableColumn::rangeGradeDown(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSIntVector &vector=*(MSIntVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSIntVector subVector=MSIntVector::select(vector,subIndex);
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

void MSIntTableColumn::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSIntVector& vectorModel = vector();
     int item = vectorModel(from_);
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
