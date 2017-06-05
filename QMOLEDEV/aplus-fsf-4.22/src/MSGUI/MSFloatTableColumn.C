///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIndexedFunctions.H>
#include <MSGUI/MSFloatTableColumn.H>
#include <MSGUI/MSTable.H>

MSFloatTableColumn::MSFloatTableColumn(MSReportTable *owner_,const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSFloat::Decimal2);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
}

MSFloatTableColumn::MSFloatTableColumn(MSReportTable *owner_,const MSStringVector& heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSFloat::Decimal2);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
}

MSFloatTableColumn::MSFloatTableColumn(MSReportTable *owner_,MSFloatVector& model_,
				       const char *heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSFloat::Decimal2);   
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
  model(model_);
}

MSFloatTableColumn::MSFloatTableColumn(MSReportTable *owner_,MSFloatVector& model_,
				       const MSStringVector& heading_,const MSSymbol& tag_) : 
MSTableColumn(owner_,heading_,tag_)
{
  _format=MSFormat(MSFloat::Decimal2);
  _clipMode=MSClipStars;
  _valueQuoted=MSFalse;
  model(model_);
}

MSFloatTableColumn::~MSFloatTableColumn(void)
{}

void MSFloatTableColumn::model(MSFloatVector& model_)
{ couple(&model_); }

void MSFloatTableColumn::model(const MSFloatVector& model_)
{ constCouple(&model_); }

void MSFloatTableColumn::vector(const MSFloatVector& aVector_)
{ if (MSView::model()!=0) vector()=aVector_; }

unsigned MSFloatTableColumn::numRows(void) const
{ return (MSView::model()!=0)?vector().length():0; }

MSBoolean MSFloatTableColumn::isDuplicate(unsigned row_)
{ return (MSView::model()!=0&&row_!=0)?vector()(row_)==vector()(row_-1)?MSTrue:MSFalse:MSFalse;}

MSBoolean MSFloatTableColumn::breakCriteria(unsigned row_)
{
  return (MSView::model()!=0&&row_!=0)?row_==vector().length()||
  vector()(row_)!=vector()(row_-1)?MSTrue:MSFalse:MSFalse;
}

MSBoolean MSFloatTableColumn::validate(const char *pString_,unsigned row_)
{
  if (MSView::model()!=0)
   {
     MSFloat aFloat;
     if (aFloat.set(pString_)==MSError::MSSuccess)
      {
	vector().set(row_,aFloat);
	return MSTrue;
      }
   }
  return MSFalse;
}

const char *MSFloatTableColumn::formatOutput(MSString &buffer_,unsigned row_) 
{
  if (MSView::model()!=0)
   {
     MSFloat aFloat(vector()(row_));
     aFloat.format(buffer_,format());
   }
  return buffer_.string();
}

// report methods
void MSFloatTableColumn::breakProcess(MSIndexVector& i_)
{
  MSString buffer;
  MSFloat aFloat(MSIndexedFunctions::computeIndexedFunction(vector(),weights(),i_,breakProcessMode()));
  breakString()<<aFloat.format(buffer,format());
}

const char *MSFloatTableColumn::formatBreak(MSString &buffer_,unsigned row_,unsigned) 
{
  if (row_<breakString().length()) buffer_=breakString()(row_);
  return buffer_.string();
}

MSIndexVector MSFloatTableColumn::gradeUp(void) const
{
  if (MSView::model()!=0) return ((MSFloatVector *)_model)->gradeUp();
  else return MSIndexVector::nullVector();
}

MSIndexVector MSFloatTableColumn::gradeDown(void) const
{
  if (MSView::model()!=0) return ((MSFloatVector *)_model)->gradeDown();
  else return MSIndexVector::nullVector();
}

void MSFloatTableColumn::permute(const MSIndexVector &indexVector_)
{
  if (MSView::model()!=0) ((MSFloatVector *)_model)->permute(indexVector_);
}

void MSFloatTableColumn::range(MSIndexVector &start_,MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSIndexVector Start=start_,End=end_;
     MSFloatVector &vector=*(MSFloatVector *)_model;
     start_.removeAll();
     end_.removeAll();
     for (unsigned i=0;i<Start.length();i++)
      {
	// Get a subVector
	MSIndexVector subIndex;
	subIndex.series(End(i)-Start(i)+1,Start(i));
	MSFloatVector subVector=MSFloatVector::select(vector,subIndex);

	// Then get a unique vector
	MSIndexVector firstOccurrences=subVector.indicesOf(subVector);
	MSIndexVector integers;
	integers.series(firstOccurrences.length());
	MSBinaryVector aMaskVector=firstOccurrences.binaryCompare(integers,MSEqualTo);
	MSFloatVector unique=MSFloatVector::compress(subVector,aMaskVector);

	// Then append the range of each unique element
	for (unsigned j=0;j<unique.length();j++)
	 {
	   start_.append(Start(i)+subVector.indexOf(unique(j)));
	   end_.append(Start(i)+subVector.lastIndexOf(unique(j)));
	 }
      }
   }
}


MSIndexVector MSFloatTableColumn::rangeGradeUp(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSFloatVector &vector=*(MSFloatVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSFloatVector subVector=MSFloatVector::select(vector,subIndex);
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

MSIndexVector MSFloatTableColumn::rangeGradeDown(const MSIndexVector &start_,const MSIndexVector &end_)
{
  if (MSView::model()!=0&&start_.length()>0&&start_.length()==end_.length())
   {
     MSFloatVector &vector=*(MSFloatVector *)_model;
     MSIndexVector index(vector.length());
     for (unsigned i=0;i<start_.length();i++)
      {
	MSIndexVector subIndex;
	subIndex.series(end_(i)-start_(i)+1,start_(i));
	MSFloatVector subVector=MSFloatVector::select(vector,subIndex);
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

void MSFloatTableColumn::moveRow(int from_, int to_)
{
  if(MSView::model()!=0)
   {
     MSFloatVector& vectorModel = vector();
     double item = vectorModel(from_);
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

        
        
