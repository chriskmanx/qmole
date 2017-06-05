///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringDelimiterList.H>

MSStringDelimiterList::MSStringDelimiterList(MSWidget *owner_,const char *title_) : 
MSDelimiterList(owner_,title_)
{}

MSStringDelimiterList::MSStringDelimiterList(MSWidget *owner_,const MSStringVector& title_) : 
MSDelimiterList(owner_,title_)
{}

MSStringDelimiterList::MSStringDelimiterList(MSWidget *owner_,MSStringVector& model_,const char *title_) : 
MSDelimiterList(owner_,title_)
{ model(model_); }

MSStringDelimiterList::MSStringDelimiterList(MSWidget *owner_,MSStringVector& model_,const MSStringVector& title_) : 
MSDelimiterList(owner_,title_)
{ model(model_); }

MSStringDelimiterList::~MSStringDelimiterList(void)
{}

void MSStringDelimiterList::model(MSStringVector& model_)
{ couple(&model_); }

void MSStringDelimiterList::list(const MSStringVector& list_)
{ if (MSView::model()!=0) list()=list_; }

void MSStringDelimiterList::calculateMaxLength(void)
{ _maxLength=(MSView::model()!=0)?list().maxLength():0; }

unsigned MSStringDelimiterList::rowLength(unsigned row_) const
{ return (MSView::model()!=0&&row_<list().length())?list()[row_].length():0; }

unsigned MSStringDelimiterList::numColumns(void) const
{ return maxLength(); }

unsigned MSStringDelimiterList::numRows(void) const
{ return (MSView::model()!=0)?list().length():0; }

const char *MSStringDelimiterList::formatOutput(MSString &buffer_,unsigned row_)
{
  if (MSView::model()!=0&&row_<list().length()) buffer_=list()(row_);
  return buffer_.string();
}

MSBoolean MSStringDelimiterList::validate(const char *pString_,unsigned row_)
{ return (MSView::model()!=0)?((list().set(row_,pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

// incremental search method override of MSList
void MSStringDelimiterList::incrementalSearch(unsigned row_)
{
  if (MSView::model()!=0)
   {
     MSStringVector& sv=list();
     unsigned startRow=selectedRow()<=(numRows()-2)?selectedRow()+row_:0;
     unsigned i,j=startRow;
     for (i=0;i<sv.length();i++,j++)
      {
	j=j>(numRows()-1)?j-numRows():j;
	if (strstr(sv[j],isearchString())==sv[j]) 
	 {
	   if (j!=selectedRow())
	    { 
	      isearchVector()<<j;
	      selectedRow(j);
	    }
	   return;
	 }
      }
     server()->bell();
   }
}

void MSStringDelimiterList::moveRow(int from_,int to_)
{
  MSBoolean wasFrozen=frozen();
  if (wasFrozen==MSFalse) freeze();
  MSString element=list().elementAt(from_);
  list().removeAt(from_);
  if (from_<to_)
   {
     if (to_>=list().length()) list().append(element);
     else list().insertAt(to_,element);
   }
  else
   {
     list().insertAt(to_,element);
   }
  MSList::moveRow(from_,to_);
  if (wasFrozen==MSFalse) unfreeze();
}



