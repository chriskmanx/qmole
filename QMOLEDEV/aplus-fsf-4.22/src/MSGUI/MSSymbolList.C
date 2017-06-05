///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSSymbolList.H>

MSSymbolList::MSSymbolList(MSWidget *owner_,const char *title_) : 
MSList(owner_,title_)
{}

MSSymbolList::MSSymbolList(MSWidget *owner_,const MSStringVector& title_) : 
MSList(owner_,title_)
{}

MSSymbolList::MSSymbolList(MSWidget *owner_,MSSymbolVector& model_,const char *title_) : 
MSList(owner_,title_)
{ model(model_); }

MSSymbolList::MSSymbolList(MSWidget *owner_,MSSymbolVector& model_,const MSStringVector& title_) : 
MSList(owner_,title_)
{ model(model_); }

MSSymbolList::~MSSymbolList(void)
{}

void MSSymbolList::model(MSSymbolVector& model_)
{ couple(&model_); }

void MSSymbolList::model(const MSSymbolVector& model_)
{ constCouple(&model_); }

void MSSymbolList::list(const MSSymbolVector& list_)
{ if (MSView::model()!=0) list()=list_; }

void MSSymbolList::calculateMaxLength(void)
{ _maxLength=(MSView::model()!=0)?list().maxLength():0; }

unsigned MSSymbolList::rowLength(unsigned row_) const
{
  if (MSView::model()!=0&&row_<list().length())
   {
     const char *pString=list()(row_).symbolName();
     return (pString!=0)?strlen(pString):0;
   }
  return 0;
}

unsigned MSSymbolList::numColumns(void) const
{ return maxLength(); }

unsigned MSSymbolList::numRows(void) const
{ return (MSView::model()!=0)?list().length():0; }

const char *MSSymbolList::formatOutput(MSString &buffer_,unsigned row_)
{
  if (MSView::model()!=0&&row_<list().length()) buffer_=list()(row_).symbolName();
  return buffer_.string();
}

MSBoolean MSSymbolList::validate(const char *pString_,unsigned row_)
{ return (MSView::model()!=0)?((list().set(row_,pString_)==MSError::MSSuccess)?MSTrue:MSFalse):MSTrue; }

// incremental search method override of MSList
void MSSymbolList::incrementalSearch(unsigned row_)
{
  if (MSView::model()!=0)
   {
     const MSSymbolVector& sv=list();
     unsigned startRow=selectedRow()<=(numRows()-2)?selectedRow()+row_:0;
     unsigned i,j=startRow;
     for (i=0;i<sv.length();i++,j++)
      {
	j=j>(numRows()-1)?j-numRows():j;
	const char *pString=sv(j).symbolName();
	if (strstr(pString,isearchString())==pString) 
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

void MSSymbolList::moveRow(int from_,int to_)
{
  MSBoolean wasFrozen=frozen();
  if (wasFrozen==MSFalse) freeze();
  MSSymbol element=list()(from_);  
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
