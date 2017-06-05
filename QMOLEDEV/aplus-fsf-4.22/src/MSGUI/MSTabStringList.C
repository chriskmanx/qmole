///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTabStringList.H>

const unsigned long MSTabStringDefaultTabSize = 6;

MSTabStringList::MSTabStringList(MSWidget *owner_,const char *title_) : 
MSStringList(owner_,title_),_tabSize(MSTabStringDefaultTabSize)
{}

MSTabStringList::MSTabStringList(MSWidget *owner_,const MSStringVector& title_) : 
MSStringList(owner_,title_),_tabSize(MSTabStringDefaultTabSize)
{}

MSTabStringList::MSTabStringList(MSWidget *owner_,MSStringVector& model_,const char *title_) : MSStringList(owner_,model_,title_),_tabSize(MSTabStringDefaultTabSize)
{}

MSTabStringList::MSTabStringList(MSWidget *owner_,MSStringVector& model_,const MSStringVector& title_) : MSStringList(owner_,model_,title_),_tabSize(MSTabStringDefaultTabSize)
{}

MSTabStringList::~MSTabStringList()
{}

const unsigned long& MSTabStringList::tabSize(void) const
{
  return _tabSize;
}

const MSUnsignedLongVector& MSTabStringList::tabStops(void) const
{
  return _tabStops;
}

void MSTabStringList::tabStops(const MSUnsignedLongVector& tabStops_)
{
  _tabStops=tabStops_;
  adjustNumVisible();
  redrawImmediately();
}

void MSTabStringList::tabSize(unsigned long tabSize_)
{
  _tabSize = tabSize_;
  adjustNumVisible(); 
  redrawImmediately();
}

void MSTabStringList::calculateMaxLength(void)
{
  if (MSView::model()) 
   {
     unsigned maxlength=0;
     for (int i=0;i<list().length();i++)
      {
	unsigned len;
	len = rowLength(i);
	maxlength = (maxlength>len)? maxlength : len;
      }
     _maxLength=maxlength;
   }
  else _maxLength=0;
}

unsigned MSTabStringList::rowLength(unsigned int row_) const
{

  if (row_<numRows() && (list()[row_].length()>0))
   {
     int position=0;
     const XFontStruct *font = textFontStruct();
     
     MSString string = list()[row_];
     int charWidth = XTextWidth(font,"M",1);
     
     if (tabStops().length()==0) 
      {
	int tabWidth = charWidth * _tabSize;
	while (string.length()>0)
	 {
	   unsigned nextTab=string.indexOf('\t');
	   if (nextTab) 
	    {
	      position += XTextWidth(font, string, nextTab);
	      string.remove(0,nextTab);
	    }
	   else
	    {
	      if(tabWidth !=0 ) position += tabWidth-(position%tabWidth);
	      string.remove(0,1);
	    }
	 }
      }
     else 
      {
	int tabIndex=0;   // which tabIndex is current
	int pixLength=0;
	while (string.length()>0)
	 {
	   unsigned nextTab=string.indexOf('\t');
	   if (nextTab)
	    {
	      pixLength = XTextWidth(font, string, nextTab);
	      position += pixLength;
	      string.remove(0,nextTab);
	    }
	   else
	    {
	      // tabIndex is modified in nextTabPosition
	      position += nextTabPosition(pixLength,charWidth,tabIndex);
	      pixLength=0;
	      string.remove(0,1);
	    }
	 }
      }
     return (position%charWidth)? (position/charWidth+1):(position/charWidth);
   }
  return 0;
}

void MSTabStringList::drawString(Display *display_,Window window_,GC gc_,const XFontStruct *fs_,
                                 int x_,int y_,const char *pString_,int)
{

  int pixLength=0;
  MSString string=pString_;
  int charWidth = XTextWidth(fs_,"M",1);

  if (tabStops().length()==0) 
   {
     int tabWidth = charWidth * _tabSize;
     while (string.length()>0)
      {
	unsigned nextTab=string.indexOf('\t');
	if (nextTab) 
	 {
	   XDrawString(display_,window_,gc_,fs_,x_,y_,string,nextTab);
	   pixLength = XTextWidth(fs_,string,nextTab);
	   x_ += pixLength;
	   string.remove(0,nextTab);
	 }
	else
	 {
	   if(tabWidth!=0) x_ += tabWidth-(pixLength%tabWidth);
	   pixLength=0;
	   string.remove(0,1);
	 }
      }
   }
  else 
   {
     int tabIndex=0;  
     while (string.length()>0)
      {
	   unsigned nextTab=string.indexOf('\t');
	   if (nextTab)
	    {
	      XDrawString(display_,window_,gc_,fs_,x_,y_,string,nextTab);
	      pixLength = XTextWidth(fs_, string, nextTab);
	      x_ += pixLength;
	      string.remove(0,nextTab);
	    }
	   else
	    {
	      // tabIndex is modified in nextTabPosition.
	      x_ += nextTabPosition(pixLength,charWidth,tabIndex);

	      pixLength=0;
	      string.remove(0,1);
	    }
	 }
      }
}


int MSTabStringList::nextTabPosition(int pixLength_,
				     int charWidth_,
				     int& tabIndex_) const
{
  int spaceLeft=0;

  if (pixLength_) 
   {

     while (pixLength_) 
      {
	int tabWidth=_tabStops[tabIndex_]*charWidth_;

	if (pixLength_ > tabWidth)
	 {
	   pixLength_ -= tabWidth;
	 }
	else if (pixLength_ == tabWidth)
	 {
	   tabIndex_ = (tabIndex_+1)%_tabStops.length();		   	   
	   spaceLeft = _tabStops[tabIndex_]*charWidth_;

	   pixLength_=0;
	 }
	else
	 {
	   spaceLeft = tabWidth-pixLength_;
	   pixLength_=0;
	 }
	tabIndex_ = (tabIndex_+1)%_tabStops.length();	
      }
   }
  else
   {
     spaceLeft = _tabStops[tabIndex_]*charWidth_;
     tabIndex_ = (tabIndex_+1)%_tabStops.length();
   }
  return spaceLeft;
}



void MSTabStringList::set(MSAttrValueList& avList_)
{
  MSStringList::set(avList_);
  MSIndexVector index;
  for(unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="tabSize")
      {
        tabSize((unsigned)avList_[i].value().asInt());
        index<<i;
      }
     else if(avList_[i].attribute()=="tabStops")
      {
        tabStops(MSUnsignedLongVector(avList_[i].value()));
        index<<i;
      }
   }
  avList_.remove(index);
}


MSAttrValueList& MSTabStringList::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("tabSize",MSString(tabSize()));
  avList_<<MSAttrValue("tabStops",tabStops().asString(),MSAttrValue::String);
  return MSStringList::get(avList_);
}
  

 




