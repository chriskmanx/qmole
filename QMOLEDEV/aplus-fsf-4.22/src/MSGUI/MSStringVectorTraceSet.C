///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringVectorTraceSet.H>

MSStringVectorTraceSet::MSStringVectorTraceSet(MSGraph *owner_,const char *legends_,const MSSymbol& tag_):
MSTraceSet(owner_,legends_,tag_)
{ init(); }

MSStringVectorTraceSet::MSStringVectorTraceSet(MSGraph *owner_,const MSStringVector& legends_,const MSSymbol& tag_):MSTraceSet(owner_,legends_,tag_)
{ init(); }

MSStringVectorTraceSet::MSStringVectorTraceSet(MSGraph *owner_,MSStringVector& m_,const char *legends_,const MSSymbol& tag_):MSTraceSet(owner_,legends_,tag_)
{ init(); model(m_); }

MSStringVectorTraceSet::MSStringVectorTraceSet(MSGraph *owner_,MSStringVector& m_,const MSStringVector& legends_,const MSSymbol& tag_):MSTraceSet(owner_,legends_,tag_)
{ init(); model(m_); }

MSStringVectorTraceSet::~MSStringVectorTraceSet(void) {} 

void MSStringVectorTraceSet::model(MSStringVector& model_)
{
  couple(&model_);
  MSTraceSet::style((unsigned long)Text);
}

void MSStringVectorTraceSet::model(const MSStringVector& model_)
{
  constCouple(&model_);
  MSTraceSet::style((unsigned long)Text);
}

void MSStringVectorTraceSet::init(void) 
{ _x=0,_y=0; }

int MSStringVectorTraceSet::dataCount(void) const
{ return 1; }     

void MSStringVectorTraceSet::moveTo(int x_,int y_)
{ moveTo((double)x_,(double)y_); }     

void MSStringVectorTraceSet::moveTo(double x_,double y_)
{ _x=x_,_y=y_; computeExtents(); owner()->redraw(); }     

void MSStringVectorTraceSet::xOrigin(double x_)
{ _x=x_; computeExtents(); owner()->redraw(); }     

void MSStringVectorTraceSet::yOrigin(double y_)
{ _y=y_; computeExtents(); owner()->redraw(); }     

double MSStringVectorTraceSet::x(int) const
{ return _x; }     

double MSStringVectorTraceSet::y(int,int) const
{ return _y; }     

MSStringVector& MSStringVectorTraceSet::text(void)
{ return MSView::model()!=0?vector():MSTraceSet::text(); }     

const MSStringVector& MSStringVectorTraceSet::text(void) const
{ return MSView::model()!=0?vector():MSTraceSet::text(); }     

unsigned MSStringVectorTraceSet::textLength(void) const
{ return text().length(); }

void MSStringVectorTraceSet::validate(int,int,double x_,double y_)
{ _x=x_,_y=y_; computeExtents(); }

unsigned long MSStringVectorTraceSet::style(unsigned) const
{return MSTraceSet::style(0);}

void MSStringVectorTraceSet::style(Style x_)
{ style((unsigned long)x_); }

void MSStringVectorTraceSet::style(unsigned long x_)
{ MSTraceSet::style(x_&Text); }

void MSStringVectorTraceSet::style(unsigned long,unsigned) {}
void MSStringVectorTraceSet::style(const MSUnsignedLongVector&) {}

void MSStringVectorTraceSet::set(MSAttrValueList& avList_)
{
  MSTraceSet::set(avList_);
  MSIndexVector index;
  for(unsigned i=0; i<avList_.length(); i++)
   {
     if (avList_[i].attribute()=="xOrigin")
      {
        xOrigin(avList_[i].value().asDouble());
        index<<i;
      }
     else if (avList_[i].attribute()=="yOrigin")
      {
        yOrigin(avList_[i].value().asDouble());
        index<<i;
      }
   }
  avList_.remove(index);
}


MSAttrValueList& MSStringVectorTraceSet::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("xOrigin",MSString(_x));
  avList_<<MSAttrValue("yOrigin",MSString(_y));
  return MSTraceSet::get(avList_);
}

