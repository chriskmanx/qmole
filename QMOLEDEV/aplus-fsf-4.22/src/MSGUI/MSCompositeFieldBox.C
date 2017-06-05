///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCompositeField.H>
#include <MSGUI/MSCompositeFieldBox.H>

static const unsigned long MSCompositeFieldBoxEventMask=(ExposureMask);
static const unsigned MSCompositeFieldBoxDefaultValueWidth=9;
static const unsigned MSCompositeFieldBoxDefaultEditWidth=256;

MSCompositeFieldBox::MSCompositeFieldBox(MSWidget *owner_,const char *title_) : 
MSLayout(owner_,title_)
{ init();}
MSCompositeFieldBox::MSCompositeFieldBox(MSWidget *owner_,const MSStringVector& title_) : 
MSLayout(owner_,title_)
{ init();}

void MSCompositeFieldBox::init(void)
{
  _valueWidth=MSCompositeFieldBoxDefaultValueWidth;
  _editWidth=MSCompositeFieldBoxDefaultEditWidth;
  _orientation=MSLayoutManager::Vertical;
  shadowStyle(MSEtchedIn);
  selectInput(MSCompositeFieldBoxEventMask);
}

MSCompositeFieldBox::~MSCompositeFieldBox(void) {}

MSCompositeField *MSCompositeFieldBox::field(const MSSymbol& tag_)
{
  MSLayoutEntry    *lentry;
  MSNodeItem       *hp=childListHead(); 
  MSNodeItem       *np=hp;
  MSCompositeField *cfield;
  
  while ((np=np->next())!=hp)
   {
     lentry=(MSLayoutEntry *)np->data();	
     cfield=(MSCompositeField *)lentry->widget();
     if (cfield->tag()==tag_) return cfield;
   }
  return 0;
} 

const MSCompositeField *MSCompositeFieldBox::field(const MSSymbol& tag_) const
{
  MSLayoutEntry    *lentry;
  MSNodeItem       *hp=(MSNodeItem *)childListHead(); 
  MSNodeItem       *np=hp;
  MSCompositeField *cfield;
  
  while ((np=np->next())!=hp)
   {
     lentry=(MSLayoutEntry *)np->data();	
     cfield=(MSCompositeField *)lentry->widget();
     if (cfield->tag()==tag_) return cfield;
   }
  return 0;
} 

void MSCompositeFieldBox::placement(void)
{
  MSLayout::placement();
  if (mapped()==MSTrue) alignLabels();  
}

void MSCompositeFieldBox::firstMapNotify(void)
{
  setPositions();
  MSLayoutEntry    *entry;
  MSNodeItem       *hp=childListHead(); 
  MSNodeItem       *np=hp;
  MSCompositeField *cfield;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     cfield=(MSCompositeField *)entry->widget();
     if (valueWidth()!=MSCompositeFieldBoxDefaultValueWidth) cfield->valueWidth(valueWidth());
     cfield->naturalSize();
   }
  alignLabels();  
}

void MSCompositeFieldBox::alignLabels(void)
{
  MSLayoutEntry    *entry;
  MSNodeItem       *hp=childListHead(); 
  MSNodeItem       *np=hp;
  MSCompositeField *cfield;
  unsigned          lw,max;
  
  for (unsigned col=0;col<columns();col++)
   {
     max=0;
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();	
	if (entry->mapped()==MSTrue&&entry->at().column()==col)
	 {
	   cfield=(MSCompositeField *)entry->widget();
	   lw=cfield->computeLabelPixelWidth();
	   max=(lw>max)?lw:max;
	 }
      }
     np=hp;
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();	
	if (entry->mapped()==MSTrue&&entry->at().column()==col)
	 {
	   cfield=(MSCompositeField *)entry->widget();
	   cfield->labelPixelWidth(max);
	 }
      }
   }
}

// remove or add extra space to last row
// override of MSLayoutManager function
void MSCompositeFieldBox::extraRowSpace(int num_,MSLayoutVector *vec_,int size_)
{
  if (num_>0)
   {
     int sum=0;
     for (unsigned i=0;i<num_;i++) sum+=vec_[i].value();
     int diff=size_-sum;
     
     if (diff>0) vec_[num_-1]._value+=diff;
     else if (diff<0) 
      {
        diff=sum-size_;
        vec_[num_-1]._value-=diff;
      }
   }
}

void MSCompositeFieldBox::valueWidth(unsigned valueWidth_)  
{ 
  if (valueWidth()!=valueWidth_)
   {
     MSBoolean f=frozen();
     freeze(); 
     _valueWidth=valueWidth_;
     MSLayoutEntry    *entry;
     MSNodeItem       *hp=childListHead(); 
     MSNodeItem       *np=hp;
     MSCompositeField *cfield;  
     while ((np=np->next())!=hp)
      {
        entry=(MSLayoutEntry *)np->data();	
        cfield=(MSCompositeField *)entry->widget();
        cfield->valueWidth(valueWidth());
      }
     if (f==MSFalse) unfreeze();
   }
}

void MSCompositeFieldBox::editWidth(unsigned editWidth_)   
{ 
  if (editWidth()!=editWidth_)
   {
     _editWidth=editWidth_; 
     MSLayoutEntry    *entry;
     MSNodeItem       *hp=childListHead(); 
     MSNodeItem       *np=hp;
     MSCompositeField *cfield;  
     while ((np=np->next())!=hp)
      {
        entry=(MSLayoutEntry *)np->data();	
        cfield=(MSCompositeField *)entry->widget();
        cfield->editWidth(editWidth());
      }
   }
}

void MSCompositeFieldBox::set(MSAttrValueList& avList_)
{
  MSLayout::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="valueWidth") valueWidth(avList_[i].value().asInt()),index<<i;  
   }
  avList_.remove(index);
}

MSAttrValueList& MSCompositeFieldBox::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("valueWidth",      MSString(valueWidth()));
  return MSLayout::get(avList_);
}










