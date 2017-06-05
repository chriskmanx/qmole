///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSActionBox.H>
#include <MSGUI/MSActionButton.H>

MSActionBox::MSActionBox(MSWidget *owner_,const char *title_) : 
MSLayout(owner_,title_)
{ init(); }

MSActionBox::MSActionBox(MSWidget *owner_,const MSStringVector& title_) : 
MSLayout(owner_,title_)
{ init(); }

void MSActionBox::init(void)
{
  _orientation=MSLayoutManager::Horizontal;
  shadowStyle(MSEtchedIn);
  _activeButton=0;
}
MSActionBox::~MSActionBox(void) {}

const MSSymbol& MSActionBox::symbol(void)
{
  static MSSymbol sym ("MSActionBox");
  return sym;
}

const MSSymbol& MSActionBox::widgetType(void) const
{ return symbol(); }

void MSActionBox::alignment(unsigned alignment_)
{
  if (alignment_!=alignment()) 
   {
     MSNodeItem     *hp=childListHead(); 
     MSNodeItem     *np=hp;
     MSLayoutEntry  *entry;
     MSActionButton *btn;
     
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();	
	btn=(MSActionButton *)entry->widget();
	btn->alignment(alignment_);
      }
   }
}

unsigned MSActionBox::alignment(void) const
{
  unsigned r=MSCenter;
  const MSLayoutEntry *entry=getEntry(0);
  if (entry!=0)
   {
     MSActionButton *btn=(MSActionButton *)entry->widget();
     r=btn->alignment();
   }
  return r;
}

MSActionButton *MSActionBox::button(const MSSymbol& tag_)
{
  MSLayoutEntry  *entry;
  MSNodeItem     *hp=childListHead(); 
  MSNodeItem     *np=hp;
  MSActionButton *btn;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     btn=(MSActionButton *)entry->widget();
     if (btn->tag()==tag_) return btn;
   }
  return 0;
} 

const MSActionButton *MSActionBox::button(const MSSymbol& tag_) const
{
  MSLayoutEntry  *entry;
  MSNodeItem     *hp=(MSNodeItem *)childListHead(); 
  MSNodeItem     *np=hp;
  MSActionButton *btn;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     btn=(MSActionButton *)entry->widget();
     if (btn->tag()==tag_) return btn;
   }
  return 0;
} 

MSActionButton *MSActionBox::button(int tag_)
{
  MSLayoutEntry  *entry;
  MSNodeItem     *hp=childListHead();
  MSNodeItem     *np=hp;
  MSActionButton *btn;
 
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     btn=(MSActionButton *)entry->widget();
     if (btn->integerTag()==tag_) return btn;
   }
  return 0;
}
 
const MSActionButton *MSActionBox::button(int tag_) const
{
  MSLayoutEntry  *entry;
  MSNodeItem     *hp=(MSNodeItem *)childListHead();
  MSNodeItem     *np=hp;
  MSActionButton *btn;
 
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     btn=(MSActionButton *)entry->widget();
     if (btn->integerTag()==tag_) return btn;
   }
  return 0;
}

void MSActionBox::placement(void)
{
  MSLayout::placement();
  redraw();
}

void MSActionBox::activate(void) 
{ activateCallback(MSWidgetCallback::activate); }

void MSActionBox::activate(MSActionButton *btn_)
{
  _activeButton=btn_;
  activate();
  _activeButton=0;
}

MSSymbolVector MSActionBox::symbolicState(void) const
{
  MSSymbolVector vector;
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSLayoutEntry *entry=(MSLayoutEntry *)np->data();
     MSActionButton *button=(MSActionButton *)entry->widget();
     if (button->state()==MSTrue) vector.append(button->tag());
   }
  return vector;
}

MSIntVector MSActionBox::integerState(void) const
{
  MSIntVector vector;
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSLayoutEntry *entry=(MSLayoutEntry *)np->data();
     MSActionButton *button=(MSActionButton *)entry->widget();
     if (button->state()==MSTrue) vector.append(button->integerTag());
   }
  return vector;
}

void MSActionBox::updateBackground(unsigned long oldbg_)
{
  MSLayout::updateBackground(oldbg_);
  MSLayoutEntry *entry;
  MSNodeItem    *hp=childListHead(); 
  MSNodeItem    *np=hp;
  MSWidget     *btn;  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     btn=(MSWidget *)entry->widget();
     btn->background(background());
   }
  MSLayout::redraw();
}

void MSActionBox::updateFont(Font oldfont_)
{
  MSLayout::updateFont(oldfont_);
  MSLayoutEntry *entry;
  MSNodeItem    *hp=childListHead(); 
  MSNodeItem    *np=hp;
  MSWidget     *btn;  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     btn=(MSWidget *)entry->widget();
     btn->font(font());
   }
  MSLayout::redraw();
}

void MSActionBox::childDestroy(MSWidget *child_)
{
  if (activeButton()==child_) _activeButton=0;
  MSLayout::childDestroy(child_);
}

void MSActionBox::set(MSAttrValueList& avList_)
{ MSLayout::set(avList_); }


MSAttrValueList& MSActionBox::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  return MSLayout::get(avList_);
}

