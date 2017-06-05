///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSManager.H>

static const int MSManagerDefaultShadowThickness=2;
static const int MSManagerDefaultHighlightThickness=0;
static const unsigned long MSManagerEventMask=(ExposureMask);

MSManager::MSManager(MSWidget *owner_) : MSWidgetCommon(owner_) { init(); }
MSManager::~MSManager(void)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  MSWidget   *wid;

  // delete the widget after the node has been removed from the list
  // this will prevent childDestroy from causing damage
  while ((np=hp->next())!=hp)
   {
     wid=(MSWidget *)np->data();
     delete np;
     if (wid!=0) safeDestroy(wid);
     _childCount--;
   } 
}

void MSManager::init(void)
{
  _childCount=0;
  _shadowThickness=MSManagerDefaultShadowThickness;
  _highlightThickness=MSManagerDefaultHighlightThickness;
  shadowStyle(MSRaised);
  selectInput(MSManagerEventMask); 
}

void MSManager::redraw(void) 
{ drawShadow(); }

void MSManager::placement(void)
{}

void MSManager::configure(void) 
{ placement(); }

void MSManager::childInsert(MSWidget *widget_) 
{ childCreate(widget_); }
void MSManager::childRemove(MSWidget *widget_) 
{ childDestroy(widget_); }

void MSManager::childCreate(MSWidget *widget_)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSBoolean found=MSFalse;

  while (found==MSFalse&&(np=np->next())!=hp)
   {
     if ((MSWidget *)np->data()==widget_)
      {
        found=MSTrue;
        np=hp->prev();
      }
   } 

  if (widget_!=0&&found!=MSTrue)
   {
     MSNodeItem *np=new MSNodeItem((void *)widget_);
     np->insert(hp->next());
     _childCount++;
   }
}

// a child has been destroyed-remove it from the child list
void MSManager::childDestroy(MSWidget *widget_)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;

  while ((np=np->next())!=hp)
   {
     if ((MSWidget *)np->data()==widget_)
      {
        delete np;
        _childCount--;
        np=hp->prev();
      }
   } 
}

void MSManager::print(const char *file_)
{
  MSBoolean   fileOpen=MSFalse;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSWidget   *wid;

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if (displayPrintOpen(this)==MSTrue) 
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
     else return;
   }
  if (mapped()==MSTrue) redraw();
  while ((np=np->next())!=hp)
   {
     wid=(MSWidget *) np->data();
     if (wid!=0&&wid->mapped()==MSTrue) 
      {
	displayPrintOriginInc(wid);
	wid->print();
	displayPrintOriginDec(wid);
      }
   } 
  if (fileOpen==MSTrue) 
   {
     displayPrintClose();
     outputMode(Draw);
   }
}

void MSManager::show(void)
{
  if (mapped()==MSFalse)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     MSWidget   *wid;
     while((np=np->next())!=hp)
      {
	wid=(MSWidget *)np->data();
	wid->show();
      }
     map();
   }
}

void MSManager::visibilityObscured(void)
{
  visible(MSFalse);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSWidget   *wid;  
  while((np=np->next())!=hp)
   {
     wid=(MSWidget *)np->data();
     visibilityObscuredNotify(wid);
   }
}

void MSManager::visibilityUnobscured(void)
{
  visible(MSTrue);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSWidget   *wid;  
  while((np=np->next())!=hp)
   {
     wid=(MSWidget *)np->data();
     visibilityUnobscuredNotify(wid);
   }
}

void MSManager::updateBackground(unsigned long oldbg_)
{ 
  MSWidgetCommon::updateBackground(oldbg_);
  if (mapped()==MSTrue) redraw();
}

MSWidgetVector MSManager::children(void)
{
  MSWidgetVector vector;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp) vector.append((MSWidget *)np->data());
  return vector;
}




