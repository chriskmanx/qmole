///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLayout.H>
#include <MSGUI/MSFontManager.H>

MSLayout::MSLayout(MSWidget *owner_,const char *title_) : MSLayoutManager(owner_)
{
  _label=0; // prevent childCreate from adding this as a managed child
  _label=new MSLabel(this,title_);
  init();
}

MSLayout::MSLayout(MSWidget *owner_,const MSStringVector& title_) : MSLayoutManager(owner_)
{
  _label=0; // prevent childCreate from adding this as a managed child
  _label=new MSLabel(this,title_);
  init();
}

void MSLayout::init(void)
{
  _title=label()->label();
  _highlightThickness=0;
  _shadowThickness=2;
  _titleAlignment=MSLeft|MSCenter;
  shadowStyle(MSEtchedIn);
  int offset=shadowThickness()+highlightThickness()+margin();
  label()->highlightThickness(0);
  label()->shadowThickness(0);
  label()->margin(4);
  label()->dynamic(MSTrue);
  label()->moveTo(offset,offset);
  if (label()->columns()>0) label()->map();
}

MSLayout::~MSLayout(void) 
{ if (label()!=0) safeDestroy(_label); }

void MSLayout::updateBackground(unsigned long oldbg_)
{
  MSLayoutManager::updateBackground(oldbg_); 
  label()->background(background());
}

void MSLayout::updateForeground(unsigned long oldfg_)
{
  MSLayoutManager::updateForeground(oldfg_); 
  if (label()->foreground()==oldfg_) label()->foreground(foreground());
}

void MSLayout::naturalSize(void)
{
  freeze();
  MSNodeItem    *hp=childListHead(); 
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();	
     entry->widget()->naturalSize();
   }
  int w=label()->width();
  label()->freeze();
  label()->naturalSize();
  unfreeze();
  label()->unfreeze();
  if (w!=label()->width()) label()->redraw();
}

void MSLayout::childCreate(MSWidget *widget_)
{ if (label()!=0&&widget_!=label()) MSLayoutManager::childCreate(widget_); }
void MSLayout::childDestroy(MSWidget *widget_)
{ if (widget_!=label()) MSLayoutManager::childDestroy(widget_); }
void MSLayout::childMap(MSWidget *widget_)
{ if (widget_!=label()) MSLayoutManager::childMap(widget_); }
void MSLayout::childUnmap(MSWidget *widget_)
{ if (widget_!=label()) MSLayoutManager::childUnmap(widget_);  }

void MSLayout::childConfigure(MSWidget *widget_)
{
  if (widget_!=label()) MSLayoutManager::childConfigure(widget_);       
  else if (label()->mapped()==MSTrue&&label()->frozen()==MSFalse) adjustSize();
}

void MSLayout::configure(void)
{
  if (label()!=0) positionLabel();
}

void MSLayout::computeSize(void)
{
  drawBackground();
  placement();
  positionLabel();
  drawShadow();  
}


void MSLayout::positionLabel(void)
{
  if (label()->columns()>0)
   {
     int xpos,ypos;
     int indent;
     if (titleAlignment()&MSLeft) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=highlightThickness()+shadowThickness()+margin()+indent;
      }
     else if (titleAlignment()&MSRight) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=width()-(highlightThickness()+shadowThickness()+margin())-indent-label()->width();
      }
     else xpos=width()/2-label()->width()/2;

     if (titleAlignment()&MSTop) 
      {
	topShadowOffset(label()->height());
	ypos=0;
      }
     else if (titleAlignment()&MSBottom) 
      {
	topShadowOffset(0);
	ypos=highlightThickness()+shadowThickness()+margin();
      }
     else
      { 
	topShadowOffset(label()->height()/2);
	ypos=0;
      }
     label()->moveTo(xpos,ypos);
     if (mapped()==MSFalse) label()->map();
   }
  else
   { 
     label()->unmap();
     topShadowOffset(0);
   }
}

void MSLayout::updateTitle(void)
{
  undrawShadow();
  label()->freeze();
  label()->foreground(titleForeground());
  int h=label()->height();
  int w=label()->width();

  label()->font(titleFont());
  MSBoolean doPlacement=MSFalse;
  label()->label(title());
  int ml=label()->columns();
  if (ml==0&&label()->mapped()==MSTrue)
   {
     label()->unmap();
     doPlacement=MSTrue;
   }
  else if (ml>0&&label()->mapped()==MSFalse)
   {
     label()->map();
     doPlacement=MSTrue;
   }
  positionLabel();
  label()->unfreeze();
  if (label()->height()!=h||label()->width()!=w||doPlacement==MSTrue) adjustSize();
  else if (label()->mapped()==MSTrue) label()->redraw();
  drawShadow();
}

int MSLayout::innerHeight(void) const
{ return (label()->mapped()==MSTrue)?label()->height():0; }

int MSLayout::realHeight(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (height()-innerHeight()-(rows()-1)*rowSpacing()-2*offset);
}

int MSLayout::realWidth(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (width()-2*innerWidth()-(columns()-1)*columnSpacing()-2*offset);
}

int MSLayout::idealHeight(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (vectorHeight()+innerHeight()+(rows()-1)*rowSpacing()+2*offset);
}

int MSLayout::idealWidth(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  int w=(vectorWidth()+2*innerWidth()+(columns()-1)*columnSpacing()+2*offset);
  if (label()->mapped()==MSTrue)
   {
     int lw=label()->width()+2*offset;
     if ((titleAlignment()&MSLeft)||(titleAlignment()&MSRight))
     lw+=(2*XTextWidth(fontManager()->fontStruct(label()->font()),"M",1));
     return (w>lw)?w:lw;
   }
  else return w;
}





