///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMethodCallback.H>
#include <MSGUI/MSArrow.H>
#include <MSGUI/MSFontManager.H>
#include <MSGUI/MSWidgetCursor.H>
#include <MSGUI/MSNotebook.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

const char * const MSNotebookDefaultBackpageBackground="deepskyblue4";
const char * const MSNotebookDefaultBackpageForeground="black";
const char * const MSNotebookDefaultFrameBackground="deepskyblue4";
const int MSNotebookDefaultHighlightThickness=0;
const int MSNotebookDefaultShadowThickness=2;
const int MSNotebookDefaultBindingWidth=36;
const int MSNotebookDefaultFrameThickness=2;
const int MSNotebookDefaultBorderWidth=7;
const int MSNotebookDefaultBorderHeight=7;
const int MSNotebookDefaultMarginWidth=4;
const int MSNotebookDefaultMarginHeight=4;
const int MSNotebookDefaultBackpages=3;
const int MSNotebookDefaultBackpageThickness=4;
const int MSNotebookMinimumBindingWidth=3;
const int MSNotebookMinimumArrowSize=15;
const int MSNotebookDefaultTabSpacing=4;
const int MSNotebookDefaultArrowOffset=4;
const int MSNotebookEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask);
const unsigned long MSNotebookArrowBrowseRepeatInterval=500;
const unsigned long MSNotebookArrowBrowseRepeatThreshold=500;
const unsigned long MSNotebookArrowSearchRepeatInterval=150;
const unsigned long MSNotebookArrowSearchRepeatThreshold=500;

inline int MSNotebook::backpageMargin(void)
{ return (backpages()*backpageThickness()); }

inline int MSNotebook::labelHeight(void)
{ return ((label()->columns()>0)?label()->height():0); }

inline int MSNotebook::labelWidth(void)
{
  int indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
  int w=highlightThickness()*2+shadowThickness()*2+indent*2;
  return ((label()->columns()>0)?w+label()->width():w);
}

MSNotebook::MSNotebook(MSWidget *owner_,const char *title_)
: MSManager(owner_)
{
  internalEvent(MSTrue);
  _label=new MSLabel(this,title_);
  init();
  internalEvent(MSFalse);
}

MSNotebook::MSNotebook(MSWidget *owner_,const MSStringVector &title_)
: MSManager(owner_)
{
  internalEvent(MSTrue);
  _label=new MSLabel(this,title_);
  init();
  internalEvent(MSFalse);
}

MSNotebook::~MSNotebook(void)
{
  internalEvent(MSTrue);
  if (redrawPixmap()!=0) delete _redrawPixmap;
  if (bindingPixmap()!=0) XFreePixmap(display(),_bindingPixmap);
  if (upLeftArrow()!=0) upLeftArrow()->destroy();
  if (downRightArrow()!=0) downRightArrow()->destroy();
  if (label()!=0) label()->destroy();
  if (_popupMenu!=0) _popupMenu->destroy();
  if (_applicationPopup!=0) _applicationPopup->destroy();
  XFreeGC(display(),_notebookGC);
  XFreeGC(display(),_bindingGC);
  XFreeGC(display(),_backpageGC);
  NotebookEntry *entry;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  while ((np=hp->next())!=hp)
   {
     entry=(NotebookEntry *) np->data();
     delete np;
     if (entry!=0)
      {
        if (entry->widget()!=0) entry->widget()->destroy();
        delete entry;
      }
     _childCount--;
   }
}

void MSNotebook::init(void)
{
  _redrawPixmap=new MSBackingStorePixmap(server(),"MSGeneral");
  _title=label()->label();
  _titleAlignment=MSLeft|MSCenter;
  _highlightThickness=MSNotebookDefaultHighlightThickness;
  _shadowThickness=MSNotebookDefaultShadowThickness;
  _firstEntry=0;
  _currentEntry=0;
  _orientation=Horizontal;
  _tabAlignment=MSRight;
  _backpageForeground=server()->pixel(MSNotebookDefaultBackpageForeground);
  _backpageBackground=server()->pixel(MSNotebookDefaultBackpageBackground);
  _frameBackground=server()->pixel(MSNotebookDefaultFrameBackground);
  _selectedPageForeground=foreground();
  _selectedPageBackground=background();
  _showBinding=MSTrue;
  _showTabs=MSTrue;
  _showPopup=MSTrue;
  _bindingWidth=MSNotebookDefaultBindingWidth;
  _frameThickness=MSNotebookDefaultFrameThickness;
  _borderWidth=MSNotebookDefaultBorderWidth;
  _borderHeight=MSNotebookDefaultBorderHeight;
  _marginWidth=MSNotebookDefaultMarginWidth;
  _marginHeight=MSNotebookDefaultMarginHeight;
  _backpages=MSNotebookDefaultBackpages;
  _backpageThickness=MSNotebookDefaultBackpageThickness;
  _tabSpacing=MSNotebookDefaultTabSpacing;
  _arrowOffset=MSNotebookDefaultArrowOffset;
  _bindingPixmap=0;
  _upLeftArrow=new NotebookArrow(this,MSArrow::Up);
  upLeftArrow()->repeatInterval(MSNotebookArrowSearchRepeatInterval);
  _downRightArrow=new NotebookArrow(this,MSArrow::Down);
  downRightArrow()->repeatInterval(MSNotebookArrowSearchRepeatInterval);
  _notebookGC=XCreateGC(display(),window(),0,0);
  _bindingGC=XCreateGC(display(),window(),0,0);
  _backpageGC=XCreateGC(display(),window(),0,0);
  _lockSize=MSFalse;
  _popupMenu=0;
  _applicationPopup=0;
  selectInput(MSNotebookEventMask);
  shadowStyle(MSEtchedIn);
  label()->margin(4);
  label()->dynamic(MSTrue);
}

void MSNotebook::orientation(Orientation orientation_)
{
  if (_orientation!=orientation_)
   {
     _orientation=orientation_;
     if (orientation()==Horizontal)
      {
        if (tabAlignment()==MSTop||tabAlignment()==MSBottom) _tabAlignment=MSRight;
	upLeftArrow()->type(MSArrow::Up);
	downRightArrow()->type(MSArrow::Down);
      }
     else
      {
        if (tabAlignment()==MSLeft||tabAlignment()==MSRight) _tabAlignment=MSBottom;
	upLeftArrow()->type(MSArrow::Left);
	downRightArrow()->type(MSArrow::Right);
      }
     adjustSize();
     if (mapped()==MSTrue)
      {
	// Force a redraw on all tabs, they don't always get
	// an expose event after the reordering
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	while ((np=np->next())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   if (entry->tab()->mapped()==MSTrue) entry->tab()->redraw();
	 }
      }
   }
}

void MSNotebook::tabAlignment(MSAlignment tabAlignment_)
{
  if (tabAlignment_!=MSNone&&tabAlignment_!=MSCenter)
   {
     if (_tabAlignment!=tabAlignment_)
      {
        _tabAlignment=tabAlignment_;
        Orientation newOrientation;
        if (tabAlignment()==MSLeft||tabAlignment()==MSRight) newOrientation=Horizontal;
        else newOrientation=Vertical;
        
        if (orientation()!=newOrientation) orientation(newOrientation);
        else
         {
           placement();
           if (mapped()==MSTrue)
            {
              // Force a redraw on all tabs, they don't always get
              // an expose event after the reordering
              MSNodeItem *hp=childListHead();
              MSNodeItem *np=hp;
              NotebookEntry *entry;
              while ((np=np->next())!=hp)
               {
                 entry=(NotebookEntry *)np->data();
                 if (entry->tab()->mapped()==MSTrue) entry->tab()->redraw();
               }
            }
         }
      }
   }
}

void MSNotebook::pageTitle(MSWidget *widget_,const char *title_)
{
  NotebookEntry *entry=getEntry(widget_);
  if (entry!=0)
   {
     entry->title(title_);
     if (entry->managed()==MSTrue)
      {
        updateTitleVector();
        if (firstMap()==MSTrue) adjustSize();
      }
   }
}

const char *MSNotebook::pageTitle(MSWidget *widget_) const
{ return titleFromWidget(widget_); }

const char *MSNotebook::titleFromWidget(MSWidget *widget_) const
{
  static MSString title;
  NotebookEntry *entry=getEntry(widget_);
  if (entry!=0) title=entry->title().asString('\n');
  else title.removeAll();
  return title.string();
}

MSWidget *MSNotebook::widgetFromTitle(const char *title_) const
{
  NotebookEntry *entry=getEntry(title_);
  if (entry!=0) return entry->widget();
  else return 0;
}

void MSNotebook::currentTitle(const char *title_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry=0;
  MSString title(title_);
  while ((np=np->next())!=hp)
   {
     NotebookEntry *ne=(NotebookEntry *) np->data();
     if (ne->managed()==MSTrue&&ne->title()==title)
      {
	entry=ne;
	break;
      }
   }  
  currentEntry(entry);
  if (firstMap()==MSTrue)
   {
     resetFirstEntry();
     positionTabs();
   }
}

const char *MSNotebook::currentTitle(void) const
{
  static MSString title;
  if (currentEntry()!=0) title=currentEntry()->title().asString('\n');
  else title.removeAll();
  return title.string();
}

void MSNotebook::currentWidget(MSWidget *widget_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry=0;
  while ((np=np->next())!=hp)
   {
     NotebookEntry *ne=(NotebookEntry *) np->data();
     if (ne->managed()==MSTrue&&ne->widget()==widget_)
      {
	entry=ne;
	break;
      }
   }  
  currentEntry(entry);
  if (firstMap()==MSTrue)
   {
     resetFirstEntry();
     positionTabs();
   }
}

MSWidget *MSNotebook::currentWidget(void) const
{
  if (currentEntry()!=0) return currentEntry()->widget();
  else return 0;
}

void MSNotebook::tabAttribute(MSWidget *child_,const MSNotebookTabAttribute &tabAttr_)
{
  // Only allow setting of tab info if the MSNotebookTabAttribute object 
  // was created using this MSNotebook widget
  if (tabAttr_.notebook()==this)
   {
     NotebookEntry *entry=getEntry(child_);
     if (entry!=0)
      {
        unsigned long oldTabBackground=entry->tab()->tabBackground();
        entry->tab()->freeze();
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabBackground)==MSTrue)
         {
           entry->tab()->tabBackground(tabAttr_.background());
           entry->tab()->background(tabAttr_.background());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabForeground)==MSTrue)
         {
           entry->tab()->tabForeground(tabAttr_.foreground());
           entry->tab()->foreground(tabAttr_.foreground());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabFont)==MSTrue)
         {
           entry->tab()->tabFont(tabAttr_.font());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabLabel)==MSTrue)
         {
           entry->tab()->label(tabAttr_.label());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabPixmap)==MSTrue)
         {
           entry->tab()->pixmap(*tabAttr_.pixmap());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabLabelAlignment)==MSTrue)
         {
           entry->tab()->labelAlignment(tabAttr_.labelAlignment());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabSensitivity)==MSTrue)
         {
           entry->tab()->sensitive(tabAttr_.sensitive());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabDisplayToolTip)==MSTrue)
         {
           entry->tab()->displayToolTip(tabAttr_.displayToolTip());
         }
        if (tabAttr_.isSet(MSNotebookTabAttribute::TabToolTip)==MSTrue)
         {
           entry->tab()->toolTip(tabAttr_.toolTip());
         }
        entry->tab()->unfreeze();
        entry->tab()->naturalSize();
        if (entry->managed()==MSTrue)
         {
           updateTitleVector();
           //We only need to adjust the size if either the font or the label has changed
           if (firstMap()==MSTrue&&
               (tabAttr_.isSet(MSNotebookTabAttribute::TabFont)||
                tabAttr_.isSet(MSNotebookTabAttribute::TabLabel)))
            {
              adjustSize();
            }
           //We need to be doubly sure that the frame gets redrawn if the tabBackground is
           //changed and the the current entry is the one we're modifying.  On rare occasions,
           //this may be reduntant, but this operation is relatively cheap.
           if (currentEntry()==entry&&oldTabBackground!=entry->tab()->tabBackground())
            {
              drawFrameShadow(window());
            }
         }
      }
   }
}

MSNotebookTabAttribute MSNotebook::tabAttribute(MSWidget *child_) const
{
  MSNotebookTabAttribute tabAttr((MSNotebook *)this);
  NotebookEntry *entry=getEntry(child_);
  if (entry!=0)
   {
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabForeground))
      {
        tabAttr.foreground(entry->tab()->tabForeground());
      }
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabBackground))
      {
        tabAttr.background(entry->tab()->tabBackground());
      }
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabFont))
      {
        tabAttr.font(entry->tab()->font());
      }
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabToolTip))
      {
        tabAttr.toolTip(entry->tab()->toolTip());
      }
     tabAttr.displayToolTip(entry->tab()->displayToolTip());
     tabAttr.sensitive(entry->tab()->sensitive());
     tabAttr.label(entry->tab()->label());
     tabAttr.labelAlignment(entry->tab()->labelAlignment());
     if (tabAttr.pixmap()!=0) tabAttr.pixmap(*tabAttr.pixmap());
   }  
  return tabAttr;
}

void MSNotebook::showBinding(MSBoolean showBinding_)
{
  if (_showBinding!=showBinding_)
   {
     _showBinding=showBinding_;
     adjustSize();
   }
}

void MSNotebook::showTabs(MSBoolean showTabs_)
{
  if (_showTabs!=showTabs_)
   {
     _showTabs=showTabs_;
     if (showTabs()==MSFalse)
      {
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	while ((np=np->next())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   entry->tab()->unmap();
	 }
	upLeftArrow()->unmap();
	downRightArrow()->unmap();
      }
     adjustSize();
   }
}

void MSNotebook::bindingWidth(unsigned bindingWidth_)
{
  if (_bindingWidth!=bindingWidth_)
   {
     _bindingWidth=bindingWidth_;
     adjustSize();
   }
}

void MSNotebook::tabSpacing(unsigned tabSpacing_)
{
  if (_tabSpacing!=tabSpacing_)
   {
     _tabSpacing=tabSpacing_;
     adjustSize();
   }
}

void MSNotebook::frameThickness(unsigned frameThickness_)
{
  if (_frameThickness!=frameThickness_)
   {
     _frameThickness=frameThickness_;
     adjustSize();
   }
}

void MSNotebook::borderWidth(unsigned borderWidth_)
{
  if (_borderWidth!=borderWidth_)
   {
     _borderWidth=borderWidth_;
     adjustSize();
   }
}

void MSNotebook::borderHeight(unsigned borderHeight_)
{
  if (_borderHeight!=borderHeight_)
   {
     _borderHeight=borderHeight_;
     adjustSize();
   }
}

void MSNotebook::marginWidth(unsigned marginWidth_)
{
  if (_marginWidth!=marginWidth_)
   {
     _marginWidth=marginWidth_;
     adjustSize();
   }
}

void MSNotebook::marginHeight(unsigned marginHeight_)
{
  if (_marginHeight!=marginHeight_)
   {
     _marginHeight=marginHeight_;
     adjustSize();
   }
}

void MSNotebook::backpages(unsigned backpages_)
{
  if (_backpages!=backpages_)
   {
     _backpages=backpages_;
     adjustSize();
   }
}

void MSNotebook::backpageThickness(unsigned backpageThickness_)
{
  if (_backpageThickness!=backpageThickness_)
   {
     _backpageThickness=backpageThickness_;
     adjustSize();
   }
}

void MSNotebook::backpageForeground(unsigned long backpageForeground_)
{
  if (_backpageForeground!=backpageForeground_)
   {
     _backpageForeground=backpageForeground_;
     redraw();
   }
}

void MSNotebook::backpageForeground(const char *backpageForeground_)
{
  backpageForeground(server()->pixel(backpageForeground_));
}

void MSNotebook::backpageBackground(unsigned long backpageBackground_)
{
  if (_backpageBackground!=backpageBackground_)
   {
     unsigned long oldBG=backpageBackground();
     _backpageBackground=backpageBackground_;
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     NotebookEntry *entry;
     while ((np=np->next())!=hp)
      {
	entry=(NotebookEntry *)np->data();
        if (entry->tab()->isModified(MSNotebookTabAttribute::TabBackground)==MSFalse)
         {
           entry->tab()->tabBackground(backpageBackground(),MSFalse);
           if (entry!=currentEntry()) entry->tab()->background(backpageBackground());
         }
      }
     redraw();
   }
}

void MSNotebook::backpageBackground(const char *backpageBackground_)
{
  backpageBackground(server()->pixel(backpageBackground_));
}

void MSNotebook::frameBackground(unsigned long frameBackground_)
{
  if (_frameBackground!=frameBackground_)
   {
     _frameBackground=frameBackground_;
     createBindingPixmap();
     redraw();
   }
}

void MSNotebook::frameBackground(const char *frameBackground_)
{
  frameBackground(server()->pixel(frameBackground_));
}

void MSNotebook::selectedPageForeground(unsigned long selectedPageForeground_)
{
  if (_selectedPageForeground!=selectedPageForeground_)
   {
     _selectedPageForeground=selectedPageForeground_;
     if (currentEntry()!=0) currentEntry()->tab()->foreground(selectedPageForeground());
   }
}

void MSNotebook::selectedPageForeground(const char *selectedPageForeground_)
{
  selectedPageForeground(server()->pixel(selectedPageForeground_));
}

void MSNotebook::selectedPageBackground(unsigned long selectedPageBackground_)
{
  if (_selectedPageBackground!=selectedPageBackground_)
   {
     _selectedPageBackground=selectedPageBackground_;
     if (currentEntry()!=0) currentEntry()->tab()->background(selectedPageBackground());
     drawFrameShadow(window());
   }
}

void MSNotebook::selectedPageBackground(const char *selectedPageBackground_)
{
  selectedPageBackground(server()->pixel(selectedPageBackground_));
}

void MSNotebook::nextPage(MSBoolean cycle_)
{
  currentEntry(nextBrowseEntry(cycle_)); 
  resetFirstEntry(); 
  positionTabs();
  pageChanged();
}

void MSNotebook::previousPage(MSBoolean cycle_)
{ 
  currentEntry(previousBrowseEntry(cycle_)); 
  resetFirstEntry(); 
  positionTabs();
  pageChanged();
}
  
void MSNotebook::configure(void)
{
  _redrawPixmap->resize(width(),height());
  placement();
}

void MSNotebook::firstMapNotify(void)
{
  if (currentEntry()==0) currentEntry(firstItem());
  int size=server()->fontManager()->fontStruct(font())->max_bounds.width;
  size=(size<MSNotebookMinimumArrowSize)?MSNotebookMinimumArrowSize:size;
  upLeftArrow()->resize(size,size);
  downRightArrow()->resize(size,size);
  if (label()->columns()>0) label()->show();
  computeSize();
  resetFirstEntry();
  positionTabs();  
}

void MSNotebook::naturalSize(void)
{ 
  internalEvent(MSTrue);
  MSNodeItem *hp=childListHead(); 
  MSNodeItem *np=hp;
  NotebookEntry *entry;
   
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *) np->data();	
     entry->widget()->naturalSize();
   }
  int w=label()->width();
  label()->freeze();
  label()->naturalSize();
  label()->unfreeze();
  if (w!=label()->width()) label()->redraw();
  internalEvent(MSFalse);
  adjustSize();
}

void MSNotebook::unfreeze(void)
{
  freezeStatus(MSFalse);
  adjustSize();
}

void MSNotebook::adjustSize(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     if (lockSize()==MSFalse) computeSize();
     else
      {
	placement();
	redraw();
      }
   }
}

void MSNotebook::computeSize(void)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  int maxW=0,maxH=0;
  NotebookEntry *entry;
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *)np->data();
     if (entry->widget()!=0)
      {
	maxW=(entry->widget()->width()>maxW)?entry->widget()->width():maxW;
	maxH=(entry->widget()->height()>maxH)?entry->widget()->height():maxH;
      }
   }
  int offset=highlightThickness()*2+shadowThickness()*2+frameThickness()*2;
  int w=offset+borderWidth()*2+marginWidth()*2+maxW+backpageMargin();
  int h=offset+borderHeight()*2+marginHeight()*2+maxH+backpageMargin()+labelHeight();
  if (orientation()==Horizontal)
   {
     w+=maxTabWidth();
     if (showBinding()==MSTrue) w+=bindingWidth();
     int minimumHeight=highlightThickness()*2+shadowThickness()*2+borderHeight()*2
     +arrowHeight()*3+arrowOffset()*3+maxTabHeight()+labelHeight();
     h=(h<minimumHeight)?minimumHeight:h;
   }
  else
   {
     h+=maxTabHeight();
     if (showBinding()==MSTrue) h+=bindingWidth();
     int minimumWidth=highlightThickness()*2+shadowThickness()*2+borderWidth()*2
     +arrowWidth()*3+arrowOffset()*3+maxTabWidth();
     w=(w<minimumWidth)?minimumWidth:w;
   }
  int labelMinimum=labelWidth();
  w=(w<labelMinimum)?labelMinimum:w;

  if (w==width()&&h==height()) placement();
  else resize(w,h);
}

void MSNotebook::placement(void)
{
  internalEvent(MSTrue);
  createBindingPixmap();
  positionLabel();
  positionTabs();
  internalEvent(MSFalse);
}

void MSNotebook::positionTabs(MSBoolean increment_)
{
  internalEvent(MSTrue);
  int maxTabW=maxTabWidth();
  int maxTabH=maxTabHeight();
  int offset=highlightThickness()+shadowThickness()+frameThickness();
  int x=offset+borderWidth()+marginWidth();
  int y=offset+borderHeight()+marginHeight();
  if (orientation()==Horizontal)
   {
     if (tabAlignment()==MSLeft)
      {
        x+=(maxTabW+backpageMargin());
      }
     else
      {
        if (showBinding()==MSTrue) x+=bindingWidth();
      }
   }
  else
   {
     if (tabAlignment()==MSTop)
      {
        y+=(maxTabH+backpageMargin());
      }
     else
      {
        if (showBinding()==MSTrue) y+=bindingWidth();
      }
   }

  // Place the label
  positionLabel();
  y+=labelHeight();
  // Place children 
  int w,h;
  computeChildSize(w,h);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry;
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *)np->data();
     if (entry->widget()!=0)
      {
	entry->widget()->resize(w,h);
	if (entry==currentEntry())
	 {
	   entry->widget()->moveTo(x,y);
	   entry->widget()->show();
	 }
	else if (entry->widget()->mapped()==MSTrue) entry->widget()->unmap();
      }
   }

  if (showTabs()==MSTrue)
   {
     MSWidgetVector managedVector=managedChildren();
     int managed=managedVector.length();
     // Place the tabs
     if (orientation()==Horizontal)
      {
        int HeightOffset=highlightThickness()+shadowThickness()+borderHeight();
        int totalSpace=availableHeightForTabs();
	if (totalSpace>=totalTabHeight()) firstEntry(firstItem());
	int top=HeightOffset+labelHeight();
	int startY=top+arrowHeight()+arrowOffset();
        int bpMargin=backpageMargin();
        int startX,currentStartX;
        if (tabAlignment()==MSLeft)
         {
           currentStartX=x-marginWidth();
           startX=currentStartX-frameThickness();
           if (bpMargin!=0) startX-=(bpMargin-1);
         }
        else
         {
           startX=x+w+marginWidth()+frameThickness();
           currentStartX=x+w+marginWidth();
           if (bpMargin!=0) startX+=(bpMargin-1);
         }
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	int numDrawn=0;
	MSBoolean foundFirstEntry=MSFalse;
	MSBoolean done=MSFalse;
	// We are using different strategy in moving the buttons, based on whether if the tabs
	// are being shifted forward or backward.  This is an optimization to minimize expose
	// events sent to the tabs
        // Normal case:
	if (increment_==MSTrue)
	 {
	   while ((np=np->next())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (done==MSFalse&&(foundFirstEntry==MSTrue||entry==firstEntry()))
	       {
		 foundFirstEntry=MSTrue;
		 if (entry->managed()==MSTrue)
		  {
		    if (entry->tab()->height()<=totalSpace)
		     {
                       int moveX;
		       if (entry==currentEntry()) moveX=currentStartX;
                       else moveX=startX;
                       if (tabAlignment()==MSLeft) moveX-=entry->tab()->width();
                       entry->tab()->moveTo(moveX,startY);
		       entry->tab()->map();
		       int used=entry->tab()->height()+tabSpacing();
		       totalSpace-=used;
		       startY+=used;
		       numDrawn++;
		     }
		    else
		     {
		       entry->tab()->unmap();
		       done=MSTrue;
		     }
		  }
		 else entry->tab()->unmap();
	       }
	      else entry->tab()->unmap();
	    }
	 }
	else
	 {
           //Optimized case:
	   NotebookEntry *last=0;
	   int space=totalSpace;
	   while ((np=np->next())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (foundFirstEntry==MSTrue||entry==firstEntry())
	       {
		 foundFirstEntry=MSTrue;
		 if (entry->managed()==MSTrue)
		  {
		    if (entry->tab()->height()<=space)
		     {
		       space-=(entry->tab()->height()+tabSpacing());
		       last=entry;
		     }
		    else break;
		  }
	       }
	    }
	   np=hp;
	   int start=startY+totalSpace-space;
	   foundFirstEntry=MSFalse;
	   while ((np=np->prev())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (foundFirstEntry==MSTrue||entry==last)
	       {
		 foundFirstEntry=MSTrue;
		 int starty=start-entry->tab()->height()-tabSpacing();
		 if (entry->managed()==MSTrue&&starty>=startY)
		  {
                    int moveX;
                    if (entry==currentEntry()) moveX=currentStartX;
                    else moveX=startX;
                    if (tabAlignment()==MSLeft) moveX-=entry->tab()->width();
		    entry->tab()->moveTo(moveX,starty);
		    entry->tab()->map();
		    int used=entry->tab()->height()+tabSpacing();
		    start-=used;
		    numDrawn++;
		  }
		 else entry->tab()->unmap();
	       }
	      else entry->tab()->unmap();
	    }
	 }
	if (firstEntry()!=0&&managed!=0&&numDrawn!=managed)
	 {
           int arrowX;
           if (tabAlignment()==MSLeft) arrowX=startX-arrowWidth();
           else arrowX=startX+arrowOffset();
	   upLeftArrow()->moveTo(arrowX,height()-HeightOffset-arrowHeight()*2-arrowOffset());
	   upLeftArrow()->map();
	   if (firstEntry()==firstItem()) upLeftArrow()->sensitive(MSFalse);
	   else upLeftArrow()->sensitive(MSTrue);
	   downRightArrow()->moveTo(arrowX,height()-HeightOffset-arrowHeight());
	   downRightArrow()->map();
	   if (managedVector.indexOf((unsigned long)firstEntry()->widget())+numDrawn==managed) downRightArrow()->sensitive(MSFalse);
	   else downRightArrow()->sensitive(MSTrue);
	 }
	else
	 {
	   upLeftArrow()->unmap();
	   downRightArrow()->unmap();
	 }
      }
     else
      {
	int WidthOffset=highlightThickness()+shadowThickness()+borderWidth();
        int totalSpace=availableWidthForTabs();
	if (totalSpace>=totalTabWidth()) firstEntry(firstItem());
	int startX=WidthOffset+arrowWidth()+arrowOffset();
	int bpMargin=backpageMargin();
        int startY,currentStartY;
        if (tabAlignment()==MSTop)
         {
           currentStartY=y-marginHeight();
           startY=currentStartY-frameThickness();
           if (bpMargin!=0) startY-=(bpMargin-1);
         }
        else
         {
           startY=y+h+marginHeight()+frameThickness();
           currentStartY=y+h+marginHeight()-1;
           if (bpMargin!=0) startY+=(bpMargin-2);
         }
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	int numDrawn=0;
	MSBoolean foundFirstEntry=MSFalse;
	MSBoolean done=MSFalse;
	// We are using different strategy in moving the buttons, based on whether if the tabs
	// are being shifted forward or backward.  This is an optimization to minimize expose
	// events sent to the tabs
	if (increment_==MSTrue)
	 {
	   while ((np=np->next())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (done==MSFalse&&(foundFirstEntry==MSTrue||entry==firstEntry()))
	       {
		 foundFirstEntry=MSTrue;
		 if (entry->managed()==MSTrue)
		  {
		    if (entry->tab()->width()<=totalSpace)
		     {
                       int moveY;
		       if (entry==currentEntry()) moveY=currentStartY;
                       else moveY=startY;
                       if (tabAlignment()==MSTop) moveY-=entry->tab()->height();
                       entry->tab()->moveTo(startX,moveY);
		       entry->tab()->map();
		       int used=entry->tab()->width()+tabSpacing();
		       totalSpace-=used;
		       startX+=used;
		       numDrawn++;
		     }
		    else
		     {
		       entry->tab()->unmap();
		       done=MSTrue;
		     }
		  }
		 else entry->tab()->unmap();
	       }
	      else entry->tab()->unmap();
	    }
	 }
	else
	 {
	   NotebookEntry *last=0;
	   int space=totalSpace;
	   while ((np=np->next())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (foundFirstEntry==MSTrue||entry==firstEntry())
	       {
		 foundFirstEntry=MSTrue;
		 if (entry->managed()==MSTrue)
		  {
		    if (entry->tab()->width()<=space)
		     {
		       space-=(entry->tab()->width()+tabSpacing());
		       last=entry;
		     }
		    else break;
		  }
	       }
	    }
	   np=hp;
	   int start=startX+totalSpace-space;
	   foundFirstEntry=MSFalse;
	   while ((np=np->prev())!=hp)
	    {
	      entry=(NotebookEntry *)np->data();
	      if (foundFirstEntry==MSTrue||entry==last)
	       {
		 foundFirstEntry=MSTrue;
		 int startx=start-entry->tab()->width()-tabSpacing();
		 if (entry->managed()==MSTrue&&startx>=startX)
		  {
                    int moveY;
                    if (entry==currentEntry()) moveY=currentStartY;
                    else moveY=startY;
                    if (tabAlignment()==MSTop) moveY-=entry->tab()->height();
                    entry->tab()->moveTo(startx,moveY);
		    entry->tab()->map();
		    int used=entry->tab()->width()+tabSpacing();
		    start-=used;
		    numDrawn++;
		  }
		 else entry->tab()->unmap();
	       }
	      else entry->tab()->unmap();
	    }
	 }
	if (firstEntry()!=0&&managed!=0&&numDrawn!=managed)
	 {
           int arrowY;
           if (tabAlignment()==MSTop) arrowY=startY-arrowHeight();
           else arrowY=startY+arrowOffset();
	   upLeftArrow()->moveTo(width()-WidthOffset-arrowWidth()*2-arrowOffset(),arrowY);
	   upLeftArrow()->map();
	   if (firstEntry()==firstItem()) upLeftArrow()->sensitive(MSFalse);
	   else upLeftArrow()->sensitive(MSTrue);
	   downRightArrow()->moveTo(width()-WidthOffset-arrowWidth(),arrowY);
	   downRightArrow()->map();
	   if (managedVector.indexOf((unsigned long)firstEntry()->widget())+numDrawn==managed) downRightArrow()->sensitive(MSFalse);
	   else downRightArrow()->sensitive(MSTrue);
	 }
	else
	 {
	   upLeftArrow()->unmap();
	   downRightArrow()->unmap();
	 }
      }
   }
  internalEvent(MSFalse);
}

int MSNotebook::visibleTabs(void)
{
  int numVisible=0;
  if (orientation()==Horizontal)
   {
     int totalSpace=availableHeightForTabs();
     if (totalSpace>=totalTabHeight()) numVisible=numManaged();
     else
      {
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	MSBoolean foundFirstEntry=MSFalse;
	while ((np=np->next())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   if (foundFirstEntry==MSTrue||entry==firstEntry())
	    {
	      foundFirstEntry=MSTrue;
	      if (entry->managed()==MSTrue)
	       {
		 if (entry->tab()->height()<=totalSpace)
		  {
		    int used=entry->tab()->height()+tabSpacing();
		    totalSpace-=used;
		    numVisible++;
		  }
		 else break;
	       }
	    }
	 }
      }
   }
  else
   {
     int totalSpace=availableWidthForTabs();
     if (totalSpace>=totalTabWidth()) numVisible=numManaged();
     else
      {
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	MSBoolean foundFirstEntry=MSFalse;
	while ((np=np->next())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   if (foundFirstEntry==MSTrue||entry==firstEntry())
	    {
	      foundFirstEntry=MSTrue;
	      if (entry->managed()==MSTrue)
	       {
		 if (entry->tab()->width()<=totalSpace)
		  {
		    int used=entry->tab()->width()+tabSpacing();
		    totalSpace-=used;
		    numVisible++;
		  }
		 else break;
	       }
	    }
	 }
      }
   }
  return numVisible;
}


void MSNotebook::childCreate(MSWidget *widget_)
{
  if (internalEvent()==MSFalse)
   {
     insertChild(widget_);
     adjustSize();
   }
}

void MSNotebook::childInsert(MSWidget *widget_)
{
  insertChild(widget_);
  if (widget_->mapped()==MSTrue) widget_->unmap();
  adjustSize();
}

void MSNotebook::insertChild(MSWidget *widget_)
{
  NotebookEntry *entry=getEntry(widget_);
  if (entry==0&&widget_!=0)
   {
     entry=new NotebookEntry(this,widget_);
     MSNodeItem *np=new MSNodeItem((void *)entry);
     np->insert(childListHead());    // fifo
     updateTitleVector();
     _childCount++;
   }
}

void MSNotebook::childDestroy(MSWidget *widget_)
{
  if (internalEvent()==MSFalse)
   {
     NotebookEntry *entry=getEntry(widget_);
     if (entry!=0)
      {
	removeChild(widget_);
	adjustSize();
      }
     
   }
}

void MSNotebook::childRemove(MSWidget *widget_)
{
  NotebookEntry *entry=getEntry(widget_);
  if (entry!=0)
   {
     removeChild(widget_);
     adjustSize();
   }
  
}

void MSNotebook::childMap(MSWidget *widget_)
{
  if (internalEvent()==MSFalse)
   {
     NotebookEntry *entry=getEntry(widget_);
     // If the child is mapping itself when it's not supposed to
     // we'll move it beyond screen limit
     if (entry!=0&&currentEntry()!=entry) widget_->moveTo(server()->width(),server()->height());
   }
}

void MSNotebook::removeChild(MSWidget *widget_)
{
  NotebookEntry *entry=0;
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  
  // check the child list
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *)np->data();
     if (entry->widget()==widget_) 
      {
	if (firstEntry()==entry)
	 {
	   NotebookEntry *prev=previousEntry();
	   if (prev!=entry) firstEntry(prev);
	   else
	    {
	      entry->managed(MSFalse);
	      firstEntry(firstItem());
	    }
	 }	   
	if (currentEntry()==entry) _currentEntry=0;
	delete np;
	delete entry;
	_childCount--;
	break;
      }
   }
  updateTitleVector();
}

void MSNotebook::childConfigure(MSWidget *widget_)
{
  if (internalEvent()==MSFalse)
   {
     NotebookEntry *entry=getEntry(widget_);
     if (entry!=0) adjustSize();
   }
}

int MSNotebook::computeMinimumWidth(void)
{
  int w=highlightThickness()*2+shadowThickness()*2+frameThickness()*2+marginWidth()*2
  +borderWidth()*2+backpageMargin();
  if (orientation()==Horizontal)
   {
     w+=maxTabWidth();
     if (showBinding()==MSTrue) w+=bindingWidth();
   }
  int labelW=labelWidth();
  return ((labelW>w)?labelW:w);
}

int MSNotebook::computeMinimumHeight(void)
{
  int h=highlightThickness()*2+shadowThickness()*2+frameThickness()*2+marginHeight()*2
  +borderHeight()*2+backpageMargin()+labelHeight();
  if (orientation()==Vertical)
   {
     h+=maxTabHeight();
     if (showBinding()==MSTrue) h+=bindingWidth();
   }
  return h;
}

void MSNotebook::computeChildSize(int &w_,int &h_)
{
  int offset=(highlightThickness()+shadowThickness()+frameThickness())*2;
  w_=width()-offset-marginWidth()*2-borderWidth()*2-backpageMargin();
  h_=height()-offset-marginHeight()*2-borderHeight()*2-backpageMargin()-labelHeight();
  if (showBinding()==MSTrue)
   {
     if (orientation()==Horizontal) w_-=bindingWidth();
     else h_-=bindingWidth();
   }
  if (orientation()==Horizontal) w_-=maxTabWidth();
  else h_-=maxTabHeight();

  w_=(w_>0)?w_:1;
  h_=(h_>0)?h_:1;
}

void MSNotebook::visibilityObscured(void)
{
  visible(MSFalse);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry;
  while((np=np->next())!=hp)
   {
     entry=(NotebookEntry *)np->data();
     if (entry!=0) visibilityObscuredNotify(entry->widget());
   }
}

// visibilityUnobscured() method needs to be selective:  only set visible to
// MSTrue on the current page, since it is the only one that will be shown.
// Otherwise, it leads to undesirable side effects, such as unvisible
// widgets having visible(MSTrue)
void MSNotebook::visibilityUnobscured(void)
{
  visible(MSTrue);
  if(_currentEntry!=0)
   {
     visibilityUnobscuredNotify(_currentEntry->widget());
   }
}

MSNotebook::NotebookEntry *MSNotebook::getEntry(MSWidget *widget_) const
{
  MSNodeItem    *hp=(MSNodeItem *)childListHead();
  MSNodeItem    *np=hp;
  NotebookEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *) np->data();
     if (entry->widget()==widget_) return entry;
   }
  return 0;
}

MSNotebook::NotebookEntry *MSNotebook::getEntry(const char *title_) const
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry;
  MSString title(title_);
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *) np->data();
     if (entry->title()==MSStringVector(title)) return entry;
   }
  return 0;
}

MSNotebook::NotebookEntry *MSNotebook::getEntry(unsigned i_) const 
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  NotebookEntry *entry;
  unsigned j=0;
  while ((np=np->next())!=hp)
   {
     entry=(NotebookEntry *) np->data();
     if (entry->managed()==MSTrue)
      {
	if (j++==i_) return entry;
      }
   }
  return 0;
}

void MSNotebook::firstEntry(NotebookEntry *firstEntry_)
{
  _firstEntry=firstEntry_;
}

void MSNotebook::lastEntry(NotebookEntry *lastEntry_)
{
  int count=0;
  if (orientation()==Horizontal)
   {
     int totalSpace=availableHeightForTabs();
     if (totalSpace>=totalTabHeight()) firstEntry(firstItem());
     else
      {
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	MSBoolean foundlastEntry=MSFalse;
	while ((np=np->prev())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   if (foundlastEntry==MSTrue||entry==lastEntry_)
	    {
	      foundlastEntry=MSTrue;
	      if (entry->managed()==MSTrue)
	       {
		 if (entry->tab()->height()<=totalSpace)
		  {
		    int used=entry->tab()->height()+tabSpacing();
		    totalSpace-=used;
		    count++;
		  }
		 else break;
	       }
	    }
	 }
	if (count>1)
	 {
	   MSWidgetVector managedVector=managedChildren();
	   int index=managedVector.indexOf((unsigned long)lastEntry_->widget())-count+1;
	   if (index<0) firstEntry(firstItem());
	   else firstEntry(getEntry(managedVector(index)));

	 }
	else firstEntry(lastEntry_);
      }
   }
  else
   {
     int totalSpace=availableWidthForTabs();
     if (totalSpace>=totalTabWidth()) firstEntry(firstItem());
     else
      {
	MSNodeItem *hp=childListHead();
	MSNodeItem *np=hp;
	NotebookEntry *entry;
	MSBoolean foundlastEntry=MSFalse;
	while ((np=np->prev())!=hp)
	 {
	   entry=(NotebookEntry *)np->data();
	   if (foundlastEntry==MSTrue||entry==lastEntry_)
	    {
	      foundlastEntry=MSTrue;
	      if (entry->managed()==MSTrue)
	       {
		 if (entry->tab()->width()<=totalSpace)
		  {
		    int used=entry->tab()->width()+tabSpacing();
		    totalSpace-=used;
		    count++;
		  }
		 else break;
	       }
	    }
	 }
	if (count>1)
	 {
	   MSWidgetVector managedVector=managedChildren();
	   int index=managedVector.indexOf((unsigned long)lastEntry_->widget())-count+1;
	   if (index<0) firstEntry(firstItem());
	   else firstEntry(getEntry(managedVector(index)));
	 }
	else firstEntry(lastEntry_);
      }
   }
}

void MSNotebook::resetFirstEntry(void)
{
  if (currentEntry()!=0)
   {
     if (firstEntry()==0) firstEntry(firstItem());
     MSWidgetVector managedVector=managedChildren();
     int first=managedVector.indexOf((unsigned long)firstEntry()->widget());
     int current=managedVector.indexOf((unsigned long)currentEntry()->widget());
     if (current<first)
      {
	firstEntry(currentEntry());
      }
     else 
      {
	int numShown=visibleTabs();
	if (current>=first+numShown)
	 {
	   lastEntry(currentEntry());
	 }
      }
   }
  else if (numManaged()==0)
   {
     firstEntry(0);
   }
}

MSNotebook::NotebookEntry *MSNotebook::nextEntry(void)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  if (firstEntry()!=0)
   {
     while ((np=np->next())!=hp)
      {
	if (firstEntry()==(NotebookEntry *)np->data()) break;
      }
     if (np==hp) return firstEntry();
   }
  NotebookEntry *next;
  while ((np=np->next())!=hp)
   {
     next=(NotebookEntry *)np->data();
     if (next->managed()==MSTrue) return next;
   }
  return firstEntry();
}

MSNotebook::NotebookEntry *MSNotebook::nextBrowseEntry(MSBoolean cycle_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  if (currentEntry()!=0)
   {
     while ((np=np->next())!=hp)
      {
	if (currentEntry()==(NotebookEntry *)np->data()) break;
      }
     if (np==hp) return currentEntry();
   }
  NotebookEntry *next;
  while ((np=np->next())!=hp)
   {
     next=(NotebookEntry *)np->data();
     if (next->managed()==MSTrue) return next;
   }
  if ( cycle_ == MSFalse )return currentEntry();
  return firstItem();
}

MSNotebook::NotebookEntry *MSNotebook::previousEntry(void)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  if (firstEntry()!=0)
   {
     while ((np=np->prev())!=hp)
      {
	if (firstEntry()==(NotebookEntry *)np->data()) break;
      }
     if (np==hp) return firstEntry();
   }
  NotebookEntry *prev;
  while ((np=np->prev())!=hp)
   {
     prev=(NotebookEntry *)np->data();
     if (prev->managed()==MSTrue) return prev;
   }
  return firstEntry();

}

MSNotebook::NotebookEntry *MSNotebook::previousBrowseEntry(MSBoolean cycle_)
{
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  if (currentEntry()!=0)
   {
     while ((np=np->prev())!=hp)
      {
	if (currentEntry()==(NotebookEntry *)np->data()) break;
      }
     if (np==hp) return currentEntry();
   }
  NotebookEntry *prev;
  while ((np=np->prev())!=hp)
   {
     prev=(NotebookEntry *)np->data();
     if (prev->managed()==MSTrue) return prev;
   }
  if ( cycle_ == MSFalse )return currentEntry();
  return lastItem();
}

void MSNotebook::currentEntry(NotebookEntry *currentEntry_)
{
  if (_currentEntry!=currentEntry_)
   {
     NotebookEntry *previousEntry=currentEntry();
     _currentEntry=currentEntry_;
     if (currentEntry()!=0)
      {
        // Only Change the tab's colors if they haven't been changed
        if (currentEntry()->tab()->isModified(MSNotebookTabAttribute::TabForeground)==MSFalse)
         {
           currentEntry()->tab()->foreground(selectedPageForeground());
         }
        if (currentEntry()->tab()->isModified(MSNotebookTabAttribute::TabBackground)==MSFalse)
         {
           currentEntry()->tab()->background(selectedPageBackground());
         }
	currentEntry()->widget()->raise();
	currentEntry()->widget()->show();
      }
     if (previousEntry!=0)
      {
	previousEntry->tab()->foreground(previousEntry->tab()->tabForeground());
	previousEntry->tab()->background(previousEntry->tab()->tabBackground());
	previousEntry->widget()->unmap();
      }
   }
}

void MSNotebook::updateTitle(void)
{
  internalEvent(MSTrue);
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
  internalEvent(MSFalse);
}

void MSNotebook::positionLabel(void)
{
  if (label()->columns()>0)
   {
     int offset=highlightThickness()+shadowThickness();
     int xpos,ypos;
     int indent;
     if (titleAlignment()&MSLeft) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=offset+indent;
      }
     else if (titleAlignment()&MSRight) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=width()-offset-indent-label()->width();
      }
     else xpos=width()/2-label()->width()/2;

     int shadowOffset;
     if (titleAlignment()&MSTop) 
      {
	shadowOffset=label()->height();
	ypos=0;
      }
     else if (titleAlignment()&MSBottom) 
      {
	shadowOffset=0;
	ypos=offset;
      }
     else
      { 
	shadowOffset=label()->height()/2+offset;
	ypos=offset;
      }
     if (shadowOffset!=topShadowOffset())
      {
	undrawShadow();
	topShadowOffset(shadowOffset);
      }
     label()->moveTo(xpos,ypos);
     label()->map();
   }
  else
   { 
     label()->unmap();
     topShadowOffset(0);
   }
}

void MSNotebook::updateBackground(unsigned long oldBG_)
{
  MSWidgetCommon::updateBackground(oldBG_);
  if (selectedPageBackground()==oldBG_) selectedPageBackground(background());
  upLeftArrow()->background(background());
  downRightArrow()->background(background());
  label()->background(background());
  if (_popupMenu!=0) _popupMenu->background(background());
  createBindingPixmap();
  redraw();
}

void MSNotebook::updateForeground(unsigned long oldFG_)
{
  MSManager::updateForeground(oldFG_);
  if (selectedPageForeground()==oldFG_) selectedPageForeground(foreground());
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabForeground)==MSFalse)
      {
        entry->tab()->tabForeground(foreground(),MSFalse);
        if (entry!=currentEntry()) entry->tab()->foreground(foreground());
      }
   }
  if (label()->foreground()==oldFG_) label()->foreground(foreground());
  if (_popupMenu!=0) _popupMenu->foreground(foreground());
  createBindingPixmap();
  redraw();
}

void MSNotebook::updateFont(Font oldFont_)
{
  internalEvent(MSTrue);
  MSManager::updateFont(oldFont_);
  int size=server()->fontManager()->fontStruct(font())->max_bounds.width;
  size=(size<MSNotebookMinimumArrowSize)?MSNotebookMinimumArrowSize:size;
  upLeftArrow()->resize(size,size);
  downRightArrow()->resize(size,size);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->tab()->isModified(MSNotebookTabAttribute::TabFont)==MSFalse)
      {
        entry->tab()->tabFont(font(),MSFalse);
      }
   }
  if (_popupMenu!=0) _popupMenu->font(font());
  internalEvent(MSFalse);
  adjustSize();
}

void MSNotebook::redraw(void)
{
  if (mapped()==MSTrue)
   {
     redrawPixmap()->lock();     
     XFillRectangle(display(),redrawPixmap()->pixmap(),backgroundShadowGC(),
		    0,0,width(),height());
     int ht=highlightThickness();
     MSRect aRect(ht,ht+topShadowOffset(),width()-2*ht,height()-topShadowOffset()-2*ht);
     drawBevel(redrawPixmap()->pixmap(),aRect,shadowStyle(),shadowThickness()); 
     drawBinding(redrawPixmap()->pixmap());
     drawBackpage(redrawPixmap()->pixmap());
     drawFrameShadow(redrawPixmap()->pixmap());
     XCopyArea(display(),redrawPixmap()->pixmap(),window(),backgroundShadowGC(),0,0,width(),height(),0,0);
     redrawPixmap()->unlock();
   }
}

void MSNotebook::show(void)
{
  if (mapped()==MSFalse)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     while ((np=np->next())!=hp)
      {
	NotebookEntry *entry=(NotebookEntry *)np->data();
	if (entry->widget()!=0)
	 {
	   entry->widget()->show();
	 }
	if (entry!=currentEntry()) entry->widget()->unmap();
      }
     map();
   }
}

void MSNotebook::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if ((open=displayPrintOpen(this))==MSTrue)
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
   }
  if (open==MSTrue)
   {
     clear();
     redraw();
     MSWidget    *pWidget;
     Window       root,parent,*children=(Window *)0;
     unsigned int nchildren=0;
     (void)XQueryTree(display(),window(),&root,&parent,&children,&nchildren);
     for (int i=0;i<nchildren;i++)
      {
        pWidget=widget(children[i]);
	if (pWidget!=0&&pWidget->mapped()==MSTrue)
	 {
	   displayPrintOriginInc(pWidget);
	   pWidget->print();
	   displayPrintOriginDec(pWidget);
	 }
      }
     XFree((char *)children);
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

void MSNotebook::drawBackpage(Window window_)
{
  if (backpages()>0&&backpageThickness()>0)
   {
     XPoint ppoints[6];
     XPoint lpoints[5];
     int increment=backpageThickness();
     int widthOffset=highlightThickness()+shadowThickness()+borderWidth();
     int heightOffset=highlightThickness()+shadowThickness()+borderHeight();
      int bindingW;
     if (showBinding()==MSTrue) bindingW=bindingWidth();
     else bindingW=0;
     int oneThirdBinding=bindingW/3;
     int twoThirdBinding=bindingW-oneThirdBinding;
     int viewPortWidth=width()-widthOffset*2-backpageMargin();
     int viewPortHeight=height()-heightOffset*2-backpageMargin()-labelHeight();
     int xincrement,yincrement;
     if (orientation()==Horizontal)
      {
	int maxTab=maxTabWidth();
        viewPortWidth-=(bindingW+maxTab);
        if (tabAlignment()==MSLeft)
         {
           ppoints[0].x=widthOffset+maxTab+backpageMargin();
           ppoints[0].y=heightOffset+labelHeight()+increment;
           ppoints[1].x=0;
           ppoints[1].y=viewPortHeight-increment;
           ppoints[2].x=viewPortWidth+twoThirdBinding-increment;
           ppoints[2].y=0;
           ppoints[3].x=0;
           ppoints[3].y=increment-1;
           ppoints[4].x=-(viewPortWidth+twoThirdBinding);
           ppoints[4].y=0;
           ppoints[5].x=0;
           ppoints[5].y=-viewPortHeight;

           lpoints[0].x=ppoints[0].x;
           lpoints[0].y=ppoints[0].y;
           lpoints[1].x=-increment;
           lpoints[1].y=0;
           lpoints[2].x=0;
           lpoints[2].y=viewPortHeight-1;
           lpoints[3].x=viewPortWidth+twoThirdBinding;
           lpoints[3].y=0;
           lpoints[4].x=0;
           lpoints[4].y=-increment;

           xincrement=-increment;
           yincrement=increment;
         }
        else
         {
           ppoints[0].x=widthOffset+oneThirdBinding+increment;
           ppoints[0].y=height()-heightOffset-backpageMargin();
           ppoints[1].x=width()-widthOffset*2-oneThirdBinding-backpageMargin()-increment-maxTab;
           ppoints[1].y=0;
           ppoints[2].x=0;
           ppoints[2].y=-(height()-heightOffset*2-backpageMargin()-labelHeight()-increment);
           ppoints[3].x=increment;
           ppoints[3].y=0;
           ppoints[4].x=0;
           ppoints[4].y=-ppoints[2].y+increment;
           ppoints[5].x=-ppoints[1].x-increment;
           ppoints[5].y=0;

           lpoints[0].x=ppoints[0].x;
           lpoints[0].y=ppoints[0].y;
           lpoints[1].x=0;
           lpoints[1].y=increment-1;
           lpoints[2].x=width()-widthOffset*2-oneThirdBinding-backpageMargin()-1-maxTab;
           lpoints[2].y=0;
           lpoints[3].x=0;
           lpoints[3].y=-(height()-heightOffset*2-backpageMargin()-labelHeight());
           lpoints[4].x=-increment;
           lpoints[4].y=0;

           xincrement=increment;
           yincrement=increment;
         }
      }
     else
      {
	int maxTab=maxTabHeight();
        viewPortHeight-=(bindingW+maxTab);
        if (tabAlignment()==MSTop)
         {
           ppoints[0].x=widthOffset+increment;
           ppoints[0].y=heightOffset+labelHeight()+maxTab+backpageMargin();
           ppoints[1].x=viewPortWidth-increment;
           ppoints[1].y=0;
           ppoints[2].x=0;
           ppoints[2].y=viewPortHeight+twoThirdBinding-increment;
           ppoints[3].x=increment;
           ppoints[3].y=0;
           ppoints[4].x=0;
           ppoints[4].y=-viewPortHeight-twoThirdBinding;
           ppoints[5].x=-viewPortWidth;
           ppoints[5].y=0;

           lpoints[0].x=ppoints[0].x;
           lpoints[0].y=ppoints[0].y;
           lpoints[1].x=0;
           lpoints[1].y=-increment;
           lpoints[2].x=viewPortWidth-1;
           lpoints[2].y=0;
           lpoints[3].x=0;
           lpoints[3].y=viewPortHeight+twoThirdBinding;
           lpoints[4].x=-increment;
           lpoints[4].y=0;

           xincrement=increment;
           yincrement=-increment;
         }
        else
         {
           ppoints[0].x=widthOffset+increment;
           ppoints[0].y=height()-heightOffset-backpageMargin()-maxTab;
           ppoints[1].x=width()-widthOffset*2-backpageMargin()-increment;
           ppoints[1].y=0;
           ppoints[2].x=0;
           ppoints[2].y=-(height()-oneThirdBinding-heightOffset*2-backpageMargin()-labelHeight()-increment-maxTab);
           ppoints[3].x=increment;
           ppoints[3].y=0;
           ppoints[4].x=0;
           ppoints[4].y=-ppoints[2].y+increment;
           ppoints[5].x=-ppoints[1].x-increment;
           ppoints[5].y=0;

           lpoints[0].x=ppoints[0].x;
           lpoints[0].y=ppoints[0].y;
           lpoints[1].x=0;
           lpoints[1].y=increment-1;
           lpoints[2].x=width()-widthOffset*2-backpageMargin()-1;
           lpoints[2].y=0;
           lpoints[3].x=0;
           lpoints[3].y=-(height()-oneThirdBinding-heightOffset*2-backpageMargin()-labelHeight()-maxTab);
           lpoints[4].x=-increment;
           lpoints[4].y=0;

           xincrement=increment;
           yincrement=increment;
         }
      }
     XSetForeground(display(),backpageGC(),backpageBackground());
     int i;
     for (i=0;i<backpages();i++)
      {
	XFillPolygon(display(),window_,backpageGC(),ppoints,6,Nonconvex,CoordModePrevious);
	ppoints[0].x+=xincrement;
	ppoints[0].y+=yincrement;
      }
     XSetForeground(display(),backpageGC(),backpageForeground());
     for (i=0;i<backpages();i++)
      {
	XDrawLines(display(),window_,backpageGC(),lpoints,5,CoordModePrevious);
	lpoints[0].x+=xincrement;
	lpoints[0].y+=yincrement;
      }
   }
}
 
void MSNotebook::drawBinding(Window window_)
{
  if (showBinding()==MSTrue&&bindingWidth()>=MSNotebookMinimumBindingWidth)
   {
     if (width()>=MSNotebookMinimumBindingWidth&&height()>=MSNotebookMinimumBindingWidth)
      {
	if (outputMode()==Print) drawSpiral();
	else
	 {
	   int w,h,xoff,yoff;
	   if (orientation()==Horizontal)
	    {
	      w=bindingWidth();
	      h=height()-highlightThickness()*2-shadowThickness()*2-borderHeight()*2-backpageMargin()-labelHeight();
              if (tabAlignment()==MSLeft)
               {
                 xoff=width()-highlightThickness()-shadowThickness()-borderWidth()-bindingWidth();
                 yoff=highlightThickness()+shadowThickness()+borderHeight()+labelHeight();     
               }
              else
               {
                 xoff=highlightThickness()+shadowThickness()+borderWidth();
                 yoff=highlightThickness()+shadowThickness()+borderHeight()+labelHeight();     
               }
	    }
	   else
	    {
	      w=width()-highlightThickness()*2-shadowThickness()*2-borderWidth()*2-backpageMargin();
	      h=bindingWidth();
              if (tabAlignment()==MSTop)
               {
                 xoff=highlightThickness()+shadowThickness()+borderWidth();
                 yoff=height()-highlightThickness()-shadowThickness()-borderHeight()-bindingWidth();
               }
              else
               {
                 xoff=highlightThickness()+shadowThickness()+borderWidth();
                 yoff=highlightThickness()+shadowThickness()+borderHeight()+labelHeight();     
               }
	    }
	   XCopyArea(display(),bindingPixmap(),window_,notebookGC(),0,0,w,h,xoff,yoff);
	 }
      }
   }
}

void MSNotebook::drawFrameShadow(Window window_)
{
  int offset=highlightThickness()+shadowThickness();
  int offset2=offset*2;
  int x,y;
  int w=width()-offset2-borderWidth()*2-backpageMargin();
  int h=height()-offset2-borderHeight()*2-backpageMargin()-labelHeight();
  if (orientation()==Horizontal)
   {
     int maxTabW=maxTabWidth();
     if (tabAlignment()==MSLeft)
      {
        x=offset+borderWidth()+maxTabW+backpageMargin();
        y=offset+borderHeight()+labelHeight();
        if (showBinding()==MSTrue) w-=bindingWidth();
        w-=maxTabW;
      }
     else
      {
        x=offset+borderWidth();
        y=offset+borderHeight()+labelHeight();
        if (showBinding()==MSTrue)
         {
           x+=bindingWidth();
           w-=bindingWidth();
         }
        w-=maxTabW;
      }
   }
  else
   {
     int maxTabH=maxTabHeight();
     if (tabAlignment()==MSTop)
      {
        x=offset+borderWidth();
        y=offset+borderHeight()+labelHeight()+maxTabH+backpageMargin();
        if (showBinding()==MSTrue) h-=bindingWidth();
        h-=maxTabH;
      }
     else
      {
        x=offset+borderWidth();
        y=offset+borderHeight()+labelHeight();
        if (showBinding()==MSTrue)
         {
           y+=bindingWidth();
           h-=bindingWidth();
         }
        h-=maxTabH;
      }
   }
  GC topGC,bottomGC;
  if (currentEntry()!=0)
   {
     topGC=currentEntry()->tab()->topShadowGC();
     bottomGC=currentEntry()->tab()->bottomShadowGC();
   }
  else
   {
     topGC=topShadowGC();
     bottomGC=bottomShadowGC();     
   }
  if (frameThickness()>0)
   {
     drawBevelShadow(window_,MSRect(x,y,w,h),frameThickness(),topGC,bottomGC);
   }
  if (currentEntry()!=0&&marginWidth()>0&&marginHeight()>0)
   {
     x+=frameThickness();
     y+=frameThickness();
     w-=(frameThickness()*2);
     h-=(frameThickness()*2);
     XFillRectangle(display(),window_,currentEntry()->tab()->backgroundShadowGC(),x,y,w,h);
   }
}

void MSNotebook::drawSpiral(void)
{
  createBindingPixmap();
}

void MSNotebook::createBindingPixmap(void)
{
  // First checking if binding width is big enough
  if (showBinding()==MSTrue)
   {
     int w,h;                 
     if (orientation()==Horizontal)
      {
	w=bindingWidth();
	h=height()-highlightThickness()*2-shadowThickness()*2-borderHeight()*2-backpageMargin()-labelHeight();
      }
     else
      {
	w=width()-highlightThickness()*2-shadowThickness()*2-borderWidth()*2-backpageMargin();
	h=bindingWidth();
      }

     if (w>=MSNotebookMinimumBindingWidth&&h>=MSNotebookMinimumBindingWidth)
      {
	int rx,ry,rw,rh;         // rectangle values for binding surface 
	int sx,sy,sw,sh,sd;      // spiral values 
	int a1,a2,a3,a4;         // spiral angle values 
	int hx,hy,hd;            // hole values 
	int lx1,ly1,lx2,ly2;     // line values for binding edge 
	int pw,ph;               // pixmap size 
	int gap;                 // gap between spirals 
	int div;                 // division of binding width 

	// Determine spiral component values
	if (orientation()==Horizontal)
	 {
           if (tabAlignment()==MSLeft)
            {
              div=w/3;
              gap=div/2;

              pw=w;
              ph=div+gap;

              sx=div-1;
              sy=gap/2;
              sw=div*2;
              sh=div;
              sd=sh/4;

              a1=275;
              a2=270;
              a3=90-10;
              a4=100;

              hx=sx-(sd/2);
              hy=sy+(sh/2)-sd+1;
              hd=MSUtil::min(div,sd*2);

              rx=0;
              ry=0;
              rw=pw-div;
              rh=ph;

              lx1=pw-div;
              ly1=0;
              lx2=lx1;
              ly2=rh;
            }
           else
            {
              div=w/3;
              gap=div/2;

              pw=w;
              ph=div+gap;

              sx=0;
              sy=gap/2;
              sw=div*2;
              sh=div;
              sd=sh/4;

              a1=270-20;
              a2=-270;
              a3=90-20;
              a4=110;

              hx=sx+sw-sd;
              hy=sy+(sh/2)-sd+1;
              hd=MSUtil::min(div,sd*2);

              rx=div;
              ry=0;
              rw=w-div;
              rh=ph;

              lx1=div;
              ly1=0;
              lx2=lx1;
              ly2=ph;
            }
	 }
	else
	 {
           if (tabAlignment()==MSTop)
            {
              div=h/3;
              gap=div/2;

              pw=div+gap;
              ph=h;

              sx=gap/2;
              sy=div-1;
              sw=div;
              sh=(div*2);
              sd=sw/4;

              a1=360-5;
              a2=-295;
              a3=90+8;
              a4=110;

              hx=sx+(sw/2);
              hy=sy;
              hd=MSUtil::min(div,sd*2);

              rx=0;
              ry=0;
              rw=pw;
              rh=ph-div;

              lx1=0;
              ly1=ph-div;
              lx2=pw;
              ly2=ly1;
            }
           else
            {
              div=h/3;
              gap=div/2;

              pw=div+gap;
              ph=h;

              sx=gap/2;
              sy=0;
              sw=div;
              sh=div*2;
              sd=sw/4;

              a1=15;
              a2=270;
              a3=90;
              a4=110;

              hx=sx+(sw/2)-sd+1;
              hy=sh-sd;
              hd=MSUtil::min(div,sd*2);

              rx=0;
              ry=div;
              rw=pw;
              rh=h-div;

              lx1=0;
              ly1=div;
              lx2=pw;
              ly2=ly1;
            }
         }

	Window dest=0;
	int xoff,yoff;
        int oldXoff, oldYoff;
	int count,dx,dy;
	// If we're int Draw mode, then we want to draw one spiral into a scratch pad pixmap
	// and use XCopyArea to repeatedly copy it to the binding pixmap that we want
	// otherwise (Print mode), we want to draw directly into the window.
	if (outputMode()<Print)
	 {
	   dest=XCreatePixmap(display(),window(),pw,ph,depth());
	   xoff=0;
	   yoff=0;
	   count=1;
	   dx=0;
	   dy=0;
	 }
	else
	 {
	   dest=window();
           if(tabAlignment()==MSRight || tabAlignment()==MSBottom)
            {
              xoff=highlightThickness()+shadowThickness()+borderWidth();
              yoff=highlightThickness()+shadowThickness()+borderHeight()+labelHeight();
            }
           else if(tabAlignment()==MSLeft)
            {
              xoff=width()-highlightThickness()-shadowThickness()-borderWidth()-bindingWidth();
              yoff=highlightThickness()+shadowThickness()+borderHeight()+labelHeight();
            }
           else
            {
              xoff=highlightThickness()+shadowThickness()+borderWidth();
              yoff=height()-highlightThickness()-shadowThickness()-borderHeight()-bindingWidth();
            }
           oldXoff=xoff;
           oldYoff=yoff;
           
	   if (orientation()==Horizontal)
	    {
	      count=h/ph;
	      dx=0;
	      dy=ph;
	    }
	   else
	    {
	      count=w/pw;
	      dx=pw;
	      dy=0;
	    }
	 }
	int i;
	for (i=0;i<count;i++)
	 {
	   // Fill pixmap with notebook background 
	   XFillRectangle(display(),dest,backgroundShadowGC(),0+xoff,0+yoff,pw,ph);
	   // draw binding surface 
	   XSetForeground(display(),notebookGC(),frameBackground());
	   XFillRectangle(display(),dest,notebookGC(),rx+xoff,ry+yoff,rw,rh);
	   // draw line along binding surface
	   XSetForeground(display(),notebookGC(),topShadowColor());
	   XSetClipMask(display(),notebookGC(),None);
	   XSetLineAttributes(display(),notebookGC(),1,LineSolid,CapRound,JoinMiter);
	   XDrawLine(display(),dest,notebookGC(),lx1+xoff,ly1+yoff,lx2+xoff,ly2+yoff);
	   // draw hole in binding surface with top/bottom shadows 
	   XFillArc(display(),dest,backgroundShadowGC(),hx+xoff,hy+yoff,hd,hd,0,360*64);
	   XDrawArc(display(),dest,bottomShadowGC(),hx+xoff,hy+yoff,hd,hd,225*64,-180*64);
	   XDrawArc(display(),dest,topShadowGC(),hx+xoff,hy+yoff,hd,hd,45*64,-180*64);
	   // draw spiral with top/bottom shadows
	   XSetForeground(display(),notebookGC(),foreground());
	   XSetLineAttributes(display(),notebookGC(),1,LineSolid,CapRound,JoinMiter);
	   for(int i=1;i<sd;i++) XDrawArc(display(),dest,notebookGC(),sx+i+xoff,sy+i+yoff,sw-i,sh-i,a1*64,a2*64);
	   XSetLineAttributes(display(),notebookGC(),MSUtil::max(0,sd-2),LineSolid,CapRound,JoinMiter);
	   XDrawArc(display(),dest,notebookGC(),sx+(sd/2)+xoff,sy+(sd/2)+yoff,sw,sh,a3*64,a4*64);
	   XDrawArc(display(),dest,topShadowGC(),sx+xoff,sy+yoff,sw,sh,a1*64,a2*64);
	   XDrawArc(display(),dest,bottomShadowGC(),sx+sd+xoff,sy+sd+yoff,sw-sd,sh-sd,a1*64,a2*64);
	   xoff+=dx;
	   yoff+=dy;
	 }
	int remainder;
	if (orientation()==Horizontal)
	 {
	   count=h/ph;
	   remainder=h%ph;
	   dx=0;
	   dy=ph;
	 }
	else
	 {
	   count=w/pw;
	   remainder=w%pw;
	   dx=pw;
	   dy=0;
	 }
	if (outputMode()<Print)
	 {
	   if (bindingPixmap()!=0) XFreePixmap(display(),bindingPixmap());
	   _bindingPixmap=XCreatePixmap(display(),window(),w,h,depth());
	   xoff=0;
	   yoff=0;
	   for (i=0;i<count;i++)
	    {
	      XCopyArea(display(),dest,bindingPixmap(),notebookGC(),0,0,pw,ph,xoff,yoff);
	      xoff+=dx;
	      yoff+=dy;
	    }
	   if (remainder>0)
	    {
	      if (orientation()==Horizontal) XCopyArea(display(),dest,bindingPixmap(),notebookGC(),0,0,pw,remainder,xoff,yoff);
	      else XCopyArea(display(),dest,bindingPixmap(),notebookGC(),0,0,remainder,ph,xoff,yoff);
	    }
	   xoff=0;
	   yoff=0;
	   XFreePixmap(display(),dest);
	 }
	else
	 {
	   if (remainder>0)
	    {
	      if (orientation()==Horizontal) XDrawLine(display(),dest,notebookGC(),lx1+xoff,ly1+yoff,lx2+xoff,ly1+remainder+yoff-1);
	      else XDrawLine(display(),dest,notebookGC(),lx1+xoff,ly1+yoff,lx1+remainder+xoff-1,ly2+yoff);
	    }
	   xoff=oldXoff;
	   yoff=oldYoff;
	 }
	if (frameThickness()>0)
	 {
	   int x1,y1,x2,y2;
	   if (orientation()==Horizontal)
	    {
              if (tabAlignment()==MSLeft)
               {
                 x1=xoff;
                 y1=yoff;
                 x2=xoff+bindingWidth()-bindingWidth()/3;
                 y2=y1;
                 XDrawLine(display(),bindingPixmap(),topShadowGC(),x1,y1,x2,y2);
                 y1=yoff+h-1;
                 y2=y1;
                 XDrawLine(display(),bindingPixmap(),bottomShadowGC(),x1,y1,x2,y2);
               }
              else
               {
                 x1=xoff+bindingWidth()/3;
                 y1=yoff;
                 x2=xoff+bindingWidth();
                 y2=y1;
                 XDrawLine(display(),bindingPixmap(),topShadowGC(),x1,y1,x2,y2);
                 y1=yoff+h-1;
                 y2=y1;
                 XDrawLine(display(),bindingPixmap(),bottomShadowGC(),x1,y1,x2,y2);
               }
	    }
	   else
	    {
              if (tabAlignment()==MSTop)
               {
                 x1=xoff;
                 y1=yoff;
                 x2=x1;
                 y2=yoff+bindingWidth()-bindingWidth()/3;
                 XDrawLine(display(),bindingPixmap(),topShadowGC(),x1,y1,x2,y2);
                 x1=xoff+w-1;
                 x2=x1;
                 XDrawLine(display(),bindingPixmap(),bottomShadowGC(),x1,y1,x2,y2);                 
               }
              else
               {
                 x1=xoff;
                 y1=yoff+bindingWidth()/3;
                 x2=x1;
                 y2=yoff+bindingWidth();
                 XDrawLine(display(),bindingPixmap(),topShadowGC(),x1,y1,x2,y2);
                 x1=xoff+w-1;
                 x2=x1;
                 XDrawLine(display(),bindingPixmap(),bottomShadowGC(),x1,y1,x2,y2);
               }
            }
	 }
      }
   }
} 

MSWidgetVector MSNotebook::children(void)
{
  MSWidgetVector vector;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     vector.append(entry->widget());
   }
  return vector;
}

MSWidgetVector MSNotebook::managedChildren(void)
{
  MSWidgetVector vector;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue) vector.append(entry->widget());
   }
  return vector;
}

void MSNotebook::updateTitleVector(void)
{
  MSStringVector vector;
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue)
      {
        vector.append(entry->title()(0));
      }
   }
  _titleVector=vector;
  if (_popupMenu!=0)
   {
     updatePopupMenu();
     _popupMenu->columns(titleVector().length()/10+1);
   }
}

void MSNotebook::updatePopupMenu(void)
{
  if (_popupMenu!=0)
   {
     unsigned i=0;
     MSNodeItem *hp=(MSNodeItem *)childListHead();
     MSNodeItem *np=hp;
     while ((np=np->next())!=hp)
      {
	NotebookEntry *entry=(NotebookEntry *)np->data();
	if (entry->managed()==MSTrue)
	 {
	   if (entry->tab()->sensitive()==MSTrue) _popupMenu->taggedMenuItem(i++)->sensitive(MSTrue);
	   else _popupMenu->taggedMenuItem(i++)->sensitive(MSFalse);
	 }
      }
   }
}

MSStringVector MSNotebook::hiddenTitles(void) const
{
  MSStringVector vector;
  MSNodeItem *hp=(MSNodeItem *)childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSFalse) vector.append(entry->title()(0));
   }
  return vector;

}

void MSNotebook::permuteTitles(const MSStringVector &aStringVector_)
{
  MSNodeItem newList;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  for (unsigned i=0;i<aStringVector_.length();i++)
   {
     np=hp;
     while((np=np->next())!=hp)
      {
	NotebookEntry *entry=(NotebookEntry *)np->data();
	if (entry->title()==aStringVector_(i))
	 {
	   entry->managed(MSTrue);
	   np->remove();
	   np->insert(&newList);
	   break;
	 }
      }
   }
  MSNodeItem *next=hp->next();
  np=next;
  while(np!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     entry->managed(MSFalse);
     entry->widget()->unmap();  // To make sure it is unmapped
     next=np->next();
     np->remove();
     np->insert(&newList);
     np=next;
   }
  next=newList.next();
  np=next;
  while(np!=&newList)
   {
     next=np->next();
     np->remove();
     np->insert(hp);
     np=next;
   }
  updateTitleVector();
  firstEntry(firstItem());
  if (currentEntry()!=0)
   {
     if (currentEntry()->managed()==MSTrue) resetFirstEntry();
     else currentEntry(0);
   }
  if (firstMap()==MSTrue)
   {
     positionTabs();
     redraw();
   }
}

void MSNotebook::permuteWidgets(const MSWidgetVector &aWidgetVector_)
{
  MSNodeItem newList;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  for (unsigned i=0;i<aWidgetVector_.length();i++)
   {
     np=hp;
     while((np=np->next())!=hp)
      {
	NotebookEntry *entry=(NotebookEntry *)np->data();
	if (entry->widget()==aWidgetVector_(i))
	 {
	   entry->managed(MSTrue);
	   np->remove();
	   np->insert(&newList);
	   break;
	 }
      }
   }
  MSNodeItem *next=hp->next();
  np=next;
  while(np!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     entry->managed(MSFalse);
     entry->widget()->unmap();  // To make sure it is unmapped
     next=np->next();
     np->remove();
     np->insert(&newList);
     np=next;
   }
  next=newList.next();
  np=next;
  while(np!=&newList)
   {
     next=np->next();
     np->remove();
     np->insert(hp);
     np=next;
   }
  updateTitleVector();
  firstEntry(firstItem());
  if (currentEntry()!=0)
   {
     if (currentEntry()->managed()==MSTrue) resetFirstEntry();
     else currentEntry(0);
   }
  if (firstMap()==MSTrue)
   {
     positionTabs();
     redraw();
   }
}

MSNotebook::NotebookEntry *MSNotebook::firstItem(void)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue) return entry;
   }
  return (NotebookEntry *)0;
}

MSNotebook::NotebookEntry *MSNotebook::lastItem(void)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while ((np=np->prev())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue) return entry;
   }
  return (NotebookEntry *)0;
}

int MSNotebook::arrowWidth(void)
{
  int w=upLeftArrow()->width();
  if (orientation()==Horizontal) return w+arrowOffset();
  else return w;
}

int MSNotebook::arrowHeight(void)
{
  int h=upLeftArrow()->height();
  if (orientation()==Vertical) return h+arrowOffset();
  else return h;
}

int MSNotebook::totalTabWidth(void)
{
  int totalW=0;
  if (showTabs()==MSTrue)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     NotebookEntry *entry;
     int numTabs=0;
     while ((np=np->next())!=hp)
      {
	entry=(NotebookEntry *)np->data();
	if (entry!=0&&entry->managed()==MSTrue)
         {
           totalW+=entry->tab()->width();
           numTabs++;
         }
      }
     if (numTabs>1) totalW+=((numTabs-1)*tabSpacing());
   }
  return totalW;
}

int MSNotebook::maxTabWidth(void)
{
  int maxW=0;
  if (showTabs()==MSTrue)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     NotebookEntry *entry;
     while ((np=np->next())!=hp)
      {
	entry=(NotebookEntry *)np->data();
	if (entry!=0&&entry->managed()==MSTrue)
	 {
	   int w=entry->tab()->width();
	   maxW=(maxW<w)?w:maxW;
	 }
      }
   }
  return maxW;
}

int MSNotebook::availableWidthForTabs(void)
{
  int WidthOffset=highlightThickness()+shadowThickness()+borderWidth();
  int totalSpace=width()-WidthOffset*2-arrowWidth()*3-arrowOffset()*3;
  return totalSpace;
}

int MSNotebook::totalTabHeight(void)
{
  int totalH=0;
  if (showTabs()==MSTrue)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     NotebookEntry *entry;
     int numTabs=0;
     while ((np=np->next())!=hp)
      {
	entry=(NotebookEntry *)np->data();
	if (entry!=0&&entry->managed()==MSTrue)
         {
           totalH+=entry->tab()->height();
           numTabs++;
         }
      }
     if (numTabs>1) totalH+=((numTabs-1)*tabSpacing());
   }
  return totalH;
}

int MSNotebook::maxTabHeight(void)
{
  int maxH=0;
  if (showTabs()==MSTrue)
   {
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     NotebookEntry *entry;
     while ((np=np->next())!=hp)
      {
	entry=(NotebookEntry *)np->data();
	if (entry!=0&&entry->managed()==MSTrue)
	 {
	   int h=entry->tab()->height();
	   maxH=(maxH<h)?h:maxH;
	 }
      }
   }
  return maxH;
}

int MSNotebook::availableHeightForTabs(void)
{
  int HeightOffset=highlightThickness()+shadowThickness()+borderHeight();
  int totalSpace=height()-HeightOffset*2-arrowHeight()*3-labelHeight()-arrowOffset()*3;
  return totalSpace;
}

void MSNotebook::menuSelection(void)
{
  NotebookEntry *entry=getEntry(_popupMenu->activeMenuItem()->tag());
  if (entry!=0&&entry!=currentEntry())
   {
     currentEntry(entry);
     resetFirstEntry();
     positionTabs();
     pageChanged();
   }
}

void MSNotebook::pageSelected(NotebookEntry *entry_)
{
  if (currentEntry()!=entry_)
   {
     currentEntry(entry_);
     positionTabs();
     pageChanged();
   }
}

void MSNotebook::pageChanged(void)
{
  activateCallback(MSWidgetCallback::pagechange);
}

int MSNotebook::numManaged(void)
{
  int num=0;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue) num++;
   }
  return num;
}

void MSNotebook::installPopup(MSPopupMenu *applicationPopup_)
{
  if (_applicationPopup!=applicationPopup_)
   {
     if (_applicationPopup!=0) _applicationPopup->destroy();
     _applicationPopup=applicationPopup_;
   }
}

MSPopupMenu *MSNotebook::popupMenu(void)
{
  if (_applicationPopup!=0) return _applicationPopup;
  else
   {
     if (_popupMenu==0)
      {
	_popupMenu=new MSStringPopupMenu(server(),titleVector());
	_popupMenu->font(font());
	_popupMenu->foreground(foreground());
	_popupMenu->background(background());
	_popupMenu->callback(MSWidgetCallback::activate,
			     new MSMethodCallback<MSNotebook>(this,&MSNotebook::menuSelection));
	_popupMenu->columns(titleVector().length()/10+1);
	updatePopupMenu();
      }
     if (titleVector().length()>0) return _popupMenu;
     else return 0;
   }
}

MSBoolean MSNotebook::okToSwitch(void)
{
  MSWidget *focus=inputFocus();
  if (focus!=0)
   {
     MSWidgetCursor cursor(this);
     for (cursor.setToFirst();cursor.isValid()==MSTrue;cursor.setToNext())
      {
	if (focus==cursor.widget()) return top()->traverseFocus(0);
      }
     return MSTrue;
   }
  else return MSTrue;
}

void MSNotebook::buttonPress(const XEvent *event_)
{
  buttonPressNotify(this,event_);
}

void MSNotebook::button3Press(const XEvent *)
{
  if (showPopup()==MSTrue&&isProtected()==MSFalse&&okToSwitch()==MSTrue)
   {
     MSPopupMenu *popup=popupMenu();
     if (popup!=0) popup->showAtPointer();
   }
  else server()->bell();
}

void MSNotebook::set(MSAttrValueList& avList_)
{
  MSManager::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="pageTitle")
      {
        MSStringVector pageTitles=avList_[i].options();
        MSWidgetVector childVector=children();
        MSString pageName;
        for(i=0;i<childVector.length()&&i<pageTitles.length();i++)
         {
           pageName=pageTitles(i);
           pageName.change("\\n",MSString('\n'));
           pageTitle(childVector(i),pageName.string());
         }
      }
     else if (avList_[i].attribute()=="lockSize")
      {
	lockSize(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="orientation")
      {
	MSString val=avList_[i].value();
	orientation(val=="Vertical"?Vertical:Horizontal);
	index<<i;
      }
     else if (avList_[i].attribute()=="tabAlignment")
      {
	tabAlignment((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="titles")
      {
	MSString names=avList_[i].value();
	names.change("\\n",MSString('\n'));
	permuteTitles(MSStringVector(names));
	index<<i;
      }
     else if (avList_[i].attribute()=="currentTitle")
      {
	currentTitle(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="showTabs")
      {
	showTabs(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showPopup")
      {
	showPopup(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showBinding")
      {
	showBinding(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="bindingWidth")
      {
	bindingWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="tabSpacing")
      {
	tabSpacing(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="frameThickness")
      {
	frameThickness(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="borderWidth")
      {
	borderWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="borderHeight")
      {
	borderHeight(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="marginWidth")
      {
	marginWidth(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="marginHeight")
      {
	marginHeight(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="backpages")
      {
	backpages(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="backpageThickness")
      {
	backpageThickness(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="backpageForeground")
      {
	backpageForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="backpageBackground")
      {
	backpageBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedPageForeground")
      {
	selectedPageForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedPageBackground")
      {
	selectedPageBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="frameBackground")
      {
	frameBackground(avList_[i].value());
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSNotebook::get(MSAttrValueList& avList_)
{
  MSStringVector aBooleanVector("MSTrue\nMSFalse");

  MSStringVector pageTitles;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     NotebookEntry *entry=(NotebookEntry *)np->data();
     if (entry->managed()==MSTrue)
      {
        pageTitles.append(MSAttrValue::stringVectorToString(entry->tab()->label()));
      }
   }
 
  avList_<<MSAttrValue("pageTitle","",pageTitles,MSAttrValue::ChildAttribute);
  avList_<<MSAttrValue("lockSize",
		       lockSize()==MSTrue?"MSTrue":"MSFalse",aBooleanVector);
  avList_<<MSAttrValue("orientation",
		       orientation()==Vertical?"Vertical":"Horizontal",
		       MSStringVector("Vertical\nHorizontal"));
  avList_<<MSAttrValue("tabAlignment",
                       MSAttrValue::alignmentToString(tabAlignment()),
                       MSStringVector("MSLeft\nMSRight\nMSTop\nMSBottom"));
  avList_<<MSAttrValue("titles",titles().asString(),MSAttrValue::ReadOnly);
  avList_<<MSAttrValue("currentTitle",currentTitle(),MSAttrValue::ReadOnly);
  avList_<<MSAttrValue("showTabs",
		       showTabs()==MSTrue?"MSTrue":"MSFalse",aBooleanVector);
  avList_<<MSAttrValue("showPopup",
		       showPopup()==MSTrue?"MSTrue":"MSFalse",aBooleanVector);
  avList_<<MSAttrValue("showBinding",
		       showBinding()==MSTrue?"MSTrue":"MSFalse",aBooleanVector);
  avList_<<MSAttrValue("bindingWidth",MSString(bindingWidth()));
  avList_<<MSAttrValue("tabSpacing",MSString(tabSpacing()));
  avList_<<MSAttrValue("frameThickness",MSString(frameThickness()));
  avList_<<MSAttrValue("borderWidth",MSString(borderWidth()));
  avList_<<MSAttrValue("borderHeight",MSString(borderHeight()));
  avList_<<MSAttrValue("marginWidth",MSString(marginWidth()));
  avList_<<MSAttrValue("marginHeight",MSString(marginHeight()));
  avList_<<MSAttrValue("backpages",MSString(backpages()));
  avList_<<MSAttrValue("backpageThickness",MSString(backpageThickness()));
  avList_<<MSAttrValue("backpageForeground",
		       server()->colorName(backpageForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("backpageBackground",
		       server()->colorName(backpageBackground()),MSAttrValue::Color);  
  avList_<<MSAttrValue("selectedPageForeground",
		       server()->colorName(selectedPageForeground()),MSAttrValue::Color);  
  avList_<<MSAttrValue("selectedPageBackground",
		       server()->colorName(selectedPageBackground()),MSAttrValue::Color);  
  avList_<<MSAttrValue("frameBackground",
		       server()->colorName(frameBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("pagechange","",MSAttrValue::Callback);
  return MSManager::get(avList_);
}

MSNotebook::NotebookEntry::NotebookEntry(MSNotebook *owner_,MSWidget *widget_,const char *title_)
: _widget(widget_),_managed(MSTrue)
{
  owner_->internalEvent(MSTrue);
  _tab=new NotebookTab(owner_,this,title_);
  // Map the tab so it can calculate its size
  tab()->foreground(owner_->foreground());
  tab()->tabForeground(owner_->foreground(),MSFalse);
  tab()->background(owner_->backpageBackground());
  tab()->tabBackground(owner_->backpageBackground(),MSFalse);
  tab()->moveTo(tab()->server()->width(),tab()->server()->height());
  tab()->show();
  tab()->unmap();
  owner_->internalEvent(MSFalse);
}

MSNotebook::NotebookEntry::~NotebookEntry(void)
{
  MSNotebook *notebook=(MSNotebook*)tab()->owner();
  MSBoolean currentEvent=notebook->internalEvent();
  notebook->internalEvent(MSTrue);
  tab()->destroy();
  notebook->internalEvent(currentEvent);
}

MSNotebook::NotebookTab::NotebookTab(MSWidget *owner_,NotebookEntry *entry_,const char *title_)
: MSIconButton(owner_,title_), _entry(entry_)
{
  _labelAlignment=MSRight;
  _acceptFocus=MSFalse;
  _highlightThickness=0;
  _margin=4;
  _dynamic=MSTrue;
  _modifiedFlag=0;
  removeFromFocusList();
}

MSNotebook::NotebookTab::~NotebookTab(void)
{}

void MSNotebook::NotebookTab::redraw(void)
{
  MSIconButton::redraw();
  blendIn();
}

void MSNotebook::NotebookTab::blendIn(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse&&shadowThickness()>0)
   {
     MSNotebook *notebook=(MSNotebook *)owner();
     GC gc=notebook->backpageGC();
     if (notebook->currentEntry()!=0&&notebook->currentEntry()->tab()==this)
      {
        XSetForeground(display(),gc,notebook->selectedPageBackground());
      }
     else
      {
        XSetForeground(display(),gc,notebook->backpageBackground());
      }
     if (armed()==MSTrue) gc=selectShadowGC();
     else gc=backgroundShadowGC();
     XPoint points[4];
     if (notebook->orientation()==Horizontal)
      {
	if (height()>=shadowThickness()*2)
	 {
           if (notebook->tabAlignment()==MSLeft)
            {
              points[0].x=width();
              points[0].y=0;
              points[1].x=0;
              points[1].y=height();
              points[2].x=-shadowThickness();
              points[2].y=-shadowThickness();
              points[3].x=0;
              points[3].y=-(height()-shadowThickness()*2);
            }
           else
            {
              points[0].x=0;
              points[0].y=0;
              points[1].x=shadowThickness();
              points[1].y=shadowThickness();
              points[2].x=0;
              points[2].y=height()-shadowThickness()*2;
              points[3].x=-shadowThickness();
              points[3].y=shadowThickness();
            }
	   XBFillPolygon(display(),window(),gc,points,4,Convex,CoordModePrevious);
	 }
      }
     else
      {
	if (width()>=shadowThickness()*2)
	 {
           if (notebook->tabAlignment()==MSTop)
            {
              points[0].x=0;
              points[0].y=height();
              points[1].x=width();
              points[1].y=0;
              points[2].x=-shadowThickness();
              points[2].y=-shadowThickness();
              points[3].x=-(width()-shadowThickness()*2);
              points[3].y=0;
            }
           else
            {
              points[0].x=0;
              points[0].y=0;
              points[1].x=shadowThickness();
              points[1].y=shadowThickness();
              points[2].x=width()-shadowThickness()*2;
              points[2].y=0;
              points[3].x=shadowThickness();
              points[3].y=-shadowThickness();
            }
	   XBFillPolygon(display(),window(),gc,points,4,Convex,CoordModePrevious);
	 }
      }
   }
}

void MSNotebook::NotebookTab::buttonPress(const XEvent *event_)
{
  if (sensitive()==MSTrue&&event_->xbutton.button==Button1)
   {
     MSNotebook *notebook=(MSNotebook *)owner();
     if (notebook->isProtected()==MSFalse&&notebook->okToSwitch()==MSTrue) arm();
     else server()->bell();
   }
  else if (event_->xbutton.button==Button3)
   {
     buttonPressNotify(owner(),event_);
   }
}

void MSNotebook::NotebookTab::motionNotify(const XEvent *event_)
{
  MSNotebook *notebook=(MSNotebook *)owner();
  if (sensitive()==MSTrue&&notebook->isProtected()==MSFalse&&notebook->okToSwitch()==MSTrue)
   {
     if (event_->xmotion.is_hint==NotifyNormal&&(event_->xmotion.state&Button1Mask)==Button1Mask)
      {
	if (event_->xmotion.x<0||event_->xmotion.x>width()||
	    event_->xmotion.y<0||event_->xmotion.y>height())
	 { disarm(); }
	else if (armed()==MSFalse)
	 {
	   if ((event_->xmotion.x>=0&&event_->xmotion.x<=width())&&
	       (event_->xmotion.y>=0&&event_->xmotion.y<=height()))
	    { arm(); }
	 }
      }
   }
}

void MSNotebook::NotebookTab::activate(void)
{

  MSNotebook *notebook=(MSNotebook *)owner();
  notebook->pageSelected(entry());
}

void MSNotebook::NotebookTab::arm(void)
{
  if (armed()==MSFalse)
   {
     _armed=MSTrue;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XBFillRectangle(display(),window(),selectShadowGC(),
	    	       offset,offset,width()-2*offset,height()-2*offset);
	if (showPixmap()==MSTrue) drawPixmap();
        if (showLabel()==MSTrue) drawLabel();
        drawSunken();
	blendIn();
        XFlush(display());
      }
   }
}

void MSNotebook::NotebookTab::disarm(void)
{
  if (armed()==MSTrue)
   {
     _armed=MSFalse;
     if (mapped()==MSTrue)
      {
        int offset=highlightThickness()+shadowThickness();
        XBFillRectangle(display(),window(),backgroundShadowGC(),
	   	       offset,offset,width()-2*offset,height()-2*offset);
	if (showPixmap()==MSTrue) drawPixmap();
        if (showLabel()==MSTrue) drawLabel();
        drawRaised();
	blendIn();
        XFlush(display());
      }
   }
}

MSBoolean MSNotebook::NotebookTab::isModified(MSNotebookTabAttribute::SetFlag flag_)
{ return (modifiedFlag()&flag_)?MSTrue:MSFalse;}

void MSNotebook::NotebookTab::tabBackground(unsigned long bg_,MSBoolean modified_)
{
  _tabBackground=bg_;
  if (modified_==MSTrue) _modifiedFlag|=MSNotebookTabAttribute::TabBackground;
}

void MSNotebook::NotebookTab::tabForeground(unsigned long fg_,MSBoolean modified_)
{
  _tabForeground=fg_;
  if (modified_==MSTrue) _modifiedFlag|=MSNotebookTabAttribute::TabForeground;
}

void MSNotebook::NotebookTab::tabFont(Font fid_,MSBoolean modified_)
{
  font(fid_);
  if (modified_==MSTrue) _modifiedFlag|=MSNotebookTabAttribute::TabFont;
}

MSNotebook::NotebookArrow::NotebookArrow(MSWidget *owner_,MSArrow::ArrowType type_)
: MSArrowButton(owner_,type_),_mode(Search)
{}

MSNotebook::NotebookArrow::~NotebookArrow(void)
{}


void MSNotebook::NotebookArrow::activate(void)
{
  if (mode()==Search) search();
  else browse();
}

void MSNotebook::NotebookArrow::search(void)
{
  if (sensitive()==MSTrue)
   {
     if (arrow()->selected()==MSFalse) arrow()->select(MSTrue);
     MSNotebook *notebook=(MSNotebook *)owner();
     NotebookEntry *first;
     if (arrow()->arrowType()==MSArrow::Up||arrow()->arrowType()==MSArrow::Left) first=notebook->previousEntry();
     else first=notebook->nextEntry();
     if (first!=notebook->firstEntry())
      {
        notebook->firstEntry(first);
        notebook->positionTabs(MSBoolean(arrow()->arrowType()==MSArrow::Down||arrow()->arrowType()==MSArrow::Right));	
      }
   }
  else server()->bell();
}

void MSNotebook::NotebookArrow::browse(void)
{
  MSNotebook *notebook=(MSNotebook *)owner();
  NotebookEntry *entry;
  if (arrow()->arrowType()==MSArrow::Up||arrow()->arrowType()==MSArrow::Left) entry=notebook->previousBrowseEntry();
  else entry=notebook->nextBrowseEntry();
  if (entry!=notebook->currentEntry())
   {
     if (arrow()->selected()==MSFalse) arrow()->select(MSTrue);
     notebook->currentEntry(entry);
     notebook->resetFirstEntry();
     notebook->positionTabs();
     notebook->pageChanged();
   }
  else server()->bell();
}

void MSNotebook::NotebookArrow::buttonPress(const XEvent *event_)
{buttonPressNotify(this,event_);}

void MSNotebook::NotebookArrow::button1Press(const XEvent *event_)
{
  if (event_->xbutton.state&ControlMask)
   {
     mode(Browse);
     browse();
   }
  else if (sensitive()==MSTrue)
   {
     mode(Search);
     search();
   }
  else server()->bell();
  if (mode()==Browse) repeatThreshold(MSNotebookArrowBrowseRepeatThreshold);
  else repeatThreshold(MSNotebookArrowSearchRepeatThreshold);
  if (arrowTimer()==0) _arrowTimer=new MSArrowTimer(repeatThreshold(),this);
  else arrowTimer()->expirationInterval(repeatThreshold());
  if (mode()==Browse) repeatInterval(MSNotebookArrowBrowseRepeatInterval);
  else repeatInterval(MSNotebookArrowSearchRepeatInterval);
  arrowTimer()->reset();
}

MSNotebookTabAttribute::MSNotebookTabAttribute(MSNotebook *notebook_)
{
  _notebook=notebook_;
  _fg=notebook()->backpageForeground();
  _bg=notebook()->backpageBackground();
  _font=notebook()->font();
  _sensitive=MSTrue;
  _label="Untitled";
  _pixmap=0;
  _labelAlignment=MSRight;
  _setFlag=0;
}

MSNotebookTabAttribute::~MSNotebookTabAttribute(void)
{
  if (_pixmap!=0) delete _pixmap;
}

void MSNotebookTabAttribute::pixmap(const MSPixmap &pixmap_)
{
  if (_pixmap!=0) delete _pixmap;
  _pixmap=new MSPixmap(pixmap_);
  _setFlag|=TabPixmap;
}

void MSNotebookTabAttribute::reset(void)
{
  if (_pixmap!=0)
   {
     delete _pixmap;
     _pixmap=0;
   }
  _setFlag=0;
}


