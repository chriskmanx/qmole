///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSCollapsibleLayout.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSButton.H>


static const int MSCollapsibleLayoutDefaultShadowThickness=2;
static const int MSCollapsibleLayoutDefaultHighlightThickness=0;
static const unsigned long MSCollapsibleLayoutEventMask=(ExposureMask);
static const char * MSCollapsibleLayoutDefaultHandleBackground="black";
static const char * MSCollapsibleLayoutDefaultHandleForeground="white";
static const int MSCollapsibleLayoutDefaultHandleSize=8;


class MSCollapsibleEntry {

public:
  enum STATE { OPEN, CLOSED, NOTREADY };

public:
  MSCollapsibleEntry(MSWidget*, MSCollapsibleLayout*);
  MSCollapsibleEntry(const MSCollapsibleEntry&);
  void resize(int,int);
  
  void changeState();
  void state(STATE);

  void height(int) ;
  void moveTo( int, int);

  void show() ;
  int width() const;
  unsigned long resizeConstraints() const;
  void setEntry();
  void unsetEntry();
  void destroy();
  void toolTip(const MSStringVector &);
  const MSStringVector &toolTip() const {return _handle->toolTip();}
  MSBoolean displayToolTip() const { return _handle->displayToolTip(); }
  void close();
  void open();

  MSWidget* widget() const { return _widget; }
  STATE state() const { return _state; }
  MSWidget* button() const { return _handle; }
  int height() const { return _widget->height(); }

  void updateTile( MSPixmap* pixmap_) { _handle->updateTile(pixmap_);}

private:

  class Handle : public MSLabel 
  {
  
  public:
    Handle(MSWidget *,MSCollapsibleEntry*);
    void updateTile( MSPixmap*);
    ~Handle(void);
  
  private:
    MSCollapsibleEntry* _entry;


    void init(void);
    virtual void buttonPress(const XEvent *);
    virtual void redraw();
  };
  
private:
  MSWidget* _widget;
  Handle* _handle;
  MSCollapsibleLayout* _owner;
  STATE _state;
};






MSCollapsibleLayout::MSCollapsibleLayout(MSWidget *owner_) : MSWidgetCommon(owner_),_tile(0)
{ init(); }

MSCollapsibleLayout::~MSCollapsibleLayout(void)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  MSWidget   *wid;

  // delete the widget after the node has been removed from the list
  // this will prevent childDestroy from causing damage
  while ((np=hp->next())!=hp)
   {
     wid=((MSCollapsibleEntry*)np->data())->widget();
     delete np;
     if (wid!=0) safeDestroy(wid);
     _childCount--;
   } 
}

void MSCollapsibleLayout::init(void)
{
  _childCount=0;
  _shadowThickness=MSCollapsibleLayoutDefaultShadowThickness;
  _highlightThickness=MSCollapsibleLayoutDefaultHighlightThickness;
  shadowStyle(MSRaised);
  selectInput(MSCollapsibleLayoutEventMask); 
  internalEvent(MSFalse);
  naturalSizing = MSFalse;
  handleBackground(MSCollapsibleLayoutDefaultHandleBackground);
  handleForeground(MSCollapsibleLayoutDefaultHandleForeground);
  handleSize(MSCollapsibleLayoutDefaultHandleSize);
}


void MSCollapsibleLayout::hideConfigure(MSCollapsibleEntry* entry_) 
{ 
  if (entry_->state()==MSCollapsibleEntry::OPEN)  // a widget just opened
    entryOpened();   
  else   // a widget just closed
    entryClosed();
  computeSize();
}


void MSCollapsibleLayout::configure(void) 
{ placement(); }

void MSCollapsibleLayout::childInsert(MSWidget *widget_) 
{ 
  childCreate(widget_); 
  if (widget_->mapped() == MSTrue ) 
    {
      getEntry(widget_)->setEntry();  // ? to help with reparenting
      computeSize();
    }
}

void MSCollapsibleLayout::childRemove(MSWidget *widget_) 
{ childDestroy(widget_); }


void MSCollapsibleLayout::childCreate(MSWidget *widget_)
{    
  if(internalEvent()==MSFalse){
    internalEvent(MSTrue);
    
    MSNodeItem *hp=childListHead();
    MSNodeItem *np=hp;
    MSBoolean found=MSFalse;
    
    while (found==MSFalse&&(np=np->next())!=hp)
      {
	if (((MSCollapsibleEntry*)np->data())->widget()==widget_)
	  {
	    found=MSTrue;
	    np=hp->prev();
	  }
      } 
    
    if (widget_!=0&&found!=MSTrue)
      {
	MSNodeItem *np=new MSNodeItem((void *)new MSCollapsibleEntry(widget_,this));
	np->insert(hp);
	_childCount++;
      }
    internalEvent(MSFalse);
  }
}





// a child has been destroyed-remove it from the child list

void MSCollapsibleLayout::childDestroy(MSWidget *widget_)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
    {
      if (((MSCollapsibleEntry *)np->data())->widget()==widget_)
        {
	  
	  ((MSCollapsibleEntry *)np->data())->destroy();
	  delete (MSCollapsibleEntry *)np->data();
	  delete np;
	  _childCount--;
	  np=hp->prev();
	  computeSize();
	}
    } 
}




void MSCollapsibleLayout::print(const char *file_)
{
  MSBoolean   fileOpen=MSFalse;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSCollapsibleEntry      *entry;
  
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
      entry=(MSCollapsibleEntry*) np->data();
      if(entry->state()!=MSCollapsibleEntry::NOTREADY)
	{
	  displayPrintOriginInc(entry->widget());
	  entry->widget()->print();
	  displayPrintOriginDec(entry->widget());
	  displayPrintOriginInc(entry->button());
	  entry->button()->print();
	  displayPrintOriginDec(entry->button());
	}
    } 
  if (fileOpen==MSTrue) 
    {
      displayPrintClose();
      outputMode(Draw);
    }
}





void MSCollapsibleLayout::firstMapNotify(){
   computeSize();
}


void MSCollapsibleLayout::show(void)
{
  if (mapped()==MSFalse)
   {
     internalEvent(MSTrue);
     MSNodeItem *hp=childListHead();
     MSNodeItem *np=hp;
     MSCollapsibleEntry   *eid;
     while((np=np->next())!=hp)
      {
	eid=(MSCollapsibleEntry *)np->data();
	eid->show();
      }
     map();
     internalEvent(MSFalse);
   }
}


void MSCollapsibleLayout::visibilityObscured(void)
{
  visible(MSFalse);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while((np=np->next())!=hp)
    {
      visibilityObscuredNotify(((MSCollapsibleEntry *)np->data())->widget());
      visibilityObscuredNotify(((MSCollapsibleEntry *)np->data())->button());
    }
 }

void MSCollapsibleLayout::visibilityUnobscured(void)
{
  visible(MSTrue);
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  while((np=np->next())!=hp)
    {
      visibilityUnobscuredNotify(((MSCollapsibleEntry *)np->data())->widget());
      visibilityUnobscuredNotify(((MSCollapsibleEntry *)np->data())->button());
    }
}



MSWidgetVector MSCollapsibleLayout::children(void)
{
  MSWidgetVector vector;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp) vector.append(((MSCollapsibleEntry *)np->data())->widget());
  return vector;
}


void MSCollapsibleLayout::internalEvent(MSBoolean internalEvent_) {_internalEvent=internalEvent_;}

MSBoolean MSCollapsibleLayout::internalEvent(void) {return _internalEvent;}


void MSCollapsibleLayout::placement(void)
{
  int width;
  int height;
  int hiddenWidgets,unconstrainedWidgets;
  if(frozen() != MSTrue){
    internalEvent(MSTrue);
    natural(width,height,hiddenWidgets,unconstrainedWidgets);
    doShownPlacement(height,hiddenWidgets,unconstrainedWidgets);
    doHiddenPlacement(); 
    internalEvent(MSFalse);
  }
}


void MSCollapsibleLayout::natural( int &naturalWidth_, int &naturalHeight_,
			 int &hiddenWidgets_,int &unconstrainedWidgets_)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  np = hp;
  MSCollapsibleEntry*   entry;
  int width =0 ,maxwidth =0;
  int yNeeded = 0;

  hiddenWidgets_ = 0;
  // unconstrained_widgets is the number of widgets whose hights are adjustable
  unconstrainedWidgets_ = 0;
   
  // this loop calculates how much space the layout should take in its present state
  while ((np=np->next())!=hp)
    {
      entry = (MSCollapsibleEntry*)np->data();
      if(entry->state()==MSCollapsibleEntry::OPEN){

	yNeeded+= entry->height();
	
	if(!(entry->resizeConstraints() & At::MinimizeHeight ) &&
	   !( entry->resizeConstraints() & At::MaintainHeight) )
	  unconstrainedWidgets_++;
	
	width = entry->width();	
	if (maxwidth < width) maxwidth =width;
      }

      if(entry->state()==MSCollapsibleEntry::CLOSED)  hiddenWidgets_++; 
      // If buttons will be on the bottom, take this into account
    }
  if(hiddenWidgets_ != 0)    yNeeded += _handleSize;
      
  naturalHeight_ = yNeeded;
  if(MSRect::width() > maxwidth && naturalSizing == MSFalse)
    naturalWidth_  =  MSRect::width();   
  else    
    naturalWidth_  =   maxwidth;
}


void MSCollapsibleLayout::doShownPlacement(int& height_, int &hiddenWidgets_, int &unconstrainedWidgets_)
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  np = hp;
  MSCollapsibleEntry*   entry;
  int yDisplacement = 0;
  int yDifference= height_ - MSRect::height() ;  
  int width =  MSRect::width() ;
  float Ydiff =0;
  int newWidth;
  int newHeight;

  if ( ( _childCount - hiddenWidgets_  )!=0){
    Ydiff =  ( unconstrainedWidgets_ != 0) ?  (yDifference /unconstrainedWidgets_)
      :(yDifference/( _childCount - hiddenWidgets_  )) ;
  }

  while ((np=np->next())!=hp)
    {
      entry = (MSCollapsibleEntry*)np->data();
      
      if(entry->state()==MSCollapsibleEntry::OPEN)
	{
	  entry->moveTo(0,yDisplacement); 
	  
	  newWidth = entry->width();
	  newHeight = entry->height();
	  
	  // if the widget doesnt have  a w or W constraint, make it the maximium width
	  if(! (entry->resizeConstraints() & At::MinimizeWidth ) && 
	     ! ( entry->resizeConstraints() & At::MaintainWidth))
	    newWidth = width;
	  
	  // no vertical constraints on this entry, so it can take the slack
	  if(!(entry->resizeConstraints() & At::MinimizeHeight ) &&
	     !( entry->resizeConstraints() & At::MaintainHeight) )
	    {
	      newHeight = (int)((entry->height() > Ydiff )? (entry->height() - Ydiff ): 0)  ;
	      yDisplacement+= newHeight;
	    }
	  else  // if their are vertical constraints
	    {
	      if(unconstrainedWidgets_ !=  0)  
		{ 
		  // if their are no widgets willing to be strethced they all contribute
		  newHeight = entry->height();
	      yDisplacement += newHeight;
		}
	      else 
		{
		  newHeight = (int)((entry->height() > Ydiff )? (entry->height() - Ydiff ): 0)   ;	   	
		  yDisplacement += newHeight;
		}
	    }
	  entry->resize(newWidth,newHeight);
	}
    }
}



void MSCollapsibleLayout::doHiddenPlacement()
{
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  np = hp;
  MSCollapsibleEntry*   entry;
  int xDisplacement =0;
  int yDisplacement_ =height() - _handleSize;
  while ((np=np->next())!=hp)
    {
      entry = (MSCollapsibleEntry*)np->data();
      if(entry->state()==MSCollapsibleEntry::CLOSED)
	xDisplacement += entry->height();
    }

  float xDifference =  xDisplacement -width();
 
  // if the buttons representing the hidden widgets are widder than this widgets current width
  // shrink the buttons proportionatly to fit 
  float xDiff  =  (xDifference >=0) ? ((float)xDifference) / xDisplacement : 0; 
  
  xDisplacement = 0;
  while ((np=np->next())!=hp)
    {
      entry = (MSCollapsibleEntry*)np->data();
      if(entry->state()==MSCollapsibleEntry::CLOSED){
	entry->moveTo(xDisplacement,yDisplacement_);
	entry->resize((int)(entry->height()*(1-xDiff)),entry->height());
	xDisplacement += (int)(entry->height()*(1-xDiff));
      }
    }
  
}




void MSCollapsibleLayout::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
    {
      if (avList_[i].attribute()=="handleBackground") handleBackground(avList_[i].value()),index<<i;
      else if (avList_[i].attribute()=="handleForeground") handleForeground(avList_[i].value()),index<<i;
   
      else if (avList_[i].attribute()=="handleToolTip")
	{
	  MSStringVector toolTips=avList_[i].options();
	  MSWidgetVector childVector=children();
	  MSString tip;
	  int k;
	  for(k=0;k<childVector.length()&&k<toolTips.length();k++)
	    {
	      tip=toolTips(k);
	      if(!( tip == "" )){ 
		tip.change("\\n",MSString('\n'));
		handleToolTip(childVector(k),tip.string());
	      }
	    }
	}
      
      else if (avList_[i].attribute()=="handleSize")
	handleSize(avList_[i].value().asInt()),index<<i;
      
      
    }
  avList_.remove(index);
}

MSAttrValueList& MSCollapsibleLayout::get(MSAttrValueList& avList_)
{

  avList_<<MSAttrValue("handleBackground",server()->colorName(handleBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("handleForeground",server()->colorName(handleForeground()),MSAttrValue::Color);
  
  MSStringVector handleColor;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  MSCollapsibleEntry * entry;
  

  MSStringVector toolTips;
  hp=childListHead();
  np=hp;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSCollapsibleEntry *)np->data();
     if(entry->displayToolTip() == MSTrue)
       toolTips.append(MSAttrValue::stringVectorToString(entry->toolTip()) );  
     else 
       toolTips.append("" );  
   }
 
  avList_<<MSAttrValue("handleToolTip","",toolTips,MSAttrValue::ChildAttribute);

  
  avList_<<MSAttrValue("handleSize",MSString(handleSize()));

  avList_<<MSAttrValue("close","",MSAttrValue::Callback);
  avList_<<MSAttrValue("revealed","",MSAttrValue::Callback);  

  return MSWidgetCommon::get(avList_);
}




void MSCollapsibleLayout::childMap(MSWidget *widget_)
{
  MSCollapsibleEntry* entry; 
  
  if(internalEvent()==MSFalse){
    entry = getEntry(widget_);
    if(entry !=0 && widget_->mapped()==MSTrue && entry->state() == MSCollapsibleEntry::NOTREADY){
      entry->setEntry(); 
      computeSize();
    }
  }
}




void MSCollapsibleLayout::childUnmap(MSWidget *widget_)
{
  MSCollapsibleEntry* entry; 
  entry = getEntry(widget_);
  
  if(entry != 0 && internalEvent() == MSFalse){
    entry->unsetEntry();
    computeSize();
  }  
}



void MSCollapsibleLayout::childConfigure(MSWidget *widget_)
{
  MSCollapsibleEntry* entry; 
  if(internalEvent()==MSFalse){
    entry = getEntry(widget_);
    if(entry !=0 && widget_->mapped()==MSTrue && entry->state() == MSCollapsibleEntry::NOTREADY){
      entry->setEntry();
      computeSize();
	}
    else
      if(entry !=0 && widget_->mapped()==MSTrue ){
	entry->setEntry();
	computeSize();
      }
  }
}


void MSCollapsibleLayout::naturalSize(){

    MSNodeItem *hp=childListHead();
    MSNodeItem *np=hp;
    internalEvent(MSTrue);
    while ((np=np->next())!=hp)
      {
	((MSCollapsibleEntry*)np->data())->widget()->naturalSize();
      }
    internalEvent(MSFalse);
    naturalSizing = MSTrue;
    computeSize();
    naturalSizing = MSFalse;
}



void MSCollapsibleLayout::childResizeConstraints(MSWidget * widget_)
{
  MSCollapsibleEntry* entry;
  entry = getEntry(widget_);
  if(entry != 0) placement(); 
}




MSCollapsibleEntry *MSCollapsibleLayout::getEntry(MSWidget *widget_) const
{
  MSNodeItem    *hp=(MSNodeItem *)childListHead();
  MSNodeItem    *np=hp;
  MSCollapsibleEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(MSCollapsibleEntry *) np->data();
     if (entry->widget()==widget_) return entry;
   }
  return 0;
}




void MSCollapsibleLayout::permuteWidgets(const MSWidgetVector &aWidgetVector_)
{
  MSNodeItem newList;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  for (unsigned i=0;i<aWidgetVector_.length();i++)
   {
     np=hp;
     while((np=np->next())!=hp)
      {
	MSCollapsibleEntry *entry=(MSCollapsibleEntry *)np->data();
	if (entry->widget()==aWidgetVector_(i))
	 {
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
     MSCollapsibleEntry *entry=(MSCollapsibleEntry *)np->data();
     if(entry->state() == MSCollapsibleEntry::OPEN) entry->close();  
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
  computeSize();
  placement(); 

}


void MSCollapsibleLayout::handleToolTip(MSWidget* widget_, const MSStringVector &toolTip_)
{
  MSCollapsibleEntry* entry = getEntry(widget_);
  if ( entry != 0 && toolTip_.length()!=0)
    {
    entry->toolTip(toolTip_);
    }
}

const MSStringVector & MSCollapsibleLayout::handleToolTip( MSWidget *widget_) const
{
  MSCollapsibleEntry *entry;
  entry = getEntry(widget_);
  if (entry != 0)
    return entry->toolTip();
  return (*(MSStringVector *)server()->toolTipHashTable()->notFound());
}

void MSCollapsibleLayout::close(MSWidget* widget_)
{
  MSCollapsibleEntry* entry = getEntry(widget_);
  if ( entry != 0 )
    {
      entry->close();
      hideConfigure(entry);
    }
}

void MSCollapsibleLayout::open(MSWidget* widget_)
{
  MSCollapsibleEntry* entry = getEntry(widget_);
  if ( entry != 0)
    {
      entry->open();
      hideConfigure(entry);
    }
}

MSBoolean MSCollapsibleLayout::isClosed(MSWidget* widget_) const
{
  MSCollapsibleEntry* entry = getEntry(widget_);
  if ( entry != 0 && entry->state() == MSCollapsibleEntry::CLOSED )
    return MSTrue;
  return MSFalse;
}

MSBoolean MSCollapsibleLayout::isOpened(MSWidget* widget_) const
{
  return (isClosed(widget_) == MSTrue ) ? MSFalse: MSTrue ; 
}


void MSCollapsibleLayout::handleBackground(const char *pString_)
{
  handleBackground(_server->pixel(pString_)); 
}



void MSCollapsibleLayout::handleBackground(unsigned long pixel_)
{ 
  if (pixel_!=handleBackground()) 
   {
     unsigned long old=handleBackground();
     _handleBg=pixel_;
     updateHandleTile();
   }
}


unsigned long MSCollapsibleLayout:: handleBackground() const
{return _handleBg;}

void MSCollapsibleLayout::handleForeground(const char *pString_)
{
  handleForeground(_server->pixel(pString_)); 
}



void MSCollapsibleLayout::handleForeground(unsigned long pixel_)
{ 
  if (pixel_!=handleForeground()) 
   {
     unsigned long old=handleForeground();
     _handleFg=pixel_;
     updateHandleTile();
   }
}


unsigned long MSCollapsibleLayout:: handleForeground() const
{return _handleFg;}


void MSCollapsibleLayout::updateHandleTile()
{
  if(_tile != 0) delete _tile;
  _tile=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,_handleFg,_handleBg);
  
  MSNodeItem *hp=childListHead();
  MSNodeItem *np;
  np = hp;
  MSCollapsibleEntry    *entry;

  while ((np=np->next())!=hp)
   {
     entry = (MSCollapsibleEntry*)np->data();
     entry->updateTile(_tile);
   } 
}

void MSCollapsibleLayout::computeSize()
{
  int width,height;
  int hiddenWidgets,unconstrainedWidgets;
  natural(width,height,hiddenWidgets,unconstrainedWidgets);
  if(height == MSRect::height() && width == MSRect::width())
    placement();
  else
    resize(width,height);
}

void MSCollapsibleLayout::handleSize(int handleSize_) 
{ 
  if( _handleSize != handleSize_ )
    {
      _handleSize = handleSize_;
      placement(); 
    }
}

void MSCollapsibleLayout::entryClosed()
{
  //activateCallback()
}

void MSCollapsibleLayout::entryOpened()
{
  //activateCallback()
}


void MSCollapsibleLayout::unfreeze()
{
  if (firstMap()==MSTrue)
   {
     freezeStatus(MSFalse);
     placement();
   }
}

/////////////////////////////////////////////////////////////////
// class MSCollapsibleEntry
/////////////////////////////////////////////////////////////////
MSCollapsibleEntry::MSCollapsibleEntry( MSWidget* widget_, MSCollapsibleLayout* owner_) :
  _widget(widget_),
  _owner(owner_)
{
  state(NOTREADY);
  _handle= new Handle(_widget->owner(),this);
  _handle->updateTile(_owner->_tile);
}

MSCollapsibleEntry::MSCollapsibleEntry(const MSCollapsibleEntry& entry_): 
  _widget(entry_._widget),
  _state(entry_._state),
  _owner(entry_._owner)
{}


void MSCollapsibleEntry::state(  STATE state_) 
{
  _state=state_;
}

void MSCollapsibleEntry::show()
{ 
  _widget->show();  // show child 
  setEntry();
 }

void MSCollapsibleEntry::setEntry()
{
  if( state() == NOTREADY )
    { 
      _handle->resize(_owner->_handleSize,_widget->height());   
      _handle->show();
      state(OPEN) ;
    }
}


void MSCollapsibleEntry::unsetEntry()
{
  _handle->hide();
  state(NOTREADY) ;
}

void MSCollapsibleEntry::destroy() 
{
  _handle->destroy();
}


int MSCollapsibleEntry::width() const
{ 
  if(state()==OPEN)
    return _widget->width()+_owner->_handleSize; 
  if(state()==CLOSED)
    return _handle->width(); 
  return 0;
}


void MSCollapsibleEntry::resize( int width_, int height_){
   if(state()==OPEN){
     _widget->resize(width_ - (_owner->_handleSize) ,height_);
     _handle->resize(_owner->_handleSize,height_);
   }  
   if(state()==CLOSED){
     _handle->resize(width_,_owner->_handleSize);
   }
}


unsigned long MSCollapsibleEntry::resizeConstraints() const{
  return _widget->resizeConstraints();
}


void MSCollapsibleEntry::changeState() {
  
  if(_state == CLOSED){
    open();
    _owner->hideConfigure(this);
  }
  else{
    close();
    _owner->hideConfigure(this);

  }
}


void MSCollapsibleEntry::close(){
    if(state()!=NOTREADY){
      state(CLOSED);
      _owner->internalEvent(MSTrue);
      _widget->hide();
      _owner->internalEvent(MSFalse);
    }
}

void MSCollapsibleEntry::open(){
  if(state()!=NOTREADY){
    state(OPEN);
    _owner->internalEvent(MSTrue);
    _widget->show();
    _owner->internalEvent(MSFalse);
  } 
}


void MSCollapsibleEntry::height(int height_)  {
  resize(width(),height_);
}


void MSCollapsibleEntry::moveTo( int x_, int y_){
  _handle->moveTo(x_,y_);
  if (state()==OPEN) _widget->moveTo(_owner->_handleSize,y_);  
}
  
void MSCollapsibleEntry::toolTip(const MSStringVector &toolTip_)
{
  _handle->displayToolTip(MSTrue);
  _handle->toolTip(toolTip_);
}


///////////////////////////////////////////////////////////////////
// class MSCollapsibleEntry::Handle
////////////////////////////////////////////////////////////////////


MSCollapsibleEntry::Handle::Handle(MSWidget *parent_, MSCollapsibleEntry* entry_)
  : MSLabel(parent_),_entry(entry_)   
{
   init();
}

MSCollapsibleEntry::Handle::~Handle(void){}

void MSCollapsibleEntry::Handle::init(void)
{
  //Set some attributes to their default values
  shadowStyle(MSRaised);
  _shadowThickness=1;
  _highlightThickness=0;
  selectInput(ExposureMask|ButtonPressMask|Button1MotionMask);
}

void MSCollapsibleEntry::Handle::updateTile(  MSPixmap* tile_)
{
  XSetWindowBackgroundPixmap(display(),window(),tile_->pixmap());
  MSWidget::clear();
  redraw();
}

void MSCollapsibleEntry::Handle::redraw(void)
{
  if (mapped()==MSTrue)
    {
      drawBevel();
    }
}

void MSCollapsibleEntry::Handle::buttonPress(const XEvent *event_)
{
  if (sensitive()==MSTrue)
   {
     if (event_->xbutton.button==Button1)
      {
	_entry->changeState();
      }
   }
}




