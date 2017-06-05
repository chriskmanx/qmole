///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSMenu.H>
#include <MSGUI/MSDisplayCursor.H>

static const int MSMenuDefaultShadowThickness=2;
static const unsigned long MSMenuEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask|ButtonMotionMask);
static const unsigned int grabMask=(unsigned int)(ButtonPressMask|ButtonReleaseMask|ButtonMotionMask);

MSMenu::MSMenuList MSMenu::_menuList;

MSMenu::MSMenu(MSWidget *owner_)
    : MSWidgetCommon(owner_),
      _parentMenuItem(0)
{ init(); }

MSMenu::MSMenu(MSDisplayServer *server_)
    : MSWidgetCommon(server_) ,
      _parentMenuItem(0)
{
  initFromServer(server_);
  init(); 
}

MSMenu::MSMenu(MSMenuItem * parentMenuItem_)
    : MSWidgetCommon(parentMenuItem_->server()),
     _parentMenuItem(parentMenuItem_)
{
  initFromServer(parentMenuItem_->server());
  init();
}

void MSMenu::initFromServer(MSDisplayServer*)
{
  XSetWindowAttributes attrs;
  attrs.backing_store=NotUseful; 
  attrs.save_under=(int)MSTrue;
  attrs.override_redirect=(int)MSTrue;
  XChangeWindowAttributes(display(),window(),CWSaveUnder|CWBackingStore|CWOverrideRedirect,&attrs);
}

MSMenu::~MSMenu(void)
{
  freeze();
  removeAllItems();
}

void MSMenu::removeAllItems(void)
{
  freeze();
  _selectedItem=-1;
  int i,n;
  MSMenuItem *mi;
  _itemVector<<_hiddenItemVector;
  _hiddenItemVector.removeAll();
  n=itemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     _itemVector.replaceAt(i,0);// this will prevent childDestroy from finding the node
     safeDestroy(mi);
   }
  _itemVector.removeAll();
  unfreeze();
}

void MSMenu::init(void)
{
  freeze();
  _columns=1;
  _selectedItem=-1;
  _highlightThickness=0;
  _shadowThickness=MSMenuDefaultShadowThickness;
  _entryBorder=0;
  _radioBehavior=MSFalse;
  sensitive(MSTrue);
  shadowStyle(MSRaised);
  selectInput(MSMenuEventMask);
  if (server()->menuGrabCursor()==0)
   {
     server()->menuGrabCursor(new MSDisplayCursor(server(),XC_arrow,
			      server()->pixel("black"),server()->pixel("white")));
   }
}

void MSMenu::unmap(void)
{
  if (mapped()==MSTrue)
   {
     menuList().removeFromList(this);
     pulldownMenus();
     MSWidgetCommon::unmap();
     reset();
   }
}

void MSMenu::map(void)
{
  if (mapped()==MSFalse)
   {
     menuList().addToList(this);
     MSWidgetCommon::map();
     XFlush(display());
   }
}


void MSMenu::pulldownMenus(void)
{
  int i,n;
  n=itemVector().length();
  MSMenuItem *mi;
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     if (mi->cascade()==MSTrue) mi->disarm();
   }
}


void MSMenu::columns(unsigned columns_)
{
  if (columns_>0&&_columns!=columns_)
   {
     _columns=columns_;
     computeSize();
   }
}

void MSMenu::radioBehavior(MSBoolean radioBehavior_)  
{ if (radioBehavior()!=radioBehavior_) _radioBehavior=radioBehavior_; }

void MSMenu::enforceRadioBehavior(void)
{
  if (radioBehavior()==MSTrue)
   {
     MSMenuItem *mi;
     MSMenuItem *ami=activeMenuItem();
     int i,n = itemVector().length();
     for (i=0;i<n;i++)
      {
	mi=(MSMenuItem *)itemVector()(i);
        if (mi!=ami) mi->radioDisarm();
      }
   }
}

void MSMenu::updateBackground(unsigned long oldbg_)
{
  MSWidgetCommon::updateBackground(oldbg_);
  int i,n;
  n=itemVector().length();
  MSMenuItem *mi;
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     // Only modify my child's background if his background is same as my old background
     if (mi->background()==oldbg_) mi->background(background());
     else mi->redraw();
   }
  n=hiddenItemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)hiddenItemVector()(i);
     // Only modify my child's background if his background is same as my old background
     if (mi->background()==oldbg_) mi->background(background());
   }
}

void MSMenu::updateForeground(unsigned long oldfg_)
{
  MSWidgetCommon::updateForeground(oldfg_);
  int i,n;
  n=itemVector().length();
  MSMenuItem *mi;
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     // Only modify my child's foreground if his foreground is same as my old foreground
     if (mi->foreground()==oldfg_) mi->foreground(foreground());
   }
  n=hiddenItemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)hiddenItemVector()(i);
     // Only modify my child's foreground if his foreground is same as my old foreground
     if (mi->foreground()==oldfg_) mi->foreground(foreground());
   }
}

void MSMenu::updateFont(Font oldfid_)
{
  MSWidgetCommon::updateFont(oldfid_);
  int i,n;
  n=itemVector().length();
  MSMenuItem *mi;
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     // Only modify my child's font if his font is same as my old font
     if (mi->font()==oldfid_) mi->font(font());
   }
  n=hiddenItemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)hiddenItemVector()(i);
     // Only modify my child's font if his font is same as my old font
     if (mi->font()==oldfid_) mi->font(font());
   }
  unfreeze();
  computeSize();
  redraw();
}

void MSMenu::firstMapNotify(void)
{
  unfreeze();
  computeSize();
}

void MSMenu::redraw(void)
{ 
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     drawBackground();
     drawItems(); 
     drawShadow();
     drawSelectedItem(); 
   }
}

void MSMenu::configure(void)
{ placement(); }

void MSMenu::calculateNaturalSize(int &naturalWidth_,int &naturalHeight_)
{
  freeze();
  naturalWidth_=naturalHeight_=0;
  int index,startIndex=0;
  for (int i=0;i<columns();i++)
   {
     unsigned rows=rowCount(i);
     int j;
     MSMenuItem *mi;
     index=startIndex;
     int maxIndent=0, indent=0;
     for (j=0;j<rows;j++)
      {
	mi=(MSMenuItem *)itemVector()(index);
	if ((indent=mi->computeIndentation())>maxIndent) maxIndent=indent;
        index++;
      }
     index=startIndex;
     for (j=0;j<rows;j++)
      {
	mi=(MSMenuItem *)itemVector()(index);
	mi->indent(maxIndent);
	index++;
      }
     index=startIndex;
     int maxWidth=0, h=0, w=0;
     for (j=0;j<rows;j++)
      {
	mi=(MSMenuItem *)itemVector()(index);
	mi->naturalSize();
	w=mi->width();
	if (w>maxWidth) maxWidth=w;
	h+=mi->height();
        index++;
      }
     naturalWidth_+=maxWidth;
     if (h>naturalHeight_) naturalHeight_=h;
     startIndex=index;
   }
  int offset=(entryBorder()+highlightThickness()+shadowThickness())<<1;
  naturalWidth_+=offset;
  naturalHeight_+=offset;
  unfreeze();
}

void MSMenu::computeSize(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     int naturalWidth, naturalHeight;
     calculateNaturalSize(naturalWidth,naturalHeight);
     if (naturalWidth==width()&&naturalHeight==height()) configure();
     else resize(naturalWidth,naturalHeight);
   }
}

void MSMenu::placement(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     freeze();
     placeMenuItems();
     unfreeze();
   }
}
void MSMenu::placeMenuItems(void)
{
  int index,startIndex=0;
  int offset=entryBorder()+highlightThickness()+shadowThickness();
  int item=0;
  int columnWidth=0;
  MSWidgetVector newItemVector;
  for (int i=0;i<columns();i++)
   {
     int x=offset+columnWidth;
     int y=offset;
     unsigned rows=rowCount(i);
     int j;
     int maxW=0;
     index=startIndex;
     MSMenuItem *mi;
     for (j=0;j<rows;j++)
      {
	mi=(MSMenuItem *)itemVector()(index);
	mi->moveTo(x,y);
	setItem(mi,item++);
	newItemVector<<mi;
	y+=mi->height();
	maxW=(mi->width()>maxW?mi->width():maxW);
	index++;
      }
     index=startIndex;
     for (j=0;j<rows;j++)
      {
	mi=(MSMenuItem *)itemVector()(index);
	mi->width(maxW);
	index++;
      }
     columnWidth+=maxW;
     startIndex=index;
   }
  _itemVector=newItemVector;  //TODO: use indexVector??
}

void MSMenu::drawItems(void)
{
  int i,n;
  n=itemVector().length();
  for(i=0;i<n;i++)
   {
     itemVector()(i)->redraw();
   }
}

void MSMenu::drawItem(int item_,MSBoolean select_)
{
  if (mapped()==MSTrue&&item_>=0)
   {
     MSMenuItem *mi=menuItem(item_);
     if (mi!=0){ (select_==MSTrue)?mi->select():mi->unselect(); }
   }
}

void MSMenu::drawSelectedItem(void)   
{ drawItem(selectedItem(),MSTrue); }
void MSMenu::undrawSelectedItem(void) 
{ drawItem(selectedItem(),MSFalse); }

void MSMenu::accelerator(char aChar_)
{
  MSMenuItem *mi=0;
  char        ch;
  char        chl=tolower(aChar_);
  char        chu=toupper(chl);

  int i,n;
  n=itemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     ch=mi->mnemonic();
     if (ch!=0&&(ch==chl||ch==chu)) break;
     else mi=0;
   }
  if (mi!=0 && mi->sensitive()==MSTrue)
   {
     if (mi->item()!=selectedItem())
      {
        undrawSelectedItem();
        selectedItem(mi->item());
        drawSelectedItem();
      }
     if (mi->cascade()==MSTrue)
      {
        mi->arm();
        mi->grab();
      }
     else mi->activate();
   }
}

MSBoolean MSMenu::loseFocus(void) 
{ 
  done(); 
  return MSTrue; 
}

void MSMenu::motionNotify(const XEvent *event_)
{
  if (event_->xmotion.state)
   {
     MSMenu *menu;
     if ((menu=menuList().findMenu(server(),event_->xbutton.x_root,event_->xbutton.y_root))!=0)
      {
	if (grabber()!=menu) menu->grab(event_->xbutton.time);
	
	MSMenuItem *mi=menu->findItem(event_->xbutton.x_root,event_->xbutton.y_root);
	if (mi!=0)
	 {
	   if (mi->item()!=menu->selectedItem())
	    {	    
	      MSMenuItem *smi=menu->menuItem(menu->selectedItem());
	      if (smi!=0)
	       {
		 smi->disarm();
		 menu->undrawSelectedItem();
	       }
	      menu->selectedItem(mi->item());
	      menu->drawSelectedItem();
	      mi->arm();
	    }
	   else menu->reselect();
	 }
	else menu->selectNone();
      }
     else
      {
	if (grabber()) grabber()->pointerLeave();
      }
   }	 
}


void MSMenu::reset(void)
{
  MSMenuItem *mi=menuItem(selectedItem()); 
  if (mi!=0&&mi->cascade()==MSTrue) mi->disarm();
  undrawSelectedItem();
  selectedItem(-1);
}

void MSMenu::grabAndSelect(unsigned long time_)
{
  if (selectedItem()==-1) 
   {
     MSMenuItem *ni=nextDownItem();
     if (ni!=0)
     selectedItem(ni->item());
   }
  drawSelectedItem();
  grab(time_);
}

void MSMenu::grab(unsigned long time_)
{
  grabber(this);
  server()->grabKeyboard(window(),False,GrabModeAsync,GrabModeAsync,time_);
  server()->grabPointer(window(),False,grabMask,GrabModeAsync,GrabModeAsync,None,
			grabCursor()->cursor(),time_);
  XFlush(display());
}

void MSMenu::ungrab(unsigned long time_)
{
  if (grabber()==this)
   {
     grabber(0);
     reset();
     server()->ungrabPointer(window(),time_);
     server()->ungrabKeyboard(window(),time_);
     XFlush(display());
   }
}

void MSMenu::releaseGrab(void)
{
  if (grabber()==this)
    {
      grabber(0);
      server()->ungrabPointer(window(),CurrentTime);
      server()->ungrabKeyboard(window(),CurrentTime);
      XFlush(display());
    }
}

void MSMenu::buttonPress(const XEvent *event_)
{
  if (event_->xbutton.same_screen&&sensitive()==MSTrue)
   {
     MSMenu *menu;
     if ((menu=menuList().findMenu(server(),event_->xbutton.x_root,event_->xbutton.y_root))!=0)
      {
	if (grabber()==0) menu->grab(event_->xbutton.time);
	else
	 { 
	   if (grabber()!=menu)
	    {
	      grabber()->reset();
	      menu->grab(event_->xbutton.time);
	    }
	 }
	
	MSMenuItem *mi=menu->findItem(event_->xbutton.x_root,event_->xbutton.y_root);
	if (mi!=0)
	 {
	   if (mi->item()!=menu->selectedItem())
	    {	       
	      MSMenuItem *smi=menu->menuItem(menu->selectedItem());
	      if (smi!=0)
	       {
		 smi->disarm();
		 menu->undrawSelectedItem();
	       }
	      menu->selectedItem(mi->item());
	      menu->drawSelectedItem();
	      mi->arm();
	    }
	   else menu->reselect();
	 }
	else
	 {
	   MSMenuItem *smi=menu->menuItem(menu->selectedItem());
	   if (smi!=0)
	    {
	      smi->disarm();
	      menu->undrawSelectedItem();
	    }
	   menu->selectedItem(-1);
	 }
      }
   }
}

void MSMenu::buttonRelease(const XEvent *event_)
{
  MSMenu *menu;
  if (event_->xbutton.same_screen&& 
      (menu=menuList().findMenu(server(),event_->xbutton.x_root,event_->xbutton.y_root))!=0)
   {
     MSMenuItem *mi=menu->findItem(event_->xbutton.x_root,event_->xbutton.y_root);
     if (mi!=0&&mi->item()==menu->selectedItem())
      {
	if (mi->cascade()==MSTrue)
	 {
	   mi->arm();
	   mi->grab();
	 }
	else mi->activate();
      }
     else done();
   }
  else done();
}

MSMenuItem *MSMenu::activeMenuItem(void)
{ return menuItem(selectedItem()); }
const MSMenuItem *MSMenu::activeMenuItem(void) const
{ return menuItem(selectedItem()); }

MSMenuItem *MSMenu::taggedMenuItem(int tag_)
{
  MSMenuItem *mi;
  int i,n;
  n=itemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     if (mi->tag()==tag_) return mi;
   }
  return 0;
}

const MSMenuItem *MSMenu::taggedMenuItem(int tag_) const
{
  MSMenuItem *mi;
  int i,n;
  n=itemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     if (mi->tag()==tag_) return mi;
   }
  return 0;
}

MSMenuItem *MSMenu::menuItem(int item_)
{
  if (item_>=0&&item_<itemCount())
   {
     return (MSMenuItem *)itemVector()(item_);
   }
  return 0;
}

const MSMenuItem *MSMenu::menuItem(int item_) const
{
  if (item_>=0&&item_<itemCount())
   {
     return (MSMenuItem *)itemVector()(item_);
   }
  return 0;
}

MSMenuItem *MSMenu::findItem(int rootX_,int rootY_)
{
  MSMenuItem *mi;  
  int X, Y;
  rootXY (X,Y);
  int xcoord = rootX_-X;
  int ycoord = rootY_-Y;
  int i,n=itemVector().length();
  for(i=0;i<n;i++)
   {
     mi=(MSMenuItem*)itemVector()(i);
     if (xcoord>=mi->x()&&ycoord>=mi->y()&&
	 xcoord<=mi->x()+mi->width()&&ycoord<=mi->y()+mi->height()) 
      { return (mi->sensitive()==MSTrue)?mi:0; }
   }
  return 0;
}

void MSMenu::activate(void)
{
  activateCallback(MSWidgetCallback::activate);
  done(); 
}

void MSMenu::done(void)
{
  releaseGrab();
  unmap();
  reset();
}

void MSMenu::left(void)
{
  MSMenuItem *ni,*mi=menuItem(selectedItem());
  if ((ni=nextLeftItem())!=0&&ni!=mi)
   {
     undrawSelectedItem();
     selectedItem(ni->item());
     drawSelectedItem();
   }  
}

void MSMenu::right(void)
{
  MSMenuItem *ni, *mi=menuItem(selectedItem());
  if (mi!=0&&mi->cascade()==MSTrue) 
   {
     mi->arm();
     mi->grab();
   }
  else if ((ni=nextRightItem())!=0&&ni!=mi)
   {
     undrawSelectedItem();
     selectedItem(ni->item());
     drawSelectedItem();
     if (ni->cascade()==MSTrue)
      {
	ni->arm();
	ni->grab();
      }     
   }

}

void MSMenu::up(void)
{
  if (itemCount()>0)
   {
     MSMenuItem *mi=menuItem(selectedItem());
     MSMenuItem *ni=nextUpItem();
     if (ni!=0&&mi!=ni)
      {
	if (mi!=0&&mi->cascade()==MSTrue) mi->disarm();
	undrawSelectedItem();
	selectedItem(ni->item());
	drawSelectedItem();
      }
   }
}

void MSMenu::down(void)
{
  if (itemCount()>0)
   {
     MSMenuItem *mi=menuItem(selectedItem());
     MSMenuItem *ni=nextDownItem();
     if (ni!=0&&mi!=ni)
      {
	if (mi!=0&&mi->cascade()==MSTrue) mi->disarm();
	undrawSelectedItem();
	selectedItem(ni->item());
	drawSelectedItem();
      }
   }
  
}

void MSMenu::escape(void)
{ done(); }
void MSMenu::returnKey(void)
{
  MSMenuItem *mi=menuItem(selectedItem());
  if (mi!=0)
   {
     if (mi->cascade()==MSTrue)
      {
	mi->arm();
	mi->grab();
      }
     else mi->activate();
   }
}

void MSMenu::keyPress(const XEvent * pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_,state_);
  if (itemCount()>0&&keyTranslate(keyPress)==MSFalse)
   {
     switch(keysym_)
      {
      case XK_Up:     up();        break;
      case XK_Down:   down();      break;
      case XK_Left:   left();      break;
      case XK_Right:  right();     break;
      case XK_Return: returnKey(); break;
      case XK_Escape: escape();    break;
      default:        if (isprint(pString_[0])) accelerator(pString_[0]); break;
      }
   }
}

void MSMenu::childConfigure(MSWidget *)
{ if (firstMap()==MSTrue&&frozen()==MSFalse) computeSize(); }

void MSMenu::childCreate(MSWidget *widget_)
{
  if(widget_!=0)
   {
     MSBoolean found=MSFalse;
     if(_itemVector.indexOf((unsigned long)widget_)!=_itemVector.length()) found=MSTrue;
     else if(_hiddenItemVector.indexOf((unsigned long)widget_)!=_hiddenItemVector.length()) found=MSTrue;
     if (found!=MSTrue)
      {
        itemVector()<<widget_; //fifo
      }
   }
}

// a child has been destroyed-remove it from the item list
void MSMenu::childDestroy(MSWidget *widget_)
{
  unsigned index;

  index=itemVector().indexOf((unsigned long)widget_);
  if(index!=itemVector().length())
   {
     itemVector().removeAt(index);
     computeSize();
   }
  else
   {
     index=hiddenItemVector().indexOf((unsigned long)widget_);
     if(index!=hiddenItemVector().length()) hiddenItemVector().removeAt(index);
   }
}

void MSMenu::childMap(MSWidget *)
{ computeSize(); }

MSMenuItem *MSMenu::nextUpItem(void)
{
  // This nextUpItem logic wraps, i.e. if it reaches the top most item
  // then try from the bottom item again until it either finds one or it returns
  // to the starting item.
  if (itemCount()>0)
   {
     int item=selectedItem();
     int i, start=0, end=0;
     if (item==-1) end=rowCount(0);
     else
      {
	for (i=0;i<columns();i++)
	 {
	   unsigned rows=rowCount(i);
	   end=start+rows;
	   if (item>=start&&item<end) break;
	   else start=end;
	 }
      }     
     MSMenuItem *mi=menuItem(item);
     MSMenuItem *ni=0;
     int next=(item==-1?1:item);
     for (i=start;i<end;i++)
      {
	next--;
	if (next<start) next=end-1;
	ni=menuItem(next);
	if (ni==mi||ni->sensitive()==MSTrue)
	return ni;
      }
   }
  return 0;
}

MSMenuItem *MSMenu::nextDownItem(void)
{
  // This nextDownItem logic wraps, i.e. if it reaches the bottom most item
  // then try from the top item again until it either finds one or it returns
  // to the starting item.
  if (itemCount()>0)
   {
     int item=selectedItem();
     int i, start=0, end=0;
     if (item==-1) end=rowCount(0);
     else
      {
	for (i=0;i<columns();i++)
	 {
	   unsigned rows=rowCount(i);
	   end=start+rows;
	   if (item>=start&&item<end) break;
	   else start=end;
	 }
      }
     MSMenuItem *mi=menuItem(item);
     MSMenuItem *ni=0;
     int next=item;
     for (i=start;i<end;i++)
      {
	next++;
	if (next>=end) next=start;
	ni=menuItem(next);
	if (ni==mi||ni->sensitive()==MSTrue)
	return ni;
      }
   }
  return 0;
}


MSMenuItem *MSMenu::nextRightItem(void)
{
  // This nextRightItem logic wraps, i.e. if it reaches the right most item
  // then try from the first item again until it either finds one or it returns
  // to the starting item.
  if (itemCount()>0)
   {
     int item=selectedItem();
     MSMenuItem *mi=menuItem(item);
     MSIntVector row;
     int index;
     if (item==-1)
      {
	row=rowItems(0);
	index=-1;
      }
     else
      {
	row=rowItems(item);
	index=row.indexOf(item);
      }
     unsigned length=row.length();
     for (unsigned j=0;j<length;j++)
      {
	if (++index==length) index=0;
	MSMenuItem *ni=menuItem(row(index));
	if (mi==ni||(ni!=0&&ni->sensitive()==MSTrue)) return ni;
      }
   }
  return 0;
}

MSMenuItem *MSMenu::nextLeftItem(void)
{
  // This nextLeftItem logic wraps, i.e. if it reaches the left most item
  // then try from the right most item again until it either finds one or it returns
  // to the starting item.
  if (itemCount()>0)
   {
     int item=selectedItem();
     MSMenuItem *mi=menuItem(item);
     MSIntVector row;
     int index;
     if (item==-1)
      {
	row=rowItems(0);
	index=1;
      }
     else
      {
	row=rowItems(item);
	index=row.indexOf(item);
      }
     unsigned length=row.length();
     for (unsigned j=0;j<length;j++)
      {
	if (index==0) index=length-1;
	else index--;
	MSMenuItem *ni=menuItem(row(index));
	if (mi==ni||(ni!=0&&ni->sensitive()==MSTrue)) return ni;
      }
   }
  return 0;
}

void MSMenu::entryBorder(int entryBorder_)
{
  if (_entryBorder!=entryBorder_)
   {
     _entryBorder=entryBorder_;
     computeSize();
     redraw();
   }
}

MSMenu::MSMenuList::~MSMenuList(void) 
{
  MSNodeItem *hp=itemListHead();
  MSNodeItem *np;
  while ((np=hp->next())!=hp) delete np;
}

MSMenu::MSMenuList::MSMenuList(void) 
{
}

void MSMenu::MSMenuList::addToList(MSMenu *menu_) 
{
  MSNodeItem *hp=itemListHead();
  MSNodeItem *np=hp;
  MSBoolean found=MSFalse;
  
  while (found==MSFalse&&(np=np->next())!=hp)
   {
     if ((MSMenu *)np->data()==menu_)
      {
        found=MSTrue;
        np=hp->prev();
      }
   } 
  
  if (menu_!=0&&found!=MSTrue)
   {
     MSNodeItem *np=new MSNodeItem((void *)menu_);
     np->insert(hp->next());
   }
}

void MSMenu::MSMenuList::removeFromList (MSMenu *menu_) 
{
  MSNodeItem *hp=itemListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     if ((MSMenu *)np->data()==menu_)
      {
	delete np;
	break;
      }
   } 
}

MSMenu *MSMenu::MSMenuList::findMenu (MSDisplayServer* server_, int rootX_, int rootY_) 
{
  int X,Y;
  MSNodeItem *hp=itemListHead();
  MSNodeItem *np=hp;
  while ((np=np->next())!=hp)
   {
     MSMenu *menu=(MSMenu *)np->data();
     // must be on the same server. This check is good enough
     // all child menus of menu will definately have the same pointer.
     if(menu->server()==server_)
      {
	menu->rootXY(X,Y);
	if (rootX_>=X&&rootX_<=X+menu->width()&&rootY_>=Y&&rootY_<=Y+menu->height())
	 {
	   return menu;
	 }
      }
   }
  return 0;
}

void MSMenu::pointerLeave(void)
{
  MSMenuItem *smi=menuItem(selectedItem());
  if (smi!=0)
   {
     smi->disarm();
     undrawSelectedItem();
   }
  selectedItem(-1);
}

void MSMenu::reselect(void)
{
  MSMenuItem *smi=menuItem(selectedItem());
  if (smi!=0&&smi->cascade()==MSTrue)
   {
     MSMenu *cas=smi->cascadedMenu();
     if (cas!=0)
      {
	MSMenuItem *scmi=cas->menuItem(cas->selectedItem());
	if (scmi!=0)
	 {
	   scmi->disarm();
	   cas->undrawSelectedItem();
	   cas->selectedItem(-1);
	 }
      }
     smi->arm();
   }
}

void MSMenu::selectNone(void)
{
  MSMenuItem *smi=menuItem(selectedItem());
  if (smi!=0)
   {
     smi->disarm();
     undrawSelectedItem();
   }
  selectedItem(-1);
}

MSWidgetVector MSMenu::children(void)
{
  MSWidgetVector childVector=_itemVector;
  childVector<<_hiddenItemVector;
  return childVector;
}

MSBoolean MSMenu::insideColumn(unsigned column_,int item_)
{
  int count=0;
  for (unsigned i=0;i<columns();i++)
   {
     int rows=rowCount(i);
     if (item_>=count&&item_<count+rows)
      {
	if (i==column_) return MSTrue;
	else return MSFalse;
      }
     count+=rows;
   }
  return MSFalse;
}

MSIntVector MSMenu::rowItems(int item_)
{
  // This method returns a vector of all the items in the row that
  // contains the specified item_. Items are in ascending order.
  
  // First find out which column the current item is in
  int count=0;
  unsigned i;
  for (i=0;i<columns();i++)
   {
     int rows=rowCount(i);
     if (item_>=count&&item_<count+rows) break;
     else count+=rows;
   }
  // Then find out the item on the first column corresponds to this row
  int firstItem=item_;
  unsigned myRow=rowCount(i);
  if (i>0)
   {
     for (unsigned j=i;j>0;j--) firstItem-=rowCount(j-1);
   }
  // Now using the first item on the row, work forward
  MSIntVector vector;
  vector<<firstItem;
  for (i=0;i<columns()-1;i++)
   {
     firstItem+=rowCount(i);
     if (insideColumn(i+1,firstItem)==MSTrue) vector<<firstItem;
     else break;
   }
  return vector;
}

void MSMenu::setItem(MSMenuItem *mi_,int i_)
{
  if (mi_!=0) mi_->item(i_);
}

void MSMenu::permuteMenuItems(const MSIntVector& vector_)
{
  unsigned index,len;
  int i,n=vector_.length();

  freeze();
  _selectedItem=-1;
  
  _itemVector<<_hiddenItemVector;
  _hiddenItemVector=_itemVector;
  _itemVector.removeAll();
       
  for(i=0;i<n;i++)
   {
     len=_hiddenItemVector.length();
     for(index=0;index<len;index++)
      {
        if(((MSMenuItem*)_hiddenItemVector(index))->tag()==vector_(i))
         {
           _itemVector<<_hiddenItemVector(index);
           _hiddenItemVector.removeAt(index);
           break;
         }
      }
   }
  unfreeze();
  computeSize();
  redraw();
}

void MSMenu::permuteMenuItems(const MSWidgetVector& vector_)
{
  unsigned index;
  int i,n=vector_.length();

  freeze();
  _selectedItem=-1;
  
  _itemVector<<_hiddenItemVector;
  _hiddenItemVector=_itemVector;
  _itemVector.removeAll();
       
  for(i=0;i<n;i++)
   {
     index=_hiddenItemVector.indexOf((unsigned long)vector_(i));
     if(index!=_hiddenItemVector.length())
      {
        _itemVector<<_hiddenItemVector(index);
        _hiddenItemVector.removeAt(index);   
      }
   }
  unfreeze();
  computeSize();
  redraw();
}

void MSMenu::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="columns")
         columns(avList_[i].value().asInt()),index<<i;
     else if(avList_[i].attribute()=="radioBehavior")
         radioBehavior(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="entryBorder")
         entryBorder(avList_[i].value().asInt()),index<<i;

   }
  avList_.remove(index);
}

MSAttrValueList& MSMenu::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("columns",MSString(columns()));
  avList_<<MSAttrValue("radioBehavior",
                       radioBehavior()==MSTrue?"MSTrue":"MSFalse",
                       MSStringVector("MSTrue\nMSFalse"));
  avList_<<MSAttrValue("entryBorder",MSString(entryBorder()));
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  return MSWidgetCommon::get(avList_);
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSMenu::focusIn(void) {}
void MSMenu::focusOut(void) {}
