///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSNodeList.H>
#include <stdlib.h>

MSNodeItem::~MSNodeItem(void)
{
  remove();
  _next=0;
  _prev=0;
  _data=0;
}

// cuts head circular list before h and inserts new circular list,
// making large circular list.  This operation is semetrical with
// respect to interchange of h and p.
void MSNodeItem::insert(MSNodeItem *h_)
{
  if (h_!=0)
   {
     MSNodeItem *t=this->_prev;	// temporary node     
     this->_prev->_next=h_;
     this->_prev=h_->_prev;
     h_->_prev->_next=this;
     h_->_prev=t;
   }
  return;
}

// remove node from its doubly linked list -- idempotent 
void MSNodeItem::remove(void)
{
  this->_prev->_next=this->_next;
  this->_next->_prev=this->_prev;
  this->_prev=this;
  this->_next=this;
}

// Take Out node And Deposit one position forward (leap-frog) 
void MSNodeItem::toad(void)
{
  MSNodeItem *o=this->_prev;
  MSNodeItem *q=this->_next;
  MSNodeItem *r;
  
  o->_next=q;
  q->_prev=o;
  this->_next=r=q->_next;
  this->_prev=q;
  q->_next=this;
  r->_prev=this;
}

void MSNodeItem::sort(MSNodeItem *hp_,NodeSortFunc func_)
{
  if (hp_!=0)
   {
     register int c;
     register MSNodeItem *np;
     
     for (c=0,np=hp_->next(); np!=hp_; c++,np=np->next());
     if (c!=0)
      {
	register MSNodeItem **ap;
	register MSNodeItem **ai;
	register MSNodeItem **ae;
	
	ap=new MSNodeItem*[c];
	for (ae=(ai=ap)+c; ai!=ae; ai++)
	 {
           np=hp_->next();
	   np->remove();
	   *ai=np;
	 }
	if (func_!=(NodeSortFunc) 0) 
         {
	   qsort((char *)(ap),c,sizeof(*ap),func_);
	   for (ai=ap; ai!=ae; ai++)
	    {
              np=*ai;
	      np->insert(hp_);
              *ap=0;
	    }
	 }
        delete [] ap;
      }
   }
  return;
}

MSNodeList& MSNodeList::append(MSNodeItem *l_)
{
  if (_last)
   {
     _last->_next=l_;
     l_->_prev=_last;
   }
  else _first=l_;
  _last=l_;
  _first->_prev=_last;
  l_->_next=_first;
  return *this;
}

MSNodeList& MSNodeList::prepend(MSNodeItem *l_)
{
  if (_first)
   {
     _first->_prev=l_;
     l_->_next=_first;
   }
  else _last=l_;
  _first=l_;
  _last->_next=_first;
  l_->_prev=_last;
  return *this;
}

MSNodeList& MSNodeList::unlink(MSNodeItem *l_)
{
  if (l_==_first&&l_==_last) 
   {
     _first=0;
     _last=0;
     l_->_next=l_;
     l_->_prev=l_;
   }
  else 
   {
     if (l_==_first) _first=_first->_next;
     if (l_==_last)  _last=_last->_prev;
     if (l_->_next) l_->_next->_prev=l_->_prev;
     if (l_->_prev) l_->_prev->_next=l_->_next;
     l_->_next=l_;
     l_->_prev=l_;
   }
  return *this;
}












