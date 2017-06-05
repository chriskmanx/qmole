///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSGUI/MSBackingStorePixmap.H>
#include <stdio.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

int MSBackingStorePixmap::_instanceCount=0;
MSHashTable * MSBackingStorePixmap::_pPixmapHashTable=0;
#if defined(MS_MULTI_THREAD)
MSMutex MSBackingStorePixmap::_hashTableMutex;
#endif

static const int PixmapMinimumSize = 200;
static const int PixmapReduceThreshold = 50;

class MSBackingStorePixmapData
{
  friend class MSBackingStorePixmap;
protected:
#ifdef MS_MULTI_THREAD
  MSMutex          _mutex;
#endif
  Pixmap           _pixmap;
  int              _width;
  int              _height;
  MSDisplayServer *_pServer;
  char            *_pName;
  MSIntVector      _widths;
  MSIntVector      _wIds;
  MSIntVector      _heights;
  MSIntVector      _hIds;

  void updatePixmap();
public:
  MSBackingStorePixmapData(void);
  MSBackingStorePixmapData(MSDisplayServer *,const char *);
  ~MSBackingStorePixmapData(void);

  void addReference(int id,int w, int h);
  void removeReference(int id);
  int  referenceCount(void) const;
  
  void resize(int id, int w, int h);

  void lock(void);
  void unlock(void);
  
  MSDisplayServer *server(void) const {return _pServer;}  
  const char *name(void)        const {return _pName;}
  int width(void)               const {return _width;}
  int height(void)              const {return _height;}
  Pixmap pixmap(void)           const {return _pixmap;}
};
  

MSBackingStorePixmapData::MSBackingStorePixmapData(void)
  :_pixmap(0),_pName(0)
{}

MSBackingStorePixmapData::MSBackingStorePixmapData(MSDisplayServer *pServer_,const char *pName_)
    :_pixmap(0),_width(0),_height(0),_pServer(pServer_),_pName(0)
{
  if (pName_!=0)
   {
     unsigned len=strlen(pName_);
     _pName=new char[len+1];
     strncpy(_pName,pName_,len);
     _pName[len]='\0';
   }
}


MSBackingStorePixmapData::~MSBackingStorePixmapData()
{
  if(_pixmap!=0) XFreePixmap(server()->display(),_pixmap);
  if (_pName!=0) delete [] _pName; 
}

void MSBackingStorePixmapData::lock(void)
{
#ifdef MS_MULTI_THREAD
  _mutex.acquire();
#endif
}

void MSBackingStorePixmapData::unlock(void)
{
#ifdef MS_MULTI_THREAD
  _mutex.release();
#endif
}

void MSBackingStorePixmapData::addReference(int id_, int w_, int h_)
{
  int i,len;

  len=_widths.length();
  for(i=0;i<len && _widths(i)<w_;i++) 
    /* nothing */ ;
  _widths.insertAt(i,w_);
  _wIds.insertAt(i,id_);

  len=_heights.length();
  for(i=0;i<len && _heights(i)<h_;i++) 
    /* nothing */ ;
  _heights.insertAt(i,h_);
  _hIds.insertAt(i,id_);

  updatePixmap();
}


void MSBackingStorePixmapData::removeReference(int id_)
{
  if(_wIds.length()==1) delete this;
  else resize(id_,-1,-1);
}  

int MSBackingStorePixmapData::referenceCount(void) const
{
  return _wIds.length();
}

void MSBackingStorePixmapData::resize(int id_, int w_, int h_)
{
  int index1, index2;
  int oldW, oldH;
  index1=_wIds.indexOf(id_);
  index2=_hIds.indexOf(id_);
  if(index1==_wIds.length()||index2==_hIds.length())
    {
      //error
      return;
    }
  oldW=_widths(index1);
  if(oldW!=w_)
   {
     _widths.removeAt(index1);
     _wIds.removeAt(index1);
     if(w_!=-1)
       {
	 int i,len;
	 len=_widths.length();
	 for(i=0;i<len && _widths(i)<w_;i++) 
	/* nothing */ ;
	 _widths.insertAt(i,w_);
	 _wIds.insertAt(i,id_);
       }
   }
  oldH = _heights(index2);
  if(oldH!=h_)
    {
      _heights.removeAt(index2);
      _hIds.removeAt(index2);
      if(h_!=-1)
	{
	  int i,len;
	  len=_heights.length();
	  for(i=0;i<len && _heights(i)<h_;i++) 
	    /* nothing */ ;
	  _heights.insertAt(i,h_);
	  _hIds.insertAt(i,id_);
	}
    }
  if(_widths.length()!=0)
    {
      updatePixmap();
    }
}
  

void MSBackingStorePixmapData::updatePixmap(void)
{
  MSBoolean needReduce = MSFalse;
  int newH, newW;

  newH = _heights.lastElement();
  newW = _widths.lastElement();
  
  if(_width-newW > PixmapReduceThreshold) 
   {
     newW = newW>PixmapMinimumSize?newW:PixmapMinimumSize;
     if(newW!=_width) needReduce=MSTrue;
   }
  if(_height-newH > PixmapReduceThreshold) 
   {
     newH = newH>PixmapMinimumSize?newH:PixmapMinimumSize;
     if(newH!=_height) needReduce=MSTrue;
   }
  
  if(needReduce==MSTrue || newW>_width || newH>_height)
   {
     if(_pixmap!=0) XFreePixmap(server()->display(),_pixmap);
     _pixmap=XCreatePixmap(server()->display(),server()->root(),newW,newH,DefaultDepthOfScreen(server()->screen()));
     _width=newW;
     _height=newH;
   }
}

MSBackingStorePixmap::MSBackingStorePixmap(MSDisplayServer *pServer_,const char *pName_)
  :_pData(0)
{ 
  char buf[255];
  Display *display=pServer_->display();
  _name=pName_;
  _id=_instanceCount++;
  sprintf(buf,"Backing_%s_%d_",pName_,display);
  if(_pPixmapHashTable==0)
   {
     _pPixmapHashTable= new MSHashTable(64);
   }
  void *pData=_pPixmapHashTable->lookup(buf);
  if((unsigned long)pData==_pPixmapHashTable->notFound()) 
    {
      Window window=pServer_->root();
      _pData=new MSBackingStorePixmapData(pServer_,buf);
      _pPixmapHashTable->add(buf,_pData);
    }
  else _pData= (MSBackingStorePixmapData*)pData;
  _pData->addReference(id(),0,0);
}

MSBackingStorePixmap::~MSBackingStorePixmap()
{
  if(_pData->referenceCount()==1) _pPixmapHashTable->remove(_pData->name());
  _pData->removeReference(id());
  _pData=0;
}

void MSBackingStorePixmap::resize(int w_, int h_)
{
  _pData->resize(id(),w_,h_);
}

void MSBackingStorePixmap::lock(void)
{
  _pData->lock();
}

void MSBackingStorePixmap::unlock(void)
{
  _pData->unlock();
}

int MSBackingStorePixmap::width(void) const
{
  return _pData->width();
}

int MSBackingStorePixmap::height(void) const
{
  return _pData->height();
}

Pixmap MSBackingStorePixmap::pixmap(void) const
{
  return _pData->pixmap();
}
