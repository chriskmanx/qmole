///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <memory.h>
#include <MSTypes/MSHashTable.H>
#include <MSGUI/MSDisplayCursor.H>

MSHashTable *MSDisplayCursor::_pCursorHashTable=0;
#ifdef MS_MULTI_THREAD
MSMutex MSDisplayCursor::_cursorHashTableMutex;
#endif

class MSDisplayCursorData
{
friend class MSDisplayCursor;
private:
  unsigned         _referenceCount;
  Cursor           _cursor;
  int              _width;
  int              _height;
  int              _depth;
  unsigned long    _fg;
  unsigned long    _bg;
  unsigned int     _shape;
  MSDisplayServer *_pServer;
  char            *_pName;

public:
  MSDisplayCursorData(void);
  MSDisplayCursorData(MSDisplayServer *,const char *,Cursor,int,int,int,
		      unsigned long,unsigned long,unsigned int);
  ~MSDisplayCursorData(void);

  void addReference(void);
  void removeReference(void);
  
  unsigned referenceCount(void) const {return _referenceCount;}
  MSDisplayServer *server(void) const {return _pServer;}  
  const char *name(void)        const {return _pName;}
  Cursor cursor(void)           const {return _cursor;}
  int width(void)               const {return _width;}
  int height(void)              const {return _height;}
  int depth(void)               const {return _depth;}
  unsigned long fg(void)        const {return _fg;}
  unsigned long bg(void)        const {return _bg;}
  unsigned int shape(void)      const {return _shape;}
};

MSDisplayCursorData::MSDisplayCursorData(void) :
_referenceCount(0),    
_pServer(0),
_pName(0),
_height(0),
_width(0),
_shape(0),
_depth(0),
_cursor(0),
_fg(0),
_bg(0)
{
}

MSDisplayCursorData::MSDisplayCursorData(MSDisplayServer *pServer_,const char *pName_,
					 Cursor cursor_,int width_,int height_,
					 int depth_,unsigned long fg_,unsigned long bg_,
					 unsigned int shape_) :
_referenceCount(0),    
_pServer(pServer_),
_pName(0),
_height(height_),
_width(width_),
_shape(shape_),
_depth(depth_),
_cursor(cursor_),
_fg(fg_),
_bg(bg_)
{
  if (pName_!=0)
   {
     unsigned len=strlen(pName_);
     _pName=new char[len+1];
     memcpy(_pName,pName_,len);
     _pName[len]='\0';
   }
}

MSDisplayCursorData::~MSDisplayCursorData(void)
{ if (_pName!=0) delete [] _pName; }

void MSDisplayCursorData::addReference(void)
{ _referenceCount++; }
void MSDisplayCursorData::removeReference(void)
{ if (--_referenceCount==0) delete this; }

MSDisplayCursor::
MSDisplayCursor(MSDisplayServer *pServer_,unsigned int shape_,unsigned long fg_,unsigned long bg_)
{ create(pServer_,shape_,fg_,bg_); }

MSDisplayCursor::
MSDisplayCursor(MSDisplayServer *pServer_,unsigned int shape_,const char *fg_,const char *bg_)
{ create(pServer_,shape_,pServer_->pixel(fg_),pServer_->pixel(bg_)); }

MSDisplayCursor::~MSDisplayCursor(void)
{
  MSGUARD(_cursorHashTableMutex);
  if (_pData->referenceCount()==1)
   {
     XFreeCursor(display(),cursor());
     _pCursorHashTable->remove((char *)name());
   } 
  _pData->removeReference();
  _pData=0;
}

MSDisplayCursor::MSDisplayCursor(const MSDisplayCursor& aDisplayCursor_)
{
  MSGUARD(_cursorHashTableMutex);
  _pData=aDisplayCursor_._pData;
  _pData->addReference();
}

MSDisplayCursor& MSDisplayCursor::operator=(const MSDisplayCursor& aDisplayCursor_)
{
  if (&aDisplayCursor_!=this)
   {
     MSGUARD(_cursorHashTableMutex);
     MSDisplayCursorData *pData=_pData;
     _pData=aDisplayCursor_._pData;
     _pData->addReference();
     pData->removeReference();
   }
  return *this;
}

void MSDisplayCursor::
create(MSDisplayServer *pServer_,unsigned int shape_,unsigned long fg_,unsigned long bg_)
{
  MSGUARD(_cursorHashTableMutex);
  if (_pCursorHashTable==0) _pCursorHashTable=new MSHashTable(64);

  static char buf[128];
  sprintf(buf,"%d_%d_%d_%d",shape_,fg_,bg_,pServer_->display());
  if ((unsigned long)(_pData=(MSDisplayCursorData *)_pCursorHashTable->lookup(buf))==_pCursorHashTable->notFound())
   {
     create(pServer_,buf,shape_,fg_,bg_);
   }  
  _pData->addReference();     
}

void MSDisplayCursor::
create(MSDisplayServer *pServer_,const char *pName_,unsigned int shape_,unsigned long fg_,unsigned long bg_)
{
  Cursor c=XCreateFontCursor(pServer_->display(),shape_);
  _pData=new MSDisplayCursorData(pServer_,pName_,c,16,16,1,fg_,bg_,shape_);
  _pCursorHashTable->add(pName_,(void *)_pData);

  XColor fgColor,bgColor,screenColor;
  
  XLookupColor(pServer_->display(),pServer_->colormap(),pServer_->colorName(fg_),&fgColor,&screenColor);
  XLookupColor(pServer_->display(),pServer_->colormap(),pServer_->colorName(bg_),&bgColor,&screenColor);
  XRecolorCursor(pServer_->display(),cursor(),&fgColor,&bgColor);
}

MSDisplayServer *MSDisplayCursor::server(void) const
{return _pData->_pServer;}

Display *MSDisplayCursor::display(void) const
{return _pData->_pServer->display();}

int MSDisplayCursor::width(void) const
{return _pData->_width;}

int MSDisplayCursor::height(void) const
{return _pData->_height;}

int MSDisplayCursor::depth(void) const
{return _pData->_depth;}

unsigned int MSDisplayCursor::shape(void) const
{return _pData->_shape;}

unsigned long MSDisplayCursor::foreground(void) const
{return _pData->_fg;}

unsigned long MSDisplayCursor::background(void) const
{return _pData->_bg;}

Cursor MSDisplayCursor::cursor(void) const
{return _pData->_cursor;}

const char *MSDisplayCursor::name(void) const
{return _pData->_pName;}







