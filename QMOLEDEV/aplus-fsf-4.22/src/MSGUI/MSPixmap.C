///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <MSTypes/MSHashTable.H>
#include <MSGUI/MSBitmaps.H>
#include <MSGUI/MSPixmap.H>
#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSMutex.H>

#ifdef MS_WINDOWS
#include <MSX11.H>
#endif

extern void applicationExit(void);

#ifdef MS_MULTI_THREAD
MSMutex MSPixmap::_hashTableMutex;
#endif

static const int PixmapTableSize=64;
static const int PredefinedPixmapWidth=16;
static const int PredefinedPixmapHeight=16;

MSHashTable *MSPixmap::_pPixmapHashTable=0;

//Predefined Pixmaps - found in MSBitmaps.H

const char *MSPixmap::SolidBackgroundPixmap      ="background";
const char *MSPixmap::ForegroundTwentyFivePixmap ="25_foreground";
const char *MSPixmap::ForegroundFiftyPixmap      ="50_foreground";
const char *MSPixmap::ForegroundSeventyFivePixmap="75_foreground";
const char *MSPixmap::VerticalPixmap             ="vertical";
const char *MSPixmap::HorizontalPixmap           ="horizontal";
const char *MSPixmap::SlantRightPixmap           ="slant_right";
const char *MSPixmap::SlantLeftPixmap            ="slant_left";
const char *MSPixmap::MenuCascadePixmap          ="menu_cascade";
const char *MSPixmap::MenuCheckmarkPixmap        ="menu_checkmark";
const char *MSPixmap::MenuDashPixmap             ="menu_dash";
const char *MSPixmap::MagnifyingGlassPixmap      ="mglass_ptr";
const char *MSPixmap::MagnifyingGlassMaskPixmap  ="mglass_ptrMask";
const char *MSPixmap::HourGlassPixmap            ="hglass_ptr";
const char *MSPixmap::HourGlassMaskPixmap        ="hglass_ptrMask";

//#############################################################################
// MSPixmapData
//#############################################################################

class MSPixmapData
{
friend class MSPixmap;
private:
  unsigned         _referenceCount;
  Pixmap           _pixmap;
  Pixmap           _clipMask;
  int              _width;
  int              _height;
  int              _depth;
  unsigned long    _fg;
  unsigned long    _bg;
  MSDisplayServer *_pServer;
  char            *_pName;

  void init(const char *);
public:
  MSPixmapData(void);
  MSPixmapData(MSDisplayServer *,const char *,Pixmap,int,int,int,unsigned long,unsigned long);
  MSPixmapData(MSDisplayServer *,const char *,Pixmap,Pixmap,int,int,int,unsigned long,unsigned long);  
  ~MSPixmapData(void);

  void addReference(void);
  void removeReference(void);
  
  unsigned referenceCount(void) const {return _referenceCount;}
  MSDisplayServer *server(void) const {return _pServer;}  
  const char *name(void)        const {return _pName;}
  int width(void)               const {return _width;}
  int height(void)              const {return _height;}
  int depth(void)               const {return _depth;}
  unsigned long fg(void)        const {return _fg;}
  unsigned long bg(void)        const {return _bg;}
  Pixmap pixmap(void)           const {return _pixmap;}
  Pixmap clipMask(void)         const {return _clipMask;}

};

MSPixmapData::MSPixmapData(void) :
_referenceCount(0),
_pServer(0),
_pName(0),
_height(0),
_width(0),
_depth(0),
_pixmap(0),
_clipMask(0),
_fg(0),
_bg(0)
{}

MSPixmapData::MSPixmapData(MSDisplayServer *pServer_,const char *pName_,
			   Pixmap pixmap_,int width_,int height_,int depth_,
                           unsigned long fg_,unsigned long bg_) :
_referenceCount(0),    
_pServer(pServer_),
_pName(0),
_height(height_),
_width(width_),
_depth(depth_),
_pixmap(pixmap_),
_clipMask(0),
_fg(fg_),
_bg(bg_)
{
  init(pName_);
}

MSPixmapData::MSPixmapData(MSDisplayServer *pServer_,const char *pName_,
			   Pixmap pixmap_,Pixmap clipMask_,int width_,int height_,int depth_,
                           unsigned long fg_,unsigned long bg_) :
_referenceCount(0),    
_pServer(pServer_),
_pName(0),
_height(height_),
_width(width_),
_depth(depth_),
_pixmap(pixmap_),
_clipMask(clipMask_),
_fg(fg_),
_bg(bg_)
{
  init(pName_);
}

void MSPixmapData::init(const char *pName_)
{
  if (pName_!=0)
   {
     unsigned len=strlen(pName_);
     _pName=new char[len+1];
     memcpy(_pName,pName_,len);
     _pName[len]='\0';
   }
}

MSPixmapData::~MSPixmapData(void)
{ if (_pName!=0) delete [] _pName; }

void MSPixmapData::addReference(void)
{ _referenceCount++; }
void MSPixmapData::removeReference(void)
{ if (--_referenceCount==0) delete this; }


//#############################################################################
// MSPixmap 
//#############################################################################

//#############################################################################
// Create a pixmap from array of characters
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,
		   const char *bitmap_,int w_,int h_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"Array_%s_%d_%d_%d_%d",pName_,w_,h_,
	  DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmap_,w_,h_);
}

//#############################################################################
// Create a pixmap from array of characters
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,
		   const char *bitmap_,int w_,int h_,unsigned long fg_,unsigned long bg_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"Array_%s_%d_%d_%d_%d_%d_%d",pName_,w_,h_,
	  fg_,bg_,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmap_,w_,h_,fg_,bg_);
}

//#############################################################################
// Create a pixmap from array of characters
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,const char *bitmap_,int w_,int h_,
		   const char *foreground_,const char *background_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  unsigned long fg=pServer_->pixel(foreground_);
  unsigned long bg=pServer_->pixel(background_);
  sprintf(buf,"Array_%s_%d_%d_%d_%d_%d_%d",pName_,w_,h_,
	  fg,bg,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmap_,w_,h_,fg,bg);
}

//#############################################################################
// Create a pixmap from an X11 Pixmap File
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,const char *bitmapFile_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"File_%s_%s_%d_%d",pName_,bitmapFile_,
	  DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmapFile_);
}

//#############################################################################
// Create a pixmap from an X11 Pixmap File
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,const char *bitmapFile_,unsigned long fg_,unsigned long bg_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"File_%s_%s_%d_%d_%d_%d",pName_,bitmapFile_,
	  fg_,bg_,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmapFile_,fg_,bg_);
}

//#############################################################################
// Create a pixmap from an X11 Pixmap File
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,const char *bitmapFile_,
		   const char *foreground_,const char *background_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  unsigned long fg=pServer_->pixel(foreground_);
  unsigned long bg=pServer_->pixel(background_);
  sprintf(buf,"File_%s_%s_%d_%d_%d_%d",pName_,bitmapFile_,
	  fg,bg,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,bitmapFile_,fg,bg);
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H)
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"Predefined_%s_%d_%d",pName_,1,pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,pName_);
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H) in the specified fg and bg
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,unsigned long fg_,unsigned long bg_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"Predefined_%s_%d_%d_%d_%d",pName_,
	  fg_,bg_,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,pName_,fg_,bg_);
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H) in the specified fg and bg
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,
		   const char *foreground_,const char *background_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  unsigned long fg=pServer_->pixel(foreground_);
  unsigned long bg=pServer_->pixel(background_);
  sprintf(buf,"Predefined_%s_%d_%d_%d_%d",pName_,
	  fg,bg,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,pName_,fg,bg);
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H) in the specified depth
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,
                   unsigned long fg_,unsigned long bg_,int depth_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"Predefined_%s_%d_%d_%d_%d",pName_,fg_,bg_,depth_,pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,pName_,fg_,bg_,depth_);
}  

//#############################################################################
//  Create a pixmap in the specified width, height, fg, and bg
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,
		   int w_,int h_,unsigned long fg_,unsigned long bg_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  sprintf(buf,"General_%s_%d_%d_%d_%d_%d_%d",pName_,w_,h_,
	  fg_,bg_,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,w_,h_,fg_,bg_);
}

//#############################################################################
//  Create a pixmap in the specified width, height, fg, and bg
//#############################################################################

MSPixmap::MSPixmap(MSDisplayServer *pServer_,const char *pName_,int w_,int h_,
		   const char *foreground_,const char *background_)
: _name(pName_)
{
  char buf[255];
  MSGUARD(_hashTableMutex);
  init();
  unsigned long fg=pServer_->pixel(foreground_);
  unsigned long bg=pServer_->pixel(background_);
  sprintf(buf,"General_%s_%d_%d_%d_%d_%d_%d",pName_,w_,h_,
	  fg,bg,DefaultDepthOfScreen(pServer_->screen()),pServer_->display());
  if(copyPixmapDataWithKey(buf)==MSFalse) create(pServer_,buf,w_,h_,fg,bg);
}


// Default constructor is for subclass only
MSPixmap::MSPixmap(void) :
_pData(0)
{ 
  MSGUARD(_hashTableMutex);
  init(); 
}

MSPixmap::MSPixmap(const MSPixmap& aPixmap_)
{
  MSGUARD(_hashTableMutex);
  _name=aPixmap_._name;
  _pData=aPixmap_._pData;
  if(_pData!=0) _pData->addReference();
}

MSPixmap& MSPixmap::operator=(const MSPixmap& aPixmap_)
{
  if (&aPixmap_!=this)
   {
     MSGUARD(_hashTableMutex);
     _name=aPixmap_._name;
     MSPixmapData *pData=_pData;
     _pData=aPixmap_._pData;
     _pData->addReference();
     pData->removeReference();
   }
  return *this;
}

MSPixmap::~MSPixmap(void)
{
  if(_pData!=0)
   {
     MSGUARD(_hashTableMutex);
     if (_pData->referenceCount()==1)
      {
        if (pixmap()!=0) XFreePixmap(server()->display(),pixmap());
        if (clipMask()!=0&&clipMask()!=pixmap()) XFreePixmap(server()->display(),clipMask());
        _pPixmapHashTable->remove(dataName());
      } 
     _pData->removeReference();
     _pData=0;
   }
}

void MSPixmap::addReference(void)
{ _pData->addReference(); }
void MSPixmap::removeReference(void)
{ _pData->removeReference(); }

MSBoolean MSPixmap::copyPixmapDataWithKey(const char *pName_)
{
  void *pData=_pPixmapHashTable->lookup(pName_);
  if ((unsigned long)pData==_pPixmapHashTable->notFound()) return MSFalse;
  _pData=(MSPixmapData*)pData;
  _pData->addReference();
  return MSTrue;
}

void *MSPixmap::pixmapDataWithKey(const char *pName_)
{
  void *pData=_pPixmapHashTable->lookup(pName_);
  if ((unsigned long)pData==_pPixmapHashTable->notFound()) return 0;
  return pData;
}

void MSPixmap::copyPixmapData(void *pVoid_)
{
  _pData=(MSPixmapData *)pVoid_;
  _pData->addReference();
}

void MSPixmap::addToHashTable(const char *pName_,void *pData_)
{ _pPixmapHashTable->add(pName_,pData_);}

void MSPixmap::init(void)
{
  if (_pPixmapHashTable==0)
   {
     _pPixmapHashTable=new MSHashTable(PixmapTableSize);  
     unsigned i=0;
     while (MSBitmapNameSet[i]!=0) 
      {
        _pPixmapHashTable->add(MSBitmapNameSet[i],(void *)MSBitmaps[i]);
	i++;
      } 
   }
}

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,Pixmap pixmap_,Pixmap clipMask_,
		      int w_,int h_,int depth_,unsigned long fg_,unsigned long bg_)
{
  _pData=new MSPixmapData(pServer_,pName_,pixmap_,clipMask_,w_,h_,depth_,fg_,bg_);
  addToHashTable(pName_,(void *)_pData);
  addReference();
}

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,Pixmap pixmap_,
		      int w_,int h_,int depth_,unsigned long fg_,unsigned long bg_)
{
  _pData=new MSPixmapData(pServer_,pName_,pixmap_,w_,h_,depth_,fg_,bg_);
  addToHashTable(pName_,(void *)_pData);
  addReference();
}

//#############################################################################
// Create a pixmap from array of characters
//#############################################################################

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,const char *bitmap_,
		      int w_,int h_)
{
  Pixmap pixmap=XCreateBitmapFromData(pServer_->display(),pServer_->root(),(char*)bitmap_,
                                      w_,h_);
  _pData=new MSPixmapData(pServer_,pName_,pixmap,pixmap,w_,h_,1,0,0);
  addToHashTable(pName_,(void *)_pData);
  addReference();  
}

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,const char *bitmap_,
		      int w_,int h_,unsigned long fg_,unsigned long bg_)
{
  int dep=DefaultDepthOfScreen(pServer_->screen());
  Pixmap p=XCreatePixmapFromBitmapData(pServer_->display(),pServer_->root(),(char*)bitmap_,
				       w_,h_,fg_,bg_,dep);
  _pData=new MSPixmapData(pServer_,pName_,p,w_,h_,dep,fg_,bg_);
  addToHashTable(pName_,(void *)_pData);
  addReference();  
}

//#############################################################################
// Create a pixmap from a Pixmap File
//#############################################################################

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,const char *bitmapFile_)
{
  Display *display=pServer_->display();
  Window window=pServer_->root();
  unsigned w,h;
  Pixmap bitmap;
  if (XReadBitmapFile(display,window,bitmapFile_,&w,&h,&bitmap,0,0)==BitmapSuccess)
   {
     _pData=new MSPixmapData(pServer_,pName_,bitmap,bitmap,w,h,1,0,0);
     addToHashTable(pName_,(void *)_pData);
     addReference();     
   }
  else
   {
     // Can't read bitmap file, create one with background color
     MSMessageLog::warningMessage("MSPixmap warning: Unable to create Pixmap from file `%s'\n",
				  bitmapFile_);
     char buf[255];
     unsigned long fg=pServer_->defaultForeground();
     unsigned long bg=pServer_->defaultBackground();
     int depth=DefaultDepthOfScreen(pServer_->screen());
     sprintf(buf,"Predefined_%s_%d_%d_%d_%d",MSPixmap::SolidBackgroundPixmap,fg,bg,depth,display);
     if(copyPixmapDataWithKey(buf)==MSFalse) 
       create(pServer_,MSPixmap::SolidBackgroundPixmap,fg,bg);
   }
}

//#############################################################################
// Create a pixmap from a Pixmap File
//#############################################################################

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,
		      const char *bitmapFile_,unsigned long fg_,unsigned long bg_)
{
  Display *display=pServer_->display();
  Window window=pServer_->root();
  int depth=DefaultDepthOfScreen(pServer_->screen());
  unsigned w,h;
  Pixmap bitmap;
#ifdef MS_WINDOWS
  int res;
  if (bitmapFile_==0) res=MSXReadBitmapFromResource(display,window,_name,&w,&h,&bitmap);
  else res=MSXReadBitmapFromXbmFile(display,window,bitmapFile_,&w,&h,&bitmap,fg_,bg_,depth);
  if( res==BitmapSuccess)
  {
     _pData=new MSPixmapData(pServer_,pName_,bitmap,w,h,depth,fg_,bg_);

#else
  if (XReadBitmapFile(display,window,bitmapFile_,&w,&h,&bitmap,0,0)==BitmapSuccess)
   {
     Pixmap pixmap=XCreatePixmap(display,window,w,h,depth);
     GC gc=XCreateGC(display,window,0,0);
     XSetForeground(display,gc,fg_);
     XSetBackground(display,gc,bg_);
     XCopyPlane(display,bitmap,pixmap,gc,0,0,w,h,0,0,1);
     XFreeGC(display,gc);
     XFreePixmap(display,bitmap);
     _pData=new MSPixmapData(pServer_,pName_,pixmap,w,h,depth,fg_,bg_);
#endif
     addToHashTable(pName_,(void *)_pData);
     addReference();     
   }
  else
   {
     // Can't read bitmap file, create one with background color
     MSMessageLog::warningMessage("MSPixmap warning: Unable to create Pixmap from file %s\n",bitmapFile_);
     char buf[255];
     sprintf(buf,"Predefined_%s_%d_%d_%d_%d",MSPixmap::SolidBackgroundPixmap,fg_,bg_,depth,display);
     if(copyPixmapDataWithKey(buf)==MSFalse) 
       create(pServer_,MSPixmap::SolidBackgroundPixmap,fg_,bg_);
   }
}

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,
		      int w_,int h_,unsigned long fg_,unsigned long bg_)
{
  int dep=DefaultDepthOfScreen(pServer_->screen());
  Pixmap p=XCreatePixmap(pServer_->display(),pServer_->root(),w_,h_,dep);
  create(pServer_,pName_,p,w_,h_,dep,fg_,bg_);
}

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_)
{
  char buf[255];
  char *bitmap;
  if ((unsigned long)(bitmap=(char *)_pPixmapHashTable->lookup(pName_))==_pPixmapHashTable->notFound())
   {
     MSMessageLog::criticalMessage("MSPixmap error: Unable to create prdefined Pixmap `%s' - unknown name\n",pName_);
     applicationExit();
   }
  else
   {
     int w=PredefinedPixmapWidth;
     int h=PredefinedPixmapHeight; 
     sprintf(buf,"Predefined_%s_%d_%d",pName_,1,pServer_->display());
     Pixmap p=XCreateBitmapFromData(pServer_->display(),pServer_->root(),(char *)bitmap,w,h);
     _pData=new MSPixmapData(pServer_,buf,p,p,w,h,1,0,0);
     addToHashTable(buf,(void *)_pData);
     addReference();
   }
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H) in the specified fg and bg
//#############################################################################

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,unsigned long fg_,unsigned long bg_)
{
  char buf[255];
  char *bitmap;
  if ((unsigned long)(bitmap=(char *)_pPixmapHashTable->lookup(pName_))==_pPixmapHashTable->notFound())
   {
     MSMessageLog::criticalMessage("MSPixmap error: Unable to create prdefined Pixmap `%s' - unknown name\n",pName_);
     applicationExit();
   }
  else
   {
     int w=PredefinedPixmapWidth;
     int h=PredefinedPixmapHeight;
     int dep=DefaultDepthOfScreen(pServer_->screen());
     sprintf(buf,"Predefined_%s_%d_%d_%d_%d",pName_,fg_,bg_,dep,pServer_->display());
     Pixmap p=XCreatePixmapFromBitmapData(pServer_->display(),pServer_->root(),
                                          (char *)bitmap,w,h,fg_,bg_,dep);   
     _pData=new MSPixmapData(pServer_,buf,p,w,h,dep,fg_,bg_);
     addToHashTable(buf,(void *)_pData);
     addReference();
   }
}

//#############################################################################
//  Create a predined pixmap (from bitmap.H) in the specified fg, bg, and depth
//#############################################################################

void MSPixmap::create(MSDisplayServer *pServer_,const char *pName_,unsigned long fg_,unsigned long bg_,int depth_)
{
  char buf[255];
  char *bitmap;
  if ((unsigned long)(bitmap=(char *)_pPixmapHashTable->lookup(pName_))==_pPixmapHashTable->notFound())
   {
     MSMessageLog::criticalMessage("MSPixmap error: Unable to create prdefined Pixmap `%s' - unknown name\n",pName_);
     applicationExit();     
   }
  else
   {
     int w=PredefinedPixmapWidth;
     int h=PredefinedPixmapHeight; 
     sprintf(buf,"Predefined_%s_%d_%d_%d_%d",pName_,fg_,bg_,depth_,pServer_->display());
     Pixmap p=XCreatePixmapFromBitmapData(pServer_->display(),pServer_->root(),(char *)bitmap,
					  w,h,fg_,bg_,depth_);   
     _pData=new MSPixmapData(pServer_,buf,p,w,h,depth_,fg_,bg_);
     addToHashTable(buf,(void *)_pData);
     addReference();
   }
}


MSDisplayServer *MSPixmap::server(void)
{ return _pData->_pServer; }
const MSDisplayServer *MSPixmap::server(void) const
{ return _pData->_pServer; }
int MSPixmap::width(void) const
{ return _pData->_width; }
int MSPixmap::height(void) const
{ return _pData->_height; }
int MSPixmap::depth(void) const
{ return _pData->_depth; }
unsigned long MSPixmap::foreground(void) const
{ return _pData->_fg; }
unsigned long MSPixmap::background(void) const
{ return _pData->_bg; }
Pixmap MSPixmap::pixmap(void) const
{ return _pData->_pixmap; }
Pixmap MSPixmap::clipMask(void) const
{ return _pData->_clipMask; }
const char *MSPixmap::dataName(void) const
{ return _pData->_pName; }
