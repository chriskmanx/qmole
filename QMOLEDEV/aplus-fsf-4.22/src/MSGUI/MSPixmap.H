#ifndef MSPixmapHEADER
#define MSPixmapHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <X11/Xlib.h>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#ifndef MSWidgetHEADER
#include <MSGUI/MSWidget.H>
#endif

#ifdef MS_MULTI_THREAD
#include <MSTypes/MSMutex.H>
#endif

// Predefined Pixmap names defined in MSBitmaps.H

class MSHashTable;
class MSPixmapData;
class MSGUIExport MSPixmap
{
public:
  static const char *SolidBackgroundPixmap;
  static const char *ForegroundTwentyFivePixmap;
  static const char *ForegroundFiftyPixmap;
  static const char *ForegroundSeventyFivePixmap;
  static const char *VerticalPixmap;
  static const char *HorizontalPixmap;
  static const char *SlantRightPixmap;
  static const char *SlantLeftPixmap;
  static const char *MenuCascadePixmap;
  static const char *MenuCheckmarkPixmap;
  static const char *MenuDashPixmap;
  static const char *MagnifyingGlassPixmap;
  static const char *MagnifyingGlassMaskPixmap;
  static const char *HourGlassPixmap;
  static const char *HourGlassMaskPixmap;

public:
  // Construct a pixmap from an array of characters
  MSPixmap(MSDisplayServer*,const char *pName_,const char *bitArray_,int w_,int h_);
  MSPixmap(MSDisplayServer*,const char *pName_,const char *bitArray_,int w_,int h_,
	   unsigned long fg_,unsigned long bg_);
  MSPixmap(MSDisplayServer*,const char *pName_,const char *bitArray_,int w_,int h_,
	   const char *fg_,const char *bg_);

  // Construct a pixmap from an X11 bitmap file
  MSPixmap(MSDisplayServer*,const char *name,const char *bitmapFile_);
  MSPixmap(MSDisplayServer*,const char *name,const char *bitmapFile_,
	   unsigned long fg_,unsigned long bg_);
  MSPixmap(MSDisplayServer*,const char *name,const char *bitmapFile_,
	   const char *fg_,const char *bg_);

  // Construct a predefined pixmap of specified foreground & background
  MSPixmap(MSDisplayServer *,const char *pName_);
  MSPixmap(MSDisplayServer *,const char *pName_,unsigned long fg_,unsigned long bg_);
  MSPixmap(MSDisplayServer *,const char *pName_,const char *fg_,const char *bg_);

  // Construct a predefined pixmap of specified foreground, background, & depth
  MSPixmap(MSDisplayServer *,const char *pName_,unsigned long fg_,unsigned long bg_,int depth_);

  // Construct a pixmap of geometry w by h and foreground & background
  MSPixmap(MSDisplayServer *,const char *pName_,int w_,int h_,unsigned long fg_,unsigned long bg_);
  MSPixmap(MSDisplayServer *,const char *pName_,int w_,int h_,const char *fg_,const char *bg_);

  // Copy Constructor
  MSPixmap(const MSPixmap&);
  ~MSPixmap(void);

  MSPixmap& operator=(const MSPixmap&);

  // Default constructor is public only for use in containers.
  // Should not be used directly.
  MSPixmap(void);  

  MSDisplayServer *server(void);
  const MSDisplayServer *server(void) const;
  int width(void) const;
  int height(void) const;
  int depth(void) const;
  unsigned long foreground(void) const;
  unsigned long background(void) const;
  Pixmap pixmap(void) const;
  Pixmap clipMask(void) const;
  const MSString &name(void) const {return _name;}

protected:
  static MSHashTable *_pPixmapHashTable;
  MSPixmapData       *_pData;
  MSString            _name;
  
  void init(void);

  void create(MSDisplayServer *,const char *pName_,Pixmap pixmap_,
	      int w_,int h_,int depth_,unsigned long fg_,unsigned long bg_);
  void create(MSDisplayServer *,const char *pName_,Pixmap pixmap_,Pixmap clipMask_,
	      int w_,int h_,int depth_,unsigned long fg_,unsigned long bg_);

  void create(MSDisplayServer *,const char *pName_,const char *bitmap_,int w_,int h_);
  void create(MSDisplayServer *,const char *pName_,const char *bitmapFile_);
  void create(MSDisplayServer *,const char *pName_);
  void create(MSDisplayServer *,const char *pName_,int w_,int h_,unsigned long fg_,unsigned long bg_);
  void create(MSDisplayServer *,const char *pName_,unsigned long fg_,unsigned long bg_);
  void create(MSDisplayServer *,const char *pName_,unsigned long fg_,unsigned long bg_,int depth_);
  void create(MSDisplayServer *,const char *pName_,const char *bitmap_,int w_,int h_,unsigned long fg_,unsigned long bg_);
  void create(MSDisplayServer *,const char *pName_,const char *bitmapFile_,unsigned long fg_,unsigned long bg_);

  const char *dataName(void) const;
  
  void addReference(void);
  void removeReference(void);  
  void *pixmapDataWithKey(const char *);
  void copyPixmapData(void *);
  void addToHashTable(const char *,void *);
  MSBoolean copyPixmapDataWithKey(const char *);

#ifdef MS_MULTI_THREAD
protected:
  static MSMutex _hashTableMutex;
#endif

};

// Following functions make it possible to use MSPixmap as an element in MSGenericVector

// compare function needed for MSGenericVector
inline long compare(const MSPixmap&, const MSPixmap&)         { return 0;        }

#if defined(_AIX) && defined(__xlC__)
// xlC will instantiate ostream function from MSGenericVector even if it's not used.
// This requires to provide dummy << operator for element of MSGenericVector.
inline ostream& operator<<(ostream& aStream_,const MSPixmap&) { return aStream_; }
#endif
  
#endif




