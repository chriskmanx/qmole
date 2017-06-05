#ifndef MSBackingStorePixmapHEADER
#define MSBackingStorePixmapHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidget.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSDefines.H>

class MSBackingStorePixmapData;

class MSGUIExport MSBackingStorePixmap
{
public:
  MSBackingStorePixmap(MSDisplayServer *,const char *pName_);
  ~MSBackingStorePixmap();
  
  void resize(int w_,int h_);

  void lock(void);
  void unlock(void);

  int width() const;
  int height() const;
  Pixmap pixmap(void) const;
  const MSString &name(void) const {return _name;}
  
protected:
#ifdef MS_MULTI_THREAD
  static MSMutex             _hashTableMutex;
#endif
  static int                 _instanceCount;
  static MSHashTable        *_pPixmapHashTable;
  MSBackingStorePixmapData  *_pData;
  int                        _id;
  MSString                   _name;
  
  void init(void);
  int id(void) const { return _id; }
  
};


#endif
