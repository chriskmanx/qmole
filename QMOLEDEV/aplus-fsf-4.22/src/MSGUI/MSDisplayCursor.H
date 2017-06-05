#ifndef MSDisplayCursorHEADER
#define MSDisplayCursorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <X11/Xlib.h>
#include <MSGUI/MSCursorfont.H>
#include <MSGUI/MSDisplayServer.H>
#include <MSTypes/MSMutex.H>

class MSDisplayCursorData;
class MSHashTable;

class MSGUIExport MSDisplayCursor
{
public:
  MSDisplayCursor(MSDisplayServer *server_,unsigned int shape_,unsigned long fg_,unsigned long bg_);
  MSDisplayCursor(MSDisplayServer *server_,unsigned int shape_,const char *fg_,const char *bg_);  
  MSDisplayCursor(const MSDisplayCursor&);
  ~MSDisplayCursor(void);

  MSDisplayCursor& operator=(const MSDisplayCursor&);
  
  MSDisplayServer *server(void) const;
  Display *display(void) const;
  int width(void) const;
  int height(void) const;
  int depth(void) const;
  unsigned int shape(void) const;
  unsigned long foreground(void) const;
  unsigned long background(void) const;
  Cursor cursor(void) const;
  const char *name(void) const;

private:
  static MSHashTable   *_pCursorHashTable;
  MSDisplayCursorData  *_pData;
#ifdef MS_MULTI_THREAD
  static MSMutex _cursorHashTableMutex;
#endif
  
  void create(MSDisplayServer *,unsigned int shape_,unsigned long fg_,unsigned long bg_);
  void create(MSDisplayServer *,const char *pName_,unsigned int shape_,unsigned long fg_,unsigned long bg_);  

};

#endif
