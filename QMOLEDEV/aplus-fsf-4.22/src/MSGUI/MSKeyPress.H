#ifndef MSKeyPressHEADER
#define MSKeyPressHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeysymdef.H>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <MSTypes/MSEnum.H>

class MSGUIExport MSKeyPress 
{
protected:
  unsigned int           _state;
  KeySym                 _keysym;
public:
  enum {NoneKeyMask=0,ShiftKeyMask=1,LockKeyMask=2,ControlKeyMask=4,MetaKeyMask=8,
	AltGraphMask=0x10,NumLockMask=0x20,ExactMask=0x40,ALL=0x80};  
  
  MSKeyPress(const char* =0);
  MSKeyPress(KeySym,unsigned int);
  ~MSKeyPress(void);
  
  MSBoolean isMatch(KeySym,unsigned int) const;
  MSBoolean isMatch(const char* ) const;

  KeySym keysym(void)                     const {return _keysym;}
  unsigned int state(void)                const {return _state;}

  static void translate(const char *,KeySym &,
			unsigned int &, unsigned int &) ;

  static MSBoolean isMatch(KeySym,unsigned int, 
			   KeySym,unsigned int, unsigned int);
};

#endif
