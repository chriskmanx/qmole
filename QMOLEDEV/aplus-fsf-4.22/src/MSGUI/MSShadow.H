#ifndef MSShadowHEADER
#define MSShadowHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <X11/Xlib.h>
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSMutex.H>

class MSDisplayServer;
class MSWidget;

class MSGUIExport MSShadow
{
private:
  class MSGUIExport ShadowColors 
  {
  public:
    GC        _tsGC;
    GC        _bsGC;
    GC        _selectGC;
    GC        _bgGC;

    XColor    _bgColor;
    XColor    _fgColor;
    XColor    _selectColor;
    XColor    _tsColor;
    XColor    _bsColor;

    int        _count;

    void increment(void)   { _count++; }
    void decrement(void)   { if (--_count==0) delete this; }
    int  count(void) const { return _count; }

    ShadowColors(void);
    ~ShadowColors(void);
  };

private:
  MSDisplayServer *_server;
  ShadowColors    *_shadowColors;
#ifdef MS_MULTI_THREAD
  static MSMutex   _shadowHashTableMutex;
#endif

  void createGCs(void);
  unsigned long brightness(void);
  void setMonochromeColors(void);
  void calculateRGBColors(void);
  void calculateColorsForLightBackground(void);
  void calculateColorsForMediumBackground(void);
  void calculateColorsForDarkBackground(void);
  void deleteColors(void);

public:
  MSShadow(void);
  MSShadow(MSWidget *);
  MSShadow(MSDisplayServer *);
  MSShadow(const MSShadow&);
  ~MSShadow(void);

  MSShadow& operator=(const MSShadow&);

  void color(unsigned long);
  void displayServer(MSDisplayServer *);
  
  MSBoolean isSet(void)            const {return MSBoolean(_shadowColors!=0);} 
  unsigned long foreground(void)   const {return _shadowColors->_fgColor.pixel;}
  unsigned long background(void)   const {return _shadowColors->_bgColor.pixel;}
  unsigned long select(void)       const {return _shadowColors->_selectColor.pixel;}
  unsigned long topShadow(void)    const {return _shadowColors->_tsColor.pixel;}
  unsigned long bottomShadow(void) const {return _shadowColors->_bsColor.pixel;}
  GC topShadowGC(void)             const {return _shadowColors->_tsGC;}
  GC bottomShadowGC(void)          const {return _shadowColors->_bsGC;}
  GC selectShadowGC(void)          const {return _shadowColors->_selectGC;}
  GC backgroundShadowGC(void)      const {return _shadowColors->_bgGC;}
};

#endif



