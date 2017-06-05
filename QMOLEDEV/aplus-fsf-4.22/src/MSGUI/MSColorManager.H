#ifndef MSColorManagerHEADER
#define MSColorManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSHashTable.H>
#include <MSGUI/MSDisplayServer.H>

class MSGUIExport MSColorManager
{
private:
  MSDisplayServer    *_server;
  Colormap            _cmap;
  MSHashTable         _colorPixelHashTable;
  MSStringHashTable   _colorStringHashTable;
  MSHashTable         _rgbHashTable;
  unsigned long       _defaultFg;
  unsigned long       _defaultBg;
  MSBoolean           _warned;
  MSBoolean           _bestMatch;
  unsigned short      _redCloseness;
  unsigned short      _greenCloseness;
  unsigned short      _blueCloseness;

  MSBoolean warned(void) const {return _warned;}
  MSHashTable& colorPixelHashTable(void)                    { return _colorPixelHashTable; }
  MSStringHashTable& colorStringHashTable(void)             { return _colorStringHashTable; }
  MSHashTable& rgbHashTable(void)                           { return _rgbHashTable; }
  const MSHashTable& colorPixelHashTable(void)        const { return _colorPixelHashTable; }
  const MSStringHashTable& colorStringHashTable(void) const { return _colorStringHashTable; }
  const MSHashTable& rgbHashTable(void)               const { return _rgbHashTable; }
  MSBoolean matchColor(XColor &color_);
  void bestMatchWarning(const char *);
  void cacheColor(const char *,unsigned long);
  
  void init(void);

public:
  MSColorManager(void);
  MSColorManager(MSDisplayServer&);
  ~MSColorManager(void);
 
  MSDisplayServer *server(void) const { return _server; }
  Colormap colormap(void) const       { return _cmap; }
  Display *display(void) const        { return server()->display(); }
  unsigned long notFound(void)  const { return colorPixelHashTable().notFound(); }
  unsigned long defaultFg(void) const { return _defaultFg; }
  unsigned long defaultBg(void) const { return _defaultBg; }

  unsigned long addColor(const char *color_);
  unsigned long pixel(const char *color_);
  const char *colorName(unsigned long pixel_) const;

  void query(XColor *) const;
  MSBoolean allocate(XColor *);

  void bestMatch(MSBoolean bestMatch_) { _bestMatch=bestMatch_; }
  MSBoolean bestMatch(void) const      { return _bestMatch; }

  void redCloseness(unsigned short closeness_) { _redCloseness=closeness_; }
  unsigned short redCloseness(void) const { return _redCloseness; }

  void greenCloseness(unsigned short closeness_) { _greenCloseness=closeness_; }
  unsigned short greenCloseness(void) const { return _greenCloseness; }

  void blueCloseness(unsigned short closeness_) { _blueCloseness=closeness_; }
  unsigned short blueCloseness(void) const { return _blueCloseness; }
};

#endif


