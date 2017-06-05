#ifndef MSFontManagerHEADER
#define MSFontManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSHashTable.H>
#include <MSGUI/MSDisplayServer.H>

class MSGUIExport MSFontHashTable : public MSHashTable
{
public:
  MSFontHashTable(void);
  MSFontHashTable(int size_);
  ~MSFontHashTable(void);
};

class MSDisplayServer;

class MSGUIExport MSFontManager
{
private:
  MSDisplayServer   *_server;
  MSHashTable        _fontIDHashTable;
  MSFontHashTable    _fontDataHashTable;
  Font               _defaultFontID;

  void init(void);

  MSFontHashTable& fontDataHashTable(void)             {return _fontDataHashTable;}
  MSHashTable& fontIDHashTable(void)                   {return _fontIDHashTable;}
  const MSFontHashTable& fontDataHashTable(void) const {return _fontDataHashTable;}
  const MSHashTable& fontIDHashTable(void)       const {return _fontIDHashTable;}

public:
  MSFontManager(MSDisplayServer& server_);
  ~MSFontManager(void);
  
  void addFont(const char *fontString_);

  MSDisplayServer *server(void) const {return _server;}
  Font defaultFontID(void) const      {return _defaultFontID;}
  Display *display(void) const        {return _server->display();}

  Font fontID(const char *fontString_);
  const char *fontName(Font fid_) const;
  const XFontStruct *fontStruct(Font fid_) const;
};

#endif





