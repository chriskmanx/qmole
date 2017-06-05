#ifndef MSPrintFontDataHEADER
#define MSPrintFontDataHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSGUI/MSRect.H>

class MSGUIExport MSPrintFontData
{
public:
  enum     {MSBadFontID=0};
  MSPrintFontData(void);
  MSPrintFontData(const MSString&);
  MSPrintFontData(const MSString&,const MSString&);
  ~MSPrintFontData(void);
  void init(void);
  void loadFont(void);

  void fontName(const MSString& x_)       {_fontName=x_;}
  void fontPath(const MSString& x_)       {_fontPath=x_;}
  void fontID(int x_)                     {_fontID=x_;}
  void bufsize(int x_)                    {_bufsize=x_;}
  void offset(int x_)                     {_offset=x_;}
  void capHeight(int x_)                  {_capHeight=x_;}
  void xHeight(int x_)                    {_xHeight=x_;}
  void underlineThichness(int x_)         {_underlineThickness=x_;}
  void underlinePosition(int x_)          {_underlinePosition=x_;}
  void ascender(int x_)                   {_ascender=x_;}
  void descender(int x_)                  {_descender=x_;}
  void isFixedPitch(MSBoolean x_)         {_isFixedPitch=x_;}

  int fontID(void)                  const {return _fontID;}
  int bufsize(void)                 const {return _bufsize;}
  int offset(void)                  const {return _offset;}
  int capHeight(void)               const {return _capHeight;}
  int xHeight(void)                 const {return _xHeight;}
  int underlineThichness(void)      const {return _underlineThickness;}
  int underlinePosition(void)       const {return _underlinePosition;}
  int ascender(void)                const {return _ascender;}
  int descender(void)               const {return _descender;}
  MSBoolean isFixedPitch(void)      const {return _isFixedPitch;}
  const MSString& fileName(void)    const {return _fileName;}
  const MSString& fontName(void)    const {return _fontName;}
  const MSString& fontPath(void)    const {return _fontPath;}

  int ascent(unsigned) const;
  int descent(unsigned) const;
  int width(unsigned) const;
  int lbearing(unsigned) const;
  int rbearing(unsigned) const;
  int height(unsigned) const;

  double fontOffset(int);
  double textWidth(int,const char *,int);
  void showFontError(void);
  void showFileError(void);
protected:
  MSRect& fontBox(void) {return _fontBox;}

private:
  enum     {_buflen=128};
  char 	    _pbuf[_buflen];
  MSString  _fontName;
  MSString  _fontPath;
  MSString  _fileName;
  int       _fontID;
  int       _bufsize;
  int       _offset;
  int       _capHeight;
  int       _xHeight;
  int       _underlineThickness;
  int       _underlinePosition;
  int       _ascender; 
  int       _descender; 
  int      *_ascent;
  int      *_descent; 
  int      *_width;
  int      *_lbearing;
  int      *_rbearing;
  MSBoolean _isFixedPitch;
  MSRect    _fontBox;
};

#endif




