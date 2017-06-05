#ifndef MSPrintHeadersHEADER
#define MSPrintHeadersHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIntVector.H>
#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSPointerArray.H>

class MSPrintColumn;

class MSGUIExport MSPrintHeaders
{
public:
  MSPrintHeaders(void);
  ~MSPrintHeaders(void);

  //
  // MSParagraph lists
  //
  MSBoolean removeHeader(const MSSymbol&);
  MSBoolean removeHeader(MSPrintItem*);
  MSBoolean removeFooter(const MSSymbol&);
  MSBoolean removeFooter(MSPrintItem*);
  void removeAllHeaders(void);
  void removeAllFooters(void);
  const MSParagraph& header(const MSSymbol&) const;
  const MSParagraph& footer(const MSSymbol&) const;
  MSParagraph& header(const MSSymbol&);
  MSParagraph& footer(const MSSymbol&);

  MSParagraph& addHeader(const MSParagraph&);
  MSParagraph& addHeader(const MSStringVector&);
  MSParagraph& addHeader(const MSString&);
  MSParagraph& addHeader(const char *);
  MSPrintColumn& addHeader(MSPrintColumn*);
  
  MSParagraph& addFooter(const MSParagraph&);
  MSParagraph& addFooter(const MSStringVector&);
  MSParagraph& addFooter(const MSString&);
  MSParagraph& addFooter(const char *);
  MSPrintColumn& addFooter(MSPrintColumn*);

  const MSParagraph& defaultParagraph(void)           const {return _defaultParagraph;}
  MSParagraph& defaultParagraph(void)                       {return _defaultParagraph;}

  MSIntVector& headerHeights(void)                          {return _headerHeights;}
  MSIntVector& footerHeights(void)                          {return _footerHeights;}

protected: 
  const MSPointerArray<MSPrintItem>& headerList(void) const {return _headerList;}
  const MSPointerArray<MSPrintItem>& footerList(void) const {return _footerList;}
  const MSPointerArray<MSParagraph>& textList(void)   const {return _textList;}
  const MSPrintItem *header(unsigned i_)              const {return _headerList.array(i_);}
  const MSPrintItem *footer(unsigned i_)              const {return _footerList.array(i_);}
  const MSParagraph *text(unsigned i_)                const {return _textList.array(i_);}

  const MSParagraph& headerParagraph(const MSSymbol&) const;
  MSParagraph& headerParagraph(const MSSymbol&);
  
  MSPointerArray<MSPrintItem>& headerList(void)             {return _headerList;}
  MSPointerArray<MSPrintItem>& footerList(void)             {return _footerList;}
  MSPointerArray<MSParagraph>& textList(void)               {return _textList;}
  MSPrintItem *header(unsigned i_)                          {return _headerList.array(i_);}
  MSPrintItem *footer(unsigned i_)                          {return _footerList.array(i_);}
  MSParagraph *text(unsigned i_)                            {return _textList.array(i_);}

private:
  MSPointerArray<MSPrintItem>  _headerList;
  MSPointerArray<MSPrintItem>  _footerList;
  MSPointerArray<MSParagraph>  _textList;

  MSParagraph        _defaultParagraph;
  MSIntVector        _headerHeights;
  MSIntVector        _footerHeights;
};
  
// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSPrintItem>
#pragma do_not_instantiate MSPointerArray<MSParagraph>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
