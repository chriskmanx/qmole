#ifndef MSPrintManagerHEADER
#define MSPrintManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSPointerArray.H>

class MSGraph;
class MSReportTable;
class MSPrintColumn;
class MSPrintItem;

class MSGUIExport MSPrintManager
{
public:
  MSPrintManager(MSPrintManager *);
  MSPrintManager(void);
  ~MSPrintManager(void);

  MSPrintManager *parent(void)                {return _parent;}
  const MSPrintManager *parent(void)    const {return _parent;}
  
  MSPrintManager *topLevel(void);
  const MSPrintManager *topLevel(void) const;

  MSPrintItem& addPageBreak(void);
  MSRulePrintItem& addRule(int width_=0);
  MSParagraph& addParagraph(const MSParagraph&);
  MSParagraph& addParagraph(const MSStringVector&);
  MSPrintColumn& addPrintItem(MSPrintColumn *);
  MSReportTable& addPrintItem(MSReportTable *);
  MSGraph& addPrintItem(MSGraph *);
  MSPrintItem& addPrintItem(MSPrintItem *);
  MSBoolean removeParagraph(const MSSymbol&);
  MSBoolean removePrintItem(const MSSymbol&);
  MSBoolean removePrintItem(MSPrintItem *);
  void removeAllPageBreaks(void);
  void removeAllParagraphs(void);
  void removeAll(void);
  const MSParagraph& paragraph(const MSSymbol&) const;
  MSParagraph& paragraph(const MSSymbol&);
  const MSPrintItem &printItem(const MSSymbol&) const;
  MSPrintItem &printItem(const MSSymbol&);
  const MSParagraph& paragraph(unsigned) const;
  MSParagraph& paragraph(unsigned);
  int paragraphCount(void)                               const {return _paragraphList.count();}
  const MSParagraph& defaultParagraph(void)              const {return _defaultParagraph;}
  MSParagraph& defaultParagraph(void)                          {return _defaultParagraph;}

  friend MSGUIExport MSParagraph& operator<<(MSPrintManager&,const MSParagraph&);
  friend MSGUIExport MSParagraph& operator<<(MSPrintManager&,const MSStringVector&);
  friend MSGUIExport MSPrintColumn& operator<<(MSPrintManager&,MSPrintColumn *);
  friend MSGUIExport MSPrintItem& operator<<(MSPrintManager&,MSPrintItem *);
  friend MSGUIExport MSReportTable& operator<<(MSPrintManager&,MSReportTable *);
  friend MSGUIExport MSGraph& operator<<(MSPrintManager&,MSGraph *);

protected: 
  MSPointerArray<MSPrintManager>& childList(void)              {return _childList;}
  MSPointerArray<MSPrintItem>& paragraphList(void)             {return _paragraphList;}
  MSPointerArray<MSPrintItem>& printItemList(void)             {return _printItemList;}
  MSPrintItem *item(unsigned i_)                               {return _printItemList.array(i_);}
  const MSPointerArray<MSPrintManager>& childList(void)  const {return _childList;}
  const MSPointerArray<MSPrintItem>& paragraphList(void) const {return _paragraphList;}
  const MSPointerArray<MSPrintItem>& printItemList(void) const {return _printItemList;}
  const MSPrintItem *item(unsigned i_)                   const {return _printItemList.array(i_);}

  void parent(MSPrintManager *x_)                              {_parent=x_;}

private:
  MSPrintManager                 *_parent; 
  static MSParagraph              _defaultParagraph;
  MSPointerArray<MSPrintItem>     _paragraphList;
  MSPointerArray<MSPrintItem>     _printItemList;
  MSPointerArray<MSPrintManager>  _childList;
};

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSPrintItem>
#pragma do_not_instantiate MSPointerArray<MSPrintManager>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif

