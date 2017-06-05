///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPrintHeaders.H>
#include <MSGUI/MSPrintColumn.H>
#include <MSGUI/MSParagraph.H>
#include <MSTypes/MSMessageLog.H>

MSPrintHeaders::MSPrintHeaders(void)
{}

MSPrintHeaders::~MSPrintHeaders(void)
{
  removeAllHeaders();
  removeAllFooters();
}

////////////////////////////////////////////////////////////////////////////
//
// header/footer lists
//
////////////////////////////////////////////////////////////////////////////
void MSPrintHeaders::removeAllHeaders(void)
{
  unsigned n=headerList().count();
  for (unsigned i=n-1;i<n;i--)
   {
     textList().remove((MSParagraph*)header(i));
     if (header(i)->isDestroyable()==MSTrue) delete header(i);
   }
  headerList().removeAll();
}

void MSPrintHeaders::removeAllFooters(void)
{
  unsigned n=footerList().count();
  for (unsigned i=n-1;i<n;i--)
   {
     textList().remove((MSParagraph*)footer(i));
     if (footer(i)->isDestroyable()==MSTrue) delete footer(i);
   }
  footerList().removeAll();
}

MSBoolean MSPrintHeaders::removeHeader(const MSSymbol& tag_)
{
  MSBoolean status=MSFalse;
  MSPrintItem *printItem=0;
  for (unsigned i=0;i<headerList().count();i++) if (tag_==header(i)->printTag()) printItem=header(i);
  if (printItem!=0)
   {
     textList().remove((MSParagraph*)printItem);
     headerList().remove(printItem);
     if (printItem->isDestroyable()==MSTrue) delete printItem;
     status=MSTrue;
   }
  return status;
}

MSBoolean MSPrintHeaders::removeHeader(MSPrintItem *item_)
{
  MSBoolean ret=MSFalse;
  if ((ret=headerList().remove(item_))==MSTrue&&item_->isDestroyable()==MSTrue) delete item_;
  return ret;
}

MSBoolean MSPrintHeaders::removeFooter(const MSSymbol& tag_)
{
  MSBoolean status=MSFalse;
  MSPrintItem *printItem=0;
  for (unsigned i=0;i<footerList().count();i++) if (tag_==footer(i)->printTag()) printItem=footer(i);
  if (printItem!=0)
   {
     textList().remove((MSParagraph*)printItem);
     footerList().remove(printItem);
     if (printItem->isDestroyable()==MSTrue) delete printItem;
     status=MSTrue;
   }
  return status;
}

MSBoolean MSPrintHeaders::removeFooter(MSPrintItem *item_)
{
  MSBoolean ret=MSFalse;
  if ((ret=footerList().remove(item_))==MSTrue&&item_->isDestroyable()==MSTrue) delete item_;
  return ret;
}

//
// add headers
//
MSPrintColumn& MSPrintHeaders::addHeader(MSPrintColumn* aPrintColumn_)
{
  aPrintColumn_->owner(this);
  aPrintColumn_->pageAlignment(MSNone);
  _headerList.add(aPrintColumn_);
  return *aPrintColumn_;
}
MSParagraph& MSPrintHeaders::addHeader(const MSParagraph& aParagraph_)
{
  MSParagraph *aParagraph=new MSParagraph(aParagraph_);
  aParagraph->pageAlignment(MSNone);
  _textList.add(aParagraph);
  _headerList.add(aParagraph);
  return *aParagraph;
}
MSParagraph& MSPrintHeaders::addHeader(const MSStringVector& aStringVector_)
{return addHeader(MSParagraph(aStringVector_));}
MSParagraph& MSPrintHeaders::addHeader(const MSString& aString_)
{return addHeader(MSStringVector(aString_));}
MSParagraph& MSPrintHeaders::addHeader(const char *string_)
{return addHeader(MSStringVector(string_));}
//
// add footers
//
MSPrintColumn& MSPrintHeaders::addFooter(MSPrintColumn* aPrintColumn_)
{
  aPrintColumn_->owner(this);
  aPrintColumn_->pageAlignment(MSNone);
  _footerList.add(aPrintColumn_);
  return *aPrintColumn_;
}
MSParagraph& MSPrintHeaders::addFooter(const MSParagraph& aParagraph_)
{
  MSParagraph *aParagraph=new MSParagraph(aParagraph_);
  aParagraph->pageAlignment(MSNone);
  _textList.add(aParagraph);
  _footerList.add(aParagraph);
  return *aParagraph;
}
MSParagraph& MSPrintHeaders::addFooter(const MSStringVector& aStringVector_)
{return addFooter(MSParagraph(aStringVector_));}
MSParagraph& MSPrintHeaders::addFooter(const MSString& aString_)
{return addFooter(MSStringVector(aString_));}
MSParagraph& MSPrintHeaders::addFooter(const char *string_)
{return addFooter(MSStringVector(string_));}

const MSParagraph& MSPrintHeaders::header(const MSSymbol& tag_) const
{
  MSBoolean found=MSFalse;
  for (unsigned i=0;i<headerList().count();i++) if (tag_==header(i)->printTag()) found=MSTrue;
  if (found==MSTrue) return headerParagraph(tag_);
  MSMessageLog::warningMessage("Warning: header \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

const MSParagraph& MSPrintHeaders::footer(const MSSymbol& tag_) const
{
  MSBoolean found=MSFalse;
  for (unsigned i=0;i<footerList().count();i++) if (tag_==footer(i)->printTag()) found=MSTrue;
  if (found==MSTrue) return headerParagraph(tag_);
  MSMessageLog::warningMessage("Warning: footer \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

MSParagraph& MSPrintHeaders::header(const MSSymbol& tag_)
{
  MSBoolean found=MSFalse;
  for (unsigned i=0;i<headerList().count();i++) if (tag_==header(i)->printTag()) found=MSTrue;
  if (found==MSTrue) return headerParagraph(tag_);
  MSMessageLog::warningMessage("Warning: header \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

MSParagraph& MSPrintHeaders::footer(const MSSymbol& tag_)
{
  MSBoolean found=MSFalse;
  for (unsigned i=0;i<footerList().count();i++) if (tag_==footer(i)->printTag()) found=MSTrue;
  MSMessageLog::warningMessage("Warning: footer \"%s\" not found\n",tag_.symbolName());
  if (found==MSTrue) return headerParagraph(tag_);
  return defaultParagraph();
}

const MSParagraph& MSPrintHeaders::headerParagraph(const MSSymbol& tag_) const
{
  for (unsigned i=0;i<textList().count();i++) if (tag_==text(i)->tag()) return *text(i);
  MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

MSParagraph& MSPrintHeaders::headerParagraph(const MSSymbol& tag_)
{
  for (unsigned i=0;i<textList().count();i++) if (tag_==text(i)->tag()) return *text(i);
  MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}
