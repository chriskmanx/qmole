///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <math.h>
#include <MSTypes/MSTime.H>
#include <AplusGUI/AGIFmstk.H>
#include <AplusGUI/EnumTables.H>
#include <AplusGUI/AplusPrintTool.H>
#include <AplusGUI/AplusPrintText.H>
#include <AplusGUI/AplusParagraph.H>
#include <AplusGUI/AplusPrintColumn.H>
#include <AplusGUI/AplusRulePrintItem.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/AplusCallback.H>
#include <AplusGUI/AplusReportEnum.H>

extern const int MSPointsPerInch;
extern MSWidgetView *validateParent(MSWidgetView *);

////////////////////////////////////////////////////////////////////////////////
// Print Stuff                                                                //
////////////////////////////////////////////////////////////////////////////////

static A q_printDisclaimer(void)      { return (A) GUIEnum.enumSymbols("disclaimer");  }
static A q_printPageOrientation(void) { return (A) GUIEnum.enumSymbols("orientation"); }
static A q_printPageSize(void) 	      { return (A) GUIEnum.enumSymbols("pageSize");    }
static A q_printMode(void)     	      { return (A) GUIEnum.enumSymbols("printMode");   }
static A q_printPaperTray(void)       { return (A) GUIEnum.enumSymbols("paperTray");   }
static A q_printOutputMode(void)      { return (A) GUIEnum.enumSymbols("outputMode");  }

// 
//  XPrint access functions
// 

unsigned long generateOrientation(A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
  {
    MSP::PageOrientation k; 
    char *s = (char *) XS(sym_->p[0])->n;
    if ((k=(MSP::PageOrientation)(long)GUIEnum.printEnumHashTable()->lookup(s))>=0 &&
	(k==MSP::Portrait||k==MSP::Landscape))
    return k;
  }
  return 0;
}

A generateSym(int enumType_, MSHashTable *table_)
{
  A a=gs(Et); 
  *a->p=MS(si((char *)table_->lookup(enumType_)));
  return a; 
}

A generatePageSizeSym(int enumType_)        { return generateSym(enumType_, GUIEnum.pageSizeEnumHashTable()); }
A generatePageOrientationSym(int enumType_) { return generateSym(enumType_, GUIEnum.stringEnumHashTable()); }
A generatePrintModeSym(int enumType_)       { return generateSym(enumType_, GUIEnum.stringEnumHashTable()); }
A generateOutputModeSym(int enumType_)      { return generateSym(enumType_, GUIEnum.stringEnumHashTable()); }
A generateDisclaimerStyleSym(int enumType_) { return generateSym(enumType_, GUIEnum.stringEnumHashTable()); }

  

static void s_printPageOrientation(A style_)
{
  MSP::PageOrientation k;

  k = (MSP::PageOrientation) generateOrientation(style_);
  if (k) MSWidgetOutput::displayPrint()->pageOrientation(k);
}
static A g_printPageOrientation(void) { return generatePageOrientationSym(MSWidgetOutput::displayPrint()->pageOrientation()); }


static void s_printDisOrientation(A style_)
{
  MSP::PageOrientation k;

  k= (MSP::PageOrientation) generateOrientation(style_);
  if (k) MSWidgetOutput::displayPrint()->disclaimer().orientation(k);
}
static A g_printDisOrientation(void)
{ return (A) generatePageOrientationSym(MSWidgetOutput::displayPrint()->disclaimer().orientation()); }


static void s_printPagesize(A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     MSP::PageSize k; 
     char *s = (char *) XS(sym_->p[0])->n;
     if ((k=(MSP::PageSize)(long)GUIEnum.pageSizeStringHashTable()->lookup(s))>=0&&
	 (k>=MSP::Letter&&k<=MSP::B))
      {
	MSWidgetOutput::displayPrint()->pageSize(k);
      }
     else
      {
	cerr << "ã ! ";
	if (s!=0) cerr << s;
	cerr << ": invalid symbol" << endl;
      }
   }
}
static A g_printPagesize(void) { return (A) generatePageSizeSym(MSWidgetOutput::displayPrint()->pageSize()); }

static void s_printDefaultPaperTray(A) {}  /* FIX THIS */
static A g_printDefaultPaperTray(void) { return (A) aplus_nl; } /* FIX THIS */

static void s_printMode(A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     MSP::PrintMode k; 
     char *s = (char *) XS(sym_->p[0])->n;
     if ((k=(MSP::PrintMode)(long)GUIEnum.printEnumHashTable()->lookup(s))>=0&&
	 (k>=MSP::Mono&&k<=MSP::Reverse))
      {
	MSWidgetOutput::displayPrint()->printMode(k);
      }
     else
      {
	cerr << "ã ! ";
	if (s!=0) cerr << s;
	cerr << ": invalid symbol" << endl;
      }
   }
}
static A g_printMode(void) { return (A) generatePrintModeSym(MSWidgetOutput::displayPrint()->printMode()); }

static void s_printOutputMode(A sym_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     MSP::OutputMode k; 
     char *s = (char *) XS(sym_->p[0])->n;
     if ((k=(MSP::OutputMode)(long)GUIEnum.printEnumHashTable()->lookup(s))>=0&&
	 (k>=MSP::PS&&k<=MSP::PPM))
      {
	MSWidgetOutput::displayPrint()->outputMode(k);
      }
     else
      {
	cerr << "ã ! ";
	if (s!=0) cerr << s;
	cerr << ": invalid symbol" << endl;
      }
   }
}
static A g_printOutputMode(void) { return (A) generateOutputModeSym(MSWidgetOutput::displayPrint()->outputMode()); }

static void s_printPageAlign(A x_) {MSWidgetOutput::displayPrint()->pageLayout(GUIEnum.alignFormat(x_));}
A g_printPageAlign(void) {return GUIEnum.alignFormat(MSWidgetOutput::displayPrint()->pageLayout());}

static void s_printDisclaimerStyle(A sym_) 
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     MSP::DisclaimerStyle k; 
     char *s = (char *) XS(sym_->p[0])->n;
     if ((k=(MSP::DisclaimerStyle)(long)GUIEnum.printEnumHashTable()->lookup(s))>=0&&
	 (k>=MSP::Text&&k<=MSP::Box||k==MSP::NoDisclaimer))
      {
	MSWidgetOutput::displayPrint()->disclaimer().style(k); 
      }
     else
      {
	cerr << "ã ! ";
	if (s!=0) cerr << s;
	cerr << ": invalid symbol" << endl;
      }
   }
}
static A g_printDisclaimerStyle(void) 
{ return (A) generateDisclaimerStyleSym(MSWidgetOutput::displayPrint()->disclaimer().style()); }

static void s_printDefaultFont(Font id_) { MSWidgetOutput::displayPrint()->defaultFont(id_); }
static Font g_printDefaultFont(void) { return (Font) MSWidgetOutput::displayPrint()->defaultFont(); }

static void s_printDisclaimerFont(Font id_) 
{
  const char *fn = MSDisplayServer::defaultDisplayServer()->fontName(id_);
  if (fn==0||*fn==0) fn=MSDisplayServer::defaultDisplayServer()->fontName(MSDisplayServer::defaultDisplayServer()->defaultFont());
  MSWidgetOutput::displayPrint()->disclaimer().font(fn);
}
static Font g_printDisclaimerFont(void) 
{
  const char *fn = MSWidgetOutput::displayPrint()->disclaimer().fontName();
  if (fn==0||*fn==0) return MSDisplayServer::defaultDisplayServer()->defaultFont();
  return MSDisplayServer::defaultDisplayServer()->fontID(fn);
}

static void s_printLeftMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->leftMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->leftMargin((double)p.i[0]); } }
static A g_printLeftMargin(void) { A r=gf(MSWidgetOutput::displayPrint()->leftMargin());  return r; }

static void s_printRightMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->rightMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->rightMargin((double)p.i[0]); } }
static A g_printRightMargin(void) { A r=gf(MSWidgetOutput::displayPrint()->rightMargin());  return r; }

static void s_printTopMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->topMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->topMargin((double)p.i[0]); } }
static A g_printTopMargin(void) { A r=gf(MSWidgetOutput::displayPrint()->topMargin());  return r; }

static void s_printBottomMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->bottomMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->bottomMargin((double)p.i[0]); } }
static A g_printBottomMargin(void) { A r=gf(MSWidgetOutput::displayPrint()->bottomMargin());  return r; }

static void s_printMargins(A value_) 
{
  if (!QS(value_) && value_->r==0)
   {
     P p; p.i=value_->p;
     if (value_->t==Ft)
      {
	MSWidgetOutput::displayPrint()->leftMargin((double)p.f[0]); 
	MSWidgetOutput::displayPrint()->rightMargin((double)p.f[0]); 
	MSWidgetOutput::displayPrint()->topMargin((double)p.f[0]); 
	MSWidgetOutput::displayPrint()->bottomMargin((double)p.f[0]); 
      }
     else
      {
	MSWidgetOutput::displayPrint()->leftMargin((double)p.i[0]);
	MSWidgetOutput::displayPrint()->rightMargin((double)p.i[0]);
	MSWidgetOutput::displayPrint()->topMargin((double)p.i[0]);
	MSWidgetOutput::displayPrint()->bottomMargin((double)p.i[0]);
      }
   }
}

static void s_printDisLeftMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->disclaimer().leftMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->disclaimer().leftMargin((double)p.i[0]); } }
static A g_printDisLeftMargin(void) 
{ A r=gf(MSWidgetOutput::displayPrint()->disclaimer().leftMargin());  return r; }

static void s_printDisRightMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->disclaimer().rightMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->disclaimer().rightMargin((double)p.i[0]); } }
static A g_printDisRightMargin(void) 
{ A r=gf(MSWidgetOutput::displayPrint()->disclaimer().rightMargin());  return r; }

static void s_printDisTopMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->disclaimer().topMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->disclaimer().topMargin((double)p.i[0]); } }
static A g_printDisTopMargin(void) 
{ A r=gf(MSWidgetOutput::displayPrint()->disclaimer().topMargin());  return r; }

static void s_printDisBottomMargin(A value_) 
{ if (!QS(value_) && value_->r==0) { P p; p.i=value_->p;
  if (value_->t==Ft) MSWidgetOutput::displayPrint()->disclaimer().bottomMargin(p.f[0]); 
  else MSWidgetOutput::displayPrint()->disclaimer().bottomMargin((double)p.i[0]); } }
static A g_printDisBottomMargin(void) 
{ A r=gf(MSWidgetOutput::displayPrint()->disclaimer().bottomMargin());  return r; }

static void s_printFile(A file_)
{
  if (file_!=0&&file_->n!=0&&file_->t==Ct)
   {
     MSWidgetOutput::displayPrint()->fileName((char *) file_->p);
   }
}
static A g_printFile(void) 
{ return (A) gsv(0,(char *)MSWidgetOutput::displayPrint()->fileName().string()); }

static void s_printDisclaimerFile(A file_) 
{
  if (file_!=0&&file_->n!=0&&file_->t==Ct)
   {
     MSWidgetOutput::displayPrint()->disclaimer().fileName((char *) file_->p);
   }
}
static A g_printDisclaimerFile(void) { return (A)gsv(0,(char*)MSWidgetOutput::displayPrint()->disclaimer().fileName().string()); }

static void s_printDisRuleWidth(int value_) { MSWidgetOutput::displayPrint()->disclaimer().lineWidth(value_); }
static I g_printDisRuleWidth(void) { return (I)MSWidgetOutput::displayPrint()->disclaimer().lineWidth(); }

static void bprint(MSWidget *xwin_) { xwin_->print(); }



////////////////////////////////////////////////////////////////////////////////
// AplusReport Stuff                                                          //
////////////////////////////////////////////////////////////////////////////////

static A q_reportPageOrientation(void)
{
  AplusReportPageOrientationConverter converter;
  return converter.stringDomain();
}

static A q_reportPageSize(void)
{
  AplusReportPageSizeConverter converter;
  return converter.stringDomain();
}

static A q_reportOutputMode(void)
{
  AplusReportOutputModeConverter converter;
  return converter.stringDomain();
}

static A q_reportDisclaimerStyle(void)
{
  AplusReportDisclaimerStyleConverter converter;
  return converter.stringDomain();
}

static A q_reportStyle(void)
{
  AplusReportStyleConverter converter;
  return converter.stringDomain();
}

static A q_reportOccurrence(void)
{
  AplusReportOccurrenceConverter converter;
  return converter.stringDomain();
}

static A q_reportCompMode(void)
{
  AplusReportCompModeConverter converter;
  return converter.stringDomain();
}


static void s_reportPageOrientation(AplusPrintTool *pTool_, A value_)
{
  AplusReportPageOrientationConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      pTool_->pageOrientation((MSP::PageOrientation)style);
    }
}

static A g_reportPageOrientation(AplusPrintTool *pTool_)
{
  AplusReportPageOrientationConverter converter;
  return converter(pTool_->pageOrientation());
}


static void s_reportDisclaimerOrientation(AplusPrintTool *pTool_, A value_)
{
  AplusReportPageOrientationConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      pTool_->disclaimer().orientation((MSP::PageOrientation)style);
    }
}

static A g_reportDisclaimerOrientation(AplusPrintTool *pTool_)
{
  AplusReportPageOrientationConverter converter;
  return converter(pTool_->disclaimer().orientation());
}


static void s_reportPagesize(AplusPrintTool *pTool_, A value_)
{
  AplusReportPageSizeConverter converter;
  unsigned long size = converter(value_);
  if (size!=converter.enumNotFound())
    {
      pTool_->pageSize((MSP::PageSize)size);
    }
}

static A g_reportPagesize(AplusPrintTool *pTool_)
{
  AplusReportPageSizeConverter converter;
  return converter(pTool_->pageSize());
}


static void s_reportOutputMode(AplusPrintTool *pTool_, A value_)
{
  AplusReportOutputModeConverter converter;
  unsigned long mode = converter(value_);
  if (mode!=converter.enumNotFound())
    {
      pTool_->MSReport::outputMode((MSP::OutputMode)mode);
    }
}

static A g_reportOutputMode(AplusPrintTool *pTool_)
{
  AplusReportOutputModeConverter converter;
  return converter(pTool_->MSReport::outputMode());
}

static void s_reportDisclaimerStyle(AplusPrintTool *pTool_, A value_) 
{
  AplusReportDisclaimerStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      pTool_->disclaimer().style((MSP::DisclaimerStyle)style);
    }
}

static A g_reportDisclaimerStyle(AplusPrintTool *pTool_)
{
  AplusReportDisclaimerStyleConverter converter;
  return converter(pTool_->disclaimer().style());
}

static void s_reportLeftMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->leftMargin(p.f[0]); 
	}
      else
	{
	  pTool_->leftMargin((double)p.i[0]);
	}
    }
}
static A g_reportLeftMargin(AplusPrintTool *pTool_) { return gf(pTool_->leftMargin()); }

static void s_reportRightMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->rightMargin(p.f[0]); 
	}
      else
	{
	  pTool_->rightMargin((double)p.i[0]);
	}
    }
}
static A g_reportRightMargin(AplusPrintTool *pTool_) { return gf(pTool_->rightMargin()); }

static void s_reportTopMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->topMargin(p.f[0]); 
	}
      else
	{
	  pTool_->topMargin((double)p.i[0]);
	}
    }
}
static A g_reportTopMargin(AplusPrintTool *pTool_) { return gf(pTool_->topMargin()); }

static void s_reportBottomMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->bottomMargin(p.f[0]); 
	}
      else
	{
	  pTool_->bottomMargin((double)p.i[0]);
	}
    }
}
static A g_reportBottomMargin(AplusPrintTool *pTool_) { return gf(pTool_->bottomMargin()); }

static void s_reportMargins(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->leftMargin((double)p.f[0]); 
	  pTool_->rightMargin((double)p.f[0]); 
	  pTool_->topMargin((double)p.f[0]); 
	  pTool_->bottomMargin((double)p.f[0]); 
	}
      else
	{
	  pTool_->leftMargin((double)p.i[0]);
	  pTool_->rightMargin((double)p.i[0]);
	  pTool_->topMargin((double)p.i[0]);
	  pTool_->bottomMargin((double)p.i[0]);
	}
    }
}

static void s_reportDisclaimerLeftMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->disclaimer().leftMargin(p.f[0]); 
	}
      else
	{
	  pTool_->disclaimer().leftMargin((double)p.i[0]);
	}
    }
}
static A g_reportDisclaimerLeftMargin(AplusPrintTool *pTool_) { return gf(pTool_->disclaimer().leftMargin()); }

static void s_reportDisclaimerRightMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->disclaimer().rightMargin(p.f[0]); 
	}
      else
	{
	  pTool_->disclaimer().rightMargin((double)p.i[0]);
	}
    }
}
static A g_reportDisclaimerRightMargin(AplusPrintTool *pTool_)  { return gf(pTool_->disclaimer().rightMargin()); }

static void s_reportDisclaimerTopMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->disclaimer().topMargin(p.f[0]); 
	}
      else
	{
	  pTool_->disclaimer().topMargin((double)p.i[0]);
	}
    }
}
static A g_reportDisclaimerTopMargin(AplusPrintTool *pTool_)  { return gf(pTool_->disclaimer().topMargin()); }

static void s_reportDisclaimerBottomMargin(AplusPrintTool *pTool_, A value_) 
{
  if (!QS(value_) && value_->r==0)
    {
      P p; p.i=value_->p;
      if (value_->t==Ft)
	{
	  pTool_->disclaimer().bottomMargin(p.f[0]); 
	}
      else
	{
	  pTool_->disclaimer().bottomMargin((double)p.i[0]);
	}
    }
}
static A g_reportDisclaimerBottomMargin(AplusPrintTool *pTool_)  { return gf(pTool_->disclaimer().bottomMargin()); }

static void s_reportFileName(AplusPrintTool *pTool_, A value_)
{
  if (value_!=0&&value_->n!=0&&value_->t==Ct)
    {
      pTool_->fileName((char *)value_->p);
    }
}
static A g_reportFileName(AplusPrintTool *pTool_) { return (A)gsv(0,(char *)pTool_->fileName().string()); }

static void s_reportDisclaimerFile(AplusPrintTool *pTool_, A value_) 
{
  if (value_!=0 && value_->n!=0 && value_->t==Ct)
    {
      pTool_->disclaimer().fileName((char *)value_->p);
    }
}
static A g_reportDisclaimerFile(AplusPrintTool *pTool_) { return (A)gsv(0,(char*)pTool_->disclaimer().fileName().string()); }

static void s_reportDisclaimerRuleWidth(AplusPrintTool *pTool_, int value_) { pTool_->disclaimer().lineWidth(value_); }
static I g_reportDisclaimerRuleWidth(AplusPrintTool *pTool_) { return (I) pTool_->disclaimer().lineWidth(); }

//
// Assume that we get a an A which contains the value and an a vector of PrintText *
//
static A g_reportFooter(AplusPrintTool *pTool_) { return (A)ic(pTool_->footers()); }
static void s_reportFooter(AplusPrintTool *pTool_, A val_)
{
  pTool_->footers(val_);
}

static A g_reportHeader(AplusPrintTool *pTool_) { return (A)ic(pTool_->headers()); }
static void s_reportHeader(AplusPrintTool *pTool_, A val_)
{
  pTool_->headers(val_);
}

static void s_reportBanner(AplusPrintTool *pt_, A value_)  { pt_->banner(value_); }
static A g_reportBanner(AplusPrintTool *pt_)  { return (A)ic(pt_->banner()); }

static void s_reportCancel(AplusPrintTool *pTool_) { pTool_->cancelReport(); }

static void s_reportFooterOffset(AplusPrintTool *pTool_, int value_) { pTool_->footerOffset(value_); }
static I g_reportFooterOffset(AplusPrintTool *pTool_) { return (I) pTool_->footerOffset(); }

static void s_reportPageFrameLineWidth(AplusPrintTool *pTool_, int value_) { pTool_->pageFrameLineWidth(value_); }
static I g_reportPageFrameLineWidth(AplusPrintTool *pTool_) { return (I) pTool_->pageFrameLineWidth(); }

static void s_reportPageFrameOffset(AplusPrintTool *pTool_, int value_) { pTool_->pageFrameOffset(value_); }
static I g_reportPageFrameOffset(AplusPrintTool *pTool_) { return (I) pTool_->pageFrameOffset(); }

static void s_reportPageFrameStyle(AplusPrintTool *pTool_, A value_)
{
  AplusReportStyleConverter converter;
  unsigned long style=converter(value_);
  if (style!=converter.enumNotFound())
    {
      pTool_->pageFrameStyle(style);
    }
}

static A g_reportPageFrameStyle(AplusPrintTool *pTool_)
{
  AplusReportStyleConverter converter;
  return converter(pTool_->pageFrameStyle());
}

static void s_reportHeaderOffset(AplusPrintTool *pTool_, int value_) { pTool_->headerOffset(value_); }
static I g_reportHeaderOffset(AplusPrintTool *pTool_) { return (I) pTool_->headerOffset();}

static I g_reportPageCount(AplusPrintTool *pTool_) { return (I) pTool_->pageCount(); }
static I g_reportPageCountTotal(AplusPrintTool *pTool_) { return (I) pTool_->pageCountTotal(); }

static void s_reportPageNumbers(AplusPrintTool *pTool_, A value_)
{ pTool_->pageNumbers(AplusConvert::asMSUnsignedVector(value_)); }
static A g_reportPageNumbers(AplusPrintTool *pTool_) { return AplusConvert::asA(pTool_->pageNumbers()); }

static void s_reportPageNumbering(AplusPrintTool *pTool_, MSBoolean value_) { pTool_->pageNumbering(value_); }
static I g_reportPageNumbering(AplusPrintTool *pTool_) 
{ return MSTrue==pTool_->pageNumbering() ? 1 : 0; }

static void s_reportPageNumberText(AplusPrintTool *pTool_, A value_)
{ pTool_->pageNumberText() = AplusConvert::asMSStringVector(value_); }
static A g_reportPageNumberText(AplusPrintTool *pTool_) { return AplusConvert::asA(pTool_->pageNumberText()); }

static void s_reportPageNumber(AplusPrintTool *pTool_, A value_) { pTool_->pageNumber(value_); }
static A g_reportPageNumber(AplusPrintTool *pTool_) { return (A)ic(pTool_->pageNumber()); }
  
static void s_reportUniformScaling(AplusPrintTool *pTool_, MSBoolean value_) { pTool_->uniformScaling(value_); }
static I g_reportUniformScaling(AplusPrintTool *pTool_) 
{  return MSTrue==pTool_->uniformScaling() ? 1 : 0; }

static void addReportCB(AplusPrintTool *pTool_, A cbname_, A fc_)   // NEED RTTI
{
  char *cbnamep;
  if (pTool_->widgetType()==AplusPrintTool::symbol())
   {
     if (cbname_->t==Ct) cbnamep=(char *)cbname_->p;
     else if (cbname_->t==Et&&QS(*cbname_->p)) cbnamep=XS(*cbname_->p)->n;

     AClientData *ac=0;
     if (fc_->t==Et&&fc_->n==2)
      {
	I f=fc_->p[0];
	I c=fc_->p[1];
	if (pTool_->model())
	  {
	    ac=new AClientData(pTool_->model()->aplusVar(),(A)f,(A)c);
	  }
	else
	  {
	    ac=new AClientData((V) 0,(A)f,(A)c);
	  }
	pTool_->MSReport::callback(cbnamep,new AplusCallback(ac));
      }
     else
       {
	 pTool_->MSReport::callback(cbnamep, (MSCallback *)0);  // Clear the callback
       }
   }
}

static A getReportCB(AplusPrintTool *pTool_, A cbname_)
{
  A r=aplus_nl;
  char *cbnamep;

  if (cbname_->t==Ct) cbnamep=(char *)cbname_->p;
  else if (cbname_->t==Et&&QS(*cbname_->p)) cbnamep=XS(*cbname_->p)->n;

  AplusCallback *acb=0;
  
  if ((acb=(AplusCallback *)pTool_->MSReport::callback(cbnamep))!=0)
   {
     AClientData *ac=acb->ac();
     if (ac!=0)
      {
	r=gv(Et,2);
	r->p[0]=ic(ac->function());
	r->p[1]=ic(ac->data());
      }
   }
  return (A)r;
}



static void s_reportPrint(AplusPrintTool *pt_)
{
  pt_->constructReport();
  pt_->MSPrintTool::print();	// have to qualify the print() method explicitly; otherwise, it's ambiguous
}


static void s_reportPageNumberOffset(AplusPrintTool *pTool_, int value_) { pTool_->pageNumberOffset(value_); }
static I g_reportPageNumberOffset(AplusPrintTool *pTool_) { return (I) pTool_->pageNumberOffset(); }

static I  g_reportFontSize(AplusPrintTool *pTool_) { return (I) pTool_->fontSize(); }

static void s_reportPrintMode(AplusPrintTool *pTool_, A value_)
{
  AplusReportPrintModeConverter converter;
  unsigned long mode=converter(value_);
  if (mode!=converter.enumNotFound())
    {
      MSWidgetOutput::displayPrint()->printMode((MSP::PrintMode)mode);
    }
}

static A g_reportPrintMode(AplusPrintTool *pTool_)
{
  AplusReportPrintModeConverter converter;
  return converter(MSWidgetOutput::displayPrint()->printMode());
}

/////////////////////////////////
// PrintText Class Attributes	//
//				//
// text               Strings	//
// fontName           String	//
// foreground         String	//
// background         String	//
// justification		//
// style			//
// fontsize           unsigned	//
// leading            unsigned	//
// topPixel           unsigned	//
// bottomPixel        unsigned	//
// fontScale          double	//
// fgGrayScale        double	//
// bgGrayScale        double	//
// xPixel             unsigned	//
// yPixel             unsigned	//
// lineWidth          unsigned	//
// row                unsigned	//
// column             unsigned	//
// firstColumn        unsigned	//
// mode				//
// xOrigin            double	//
// yOrigin            double	//
// topOffset          double	//
// bottomOffset       double	//
//				//
//////////////////////////////////

void s_printtextText(MSPrintText *printText_, A x_) { printText_->text(AplusConvert::asMSStringVector(x_)); }

A g_printtextText(MSPrintText *printText_) { return AplusConvert::asA(printText_->text()); }

void s_printtextFont(MSPrintText *printText_, A sym_)
{
  MSString f = AplusConvert::asMSString(sym_);
  if (f.length()>0)
    printText_->font(f);
}

A g_printtextFont(MSPrintText *printText_) { return AplusConvert::asA(printText_->fontName()); }


void s_printtextForeground(MSPrintText *printText_, A x_)
{
  MSString f = AplusConvert::asMSString(x_);
  if (f.length()>0)
    printText_->foreground(f);
}
A g_printtextForeground(MSPrintText *printText_) { return AplusConvert::asA(printText_->foreground()); }


void s_printtextBackground(MSPrintText *printText_, A x_)
{
  MSString b = AplusConvert::asMSString(x_);
  if (b.length()>0)
    printText_->background(b);
}
A g_printtextBackground(MSPrintText *printText_) { return AplusConvert::asA(printText_->background()); }


void s_printtextJustification(MSPrintText *printText_, A sym_) { printText_->justification(GUIEnum.alignFormat(sym_)); }
A g_printtextJustification(MSPrintText *printText_) { return GUIEnum.alignFormat(printText_->justification()); }

void s_printtextStyle(MSPrintText *printText_, A x_) { printText_->style(GUIEnum.formatStyle(x_)); }
A g_printtextStyle(MSPrintText *printText_) { return GUIEnum.formatStyle(printText_->style()); }

void s_printtextMode(MSPrintText *printText_, A sym_) { printText_->occurrence(AplusPrintText::convertMode(sym_)); }
A g_printtextMode(MSPrintText *printText_) { return AplusPrintText::convertMode(printText_->occurrence()); }

void s_printtextFontSize(MSPrintText *printText_, unsigned x_) { printText_->fontSize(x_); }
unsigned g_printtextFontSize(MSPrintText *printText_) { return printText_->fontSize(); }

void s_printtextLeading(MSPrintText *printText_, unsigned x_) { printText_->leading(x_); }
unsigned g_printtextLeading(MSPrintText *printText_) { return printText_->leading(); }

void s_printtextTopPixel(MSPrintText *printText_, unsigned x_) { printText_->topPixel(x_); }
unsigned long g_printtextTopPixel(MSPrintText *printText_) { return printText_->topPixel(); }

void s_printtextBottomPixel(MSPrintText *printText_, unsigned x_) { printText_->bottomPixel(x_); }
unsigned g_printtextBottomPixel(MSPrintText *printText_) { return printText_->bottomPixel(); }

void s_printtextFontScale(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->fontScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextFontScale(MSPrintText *printText_) { return (A) gf(printText_->fontScale()); }

void s_printtextFgGrayScale(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->fgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextFgGrayScale(MSPrintText *printText_) { return (A) gf(printText_->fgGrayScale()); }

void s_printtextBgGrayScale(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->bgGrayScale(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextBgGrayScale(MSPrintText *printText_) { return (A) gf(printText_->bgGrayScale()); }

void s_printtextLineWidth(MSPrintText *printText_, unsigned x_) { printText_->lineWidth(x_); }
unsigned g_printtextLineWidth(MSPrintText *printText_) { return printText_->lineWidth(); }

void s_printtextRow(MSPrintText *printText_, unsigned x_) { printText_->row(x_); }
unsigned g_printtextRow(MSPrintText *printText_) { return printText_->row(); }

void s_printtextColumn(MSPrintText *printText_, unsigned x_) { printText_->column(x_); }
unsigned g_printtextColumn(MSPrintText *printText_) { return printText_->column(); }

void s_printtextFirstColumn(MSPrintText *printText_, unsigned x_) { printText_->column(x_); }
unsigned g_printtextFirstColumn(MSPrintText *printText_) { return printText_->column(); }

void s_printtextxOrigin(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->xOrigin(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextxOrigin(MSPrintText *printText_) { return (A) gf(printText_->xOrigin()); }

void s_printtextyOrigin(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->yOrigin(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextyOrigin(MSPrintText *printText_) { return (A) gf(printText_->yOrigin()); }

void s_printtextTopOffset(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->topOffset(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextTopOffset(MSPrintText *printText_) { return (A) gf(printText_->topOffset()); }

void s_printtextBottomOffset(MSPrintText *printText_, A value_)
{
  if(!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p; p.i=value_->p; 
     printText_->bottomOffset(value_->t==Ft?p.f[0]:(double)p.i[0]);
   }
}
A g_printtextBottomOffset(MSPrintText *printText_) { return (A) gf(printText_->bottomOffset()); }


//************************* ReportParagraph attributes *******************************

static void s_reportParagraphLeftMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->leftMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->leftMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphLeftMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusParagraph *)pWidget_)->leftMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportParagraphRightMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->rightMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->rightMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphRightMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusParagraph *)pWidget_)->rightMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportParagraphTopOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->topOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->topOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphTopOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusParagraph *)pWidget_)->topPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportParagraphBottomOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->bottomOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->bottomOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphBottomOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusParagraph *)pWidget_)->bottomPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportParagraphReportFont(MSWidgetView *pWidget_, A value_)
{ ((AplusParagraph *)pWidget_)->printFont(AplusConvert::asMSString(value_)); }
static A g_reportParagraphReportFont(MSWidgetView *pWidget_)
{ return AplusConvert::asA(((AplusParagraph *)pWidget_)->printFont()); }

static void s_reportParagraphPrintRow(MSWidgetView *pWidget_, int value_)
{ ((MSPrintItem *)(AplusParagraph *)pWidget_)->printRow(value_); }
static I  g_reportParagraphPrintRow(MSWidgetView *pWidget_)
{ return ((MSPrintItem *)(AplusParagraph *)pWidget_)->printRow(); }

static void s_reportParagraphPrintColumn(MSWidgetView *pWidget_, int value_)
{ ((MSPrintItem *)(AplusParagraph *)pWidget_)->printColumn(value_); }
static I  g_reportParagraphPrintColumn(MSWidgetView *pWidget_)
{ return ((MSPrintItem *)(AplusParagraph *)pWidget_)->printColumn(); }

static void s_reportParagraphJustify(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long justify = converter(value_);
  if (justify!=converter.enumNotFound())
    {
      ((AplusParagraph *)pWidget_)->justification(justify);
    }
}

static A g_reportParagraphJustify(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusParagraph *)pWidget_)->justification());
}  

static void s_reportParagraphPageAlign(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long align = converter(value_);
  if (align!=converter.enumNotFound())
    {
      ((AplusParagraph *)pWidget_)->pageAlignment(align);
    }
}  

static A g_reportParagraphPageAlign(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusParagraph *)pWidget_)->pageAlignment());
}


static void s_reportParagraphOccurrence(MSWidgetView *pWidget_, A value_)
{
  AplusReportOccurrenceConverter converter;
  unsigned long occur = converter(value_);
  if (occur!=converter.enumNotFound())
    {
      ((AplusParagraph *)pWidget_)->occurrence(occur);
    }
}

static A g_reportParagraphOccurrence(MSWidgetView *pWidget_)
{
  AplusReportOccurrenceConverter converter;
  return converter(((AplusParagraph *)pWidget_)->occurrence());
}


static void s_reportParagraphFirstLineIndent(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->firstLineIndent(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->firstLineIndent((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphFirstLineIndent(MSWidgetView *pWidget_)
{ return gf(((AplusParagraph *)pWidget_)->firstLineIndent()); }

static void s_reportParagraphLeading(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusParagraph *)pWidget_)->leading(value_); }
static unsigned g_reportParagraphLeading(MSWidgetView *pWidget_) { return ((AplusParagraph *)pWidget_)->leading(); }

static void s_reportParagraphLineWidth(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusParagraph *)pWidget_)->lineWidth(value_); }
static unsigned g_reportParagraphLineWidth(MSWidgetView *pWidget_) { return ((AplusParagraph *)pWidget_)->lineWidth(); }

static void s_reportParagraphFgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->fgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->fgGrayScale((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphFgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusParagraph *)pWidget_)->fgGrayScale()); }

static void s_reportParagraphBgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  paragraph->bgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  paragraph->bgGrayScale((double)*(I*)value_->p);
	}
    }
}

static A g_reportParagraphBgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusParagraph *)pWidget_)->bgGrayScale()); }

static void s_reportParagraphOrphanRows(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusParagraph *)pWidget_)->orphanRows(value_); }
static unsigned g_reportParagraphOrphanRows(MSWidgetView *pWidget_)
{ return ((AplusParagraph *)pWidget_)->orphanRows(); }

static void s_reportParagraphWidowRows(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusParagraph *)pWidget_)->widowRows(value_); }
static unsigned g_reportParagraphWidowRows(MSWidgetView *pWidget_)
{ return ((AplusParagraph *)pWidget_)->widowRows(); }

static void s_reportParagraphStyle(MSWidgetView *pWidget_, A value_)
{
  AplusReportStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      ((AplusParagraph *)pWidget_)->style(style);
    }
}

static A g_reportParagraphStyle(MSWidgetView *pWidget_)
{
  AplusReportStyleConverter converter;
  return converter(((AplusParagraph *)pWidget_)->style());
}

static void s_reportParagraphColumn(MSWidgetView *pWidget_, A value_)
{
  if (QA(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	paragraph->column((long)*(double*)value_->p);
      else if (value_->t==It)  // it's an integer
	paragraph->column(*(long*)value_->p);
    }
}
static I g_reportParagraphColumn(MSWidgetView *pWidget_) 
{ 
  return (I) ((AplusParagraph *)pWidget_)->column();
}

static void s_reportParagraphColumnSpan(MSWidgetView *pWidget_, A value_)
{
  if (QA(value_))
    {
      AplusParagraph *paragraph = (AplusParagraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	paragraph->columnSpan((long)*(double*)value_->p);
      else if (value_->t==It)  // it's an integer
	paragraph->columnSpan(*(long*)value_->p);
    }
}
static I g_reportParagraphColumnSpan(MSWidgetView *pWidget_)
{ 
  return (I) ((AplusParagraph *)pWidget_)->columnSpan();
}



//************** ReportColumn attributes ********************

static void s_reportColumnLeftMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusPrintColumn *pColumn = (AplusPrintColumn *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pColumn->leftMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pColumn->leftMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportColumnLeftMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusPrintColumn *)pWidget_)->leftMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportColumnRightMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusPrintColumn *pColumn = (AplusPrintColumn *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pColumn->rightMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pColumn->rightMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportColumnRightMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusPrintColumn *)pWidget_)->rightMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportColumnTopOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusPrintColumn *pColumn = (AplusPrintColumn *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pColumn->topOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pColumn->topOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportColumnTopOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusPrintColumn *)pWidget_)->topPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportColumnBottomOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusPrintColumn *pColumn = (AplusPrintColumn *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pColumn->bottomOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pColumn->bottomOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportColumnBottomOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusPrintColumn *)pWidget_)->bottomPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportColumnReportFont(MSWidgetView *pWidget_, A value_)
{ ((AplusPrintColumn *)pWidget_)->printFont(AplusConvert::asMSString(value_)); }
static A g_reportColumnReportFont(MSWidgetView *pWidget_)
{ return AplusConvert::asA(((AplusPrintColumn *)pWidget_)->printFont()); }

static void s_reportColumnPrintRow(MSWidgetView *pWidget_, int value_) { ((AplusPrintColumn *)pWidget_)->printRow(value_); }
static I  g_reportColumnPrintRow(MSWidgetView *pWidget_) { return (I) ((AplusPrintColumn *)pWidget_)->printRow(); }

static void s_reportColumnPrintColumn(MSWidgetView *pWidget_, int value_)
{ ((AplusPrintColumn *)pWidget_)->printColumn(value_); }
static I  g_reportColumnPrintColumn(MSWidgetView *pWidget_)
{ return (I) ((AplusPrintColumn *)pWidget_)->printColumn(); }

static void s_reportColumnJustify(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long justify = converter(value_);
  if (justify!=converter.enumNotFound())
    {
      ((AplusPrintColumn *)pWidget_)->justification(justify);
    }
}

static A g_reportColumnJustify(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusPrintColumn *)pWidget_)->justification());
}  

static void s_reportColumnPageAlign(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long align = converter(value_);
  if (align!=converter.enumNotFound())
    {
      ((AplusPrintColumn *)pWidget_)->pageAlignment(align);
    }
}  

static A g_reportColumnPageAlign(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusPrintColumn *)pWidget_)->pageAlignment());
}


static void s_reportColumnOccurrence(MSWidgetView *pWidget_, A value_)
{
  AplusReportOccurrenceConverter converter;
  unsigned long occur = converter(value_);
  if (occur!=converter.enumNotFound())
    {
      ((AplusPrintColumn *)pWidget_)->occurrence(occur);
    }
}

static A g_reportColumnOccurrence(MSWidgetView *pWidget_)
{
  AplusReportOccurrenceConverter converter;
  return converter(((AplusPrintColumn *)pWidget_)->occurrence());
}


static void s_reportColumnColumnWidths(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_) && value_->r<2 && (value_->t==It || value_->t==Ft))  // if it's an int or float scalar or vector
    {
      AplusPrintColumn *pColumn = (AplusPrintColumn *)pWidget_;
      if (value_->r==0)	// if it's a scalar (as opposed to a one-element vector)
	{
	  double width = (value_->t==Ft) ? *(double*)value_->p : *(I*)value_->p;
	  pColumn->columnWidths(MSFloatVector(1,width));
	}
      else
	{
	  MSFloatVector widths(AplusConvert::asMSFloatVector(value_));
	  if (widths.length() > 0)
	    {
	      pColumn->columnWidths(widths);
	    }
	}
    }
}

static A g_reportColumnColumnWidths(MSWidgetView *pWidget_)
{
  return AplusConvert::asA(((AplusPrintColumn *)pWidget_)->columnWidths());
}

static void s_reportColumnNumColumns(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusPrintColumn *)pWidget_)->numColumns(value_); }
static unsigned g_reportColumnNumColumns(MSWidgetView *pWidget_)
{ return ((AplusPrintColumn *)pWidget_)->numColumns(); }

static void s_reportColumnStyle(MSWidgetView *pWidget_, A value_)
{
  AplusReportStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      ((AplusPrintColumn *)pWidget_)->style(style);
    }
}

static A g_reportColumnStyle(MSWidgetView *pWidget_)
{
  AplusReportStyleConverter converter;
  return converter(((AplusPrintColumn *)pWidget_)->style());
}


//**************** ReportRule attributes *********************

static void s_reportRuleLeftMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusRulePrintItem *pRule = (AplusRulePrintItem *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pRule->leftMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pRule->leftMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportRuleLeftMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusRulePrintItem *)pWidget_)->leftMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportRuleRightMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusRulePrintItem *pRule = (AplusRulePrintItem *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pRule->rightMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pRule->rightMargin((double)*(I*)value_->p);
	}
    }
}

static A g_reportRuleRightMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((AplusRulePrintItem *)pWidget_)->rightMargin();
  return gf((margin<0)?-1:margin);
}

static void s_reportRuleTopOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusRulePrintItem *pRule = (AplusRulePrintItem *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pRule->topOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pRule->topOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportRuleTopOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusRulePrintItem *)pWidget_)->topPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportRuleBottomOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusRulePrintItem *pRule = (AplusRulePrintItem *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pRule->bottomOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pRule->bottomOffset((double)*(I*)value_->p);
	}
    }
}

static A g_reportRuleBottomOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusRulePrintItem *)pWidget_)->bottomPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_reportRuleFgGrayScale(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusRulePrintItem *pRule = (AplusRulePrintItem *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pRule->fgGrayScale(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pRule->fgGrayScale((double)*(I*)value_->p);
	}
    }
}

static A g_reportRuleFgGrayScale(MSWidgetView *pWidget_) { return gf(((AplusRulePrintItem *)pWidget_)->fgGrayScale()); }

static void s_reportRuleRuleWidth(MSWidgetView *pWidget_, unsigned value_)
{ ((AplusRulePrintItem *)pWidget_)->ruleWidth(value_); }
static unsigned g_reportRuleRuleWidth(MSWidgetView *pWidget_)
{ return ((AplusRulePrintItem *)pWidget_)->ruleWidth(); }


////////////////////////////////////////////////////////////////////////////////


static MSPrintText *c_APrintText(void)
{ return new AplusPrintText();}

static MSWidgetView *c_AXReportParagraph(MSWidgetView *parent_)
{ return (MSWidgetView *)new AplusParagraph(validateParent(parent_)); }

static MSWidgetView *c_AXReportRule(MSWidgetView *parent_)
{ return (MSWidgetView *)new AplusRulePrintItem(validateParent(parent_)); }


static MSWidgetView *c_AXReportColumn(MSWidgetView *parent_)
{ return (MSWidgetView *)new AplusPrintColumn(validateParent(parent_)); }

static MSWidgetView *c_AXReport(MSWidgetView *parent_)
{ return (MSWidgetView *)new AplusPrintTool(validateParent(parent_)); }


////////////////////////////////////////////////////////////////////////////////

void installPrintCommands(void)
{
  // Print Commands
  install((PFI)s_printPageOrientation,"s_printPageOrientation",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printPageOrientation,"g_printPageOrientation",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printPagesize,"s_printPagesize",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printPagesize,"g_printPagesize",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printMode,"s_printMode",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printMode,"g_printMode",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printOutputMode,"s_printOutputMode",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printOutputMode,"g_printOutputMode",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printPageAlign,"s_printPageStyle",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printPageAlign,"g_printPageStyle",A_,0,0,0,0,0,0,0,0,0);

  install((PFI)s_printDefaultFont,"s_printDefaultFont",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_printDefaultFont,"g_printDefaultFont",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printLeftMargin,"s_printLeftMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printLeftMargin,"g_printLeftMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printRightMargin,"s_printRightMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printRightMargin,"g_printRightMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printTopMargin,"s_printTopMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printTopMargin,"g_printTopMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printBottomMargin,"s_printBottomMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printBottomMargin,"g_printBottomMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printMargins,"s_printMargins",V_,1,A_,0,0,0,0,0,0,0);

  install((PFI)s_printFile,"s_printFile",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printFile,"g_printFile",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisOrientation,"s_printDisOrientation",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisOrientation,"g_printDisOrientation",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisclaimerStyle,"s_printDisclaimerStyle",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisclaimerStyle,"g_printDisclaimerStyle",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisclaimerFile,"s_printDisclaimerFile",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisclaimerFile,"g_printDisclaimerFile",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisclaimerFont,"s_printDisclaimerFont",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_printDisclaimerFont,"g_printDisclaimerFont",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisLeftMargin,"s_printDisLeftMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisLeftMargin,"g_printDisLeftMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisRightMargin,"s_printDisRightMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisRightMargin,"g_printDisRightMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisTopMargin,"s_printDisTopMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisTopMargin,"g_printDisTopMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisBottomMargin,"s_printDisBottomMargin",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_printDisBottomMargin,"g_printDisBottomMargin",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_printDisRuleWidth,"s_printDisRuleWidth",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_printDisRuleWidth,"g_printDisRuleWidth",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)bprint,"bprint",V_,1,IV,0,0,0,0,0,0,0);

  install((PFI)q_printDisclaimer,"q_printDisclaimer",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printPageOrientation,"q_printDisOrientation",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printPageOrientation,"q_printPageOrientation",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printPageSize,"q_printPageSize",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printMode,"q_printMode",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printPaperTray,"q_printPaperTray",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_printOutputMode,"q_printOutputMode",A_,0,0,0,0,0,0,0,0,0);
}

void installReportParagraphCommands(void)
{
  install((PFI)c_AXReportParagraph, "c_AXReportParagraph",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphLeftMargin,"s_reportParagraphLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphLeftMargin,"g_reportParagraphLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphRightMargin,"s_reportParagraphRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphRightMargin,"g_reportParagraphRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphTopOffset,"s_reportParagraphTopOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphTopOffset,"g_reportParagraphTopOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphBottomOffset,"s_reportParagraphBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphBottomOffset,"g_reportParagraphBottomOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphReportFont,"s_reportParagraphReportFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphReportFont,"g_reportParagraphReportFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphPrintRow,"s_reportParagraphPrintRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphPrintRow,"g_reportParagraphPrintRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphPrintColumn,"s_reportParagraphPrintColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphPrintColumn,"g_reportParagraphPrintColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphJustify,"s_reportParagraphJustify",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphJustify,"g_reportParagraphJustify",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphPageAlign,"s_reportParagraphPageAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphPageAlign,"g_reportParagraphPageAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphOccurrence,"s_reportParagraphOccurrence",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphOccurrence,"g_reportParagraphOccurrence",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphFirstLineIndent,"s_reportParagraphFirstLineIndent",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphFirstLineIndent,"g_reportParagraphFirstLineIndent",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphLeading,"s_reportParagraphLeading",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphLeading,"g_reportParagraphLeading",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphLineWidth,"s_reportParagraphLineWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphLineWidth,"g_reportParagraphLineWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphFgGrayScale,"s_reportParagraphFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphFgGrayScale,"g_reportParagraphFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphBgGrayScale,"s_reportParagraphBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphBgGrayScale,"g_reportParagraphBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphOrphanRows,"s_reportParagraphOrphanRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphOrphanRows,"g_reportParagraphOrphanRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphWidowRows,"s_reportParagraphWidowRows",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportParagraphWidowRows,"g_reportParagraphWidowRows",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphStyle,"s_reportParagraphStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphStyle,"g_reportParagraphStyle",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_reportParagraphColumn,"s_reportParagraphColumn",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphColumn,"g_reportParagraphColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportParagraphColumnSpan,"s_reportParagraphColumnSpan",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportParagraphColumnSpan,"g_reportParagraphColumnSpan",IV,1,IV,0,0,0,0,0,0,0);

}

void installReportColumnCommands(void)
{
  install((PFI)c_AXReportColumn, "c_AXReportColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnLeftMargin,"s_reportColumnLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnLeftMargin,"g_reportColumnLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnRightMargin,"s_reportColumnRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnRightMargin,"g_reportColumnRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnTopOffset,"s_reportColumnTopOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnTopOffset,"g_reportColumnTopOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnBottomOffset,"s_reportColumnBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnBottomOffset,"g_reportColumnBottomOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnReportFont,"s_reportColumnReportFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnReportFont,"g_reportColumnReportFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnPrintRow,"s_reportColumnPrintRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportColumnPrintRow,"g_reportColumnPrintRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnPrintColumn,"s_reportColumnPrintColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportColumnPrintColumn,"g_reportColumnPrintColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnJustify,"s_reportColumnJustify",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnJustify,"g_reportColumnJustify",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnPageAlign,"s_reportColumnPageAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnPageAlign,"g_reportColumnPageAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnOccurrence,"s_reportColumnOccurrence",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnOccurrence,"g_reportColumnOccurrence",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnColumnWidths,"s_reportColumnColumnWidths",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnColumnWidths,"g_reportColumnColumnWidths",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnNumColumns,"s_reportColumnNumColumns",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportColumnNumColumns,"g_reportColumnNumColumns",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportColumnStyle,"s_reportColumnStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportColumnStyle,"g_reportColumnStyle",A_,1,IV,0,0,0,0,0,0,0);
}

void installReportRuleCommands(void)
{
  install((PFI)c_AXReportRule, "c_AXReportRule",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleLeftMargin,"s_reportRuleLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRuleLeftMargin,"g_reportRuleLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleRightMargin,"s_reportRuleRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRuleRightMargin,"g_reportRuleRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleTopOffset,"s_reportRuleTopOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRuleTopOffset,"g_reportRuleTopOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleBottomOffset,"s_reportRuleBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRuleBottomOffset,"g_reportRuleBottomOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleFgGrayScale,"s_reportRuleFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRuleFgGrayScale,"g_reportRuleFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRuleRuleWidth,"s_reportRuleRuleWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportRuleRuleWidth,"g_reportRuleRuleWidth",IV,1,IV,0,0,0,0,0,0,0);
}

void installPrintTextCommands(void)
{
  install((PFI)s_printtextText,"s_printtextText",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextText,"g_printtextText",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextFont,"s_printtextFont",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextFont,"g_printtextFont",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextForeground,"s_printtextForeground",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextForeground,"g_printtextForeground",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextBackground,"s_printtextBackground",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextBackground,"g_printtextBackground",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextJustification,"s_printtextJustification",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextJustification,"g_printtextJustification",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextStyle,"s_printtextStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextStyle,"g_printtextStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextMode,"s_printtextMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_printtextMode,"g_printtextMode",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_printtextFontSize,"s_printtextFontSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextFontSize,"g_printtextFontSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextLeading,"s_printtextLeading",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextLeading,"g_printtextLeading",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextTopPixel,"s_printtextTopPixel",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextTopPixel,"g_printtextTopPixel",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextBottomPixel,"s_printtextBottomPixel",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextBottomPixel,"g_printtextBottomPixel",IV,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_printtextFontScale,"s_printtextFontScale",V_,2,IV,A_,0,0,0,0,0,0);      // DOUBLEs
  install((PFI)g_printtextFontScale,"g_printtextFontScale",A_,1,IV,0,0,0,0,0,0,0);       // DOUBLEs
  install((PFI)s_printtextFgGrayScale,"s_printtextFgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);  // DOUBLEs
  install((PFI)g_printtextFgGrayScale,"g_printtextFgGrayScale",A_,1,IV,0,0,0,0,0,0,0);   // DOUBLEs
  install((PFI)s_printtextBgGrayScale,"s_printtextBgGrayScale",V_,2,IV,A_,0,0,0,0,0,0);  // DOUBLEs
  install((PFI)g_printtextBgGrayScale,"g_printtextBgGrayScale",A_,1,IV,0,0,0,0,0,0,0);   // DOUBLEs

  install((PFI)s_printtextLineWidth,"s_printtextLineWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextLineWidth,"g_printtextLineWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextRow,"s_printtextRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextRow,"g_printtextRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextColumn,"s_printtextColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextColumn,"g_printtextColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_printtextFirstColumn,"s_printtextFirstColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_printtextFirstColumn,"g_printtextFirstColumn",IV,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_printtextxOrigin,"s_printtextxOrigin",V_,2,IV,A_,0,0,0,0,0,0);             // DOUBLEs
  install((PFI)g_printtextxOrigin,"g_printtextxOrigin",A_,1,IV,0,0,0,0,0,0,0);              // DOUBLEs
  install((PFI)s_printtextyOrigin,"s_printtextyOrigin",V_,2,IV,A_,0,0,0,0,0,0);             // DOUBLEs
  install((PFI)g_printtextyOrigin,"g_printtextyOrigin",A_,1,IV,0,0,0,0,0,0,0);              // DOUBLEs
  install((PFI)s_printtextTopOffset,"s_printtextTopOffset",V_,2,IV,A_,0,0,0,0,0,0);         // DOUBLEs
  install((PFI)g_printtextTopOffset,"g_printtextTopOffset",A_,1,IV,0,0,0,0,0,0,0);          // DOUBLEs
  install((PFI)s_printtextBottomOffset,"s_printtextBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);   // DOUBLEs
  install((PFI)g_printtextBottomOffset,"g_printtextBottomOffset",A_,1,IV,0,0,0,0,0,0,0);    // DOUBLEs

  install((PFI)c_APrintText, "c_APrintText",IV,0,0,0,0,0,0,0,0,0);
}

void installReportCommands(void)
{
  // Report Commands
  install((PFI)s_reportPageOrientation,"s_reportPageOrientation",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportPageOrientation,"g_reportPageOrientation",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerOrientation,"s_reportDisclaimerOrientation",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerOrientation,"g_reportDisclaimerOrientation",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPagesize,"s_reportPagesize",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportPagesize,"g_reportPagesize",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportOutputMode,"s_reportOutputMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportOutputMode,"g_reportOutputMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerStyle,"s_reportDisclaimerStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerStyle,"g_reportDisclaimerStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportLeftMargin,"s_reportLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportLeftMargin,"g_reportLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportRightMargin,"s_reportRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportRightMargin,"g_reportRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportTopMargin,"s_reportTopMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportTopMargin,"g_reportTopMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportBottomMargin,"s_reportBottomMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportBottomMargin,"g_reportBottomMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportMargins,"s_reportMargins",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerLeftMargin,"s_reportDisclaimerLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerLeftMargin,"g_reportDisclaimerLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerRightMargin,"s_reportDisclaimerRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerRightMargin,"g_reportDisclaimerRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerTopMargin,"s_reportDisclaimerTopMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerTopMargin,"g_reportDisclaimerTopMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerBottomMargin,"s_reportDisclaimerBottomMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerBottomMargin,"g_reportDisclaimerBottomMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_reportFileName,"g_reportFileName",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportFileName,"s_reportFileName",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerFile,"g_reportDisclaimerFile",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerFile,"s_reportDisclaimerFile",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportDisclaimerRuleWidth,"g_reportDisclaimerRuleWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportDisclaimerRuleWidth,"s_reportDisclaimerRuleWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportFooter,"g_reportFooter",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportFooter,"s_reportFooter",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportHeader,"g_reportHeader",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportHeader,"s_reportHeader",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportBanner,"g_reportBanner",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportBanner,"s_reportBanner",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_reportCancel,"s_reportCancel",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_reportFooterOffset,"g_reportFooterOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportFooterOffset,"s_reportFooterOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageFrameLineWidth,"g_reportPageFrameLineWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageFrameLineWidth,"s_reportPageFrameLineWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageFrameOffset,"g_reportPageFrameOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageFrameOffset,"s_reportPageFrameOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageFrameStyle,"g_reportPageFrameStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageFrameStyle,"s_reportPageFrameStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportHeaderOffset,"g_reportHeaderOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportHeaderOffset,"s_reportHeaderOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageCount,"g_reportPageCount",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_reportPageCountTotal,"g_reportPageCountTotal",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_reportPageNumbers,"g_reportPageNumbers",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageNumbers,"s_reportPageNumbers",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportPageNumbering,"g_reportPageNumbering",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageNumbering,"s_reportPageNumbering",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageNumberText,"g_reportPageNumberText",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageNumberText,"s_reportPageNumberText",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportPageNumber,"g_reportPageNumber",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPageNumber,"s_reportPageNumber",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportUniformScaling,"g_reportUniformScaling",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportUniformScaling,"s_reportUniformScaling",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_reportPrint,"s_reportPrint",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)addReportCB,"bAddReportCB",V_,3,IV,A_,A_,0,0,0,0,0);
  install((PFI)getReportCB,"bGetReportCB",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_reportPageNumberOffset,"s_reportPageNumberOffset",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_reportPageNumberOffset,"g_reportPageNumberOffset",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_reportFontSize,"g_reportFontSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_reportPrintMode,"s_reportPrintMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_reportPrintMode,"g_reportPrintMode",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)q_reportPageOrientation,"q_reportPageOrientation",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportPageSize,"q_reportPageSize",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportOutputMode,"q_reportOutputMode",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportDisclaimerStyle,"q_reportDisclaimerStyle",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportStyle,"q_reportStyle",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportOccurrence,"q_reportOccurrence",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)q_reportCompMode,"q_reportCompMode",A_,0,0,0,0,0,0,0,0,0);

  install((PFI)c_AXReport, "c_AXReport",IV,1,IV,0,0,0,0,0,0,0);

  installPrintTextCommands();
  installReportParagraphCommands();
  installReportRuleCommands();
  installReportColumnCommands();

}

void AGIFPrintInstall(void)
{
  CX context=Cx;
  Cx=cx("s");
  installPrintCommands();
  installReportCommands();
  Cx=context;
}


/********************************************************************************

	       MSReportTable Attributes
	       ========================

Method                   Arg Type                  Type
------              -------------------     ----------------     
AddGroupHeading       
breakColumn          Index Vector              r only
breakFont            string                    r/w
breakIndex           Index Vector              r only
breakStyle           unsigned                  r/w  
breakTextColumn      Index Vector              r only
generateReport       void                      r only
grandTotal           void                      r only
grandTotalText       print text sf             r/w  
groupHeading            PROBABLY DOESN'T WORK
headingStyle         unsigned                  r/w
pageBreakIndex       Index Vector              r only
pageTotalOn          boolean                   r/w
reportFont           string                    r/w
reportGrandTotalOn   boolean                   r/w
reportHeadingFont    string                    r/w
reportTitle          string vector             r/w
reportTotalFont      string                    r/w 
reportTotalLeading   unsigned                  r/w 
reportTotalOn        boolean                   r/w
reportTotalStyle     unsigned                  r/w 
style                unsigned                  r/w


MSTableColumn
=================

Method                   Arg Type                  Type
------              -------------------     ----------------     
addBreakText        PrintTextSlotFiller       w only
bgGrayScale         double                    r/w   
breakFont           string                    r/w
breakIndex          index vector              r only                             
breakLeading        int                       r/w                             
breakOffset         int                       r/w
breakon             Boolean                   r/w                             
breakProcessOn      Boolean                   r/w
breakString         string vector             r/w                                      
breakStyle          unsigned                  r/w                             
breaktext           unsigned (returns sf)     r only                        
fgGrayScale         double                    r/w                             
headingStyle        unsigned                  r/w 
pageBreakOn         Boolean                   r/w                             
removeAllBreakText  void                      r only                             
removeBreakText     unsigned                  r only
reportFont          string                    r/w                                       
style               unsigned                  r/w                             
suppressDuplicate   boolean                   r/w

                 virtual functions
                 - - - - - - - - -
formatBreak         functional attribute      r/w                             
isDuplicate         bool func attr            r/w
style
breakStyle
breakOffset
breakLeading
reportFont
breakFont
fgGrayScale
bgGrayScale

*/
