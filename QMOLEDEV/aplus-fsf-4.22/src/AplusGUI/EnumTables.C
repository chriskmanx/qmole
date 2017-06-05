///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSGUI/MSG.H>
#include <MSGUI/MSPostScript.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusTableColumn.H>
#include <AplusGUI/EnumTables.H>

MSHashTable *EnumTables::_printEnumHashTable=0;
MSHashTable *EnumTables::_stringEnumHashTable=0;
MSHashTable *EnumTables::_alignEnumHashTable=0;
MSHashTable *EnumTables::_alignStringHashTable=0;
MSHashTable *EnumTables::_styleEnumHashTable=0;
MSHashTable *EnumTables::_styleStringHashTable=0;
MSHashTable *EnumTables::_pageSizeEnumHashTable=0;
MSHashTable *EnumTables::_pageSizeStringHashTable=0;
MSHashTable *EnumTables::_reportStyleEnumHashTable=0;
MSHashTable *EnumTables::_reportStyleStringHashTable=0;
MSHashTable *EnumTables::_reportCompModeEnumHashTable=0;
MSHashTable *EnumTables::_reportCompModeStringHashTable=0;
MSHashTable *EnumTables::_cycleColorModeEnumHashTable=0;
MSHashTable *EnumTables::_cycleColorModeStringHashTable=0;

EnumTables GUIEnum;

EnumTables::EnumTables(void)
{
  _stringEnumHashTable  = new MSHashTable(128);
  _printEnumHashTable   = new MSHashTable(128);

  _pageSizeEnumHashTable      = new MSHashTable(32);
  _pageSizeStringHashTable    = new MSHashTable(32);
  _alignStringHashTable       = new MSHashTable(128);
  _alignEnumHashTable         = new MSHashTable(128);
  _styleEnumHashTable         = new MSHashTable(128);
  _styleStringHashTable       = new MSHashTable(128);
  _reportStyleEnumHashTable   = new MSHashTable(16);
  _reportStyleStringHashTable = new MSHashTable(16);
  _reportCompModeEnumHashTable   = new MSHashTable(16);
  _reportCompModeStringHashTable = new MSHashTable(16);
  _cycleColorModeEnumHashTable   = new MSHashTable(16);
  _cycleColorModeStringHashTable = new MSHashTable(16);

  initStringEnumHashTable();
  initPrintEnumHashTable();
  initPageSizeHashTables();
  initAlignHashTables();
  initStyleHashTables();
  initReportStyleHashTables();
  initReportCompModeHashTables();
  initCycleColorModeHashTables();
}

EnumTables::~EnumTables(void)
{
  if (_printEnumHashTable) { delete _printEnumHashTable;  _printEnumHashTable = 0; }
  if (_stringEnumHashTable) { delete _stringEnumHashTable;  _stringEnumHashTable=0; }
  if (_pageSizeEnumHashTable) { delete _pageSizeEnumHashTable; _pageSizeEnumHashTable=0; }
  if (_pageSizeStringHashTable) { delete _pageSizeStringHashTable; _pageSizeStringHashTable=0; }
  if (_alignStringHashTable) { delete _alignStringHashTable; _alignStringHashTable=0; }
  if (_alignEnumHashTable) { delete _alignEnumHashTable; _alignEnumHashTable=0; }
  if (_styleEnumHashTable) { delete _styleEnumHashTable; _styleEnumHashTable=0; }
  if (_styleStringHashTable) { delete _styleStringHashTable; _styleStringHashTable=0; }
  if (_reportStyleEnumHashTable) { delete _reportStyleEnumHashTable; _reportStyleEnumHashTable=0; }
  if (_reportStyleStringHashTable) { delete _reportStyleStringHashTable; _reportStyleStringHashTable=0; }
  if (_reportCompModeEnumHashTable) { delete _reportCompModeEnumHashTable; _reportCompModeEnumHashTable=0; }
  if (_reportCompModeStringHashTable) { delete _reportCompModeStringHashTable; _reportCompModeStringHashTable=0; }
}

void EnumTables::initAlignHashTables(void)
{
  alignEnumHashTable()->notFound((unsigned long)0x5f5f5f);
  alignEnumHashTable()->add("none"  ,(void*)MSNone);
  alignEnumHashTable()->add("center",(void*)MSCenter);
  alignEnumHashTable()->add("left"  ,(void*)MSLeft);
  alignEnumHashTable()->add("right" ,(void*)MSRight);
  alignEnumHashTable()->add("top"   ,(void*)MSTop);
  alignEnumHashTable()->add("bottom",(void*)MSBottom);
  alignEnumHashTable()->add("inside",(void*)MSG::Inside);
  alignEnumHashTable()->add("outside",(void*)MSG::Outside);

  alignStringHashTable()->notFound((unsigned long)0);
  alignStringHashTable()->add(MSCenter,(void*)"center");
  alignStringHashTable()->add(MSLeft  ,(void*)"left");
  alignStringHashTable()->add(MSRight ,(void*)"right");
  alignStringHashTable()->add(MSTop   ,(void*)"top");
  alignStringHashTable()->add(MSBottom,(void*)"bottom");
  alignStringHashTable()->add(MSG::Inside,(void*)"inside");
  alignStringHashTable()->add(MSG::Outside,(void*)"outside");
}

void EnumTables::initPageSizeHashTables(void)
{
  pageSizeEnumHashTable()->notFound(0);
  pageSizeEnumHashTable()->add(MSP::Letter,(void*)"letter");
  pageSizeEnumHashTable()->add(MSP::Legal ,(void*)"legal");
  pageSizeEnumHashTable()->add(MSP::A4	   ,(void*)"a4");
  pageSizeEnumHashTable()->add(MSP::B5	   ,(void*)"b5");
  pageSizeEnumHashTable()->add(MSP::A 	   ,(void*)"a");
  pageSizeEnumHashTable()->add(MSP::B 	   ,(void*)"b");

  pageSizeStringHashTable()->notFound(0);
  pageSizeStringHashTable()->add("letter"   ,(void*)MSP::Letter);
  pageSizeStringHashTable()->add("legal"    ,(void*)MSP::Legal);
  pageSizeStringHashTable()->add("a4"       ,(void*)MSP::A4);
  pageSizeStringHashTable()->add("b5"       ,(void*)MSP::B5);
  pageSizeStringHashTable()->add("a"        ,(void*)MSP::A);
  pageSizeStringHashTable()->add("b"        ,(void*)MSP::B);
}


void EnumTables::initStringEnumHashTable(void)
{
  stringEnumHashTable()->notFound(0);
  stringEnumHashTable()->add(MSP::Portrait    ,(void*) "portrait"); 
  stringEnumHashTable()->add(MSP::Landscape   ,(void*) "landscape");
  stringEnumHashTable()->add(MSP::Mono        ,(void*) "mono");
  stringEnumHashTable()->add(MSP::Color       ,(void*) "color");
  stringEnumHashTable()->add(MSP::Colorfg     ,(void*) "colorfg");
  stringEnumHashTable()->add(MSP::Reverse     ,(void*) "reverse");
  stringEnumHashTable()->add(MSP::PPM         ,(void*) "ppm");
  stringEnumHashTable()->add(MSP::EPS         ,(void*) "eps");
  stringEnumHashTable()->add(MSP::PS          ,(void*) "ps");
  stringEnumHashTable()->add(MSP::Append      ,(void*) "append");
  stringEnumHashTable()->add(MSP::AppendBox   ,(void*) "appendbox");
  stringEnumHashTable()->add(MSP::Text	      ,(void*) "text");
  stringEnumHashTable()->add(MSP::Box 	      ,(void*) "box");
  stringEnumHashTable()->add(MSP::Rule	      ,(void*) "rule");
  stringEnumHashTable()->add(MSP::Toprule     ,(void*) "toprule");
  stringEnumHashTable()->add(MSP::NoDisclaimer,(void*) "none");

  stringEnumHashTable()->add((unsigned long)MSSolid,(void*)"solid");
//  stringEnumHashTable()->add(MSDash,(void*)"dash1");
//  stringEnumHashTable()->add(MSDot,(void*)"dot1");

//  stringEnumHashTable()->add(MSCenter,(void*)"center");
//  stringEnumHashTable()->add(MSLeft,(void*)"left");
//  stringEnumHashTable()->add(MSRight,(void*)"right");
  stringEnumHashTable()->add(MSTop   ,(void*)"top");
  stringEnumHashTable()->add(MSBottom,(void*)"bottom");
}

void EnumTables::initPrintEnumHashTable(void)
{
  printEnumHashTable()->notFound((unsigned long)0);
  printEnumHashTable()->add("",(void*)1);

  printEnumHashTable()->add("portrait" ,(void*)MSP::Portrait);
  printEnumHashTable()->add("landscape",(void*)MSP::Landscape);
  printEnumHashTable()->add("mono"     ,(void*)MSP::Mono);
  printEnumHashTable()->add("gs"       ,(void*)MSP::Mono);
  printEnumHashTable()->add("color"    ,(void*)MSP::Color);
  printEnumHashTable()->add("colorfg"  ,(void*)MSP::Colorfg);
  printEnumHashTable()->add("reverse"  ,(void*)MSP::Reverse);
  printEnumHashTable()->add("ppm"      ,(void*)MSP::PPM);
  printEnumHashTable()->add("eps"      ,(void*)MSP::EPS);
  printEnumHashTable()->add("ps"       ,(void*)MSP::PS);

  printEnumHashTable()->add("append"   ,(void*)MSP::Append);
  printEnumHashTable()->add("appendbox",(void*)MSP::AppendBox);
  printEnumHashTable()->add("text"     ,(void*)MSP::Text);
  printEnumHashTable()->add("box"      ,(void*)MSP::Box);
  printEnumHashTable()->add("rule"     ,(void*)MSP::Rule);
  printEnumHashTable()->add("toprule"  ,(void*)MSP::Toprule);

  printEnumHashTable()->add("solid"    ,(void*)MSSolid);
  printEnumHashTable()->add("dash"     ,(void*)MSDash);
  printEnumHashTable()->add("dot"      ,(void*)MSDot);

  printEnumHashTable()->add("left"     ,(void*)MSLeft);
  printEnumHashTable()->add("right"    ,(void*)MSRight);
  printEnumHashTable()->add("top"      ,(void*)MSTop);
  printEnumHashTable()->add("bottom"   ,(void*)MSBottom);

}


void EnumTables::initStyleHashTables(void)
{
  styleEnumHashTable()->notFound((int)0);
  styleEnumHashTable()->add(MSP::Underline ,(void *)"underline");
  styleEnumHashTable()->add(MSP::DUnderline,(void *)"dul");
  styleEnumHashTable()->add(MSP::Superscript,(void *)"superscript");
  styleEnumHashTable()->add(MSP::Subscript,(void *)"subscript");
  styleEnumHashTable()->add(MSP::Outline   ,(void *)"outline");
  styleEnumHashTable()->add(MSP::Smallcap  ,(void *)"smallcap");
  styleEnumHashTable()->add(MSP::Strikethru,(void *)"strikethru");
  styleEnumHashTable()->add(MSP::Box       ,(void *)"box");
  styleEnumHashTable()->add(MSP::BoxL      ,(void *)"boxl");
  styleEnumHashTable()->add(MSP::BoxR      ,(void *)"boxr");
  styleEnumHashTable()->add(MSP::BoxT      ,(void *)"boxt");
  styleEnumHashTable()->add(MSP::BoxB      ,(void *)"boxb");
  styleEnumHashTable()->add(MSP::Cell      ,(void *)"cell");
  styleEnumHashTable()->add(MSP::Stipple   ,(void *)"stipple");
  styleEnumHashTable()->add(MSP::CharOnly  ,(void *)"charonly");
  styleEnumHashTable()->add(MSP::PosAbove  ,(void *)"posabove");
  styleEnumHashTable()->add(MSP::PosBelow  ,(void *)"posbelow");
  styleEnumHashTable()->add(MSLeft         ,(void *)"left");
  styleEnumHashTable()->add(MSRight        ,(void *)"right");
  styleEnumHashTable()->add(MSTop          ,(void *)"top");
  styleEnumHashTable()->add(MSBottom       ,(void *)"bottom");
  styleEnumHashTable()->add(MSCenter       ,(void *)"center");

  styleStringHashTable()->notFound((int)0);
  styleStringHashTable()->add("underline"      	,(void *) MSP::Underline);
  styleStringHashTable()->add("dunderline"     	,(void *) MSP::DUnderline);
  styleStringHashTable()->add("dul"            	,(void *) MSP::DUnderline);
  styleStringHashTable()->add("superscript"     ,(void *) MSP::Superscript);
  styleStringHashTable()->add("subscript"       ,(void *) MSP::Subscript);
  styleStringHashTable()->add("outline"        	,(void *) MSP::Outline);
  styleStringHashTable()->add("smallcap"       	,(void *) MSP::Smallcap);
  styleStringHashTable()->add("strikethru"     	,(void *) MSP::Strikethru);
  styleStringHashTable()->add("box"		,(void *) MSP::Box);
  styleStringHashTable()->add("boxl"		,(void *) MSP::BoxL);
  styleStringHashTable()->add("boxr"		,(void *) MSP::BoxR);
  styleStringHashTable()->add("boxt"		,(void *) MSP::BoxT);
  styleStringHashTable()->add("boxb"		,(void *) MSP::BoxB);
  styleStringHashTable()->add("cell"		,(void *) MSP::Cell);
  styleStringHashTable()->add("stipple" 	,(void *) MSP::Stipple);
  styleStringHashTable()->add("charonly"	,(void *) MSP::CharOnly);
  styleStringHashTable()->add("posabove"	,(void *) MSP::PosAbove);
  styleStringHashTable()->add("posbelow"	,(void *) MSP::PosBelow);
  styleStringHashTable()->add("left"  		,(void *) MSLeft);
  styleStringHashTable()->add("right" 		,(void *) MSRight);
  styleStringHashTable()->add("top"   		,(void *) MSTop);
  styleStringHashTable()->add("bottom"		,(void *) MSBottom);
  styleStringHashTable()->add("center"		,(void *) MSCenter);
}

void EnumTables::initReportStyleHashTables(void)
{
  reportStyleEnumHashTable()->notFound((int)0);
  reportStyleEnumHashTable()->add(MSP::ColMajor  ,(void *)"columnmajor");
  reportStyleEnumHashTable()->add(MSP::RowMajor  ,(void *)"rowmajor");
  reportStyleEnumHashTable()->add(MSP::Aligned   ,(void *)"aligned");
  reportStyleEnumHashTable()->add(MSP::NoHeadings,(void *)"noheadings");

  reportStyleStringHashTable()->notFound((int)0);
  reportStyleStringHashTable()->add("columnmajor",(void *)MSP::ColMajor);
  reportStyleStringHashTable()->add("rowmajor"   ,(void *)MSP::RowMajor);
  reportStyleStringHashTable()->add("aligned"    ,(void *)MSP::Aligned);
  reportStyleStringHashTable()->add("noheadings" ,(void *)MSP::NoHeadings);
}

void EnumTables::initReportCompModeHashTables(void)
{
  reportCompModeEnumHashTable()->notFound((int)0);
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::Sum	  ,(void *)"sum");
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::Max	  ,(void *)"max");
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::Min	  ,(void *)"min");
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::Avg	  ,(void *)"avg");
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::StdDev  ,(void *)"stddev");
  reportCompModeEnumHashTable()->add((unsigned long) AplusReportAlgorithm::Variance,(void *)"variance");

  reportCompModeStringHashTable()->notFound((int)0);
  reportCompModeStringHashTable()->add("sum"   	 ,(void *)AplusReportAlgorithm::Sum);
  reportCompModeStringHashTable()->add("max"   	 ,(void *)AplusReportAlgorithm::Max);
  reportCompModeStringHashTable()->add("min"   	 ,(void *)AplusReportAlgorithm::Min);
  reportCompModeStringHashTable()->add("avg"   	 ,(void *)AplusReportAlgorithm::Avg);
  reportCompModeStringHashTable()->add("stddev"	 ,(void *)AplusReportAlgorithm::StdDev);
  reportCompModeStringHashTable()->add("variance",(void *)AplusReportAlgorithm::Variance);
}

void EnumTables::initCycleColorModeHashTables(void)
{
  cycleColorModeEnumHashTable()->notFound((int)0);
  cycleColorModeEnumHashTable()->add((unsigned long)MSForeground, (void *)"fg");
  cycleColorModeEnumHashTable()->add((unsigned long)MSBackground, (void *)"bg");
  cycleColorModeEnumHashTable()->add((unsigned long)MSReverseVideo, (void *)"reverse");

  cycleColorModeStringHashTable()->notFound((int)-1);
  cycleColorModeStringHashTable()->add("fg", (void *)MSForeground);
  cycleColorModeStringHashTable()->add("bg", (void *)MSBackground);
  cycleColorModeStringHashTable()->add("reverse", (void *)MSReverseVideo);
}

A EnumTables::enumSymbols(const char *str_)  
{ 
  A r=aplus_nl;

  if (MSSymbol(str_) == MSSymbol("disclaimer"))
   {
     r=gv(Et,6);
     r->p[0]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Text)));
     r->p[1]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Box)));
     r->p[2]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Rule)));
     r->p[3]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Toprule)));
     r->p[4]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Append)));
     r->p[5]=MS(si((char *)stringEnumHashTable()->lookup(MSP::NoDisclaimer)));
   }
  else if (MSSymbol(str_) == MSSymbol("orientation"))
   {
     r=gv(Et,2);
     r->p[0]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Portrait)));
     r->p[1]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Landscape)));
   }
  else if (MSSymbol(str_) == MSSymbol("pageSize"))
   {
     r=gv(Et,6);
     r->p[0]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::Letter)));
     r->p[1]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::Legal)));
     r->p[2]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::A4)));
     r->p[3]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::B5)));
     r->p[4]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::A)));
     r->p[5]=MS(si((char *)pageSizeEnumHashTable()->lookup(MSP::B)));
   }
  else if (MSSymbol(str_) == MSSymbol("printMode"))
   {
     r=gv(Et,4);
     r->p[0]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Mono)));
     r->p[1]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Color)));
     r->p[2]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Colorfg)));
     r->p[3]=MS(si((char *)stringEnumHashTable()->lookup(MSP::Reverse)));
   }
  else if (MSSymbol(str_) == MSSymbol("printTray"))
   {
     r=gv(Et,2);
     r->p[0]=MS(si((char *)stringEnumHashTable()->lookup(MSTop)));
     r->p[1]=MS(si((char *)stringEnumHashTable()->lookup(MSBottom)));
   }
  else if (MSSymbol(str_) == MSSymbol("outputMode"))
   {
     r=gv(Et,3);
     r->p[0]=MS(si((char *)stringEnumHashTable()->lookup(MSP::PS)));
     r->p[1]=MS(si((char *)stringEnumHashTable()->lookup(MSP::EPS)));
     r->p[2]=MS(si((char *)stringEnumHashTable()->lookup(MSP::PPM)));
   }
  else if (MSSymbol(str_) == MSSymbol("reportStyle"))
   {
     r=gv(Et,4);
     r->p[0]=MS(si((char *)reportStyleEnumHashTable()->lookup(MSP::ColMajor)));
     r->p[1]=MS(si((char *)reportStyleEnumHashTable()->lookup(MSP::RowMajor)));
     r->p[2]=MS(si((char *)reportStyleEnumHashTable()->lookup(MSP::Aligned)));
     r->p[3]=MS(si((char *)reportStyleEnumHashTable()->lookup(MSP::NoHeadings)));
   }
  else if (MSSymbol(str_) == MSSymbol("printTextStyle"))
   {
     r=gv(Et,16);
     r->p[0] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Underline )));
     r->p[1] =MS(si((char *)styleEnumHashTable()->lookup(MSP::DUnderline)));
     r->p[2] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Superscript)));
     r->p[3] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Subscript )));
     r->p[4] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Outline   )));
     r->p[5] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Smallcap  )));
     r->p[6] =MS(si((char *)styleEnumHashTable()->lookup(MSP::Strikethru)));
     r->p[7] =MS(si((char *)styleEnumHashTable()->lookup(MSP::BoxL      )));
     r->p[8] =MS(si((char *)styleEnumHashTable()->lookup(MSP::BoxR      )));
     r->p[9] =MS(si((char *)styleEnumHashTable()->lookup(MSP::BoxT      )));
     r->p[10] =MS(si((char *)styleEnumHashTable()->lookup(MSP::BoxB      )));
     r->p[11]=MS(si((char *)styleEnumHashTable()->lookup(MSP::Cell      )));
     r->p[12]=MS(si((char *)styleEnumHashTable()->lookup(MSP::Stipple   )));
     r->p[13]=MS(si((char *)styleEnumHashTable()->lookup(MSP::CharOnly  )));
     r->p[14]=MS(si((char *)styleEnumHashTable()->lookup(MSP::PosAbove  )));
     r->p[15]=MS(si((char *)styleEnumHashTable()->lookup(MSP::PosBelow  )));
   }
  else if (MSSymbol(str_) == MSSymbol("compmode"))
   {
     r=gv(Et,6);
     r->p[0] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::Sum     )));
     r->p[1] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::Max     )));
     r->p[2] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::Min     )));
     r->p[3] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::Avg     )));
     r->p[4] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::StdDev  )));
     r->p[5] =MS(si((char *)reportCompModeEnumHashTable()->lookup((unsigned long) AplusReportAlgorithm::Variance)));
   }
  return r;
}


unsigned long EnumTables::alignFormat(A sym_)   /* This originated from AXAlign::format */
{
  unsigned long align=0;
  if (sym_!=0)
   {
     A *p=(A *)sym_->p;
     for (int i=0; i<(int)sym_->n; i++)
      {
	char *s=(char *)XS(sym_->p[i])->n;
	if (QS(p[i]))
	{
	  unsigned long val = (unsigned long)alignEnumHashTable()->lookup(s);
	  if (val==alignEnumHashTable()->notFound())
	   {
	     cerr << "ã ! ";
	     if (s!=0) cerr<<s;
	     cerr<<": invalid alignment symbol"<<endl;
	   }
	  else
	    {
	      align |= val;
	    }
	}
      }
   }
  if ((align&MSLeft)&&(align&MSRight)) align-=MSRight;
  if ((align&MSTop)&&(align&MSBottom)) align-=MSBottom;
  return align;
}

A EnumTables::alignFormat(unsigned long x_)   /* from AXAlign::format */
{
  A   r=aplus_nl;
  char *str;
  unsigned long x=x_, align=0x1;
  unsigned int count=0;

  while (x!=0)
    {
      if ((x & 0x1) && alignStringHashTable()->lookup(align)!=0)
	{
	  count++;
	}

      x>>=1;
      align<<=1;
    }

  if (count>0)
    {
      r=gv(Et,count);
      x=x_;
      align=0x1;
      count=0;
      while (x!=0)
	{
	  if (x & 0x1)
	    {
	      str = (char *)alignStringHashTable()->lookup(align);
	      if (str!=0)
		{
		  r->p[count++]=MS(si(str));
		}
	    }

	  x>>=1;
	  align<<=1;
	}
    }

  return r;

  /***
  for (i=2; i<=MSBottom; i*=2)
   {
     if (alignStringHashTable()->lookup((unsigned long)(x_&i))!=0) count++;
   }
  if (count>0)
   {
     r=gv(Et,count);
     for (j=0,i=2; i<=MSBottom; i*=2) 
      {
	if ((str=(char *)alignStringHashTable()->lookup((unsigned long)(x_&i)))!=0)
	  r->p[j++]=MS(si(str));
      }
   }
  return r;
  ***/
}

A EnumTables::formatStyle(unsigned long x_)
{
  A   r=aplus_nl;
  int i,j,count=0;
  char *str;
  
  for (i=2; i<=MSP::PosBelow; i*=2)
   {
     if (styleEnumHashTable()->lookup((unsigned long)(x_&i))!=0) count++;
   }
  if (count>0)
   {
     r=gv(Et,count);
     for (j=0,i=2; i<=MSP::PosBelow; i*=2)
      {
	if ((str=(char*)styleEnumHashTable()->lookup((unsigned long)(x_&i)))!=0) r->p[j++]=MS(si(str));
      }
   }
  return r;
}

unsigned long EnumTables::formatStyle(A sym_)
{
  unsigned long style=0;
  if (sym_!=0)
   {
     A *p=(A *)sym_->p;
     for (int i=0; i<(int)sym_->n; i++)
      {
	char *s=(char *)XS(sym_->p[i])->n;
	if (QS(p[i]))
	{
	  unsigned long val = (unsigned long)styleStringHashTable()->lookup(s);
	  style|=val;
	  if (s==0)
	   {
	     cerr << "ã ! ";
	     if (s!=0) cerr<<s;
	     cerr<<": invalid style symbol"<<endl;
	   }
	}
      }
   }
  return style;
}

unsigned long EnumTables::formatCompMode(A sym_)
{
  unsigned long mode=0;
  if (sym_!=0)
   {
     A *p=(A *)sym_->p;
     char *s=(char *)XS(sym_->p[0])->n;
     if (QS(p[0]))
     {
       unsigned long val=(unsigned long)reportCompModeStringHashTable()->lookup(s);
       mode|=val;
       if (val==0)
	{
	  cerr << "ã ! ";
	  if (s!=0) cerr<<s;
	  cerr<<": invalid computation mode symbol"<<endl;
	}
     }
   }
  return mode;
}

A EnumTables::formatCompMode(unsigned long mode_)
{
  A   r=aplus_nl;
  char *str;
  if ((str = (char *)reportCompModeEnumHashTable()->lookup(mode_))!=0)
  r = (A)MS(si(str));
  return r;
}
  

MSCycleColorMode EnumTables::cycleColorMode(A sym_)
{
  long mode=0;
  if (sym_!=0)
  {
    A *p=(A *)sym_->p;
    char *s=(char *)XS(sym_->p[0])->n;
    if (QS(p[0]))
    {
      long val=(long)cycleColorModeStringHashTable()->lookup(s);
      mode=val;
      if (val==-1)
      {
	cerr << "ã ! ";
	if (s!=0) cerr<<s;
	cerr<<": invalid computation mode symbol"<<endl;
      }
    }
  }
  return (MSCycleColorMode) mode;
}

A EnumTables::cycleColorMode(MSCycleColorMode mode_)
{
  A   r=aplus_nl;
  char *str;
  if ((str = (char *)cycleColorModeEnumHashTable()->lookup((unsigned long)mode_))!=0)
  r = (A)MS(si(str));
  return r;
}
  

