///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSP.H>
#include <MSGUI/MSGUIEnum.H>
#include <AplusGUI/AplusHashTable.H>
#include <AplusGUI/AplusReportAlgo.H>
#include <AplusGUI/AplusReportEnum.H>

const AplusHashTable& AplusReportCompModeConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("sum", (void *)AplusReportAlgorithm::Sum);
      table.add("min", (void *)AplusReportAlgorithm::Min);
      table.add("max", (void *)AplusReportAlgorithm::Max);
      table.add("avg", (void *)AplusReportAlgorithm::Avg);
      table.add("stddev", (void *)AplusReportAlgorithm::StdDev);
      table.add("variance", (void *)AplusReportAlgorithm::Variance);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportCompModeConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(AplusReportAlgorithm::Sum, (void *)"sum");
      table.add(AplusReportAlgorithm::Min, (void *)"min");
      table.add(AplusReportAlgorithm::Max, (void *)"max");
      table.add(AplusReportAlgorithm::Avg, (void *)"avg");
      table.add(AplusReportAlgorithm::StdDev, (void *)"stddev");
      table.add(AplusReportAlgorithm::Variance, (void *)"variance");

      initialized = 1;
    }

  return table;
}


const char *AplusReportCompModeConverter::type(void) const
{
  return "compmode";
}


const AplusHashTable& AplusReportOccurrenceConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("everypage", (void *)MSP::EveryPage);
      table.add("oddpage", (void *)MSP::OddPage);
      table.add("evenpage", (void *)MSP::EvenPage);
      table.add("firstpage", (void *)MSP::FirstPage);
      table.add("lastpage", (void *)MSP::LastPage);
      table.add("anybutfirstandlast", (void *)MSP::AnyButFirstAndLast);
      table.add("diagonal", (void *)MSP::Diagonal);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportOccurrenceConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::EveryPage, (void *)"everypage");
      table.add(MSP::OddPage, (void *)"oddpage");
      table.add(MSP::EvenPage, (void *)"evenpage");
      table.add(MSP::FirstPage, (void *)"firstpage");
      table.add(MSP::LastPage, (void *)"lastpage");
      table.add(MSP::AnyButFirstAndLast, (void *)"anybutfirstandlast");
      table.add(MSP::Diagonal, (void *)"diagonal");

      initialized = 1;
    }

  return table;
}


const char *AplusReportOccurrenceConverter::type(void) const
{
  return "reportoccurrence";
}


const AplusHashTable& AplusReportStyleConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(16);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("none", (void *)MSNone);
      table.add("dunderline", (void *)MSP::Underline);
      table.add("dul", (void *)MSP::DUnderline);
      table.add("superscript", (void *)MSP::Superscript);
      table.add("subscript", (void *)MSP::Subscript);
      table.add("outline", (void *)MSP::Outline);
      table.add("smallcap", (void *)MSP::Smallcap);
      table.add("strikethru", (void *)MSP::Strikethru);
      table.add("boxl", (void *)MSP::BoxL);
      table.add("boxr", (void *)MSP::BoxR);
      table.add("boxt", (void *)MSP::BoxT);
      table.add("boxb", (void *)MSP::BoxB);
      table.add("box", (void *)MSP::Box);
      table.add("cell", (void *)MSP::Cell);
      table.add("stipple", (void *)MSP::Stipple);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportStyleConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(16);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add((unsigned long)MSNone, (void *)"none");
      table.add(MSP::Underline, (void *)"underline");
      table.add(MSP::DUnderline, (void *)"dunderline");
      table.add(MSP::Superscript, (void *)"superscript");
      table.add(MSP::Subscript, (void *)"subscript");
      table.add(MSP::Outline, (void *)"outline");
      table.add(MSP::Smallcap, (void *)"smallcap");
      table.add(MSP::Strikethru, (void *)"strikethru");
      table.add(MSP::BoxL, (void *)"boxl");
      table.add(MSP::BoxR, (void *)"boxr");
      table.add(MSP::BoxT, (void *)"boxt");
      table.add(MSP::BoxB, (void *)"boxb");
      table.add(MSP::Box, (void *)"box");
      table.add(MSP::Cell, (void *)"cell");
      table.add(MSP::Stipple, (void *)"stipple");

      initialized = 1;
    }

  return table;
}


const char *AplusReportStyleConverter::type(void) const
{
  return "reportstyle";
}


const AplusHashTable& AplusReportPageOrientationConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("portrait", (void *)MSP::Portrait);
      table.add("landscape", (void *)MSP::Landscape);
      table.add("upsidedown", (void *)MSP::UpsideDown);
      table.add("seascape", (void *)MSP::Seascape);
      table.add("default", (void *)MSP::Default);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportPageOrientationConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::Portrait, (void *)"portrait");
      table.add(MSP::Landscape, (void *)"landscape");
      table.add(MSP::UpsideDown, (void *)"upsidedown");
      table.add(MSP::Seascape, (void *)"seascape");
      table.add(MSP::Default, (void *)"default");

      initialized = 1;
    }

  return table;
}


const char *AplusReportPageOrientationConverter::type(void) const
{
  return "pageorientation";
}


const AplusHashTable& AplusReportPageSizeConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("letter", (void *)MSP::Letter);
      table.add("legal", (void *)MSP::Legal);
      table.add("a4", (void *)MSP::A4);
      table.add("b5", (void *)MSP::B5);
      table.add("a", (void *)MSP::A);
      table.add("b", (void *)MSP::B);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportPageSizeConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::Letter, (void *)"letter");
      table.add(MSP::Legal, (void *)"legal");
      table.add(MSP::A4, (void *)"a4");
      table.add(MSP::B5, (void *)"b5");
      table.add(MSP::A, (void *)"a");
      table.add(MSP::B, (void *)"b");

      initialized = 1;
    }

  return table;
}


const char *AplusReportPageSizeConverter::type(void) const
{
  return "pagesize";
}


const AplusHashTable& AplusReportOutputModeConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("ps", (void *)MSP::PS);
      table.add("eps", (void *)MSP::EPS);
      table.add("ppm", (void *)MSP::PPM);
      table.add("ascii", (void *)MSP::ASCII);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportOutputModeConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::PS, (void *)"ps");
      table.add(MSP::EPS, (void *)"eps");
      table.add(MSP::PPM, (void *)"ppm");
      table.add(MSP::ASCII, (void *)"ascii");

      initialized = 1;
    }

  return table;
}


const char *AplusReportOutputModeConverter::type(void) const
{
  return "outputmode";
}


const AplusHashTable& AplusReportDisclaimerStyleConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("text", (void *)MSP::Text);
      table.add("rule", (void *)MSP::Rule);
      table.add("toprule", (void *)MSP::Toprule);
      table.add("appendbox", (void *)MSP::AppendBox);
      table.add("append", (void *)MSP::Append);
      table.add("box", (void *)MSP::Box);
      table.add("none", (void *)MSP::NoDisclaimer);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportDisclaimerStyleConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::Text, (void *)"text");
      table.add(MSP::Rule, (void *)"rule");
      table.add(MSP::Toprule, (void *)"toprule");
      table.add(MSP::AppendBox, (void *)"appendbox");
      table.add(MSP::Append, (void *)"append");
      table.add(MSP::Box, (void *)"box");
      table.add(MSP::NoDisclaimer, (void *)"none");

      initialized = 1;
    }

  return table;
}


const char *AplusReportDisclaimerStyleConverter::type(void) const
{
  return "disclaimerstyle";
}


const AplusHashTable& AplusReportPrintModeConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("mono", (void *)MSP::Mono);
      table.add("color", (void *)MSP::Color);
      table.add("colorfg", (void *)MSP::Colorfg);
      table.add("reverse", (void *)MSP::Reverse);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusReportPrintModeConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add(MSP::Mono, (void *)"mono");
      table.add(MSP::Color, (void *)"color");
      table.add(MSP::Colorfg, (void *)"colorfg");
      table.add(MSP::Reverse, (void *)"reverse");

      initialized = 1;
    }

  return table;
}


const char *AplusReportPrintModeConverter::type(void) const
{
  return "printmode";
}
