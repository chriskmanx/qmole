///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSTypes/MSTime.H>
#include <a/fncdcls.h>
#include <MSGUI/MSG.H>
#include <MSGUI/MSGUIEnum.H>
#include <AplusGUI/AplusEnumConverter.H>

unsigned long AplusEnumConverter::convert(A symbols_) const
{
  unsigned long enums=enumNotFound(); // initialize enums with an invalid value
  if (symbols_!=0)
   {
     I *p=symbols_->p;
     for (int i=0; i<(int)symbols_->n; i++)
       {
	 char *s=(char *)XS(p[i])->n;
	 if (QS(p[i]))
	   {
	     unsigned long val = enumLookup(s);
	     if (val==enumNotFound())
	       {
		 cerr << "ã ! ";
		 if (s!=0) cerr<<s;
		 cerr<<": invalid "<<type()<<" symbol"<<endl;
	       }
	     else if (enums==enumNotFound())  // if this is the first element successfully converted to enum
	       {
		 enums = val;
	       }
	     else
	       {
		 enums |= val;
	       }
	   }
       }
   }
     
  return enums;
}
  

A AplusEnumConverter::convert(unsigned long enums_) const
{
  unsigned long x=enums_, bit=0x1;
  unsigned int count=0;

  while (x!=0)
    {
      if ((x & 0x1) && stringLookup(bit)!=stringNotFound())
	{
	  count++;
	}

      x>>=1;
      bit<<=1;
    }

  if (count>0)
    {
      char *str;
      A symbols=gv(Et,count);
      x=enums_;
      bit=0x1;
      count=0;
      while (x!=0)
	{
	  if (x & 0x1)
	    {
	      str = (char *)stringLookup(bit);
	      if (str!=stringNotFound())
		{
		  symbols->p[count++]=MS(si(str));
		}
	    }

	  x>>=1;
	  bit<<=1;
	}

      return symbols;
    }
  else	// count==0
    {
      char *str = (char *)stringLookup(0); // check for `none symbol or something similar
      if (str==stringNotFound())   // there is no `none equivalent
	{
	  return aplus_nl;
	}
      else  // found the string in the hash table => there is a `none equivalent
	{
	  A symbol=gs(Et);
	  *symbol->p = MS(si(str));
	  return symbol;
	}
    }
}  


unsigned long AplusHashEnumConverter::enumLookup(const char *str_) const
{
  return (unsigned long)enumTable().lookup(str_);
}


unsigned long AplusHashEnumConverter::enumNotFound(void) const
{
  return enumTable().notFound();
}


const char *AplusHashEnumConverter::stringLookup(unsigned long enum_) const
{
  return (const char *)stringTable().lookup(enum_);
}


const char *AplusHashEnumConverter::stringNotFound(void) const
{
  return (const char *)stringTable().notFound();
}


unsigned long AplusHashSingleEnumConverter::convert(A sym_) const
{
  if (!QS(sym_) && sym_->t==Et && sym_->n>0 && QS(*sym_->p))
    {
      return fromString((char *)XS(sym_->p[0])->n);
    }
  else
    {
      return enumNotFound();
    }
}  


A AplusHashSingleEnumConverter::convert(unsigned long enum_) const
{
  A a = gs(Et); 
  *a->p = MS(si((char *)toString(enum_)));
  return a; 
}  


unsigned long AplusHashSingleEnumConverter::fromString(const char *str_) const
{
  unsigned long en = enumLookup(str_);
  if (en==enumNotFound())
    {
      cerr << "ã ! ";
      if (str_!=0) cerr<<str_;
      cerr<<": invalid "<<type()<<" symbol"<<endl;
    } 
  
  return en;
}


const char *AplusHashSingleEnumConverter::toString(unsigned long enum_) const
{
  return stringLookup(enum_);
}


const AplusHashTable& AplusShadowStyleConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("raised", (void *)MSRaised);
      table.add("sunken", (void *)MSSunken);
      table.add("etchedin", (void *)MSEtchedIn);
      table.add("etchedout", (void *)MSEtchedOut);
      table.add("flat", (void *)MSFlat);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusShadowStyleConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add((unsigned long)MSRaised, (void *)"raised");
      table.add((unsigned long)MSSunken, (void *)"sunken");
      table.add((unsigned long)MSEtchedIn, (void *)"etchedin");
      table.add((unsigned long)MSEtchedOut, (void *)"etchedout");
      table.add((unsigned long)MSFlat, (void *)"flat");

      initialized = 1;
    }

  return table;
}


const char *AplusShadowStyleConverter::type(void) const
{
  return "shadowstyle";
}


const AplusHashTable& AplusAlignmentConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("none", (void *)MSNone);
      table.add("center", (void *)MSCenter);
      table.add("left", (void *)MSLeft);
      table.add("right", (void *)MSRight);
      table.add("top", (void *)MSTop);
      table.add("bottom", (void *)MSBottom);
      table.add("inside", (void *)MSG::Inside);
      table.add("outside", (void *)MSG::Outside);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusAlignmentConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(8);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add((unsigned long)MSNone, (void *)"none");
      table.add((unsigned long)MSCenter, (void *)"center");
      table.add((unsigned long)MSLeft, (void *)"left");
      table.add((unsigned long)MSRight, (void *)"right");
      table.add((unsigned long)MSTop, (void *)"top");
      table.add((unsigned long)MSBottom, (void *)"bottom");
      table.add((unsigned long)MSG::Inside, (void *)"inside");
      table.add((unsigned long)MSG::Outside, (void *)"outside");

      initialized = 1;
    }

  return table;
}


const char *AplusAlignmentConverter::type(void) const
{
  return "alignment";
}


const AplusHashTable& AplusLineStyleConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("solid", (void *)MSSolid);
      table.add("dot", (void *)MSDot);
      table.add("dash", (void *)MSDash);

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusLineStyleConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(4);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add((unsigned long)MSSolid, (void *)"solid");
      table.add((unsigned long)MSDot, (void *)"dot");
      table.add((unsigned long)MSDash, (void *)"dash");

      initialized = 1;
    }

  return table;
}


const char *AplusLineStyleConverter::type(void) const
{
  return "linestyle";
}
