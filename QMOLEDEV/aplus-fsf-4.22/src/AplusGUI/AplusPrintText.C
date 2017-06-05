///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include "AplusPrintText.H"

static const unsigned EnumHashTableSize = 128;

MSHashTable AplusPrintText::_stringEnumHashTable(EnumHashTableSize);
MSHashTable AplusPrintText::_enumHashTable(EnumHashTableSize);
MSBoolean   AplusPrintText::_initialized = MSFalse;

AplusPrintText::AplusPrintText(void)
{
  if (_initialized == MSFalse)
   {
     _initialized = MSTrue;
     initEnumHashTable();
     initStringEnumHashTable();
   }
}

AplusPrintText::~AplusPrintText(void)
{}

void AplusPrintText::initEnumHashTable(void)
{
  enumHashTable()->notFound((int)0);
  enumHashTable()->add("Evenpage" 	       	      ,(void*)EvenPage);
  enumHashTable()->add("oddpage" 	       	      ,(void*)OddPage);
  enumHashTable()->add("evenpage" 	       	      ,(void*)EvenPage);
  enumHashTable()->add("diagonal" 	       	      ,(void*)Diagonal);
  enumHashTable()->add("repetitive" 	       	      ,(void*)Repetitive);
}

void AplusPrintText::initStringEnumHashTable(void)
{
  stringEnumHashTable()->notFound((int)0);
  stringEnumHashTable()->add(EveryPage,  (void*)"everypage");
  stringEnumHashTable()->add(OddPage, 	 (void*)"oddpage");
  stringEnumHashTable()->add(EvenPage, 	 (void*)"evenpage");
  stringEnumHashTable()->add(Diagonal, 	 (void*)"diagonal");
  stringEnumHashTable()->add(Repetitive, (void*)"repetitive");
}

::A AplusPrintText::convertMode(unsigned long mode_)
{
  ::A   r=aplus_nl;
  int i,j,s=0,count=0;
  char *str;
  
  for (i=2; i<=MSBottom; i*=2)
   {
     if (stringEnumHashTable()->lookup((unsigned long)(mode_&i))!=0) count++;
   }
  if (count>0)
   {
     r=gv(Et,count);
     for (j=0,i=2; i<=MSBottom; i*=2) 
      {
	if ((str=(char *)stringEnumHashTable()->lookup((unsigned long)(mode_&i)))!=0) r->p[j++]=MS(si(str));
      }
   }
  return r;
}

unsigned long AplusPrintText::convertMode(::A sym_)
{
  unsigned long mode=0;
  if (sym_!=0)
   {
     ::A *p=(::A *)sym_->p;
     for (int i=0; i<(int)sym_->n; i++)
      {
	if (QS(p[i])) mode|=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[i])->n);
      }
   }
  return mode;
}
