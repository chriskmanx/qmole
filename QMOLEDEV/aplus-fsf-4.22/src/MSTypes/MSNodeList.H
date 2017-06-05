#ifndef MSNodeListHEADER
#define MSNodeListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

typedef int (*NodeSortFunc)(const void *,const void *); 

class MSTypesExport MSNodeItem
{
friend class MSNodeList;
private:
  MSNodeItem  *_next;
  MSNodeItem  *_prev;
  void        *_data;

public:
  MSNodeItem(void)    { _data=0; _next=this; _prev=this; }
  MSNodeItem(void *d) { _data=d; _next=this; _prev=this; }
  ~MSNodeItem(void);
 
  void insert(MSNodeItem *h);
  void remove(void);
  void sort(MSNodeItem *hp,NodeSortFunc func_);
  void toad(void);

  void data(void *d)        {_data=d;}
  void *data(void)          {return _data;}
  MSNodeItem *next(void)    {return _next;}
  MSNodeItem *prev(void)    {return _prev;}
  MSNodeItem *address(void) {return this;}
};

class MSTypesExport MSNodeList
{
protected:
  MSNodeItem *_first;
  MSNodeItem *_last;

public:
  
  MSNodeList(void) : _first(0),_last(0) {}
  ~MSNodeList(void) {}

  MSNodeItem *first(void) {return _first;}
  MSNodeItem *head(void)  {return _first;}
  MSNodeItem *last(void)  {return _last;}

  MSNodeList& append(MSNodeItem*);
  MSNodeList& prepend(MSNodeItem*);
  MSNodeList& unlink(MSNodeItem*);
};

#endif

  






