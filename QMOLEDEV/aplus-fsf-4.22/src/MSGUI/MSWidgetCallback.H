#ifndef MSWidgetCallbackHEADER
#define MSWidgetCallbackHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSymbol.H>

// this should be a C++ NameSpace, but the NameSpace feature of C++ has not yet
// been implemented by any of the commercial compilers - some day we will change this.

class MSGUIExport MSWidgetCallback
{
public:
  static MSSymbol activate;
  static MSSymbol arm;
  static MSSymbol disarm;
  static MSSymbol reference;
  static MSSymbol valuechange;
  static MSSymbol save;
  static MSSymbol quit;
  static MSSymbol dismiss;
  static MSSymbol destroy;  
  static MSSymbol deleterow;
  static MSSymbol saveyourself;
  static MSSymbol selectionnotify;
  static MSSymbol selectionclear;
  static MSSymbol selection;
  static MSSymbol rowselection;
  static MSSymbol columnselection;
  static MSSymbol columnresize;
  static MSSymbol delimiterchange;
  static MSSymbol delimiterselection;
  static MSSymbol permutecolumns;
  static MSSymbol labelselection;
  static MSSymbol insertbelow;
  static MSSymbol insertabove;
  static MSSymbol tracereference;
  static MSSymbol traceptreference;
  static MSSymbol graphreference;
  static MSSymbol graphzoom;
  static MSSymbol addtrace;
  static MSSymbol copytrace;
  static MSSymbol addtexttrace;
  static MSSymbol copytexttrace;
  static MSSymbol deletetrace;
  static MSSymbol firstcolumnchange;
  static MSSymbol firstrowchange;
  static MSSymbol pagechange;
  static MSSymbol doubleclick;
  static MSSymbol increment;
  static MSSymbol decrement;
  static MSSymbol pagedecrement;
  static MSSymbol pageincrement;
  static MSSymbol takefocus;
  static MSSymbol rowmoved;
  static MSSymbol drag;
  static MSSymbol subtreecollapsed;
  static MSSymbol subtreeexpanded;
  static MSSymbol button2selection;
  static MSSymbol button3selection;
  static MSSymbol directorychange;
  static MSSymbol fileselection;
  static MSSymbol childresize;
  static MSSymbol home;
  static MSSymbol end;
  static MSSymbol yes;
  static MSSymbol no;
  static MSSymbol ok;
  static MSSymbol cancel;
  static MSSymbol help;
  static MSSymbol apply;
  static MSSymbol close;
  static MSSymbol iconized;
  static MSSymbol deiconized;
  static MSSymbol incurrentworkspace;
  static MSSymbol outofcurrentworkspace;
  static MSSymbol workspacepresencechange;
  static MSSymbol buttonactivate;
  static MSSymbol menubutton;
  static MSSymbol columnmenubutton;
  static MSSymbol editbegin;
  static MSSymbol editend;
  static MSSymbol menushow;
  static MSSymbol f1;
  static MSSymbol f2;
  static MSSymbol f3;
  static MSSymbol f4;
  static MSSymbol f5;
  static MSSymbol f6;
  static MSSymbol f7;
  static MSSymbol f8;
  static MSSymbol f9;
  static MSSymbol f10;
  static MSSymbol f11;
  static MSSymbol f12;
};

#endif

