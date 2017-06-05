#ifndef MSWidgetCommonHEADER
#define MSWidgetCommonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetView.H>
#include <MSTypes/MSStringVector.H>

class MSGUIExport MSWidgetCommon : public MSWidgetView
{
public:
  MSWidgetCommon(void);
  MSWidgetCommon(MSDisplayServer *);
  MSWidgetCommon(MSWidget *,const char *title_=0);  
  MSWidgetCommon(MSWidget *,const MSStringVector&);
  ~MSWidgetCommon(void);

  const MSStringVector& title(void) const;
  unsigned long titleForeground(void) const;
  Font titleFont(void) const;
  unsigned long titleAlignment(void) const;

  void title(const MSStringVector&);
  void title(const char *);
  void titleForeground(unsigned long);
  void titleForeground(const char *);
  void titleFont(Font);
  void titleFont(const char *);
  void titleAlignment(unsigned long);

  // methods for builder support
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  MSStringVector    _title;       // title of this widget
  unsigned long     _titleFg;
  Font              _titleFont;
  unsigned long     _titleAlignment;

  void init(void);
  void create(void);
  
  virtual void computeSize(void);
  virtual void updateTitle(void);
};

#ifndef MS_NO_INLINES
#ifndef MSWidgetCommonINLINES
#include <MSGUI/MSWidgetCommonInlines.C>
#endif
#endif

#endif
