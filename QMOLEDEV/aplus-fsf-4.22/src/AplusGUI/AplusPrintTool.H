#ifndef AplusPrintToolHEADER
#define AplusPrintToolHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <MSGUI/MSWidgetView.H>
#include <MSGUI/MSPrintTool.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEnumConverter.H>

class AplusPrintTool : public MSWidgetView, public MSPrintTool
{
protected:
  ::A _headers;
  ::A _footers;
  ::A _banner;
  ::A _pageNumber;

public:
  AplusPrintTool(MSWidget *);
  ~AplusPrintTool(void);

  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);

  static MSBoolean verifyData(V,::A);

  inline void headers(::A h_);
  inline ::A headers(void) const;

  inline void footers(::A f_);
  inline ::A footers(void) const;

  inline void banner(::A b_);
  inline ::A banner(void) const;

  inline void pageNumber(::A pn_);
  inline ::A pageNumber(void) const;

  void constructReport(void);

  void constructHeaders(void);
  void constructBody(void);
  void constructFooters(void);
  void constructBanner(void);
  void constructPageNumber(void);

  static void constructPrintManager(MSPrintManager *, ::A);
  static void constructPrintItem(MSPrintManager *, ::A);
  static void constructPrintItem(MSPrintManager *, S);

  void constructHeaderItem(S);
  void constructFooterItem(S);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  inline AplusModel *model(void);
  inline const AplusModel *model(void) const;

  STANDARD_WIDGET_METHODS_H
};

inline void AplusPrintTool::headers(::A h_) { dc(_headers); _headers=(::A)ic(h_); }
inline ::A AplusPrintTool::headers(void) const { return _headers; }

inline void AplusPrintTool::footers(::A f_) { dc(_footers); _footers=(::A)ic(f_); }
inline ::A AplusPrintTool::footers(void) const { return _footers; }

inline void AplusPrintTool::banner(::A b_) { dc(_banner); _banner=(::A)ic(b_); }
inline ::A AplusPrintTool::banner(void) const { return _banner; }

inline void AplusPrintTool::pageNumber(::A pn_) { dc(_pageNumber); _pageNumber=(::A)ic(pn_); }
inline ::A AplusPrintTool::pageNumber(void) const { return _pageNumber; }

inline AplusModel *AplusPrintTool::model(void) { return (AplusModel *)_model; }
inline const AplusModel *AplusPrintTool::model(void) const { return (const AplusModel *)_model; }

#endif //AplusPrintToolHEADER
