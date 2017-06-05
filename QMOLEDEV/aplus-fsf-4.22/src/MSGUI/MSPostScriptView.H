#ifndef MSPostScriptViewHEADER
#define MSPostScriptViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSP.H>

#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSBusy.H>

#include <stdio.h>

class MSPostScriptViewInChannel;
class MSPostScriptViewOutChannel;
class MSPostScriptViewDocument;

class MSGUIExport MSPostScriptView: public MSWidgetCommon, public MSP
{
public:
  
  enum ErrorCode {  NoError = 0, BadFile = 1, InvalidPostScript=2, NOGS=3 };

  static MSSymbol ghostscriptexecerror;
  static MSSymbol ghostscriptwriteerror;
  static MSSymbol ghostscriptfileerror;
  static MSSymbol ghostscriptmessage;
  
  MSPostScriptView(MSWidget *);
  ~MSPostScriptView();

  ErrorCode loadFile(const MSString& file_, int firstPage_ = 0);
  void unloadFile(void);

  void reload(void);
  void reloadPage(void);
  
  MSBoolean  isFileLoaded(void) const;
  MSBoolean isBusy(void) const    { return _busy; }
  MSBoolean isRunning(void) const { return (_pid==-1)?MSFalse:MSTrue; }
  MSBoolean isReady(void) const   { return (_pid!=-1&&_busy==MSFalse)?MSTrue:MSFalse;}

  MSBoolean showBusyCursor(void) const { return _showBusyCursor; }
  void showBusyCursor(MSBoolean sbc_);

  PageOrientation pageOrientation(void) const { return _orientation; }
  void pageOrientation(PageOrientation);

  double zoomFactor(void) const { return _zoomFactor; }
  void zoomFactor(double);

  PageSize pageSize(void) const { return _pageSize; }
  void pageSize(PageSize );

  void scrollbarBackground(const char *);
  void scrollbarBackground(unsigned long);
  unsigned long scrollbarBackground(void) const;

  void scrollInc(int);
  int scrollInc(void) const { return _hsb->inc(); }

  int  pageCount(void) const;

  int  currentPage(void) const { return _currentPage; }
  void currentPage(int);

  void nextPage(void);
  void prevPage(void);
  void firstPage(void);
  void lastPage(void);
  

  int pageWidth(void) const;
  int pageHeight(void) const;
  
  int pointPageWidth(void) const;
  int pointPageHeight(void) const;

  virtual void naturalSize(void);
  virtual void redraw(void);
  void redrawImmediately(void);

  ErrorCode printToFile(const MSString& fileName, const MSIndexVector& pageIndex);
  ErrorCode printToPrinter(const MSString& printerCmd, const MSIndexVector& pageIndex);

  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void home(void);
  virtual void end(void);
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

  const MSString& fileName(void) const { return _fileName; }

  static const MSString& gsInterpreter(void) { return _gsInterpreter; }
  static void gsInterpreter(const MSString& gsInterpreter_) { _gsInterpreter=gsInterpreter_;}

protected:
  class MSGUIExport VScrollBar : public MSVScrollBar
  {
  protected:
    virtual void change(void);
  public:
    VScrollBar(MSWidget *);
  };
  
  class MSGUIExport HScrollBar : public MSHScrollBar
  {
  protected:
    virtual void change(void);
  public:
    HScrollBar(MSWidget *);
  };

  class MSGUIExport ClipWindow : public MSWidgetCommon
  {
  public:
    ClipWindow(MSWidget *);
  };

  friend class VScrollBar;
  friend class HScrollBar;
  friend class MSPostScriptViewInChannel;
  friend class MSPostScriptViewOutChannel;

  void writeFailed(void);
  void init(void);

  double PointToPixel(double pt_,double) const;
  double PixelToPoint(double pixel_,double) const;

  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);

  void drawAreaShadow(void);
  void drawPage(void);

  MSBoolean  startInterpreter(MSBoolean p=MSFalse);
  int stopInterpreter(void);

  void interpreterFinished();
  void  setProperties(void);
  void  setEnviroment(void);

  void updateDpi(void);
  void updatePixmap(MSBoolean bCreate_=MSFalse);
  void updateRectangle(void);
  void updateFile(void);

  void setBusyState(MSBoolean);

  void  sendNextEvent(void);

  virtual void ghostScriptExecError(void);
  virtual void ghostScriptWriteError(void);
  virtual void ghostScriptFileError(const MSString&);
  virtual void ghostScriptMessage(const MSString&);

  const MSString& errorString(void) { return _errorString; }
  
  virtual void focusIn(void);
  virtual void focusOut(void);
  
  virtual void clientMessage(const XEvent *);
  virtual void configure(void);

  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  virtual void button1Press(const XEvent*);

  virtual void firstMapNotify(void);
  
  virtual void updateBusyState(void);
  virtual void pageChange(void);

  void adjustXPos(void);
  void adjustYPos(void);
  void hsbChanged(void);
  void vsbChanged(void);

  void updateHsb(void);
  void updateVsb(void);

  void clearPixmap(void);

  MSPixmap * redrawPixmap(void) { return _redrawPixmap; }
  HScrollBar *hsb(void) {return _hsb;}
  VScrollBar *vsb(void) {return _vsb;}
  ClipWindow *clipWindow(void) {return _clipWindow;}

  int xPos(void) { return _xPos; }
  void xPos(int);
  
  int yPos(void) { return _yPos; }
  void yPos(int);

  void printPages(FILE *, const MSIndexVector& pageIndex);
  
  static MSString    _gsInterpreter; 

  Atom     GHOSTVIEW;
  Atom     GHOSTVIEW_COLORS;

  Atom     PAGE;
  Atom     DONE;
  Atom     NEXT;

  double      _defaultXdpi;
  double      _defaultYdpi;

  HScrollBar *_hsb;
  VScrollBar *_vsb;
  ClipWindow *_clipWindow;
  MSPixmap *_redrawPixmap;

  double   _xdpi;
  double   _ydpi;

  int      _llx;
  int      _lly;
  int      _urx;
  int      _ury;

  PageSize _pageSize;
  PageOrientation _orientation;
  double   _zoomFactor;
  int      _currentPage;

  MSString _fileName;
  FILE   *_docFile;
  
  int   _xPos;
  int   _yPos;

  //gs info.
  int      _pid;
  Window   _replyWin;

  MSBoolean _busy;
  
  MSBoolean _showBusyCursor;
  MSShellBusy *_busyObject;

  MSPostScriptViewDocument    *_doc;
  MSPostScriptViewInChannel   *_in;
  MSPostScriptViewOutChannel  *_out;
  MSPostScriptViewOutChannel  *_err;

  int _naturalWidth;
  int _naturalHeight;

  static int _pixmapCount;
  MSString _errorString;
  MSString _pixmapName;
};
  


#endif
