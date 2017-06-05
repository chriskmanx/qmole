///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPostScriptView.H>

#include <MSIPC/MSChannel.H>
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSException.H>

#include <memory.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
typedef void Sigfunc(int);
extern "C" Sigfunc *aplus_signal(int, Sigfunc);

#if defined(MS_NEED_STRCASECMP_DECLARATION)
extern "C" int strcasecmp(const char *,const char *);
#endif

#if defined(MS_SIGNAL_HAS_DOT_PARAM)
#define SIGPARAM ...
#else
#define SIGPARAM int
#endif

MSSymbol MSPostScriptView::ghostscriptexecerror("ghostscriptexecerror");
MSSymbol MSPostScriptView::ghostscriptwriteerror("ghostscriptwriteerror");
MSSymbol MSPostScriptView::ghostscriptfileerror("ghostscriptfileerror");
MSSymbol MSPostScriptView::ghostscriptmessage("ghostscriptmessage");

class MSPostScriptViewInChannel : public MSChannel
{
public:
  MSPostScriptViewInChannel(MSPostScriptView *owner_, const char* name_,int fd_);
  ~MSPostScriptViewInChannel();

  void sendPostScript(FILE*, int, int);

  virtual void process(void);

protected:

  void writeFailed(void);
  static void writeError(SIGPARAM);
  static MSBoolean  broken_pipe;

  struct record_list
  {
    FILE *fp;
    long begin;
    unsigned int len;
    MSBoolean seek_needed;
    MSBoolean close;
    struct record_list *next;
  };

  MSPostScriptView *_owner;
  struct record_list *_inputQueue;
  char *_inputBuffer;
  char *_inputBufferPtr;
  int _bufferBytesLeft;
  int _bytesLeft;

};


class MSPostScriptViewOutChannel : public MSChannel
{
public:
  MSPostScriptViewOutChannel(MSPostScriptView* owner_, const char *name_,int fd_);
  ~MSPostScriptViewOutChannel();

  virtual void process(void);
protected:
  MSPostScriptView *_owner;
};

/*
 * This class scans the postScript file and saves the information about its structure.
 * The code is almost a verbatim copy from Ghostview implementation (after c++fication).
 */

static const int PSLINELENGTH = 257;	/* 255 characters + 1 newline + 1 NULL */

class MSPostScriptViewDocument
{
public:
  enum { LLX=0, LLY=1, URX=2, URY=3 };
  enum PageOrder {ATEND = -1, NONE = 0, PORTRAIT, LANDSCAPE, ASCEND, DESCEND, SPECIAL};

  MSPostScriptViewDocument();
  ~MSPostScriptViewDocument();

  MSBoolean scanFile(FILE *);
  void pscopy(FILE*,FILE*,long,long);
  char * pscopyuntil(FILE *, FILE*, long, long, const char *);

  int numPages(void) { return _doc==0?-1:(_doc->numpages==0?-1:(int)_doc->numpages); }

  int beginProlog(void) { return _doc->beginprolog; }
  int lenProlog(void) { return _doc->lenprolog; }

  int beginSetup(void) { return _doc->beginsetup; }
  int lenSetup(void) { return _doc->lensetup; }

  PageOrder pageOrder(void) { return (PageOrder)_doc->pageorder; }
  PageOrder defaultPageOrientation(void) { return (PageOrder)((_doc==0)?NONE:_doc->default_page_orientation); }

  PageOrder pageOrientation(void) { return (PageOrder)((_doc==0)?NONE:_doc->orientation); }

  int pageStart(int i) { return _doc->pages[i].begin; }
  int pageLen(int i)   { return _doc->pages[i].len; }

public:
//protected:


  struct documentmedia {
    char *name;
    int width, height;
};

  //This to make xlC happy.
  struct page;
  friend struct page;
  struct document;
  friend struct document;
  
  struct page {
    char *label;
    int  boundingbox[4];
    struct documentmedia *media;
    int  orientation;			/* PORTRAIT, LANDSCAPE */
    long begin, end;			/* offsets into file */
    unsigned int len;
  };

  struct document {
    int  epsf;				/* Encapsulated PostScript flag. */
    char *title;			/* Title of document. */
    char *date;				/* Creation date. */
    int  pageorder;			/* ASCEND, DESCEND, SPECIAL */
    long beginheader, endheader;	/* offsets into file */
    unsigned int lenheader;
    long beginpreview, endpreview;
    unsigned int lenpreview;
    long begindefaults, enddefaults;
    unsigned int lendefaults;
    long beginprolog, endprolog;
    unsigned int lenprolog;
    long beginsetup, endsetup;
    unsigned int lensetup;
    long begintrailer, endtrailer;
    unsigned int lentrailer;
    int  boundingbox[4];
    int  default_page_boundingbox[4];
    int  orientation;			/* PORTRAIT, LANDSCAPE */
    int  default_page_orientation;	/* PORTRAIT, LANDSCAPE */
    unsigned int nummedia;
    struct documentmedia *media;
    struct documentmedia *default_page_media;
    unsigned int numpages;
    struct page *pages;
};

  document * _doc;

  static documentmedia papersizes[];
  
  int blank(char *);
  char *readline(char*,int,FILE*,long*,unsigned int*);

#ifdef gettext
#undef gettext
#endif

  char *gettext(char*,char**);
  char *gettextline(char*);

  void freeDoc(document *);
};

extern int MSPageSizeXTable[];
extern int MSPageSizeYTable[];

static const double MSPostScriptViewMaxZoom = 4.0;
static const int MSPostScriptViewDefaultScrollBarSize = 15;
static const int MSPostScriptViewDefaultScrollBarInc = 5;
static const int MSPostScriptViewDefaultWidth=500;
static const int MSPostScriptViewDefaultHeight=300;
static const unsigned long MSPostScriptViewEventMask =
(ExposureMask|PropertyChangeMask|ButtonPressMask/*|ButtonReleaseMask*/);

int MSPostScriptView::_pixmapCount=0;
MSString MSPostScriptView::_gsInterpreter="gs";

MSPostScriptViewOutChannel::MSPostScriptViewOutChannel(MSPostScriptView* owner_,const char* name_,int fd_) :
    _owner(owner_),MSChannel(name_,fd_,Default,Read,0)
{
  enable();
}

MSPostScriptViewOutChannel::~MSPostScriptViewOutChannel()
{
  close(fd());
}

void MSPostScriptViewOutChannel::process(void)
{
  //got output from gs.
  //just read and ignore for now.
  //
  int bytes;
  char buf[BUFSIZ];
  bytes=read(fd(),buf,BUFSIZ);
  if(bytes==0)
   {
     disable();
     if(_owner->_replyWin!=0) _owner->stopInterpreter();
   }
  else if(bytes==-1)
   {
     MSMessageLog::errorMessage("MSPostscriptViewer: gs failed on fd:%d\n", fd());
   }
  else if(bytes >0)
   {
     buf[bytes]='\0';
     MSString str(buf);
     if(str.indexOf("MSFailure")!=str.length())
      {
        _owner->ghostScriptExecError();
      }
     if(str.indexOf("Error")!=str.length())
      {
        _owner->stopInterpreter();
        _owner->ghostScriptFileError(str);
      }
     else if(str.indexOf("GS>")!=str.length())
      {
        _owner->interpreterFinished();
      }
     else
      {
        _owner->ghostScriptMessage(str);
      }
   }
}


MSPostScriptViewInChannel::MSPostScriptViewInChannel(MSPostScriptView* owner_,const char* name_,int fd_) :
    _owner(owner_),MSChannel(name_,fd_,Default,Write,0)
{
  _inputBuffer=0;
  _inputBufferPtr=0;
  _inputQueue=0;
  _bytesLeft=0;
  _bufferBytesLeft=0;
}

MSPostScriptViewInChannel::~MSPostScriptViewInChannel()
{
  while(_inputQueue)
   {
     record_list *ps_old=_inputQueue;
     _inputQueue = _inputQueue->next;
     if(ps_old->close) fclose(ps_old->fp);
     free(ps_old);
   }
  if(_inputBuffer!=0) free(_inputBuffer);
  close(fd());
}

MSBoolean MSPostScriptViewInChannel::broken_pipe=MSFalse;


void MSPostScriptViewInChannel::writeError(SIGPARAM)
{
  broken_pipe=MSTrue;
}

void MSPostScriptViewInChannel::process(void)
{
    int bytes_written;
    void (*oldsig)(SIGPARAM);
    oldsig = aplus_signal(SIGPIPE, writeError);
    
#ifdef NON_BLOCKING_IO
    do {
#endif

	if (_bufferBytesLeft == 0) {

	    /* Get a new section if required */
	    if (_inputQueue && _bytesLeft == 0) {
		record_list *ps_old = _inputQueue;
		_inputQueue = ps_old->next;
		if (ps_old->close) fclose(ps_old->fp);
		free((char *)ps_old);
	    }

	    /* Have to seek at the beginning of each section */
	    if (_inputQueue &&
		_inputQueue->seek_needed) {
		if (_inputQueue->len > 0)
		    fseek(_inputQueue->fp,
			  _inputQueue->begin, SEEK_SET);
		_inputQueue->seek_needed = MSFalse;
		_bytesLeft = _inputQueue->len;
	    }

	    if (_bytesLeft > BUFSIZ) {
		_bufferBytesLeft =
			fread(_inputBuffer,
			      sizeof (char), BUFSIZ,
			      _inputQueue->fp);
	    } else if (_bytesLeft > 0) {
		_bufferBytesLeft =
			fread(_inputBuffer,
			      sizeof (char), _bytesLeft,
			      _inputQueue->fp);
	    } else {
		_bufferBytesLeft = 0;
	    }
	    if (_bytesLeft > 0 &&
		_bufferBytesLeft == 0) {
              writeFailed();	/* Error occurred */
              aplus_signal(SIGPIPE, oldsig);
              return;
	    }
	    _inputBufferPtr = _inputBuffer;
	    _bytesLeft -= _bufferBytesLeft;
	}

	if (_bufferBytesLeft > 0) {
	    bytes_written = write(fd(),
				  _inputBufferPtr,
				  _bufferBytesLeft);

	    if (broken_pipe) {
		broken_pipe = MSFalse;
		writeFailed();		/* Something bad happened */
                aplus_signal(SIGPIPE, oldsig);
                return;
	    } else if (bytes_written == -1) {
		if ((errno != EWOULDBLOCK) && (errno != EAGAIN)) {
		    writeFailed();	/* Something bad happened */
                    aplus_signal(SIGPIPE, oldsig);
                    return;
		}
	    } else {
		_bufferBytesLeft -= bytes_written;
		_inputBufferPtr += bytes_written;
	    }
	}
#ifdef NON_BLOCKING_IO
    } while(_inputQueue && _bufferBytesLeft == 0);
#endif
    aplus_signal(SIGPIPE, oldsig);
    if (_inputQueue == NULL && _bufferBytesLeft == 0)
     {
       disable();
     } 
}

void MSPostScriptViewInChannel::sendPostScript(FILE *fp,int begin, int len)
{
  struct record_list *ps_new;
  
    ps_new = (struct record_list *)malloc(sizeof (struct record_list));
    ps_new->fp = fp;
    ps_new->begin = begin;
    ps_new->len = len;
    ps_new->seek_needed = MSTrue;
    ps_new->close = MSFalse;
    ps_new->next = NULL;

    if (_inputBuffer == NULL) {
	_inputBuffer = (char *)malloc(BUFSIZ);
    }

    if (_inputQueue == NULL) {
	_bytesLeft = len;
        _bufferBytesLeft=0;
	_inputQueue = ps_new;
        enable();
    } else {
	record_list *p = _inputQueue;
	while (p->next != NULL) {
	    p = p->next;
	}
	p->next = ps_new;
    }
}

void MSPostScriptViewInChannel::writeFailed(void)
{
  disable();
  _owner->writeFailed();
}

MSPostScriptView::HScrollBar::HScrollBar(MSWidget *pOwner_)
    :MSHScrollBar(pOwner_)
{
  min(0);
  inc(MSPostScriptViewDefaultScrollBarInc);
  pageInc(1);
  _highlightThickness=0;
  _acceptFocus=MSFalse;
  height(MSPostScriptViewDefaultScrollBarSize);
}

void MSPostScriptView::HScrollBar::change(void)
{
  MSPostScriptView *pView = (MSPostScriptView*)owner();
  if(pView->acceptFocus()==MSTrue) traverseFocus(pView);
  pView->hsbChanged();
}

MSPostScriptView::VScrollBar::VScrollBar(MSWidget *pOwner_)
    :MSVScrollBar(pOwner_)
{
  min(0);
  inc(MSPostScriptViewDefaultScrollBarInc);
  pageInc(1);
  _highlightThickness=0;
  _acceptFocus=MSFalse;
  width(MSPostScriptViewDefaultScrollBarSize);
}

void MSPostScriptView::VScrollBar::change(void)
{
  MSPostScriptView *pView = (MSPostScriptView*)owner();
  if(pView->acceptFocus()==MSTrue) traverseFocus(pView);
  pView->vsbChanged();
}

MSPostScriptView::ClipWindow::ClipWindow(MSWidget *owner_)
: MSWidgetCommon(owner_)
{
  _highlightThickness=0;
  _shadowThickness=0;
  _acceptFocus=MSFalse;
}

MSPostScriptView::MSPostScriptView(MSWidget *pOwner_) 
    :MSWidgetCommon(pOwner_)
{
  _pixmapName="MSPostScriptView" +MSString(_pixmapCount);
  _pixmapCount++;
  init();
}

MSPostScriptView::~MSPostScriptView()
{
  stopInterpreter();
  if(redrawPixmap()!=0) delete _redrawPixmap;
  if(_doc!=0) delete _doc;
  if(_hsb!=0) _hsb->destroy();
  if(_vsb!=0) _vsb->destroy();
  if(_clipWindow!=0) _clipWindow->destroy();
  if(_docFile!=0) fclose(_docFile);
}

void MSPostScriptView::scrollInc(int inc_)
{
  _hsb->inc(inc_);
  _vsb->inc(inc_);
}

double MSPostScriptView::PointToPixel(double pt_, double dpi_) const
{
  return pt_ *dpi_/72.0;
}

double MSPostScriptView::PixelToPoint(double pixel_, double dpi_) const
{
  return pixel_ *72.0/dpi_;
}


void MSPostScriptView::init(void)
{
  GHOSTVIEW=XInternAtom(display(),"GHOSTVIEW",False);
  GHOSTVIEW_COLORS=XInternAtom(display(),"GHOSTVIEW_COLORS",False);
  PAGE=XInternAtom(display(),"PAGE",False);
  DONE=XInternAtom(display(),"DONE",False);
  NEXT=XInternAtom(display(),"NEXT",False);
  _defaultXdpi=25.4*WidthOfScreen(server()->screen())/WidthMMOfScreen(server()->screen());
  _defaultYdpi=25.4*HeightOfScreen(server()->screen())/HeightMMOfScreen(server()->screen());

  _vsb=new VScrollBar(this);
  _hsb=new HScrollBar(this);
  _clipWindow=new ClipWindow(this);
 
  _pid=-1;
  _replyWin=0;
  _xPos = _yPos=0;
  
  _orientation=Portrait;
  _pageSize = Letter;
  _zoomFactor = 1.0;
  _redrawPixmap=0;
  updateDpi();
  updateRectangle();
  updatePixmap();
  _busy = MSFalse;
  _showBusyCursor = MSFalse;
  _busyObject = 0;
  selectInput(MSPostScriptViewEventMask);
  _acceptFocus=MSTrue;
  addToFocusList();
  _highlightThickness=0;
  _shadowThickness=2;
  _docFile=0;
  _doc=0;
  _in=0;
  _out=0;
  _err=0;
  resize(MSPostScriptViewDefaultWidth,MSPostScriptViewDefaultHeight);
}

void MSPostScriptView::firstMapNotify(void)
{
  _naturalWidth=width();
  _naturalHeight=height();
}

void MSPostScriptView::naturalSize(void)
{ resize(_naturalWidth,_naturalHeight); }


void MSPostScriptView::focusIn(void)
{ highlight(); }

void MSPostScriptView::focusOut(void)
{ unHighlight(); }

void MSPostScriptView::button1Press(const XEvent*)
{ if(acceptFocus()==MSTrue) traverseFocus(this); }


void MSPostScriptView::showBusyCursor(MSBoolean sbc_)
{
  _showBusyCursor = sbc_;
  if(isBusy()==MSTrue)
   {
     if(showBusyCursor()==MSTrue)
      {
        if(_busyObject == 0)_busyObject=new MSShellBusy((MSShell *)top());
      }
     else if(_busyObject != 0)
      {
        delete _busyObject;
        _busyObject=0;
      }
   }
}

void MSPostScriptView::updateBusyState(void)
{
  if(_busy == MSFalse)
   {
     if(_busyObject!=0)
      {
        delete _busyObject;
        _busyObject=0;
      }
   }
  else if(showBusyCursor()==MSTrue)
   {
     _busyObject = new MSShellBusy((MSShell *)top());
   }
}

void MSPostScriptView::setBusyState(MSBoolean busy_)
{
  if(_busy!=busy_)
   {
     _busy = busy_;
     updateBusyState();
   }
}

void MSPostScriptView::keyPress(const XEvent *pEvent_,KeySym k_,unsigned int state_,const char *)
{
  MSKeyPress keyPress(k_, state_);
  if (sensitive()==MSTrue&& keyTranslate(keyPress)==MSFalse)
   {
     switch (k_) 
      {
      case XK_Up:     up();        break;
      case XK_Down:   down();      break;
      case XK_Left:   left();      break;
      case XK_Right:  right();     break;
      case XK_F29:
      case XK_Prior:  pageUp();    break;
      case XK_F35:
      case XK_Next:   pageDown();  break;
      case XK_F27:
      case XK_Home:   home();      break;
      case XK_R13:
      case XK_End:    end();       break;
      case XK_period: reloadPage(); break;
      }
   }
}

void MSPostScriptView::pageUp(void)
{
  prevPage();
  pageChange();
}

void MSPostScriptView::pageDown(void)
{
  nextPage();
  pageChange();
}

void MSPostScriptView::home(void)
{
  firstPage();
  pageChange();
}

void MSPostScriptView::end(void)
{
  lastPage();
  pageChange();
}

void MSPostScriptView::up(void)
{
  if(vsb()->value() > vsb()->min())
   {
     vsb()->value(vsb()->value()-scrollInc());
   }
}

void MSPostScriptView::down(void)
{
 if(vsb()->value() < vsb()->max())
   {
     vsb()->value(vsb()->value()+scrollInc());
   }
}

void MSPostScriptView::left(void)
{
  if(hsb()->value() > hsb()->min())
   {
     hsb()->value(hsb()->value()-1);
   }
}

void MSPostScriptView::right(void)
{
 if(hsb()->value() < hsb()->max())
   {
     hsb()->value(hsb()->value()+1);
   }
}

void MSPostScriptView::firstPage(void)
{
  if(isBusy()==MSTrue) return;
  if (_doc!=0 && pageCount()>0 && currentPage()!=0) currentPage(0); 
}

void MSPostScriptView::lastPage(void)
{
  if(isBusy()==MSTrue) return;
  if (_doc!=0 && pageCount()>0 && currentPage()!=(pageCount()-1)) currentPage(pageCount()-1); 
}

void MSPostScriptView::prevPage(void)
{
  if(isBusy()==MSTrue) return;  
  if(_doc!=0 && _doc->numPages()>0 && currentPage()>0)
   {
     currentPage(currentPage()-1);
   }
}

void MSPostScriptView::nextPage(void)
{
  if(isBusy()==MSTrue) return;
  if(_doc!=0 && _doc->numPages()>0)
   {
     if(currentPage()<pageCount()-1)  currentPage(currentPage()+1);
   }
  else
   {
     if(isReady()==MSTrue)
      {
        sendNextEvent();
        _currentPage++;
      }
   }
}

MSBoolean MSPostScriptView::isFileLoaded(void) const
{
  return (_docFile!=0)?MSTrue:MSFalse;
}

void MSPostScriptView::xPos(int xPos_)
{
  if(_xPos != xPos_)
   {
     _xPos = xPos_;
     if(firstMap()==MSTrue)
      {
        adjustXPos();
        drawPage();
      }
   }
}

void MSPostScriptView::yPos(int yPos_)
{
  if(_yPos != yPos_)
   {
     _yPos = yPos_;
     if(firstMap()==MSTrue)
      {
        adjustYPos();
        drawPage();
      }
   }
}

void MSPostScriptView::redrawImmediately(void)
{
  if(mapped()==MSTrue) redraw();
}

void MSPostScriptView::adjustXPos(void)
{
  if(hsb()->mapped()==MSTrue)
   {
     int w = width() - 2 *highlightThickness() - 2*shadowThickness();
     if(vsb()->mapped()==MSTrue) w-=vsb()->width();
     w=(w>0)?w:1;
     
     if (xPos()+w>pageWidth())
      {
        if (w>pageWidth()) _xPos=0;
        else _xPos = pageWidth()-w;
      }
   }
  else _xPos =0;
  hsb()->valueChange(xPos());
}

void MSPostScriptView::adjustYPos(void)
{
  if(vsb()->mapped()==MSTrue)
   {
     int h=height()-highlightThickness()*2 -shadowThickness()*2;
     if(hsb()->mapped()==MSTrue) h-=hsb()->height();
     
     if (yPos()+h>pageHeight())
      {
        if (h>pageHeight()) _yPos=0;
        else _yPos = pageHeight()-h;
      }
   }
  else _yPos =0;
  vsb()->valueChange(yPos());
}


void MSPostScriptView::drawPage(void)
{
     int w,h;
     int offset = highlightThickness() + shadowThickness();
     w=width()-2*offset;
     if(vsb()->mapped()==MSTrue) w-= vsb()->width();
     h=height()-2*offset;
     if(hsb()->mapped()==MSTrue) h-= hsb()->height();
     w= MSUtil::min(pageWidth(), w);
     h= MSUtil::min(pageHeight(), h);
     
     XCopyArea(display(),redrawPixmap()->pixmap(),window(),
               backgroundShadowGC(),
               _xPos,_yPos,w,h,offset,offset);
}

void MSPostScriptView::redraw(void)
{
  if(isBusy() == MSFalse && redrawPixmap()!=0)
   {
     drawPage();
   }
  else  drawBackground();
  drawAreaShadow();
  if(highlighted() == MSTrue) drawHighlight();
}

void MSPostScriptView::drawAreaShadow(void)
{
  if (shadowThickness()>0)
   {
     int viewWidth=width()-highlightThickness()*2;
     viewWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
     int viewHeight=height()-highlightThickness()*2;
     viewHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
     drawBevel(window(),MSRect(highlightThickness(),highlightThickness(),viewWidth,viewHeight),shadowStyle(),shadowThickness());
   }
}

void MSPostScriptView::updateDpi(void)
{
  _xdpi = _defaultXdpi *_zoomFactor;
  _ydpi = _defaultYdpi *_zoomFactor;
}


void MSPostScriptView::ghostScriptFileError(const MSString& str_)
{
  _errorString=str_;
  if(activateCallback(MSPostScriptView::ghostscriptfileerror)==MSFalse)
   {
     MSMessageLog::errorMessage("MSPostScriptView: Error in postscript file:\n%s\n",str_.string());
   }
  
}

void MSPostScriptView::ghostScriptMessage(const MSString& str_)
{
  _errorString=str_;
   if(activateCallback(MSPostScriptView::ghostscriptmessage)==MSFalse)
   {
     MSMessageLog::warningMessage(str_.string());
   }
}


void MSPostScriptView::ghostScriptExecError(void)
{
  _errorString="Unable to exec gs";
  if(activateCallback(MSPostScriptView::ghostscriptexecerror)!=MSTrue)
   {
     MSMessageLog::errorMessage("MSPostScriptView: %s\n",_errorString.string());
   }
}

void MSPostScriptView::ghostScriptWriteError(void)
{
  _errorString="Failed writing to gs";
  if(activateCallback(MSPostScriptView::ghostscriptwriteerror)!=MSTrue)
   {
     MSMessageLog::errorMessage("MSPostScriptView: %s\n",_errorString.string());
   }
}


void MSPostScriptView::sendNextEvent(void)
{
  if(_replyWin != 0)
   {
     XEvent ev;
     ev.type = ClientMessage;
     ev.xclient.message_type=NEXT;
     ev.xclient.window=_replyWin;
     ev.xclient.display=display();
     ev.xclient.format=32;
     XSendEvent(display(),_replyWin,False,0,&ev);
     XFlush(display());
     setBusyState(MSTrue);     
   }
}

void MSPostScriptView::interpreterFinished(void)
{
  stopInterpreter();
}


void MSPostScriptView::clientMessage(const XEvent *pEvent_)
{
  if(pEvent_->xclient.message_type==PAGE)
   {
     _replyWin=pEvent_->xclient.data.l[0];
     setBusyState(MSFalse);
     drawPage();
   }
  else if(pEvent_->xclient.message_type==DONE)
   {
     stopInterpreter();
   }
  else
   {
     // This should never happen.
     // However this happens on irix.6.2, n32 with buggy libX11.so.1
     // XInternAtom would sometimes return bogus values.
     // Patch for libX11.so.1 is available from SGI.
     char *s=0;
     s=XGetAtomName(server()->display(),pEvent_->xclient.message_type);
     if(strcmp(s,"PAGE")==0)
      {
        PAGE=pEvent_->xclient.message_type;
        _replyWin=pEvent_->xclient.data.l[0];
        setBusyState(MSFalse);
        drawPage();
      }
     else if(strcmp(s,"DONE")==0)
      {
        DONE=pEvent_->xclient.message_type;
        stopInterpreter();
      }
     XFree(s);
   }
}


int MSPostScriptView::pageCount() const
{
  if(_doc == 0) return -1;
  else return _doc->numPages();
}

void MSPostScriptView::pageChange(void)
{
  activateCallback(MSWidgetCallback::pagechange);
}

void MSPostScriptView::currentPage(int pageNumber)
{
  if(isFileLoaded()==MSFalse) return;
  int i;
  
  if(pageCount() >0)
   {
     if(pageNumber >= _doc->numPages()) pageNumber=_doc->numPages() -1;
     else if(pageNumber <0) pageNumber =0;

     if(isReady()==MSTrue) sendNextEvent();
     else
      {
        stopInterpreter();
        startInterpreter();
        _in->sendPostScript(_docFile,_doc->beginProlog(), _doc->lenProlog());
        _in->sendPostScript(_docFile, _doc->beginSetup(), _doc->lenSetup());
      }
     
     if(_doc->pageOrder()== MSPostScriptViewDocument::DESCEND)
       i=(_doc->numPages() -1) - pageNumber;
     else
       i=pageNumber;
     _in->sendPostScript(_docFile, _doc->pageStart(i),_doc->pageLen(i));
     _currentPage = pageNumber;
   }
  else
   {
     if(isRunning()==MSFalse)
      {
        startInterpreter(MSTrue);
        _currentPage=0;
      }
     else
      {
        if(isReady()==MSTrue) sendNextEvent();
        _currentPage++;
      }
   }
  setBusyState(MSTrue);
}


void MSPostScriptView::updateForeground(unsigned long fg_)
{
  MSWidgetCommon::updateForeground(fg_);
  updatePixmap(MSTrue);
  if(isFileLoaded()==MSTrue)
   {
     stopInterpreter();
     currentPage(currentPage());
     pageChange();
   }
}

void MSPostScriptView::updateBackground(unsigned long oldBg_)
{
  MSWidgetCommon::updateBackground(oldBg_);
  updatePixmap(MSTrue);
  if(isFileLoaded()==MSTrue)
   {
     stopInterpreter();
     currentPage(currentPage());
     pageChange();
   }
}

unsigned long MSPostScriptView::scrollbarBackground(void) const
{ return _hsb->background(); }

void MSPostScriptView::scrollbarBackground(const char * bg_)
{ scrollbarBackground(server()->pixel(bg_)); }

void MSPostScriptView::scrollbarBackground(unsigned long bg_)
{
  if(bg_!= scrollbarBackground())
   {
     hsb()->background(bg_);
     vsb()->background(bg_);
     clipWindow()->background(bg_);
   }
}


int MSPostScriptView::pointPageWidth(void) const
{
  if(pageOrientation()==Portrait||pageOrientation()==UpsideDown)
      return MSPageSizeXTable[pageSize()-MSP::Letter];
  else return MSPageSizeYTable[pageSize()-MSP::Letter];
}

int MSPostScriptView::pageWidth(void) const
{
  return ((int)PointToPixel(pointPageWidth(),_xdpi));
}

int MSPostScriptView::pointPageHeight(void) const
{
  if(pageOrientation()==Portrait||pageOrientation()==UpsideDown)
     return MSPageSizeYTable[pageSize()-MSP::Letter];
  else return MSPageSizeXTable[pageSize()-MSP::Letter];
}

int MSPostScriptView::pageHeight(void) const
{
  return ((int)PointToPixel(pointPageHeight(),_ydpi));
}

void MSPostScriptView::updateRectangle(void)
{
  int ph=pointPageHeight();
  int pw=pointPageWidth();
  _llx=_lly=0;
  switch(pageOrientation())
   {
   case Landscape:
   case Seascape:
     _urx=ph;
     _ury=pw;
     break;
   case Portrait:
   case UpsideDown:
     _urx=pw;
     _ury=ph;
     break;
   }
}

void MSPostScriptView::zoomFactor(double zoomFactor_)
{
  if(_zoomFactor != zoomFactor_)
   {
     if(zoomFactor_ <=0.0 || zoomFactor_ > MSPostScriptViewMaxZoom ||
        zoomFactor_ *_defaultXdpi < 1.0 ||zoomFactor_*_defaultYdpi <1.0) return;
     _zoomFactor= zoomFactor_;
     updateDpi();
     updateRectangle();
     updatePixmap();
     if(isFileLoaded()==MSTrue)
      {
        stopInterpreter();
        currentPage(currentPage());
      }
    }
}

void MSPostScriptView::pageSize(MSP::PageSize pageSize_)
{
  if(pageSize() != pageSize_)
   {
     _pageSize = pageSize_;
     updateRectangle();
     updatePixmap();
     if(isFileLoaded()==MSTrue)
      {
        stopInterpreter();
        currentPage(currentPage());
      }
   }
}

void MSPostScriptView::updatePixmap(MSBoolean )
{
  if(redrawPixmap()!=0) delete _redrawPixmap;
  int pw=pageWidth();
  int ph=pageHeight(); 
  _redrawPixmap=new MSPixmap(server(),_pixmapName.string(),pw,ph,
                             foreground(),background());
  clearPixmap();
  configure();
}

void MSPostScriptView::clearPixmap(void)
{
  XFillRectangle(display(),_redrawPixmap->pixmap(),
                 backgroundShadowGC(),
                 0,0,pageWidth()+1,pageHeight()+1);

  XClearWindow(display(),window());
}

     
void MSPostScriptView::pageOrientation(PageOrientation orientation_)
{
  if(pageOrientation()!=orientation_)
   {
     _orientation=orientation_;
     updateRectangle();
     updatePixmap();
     if(isFileLoaded()==MSTrue)
      {
        stopInterpreter();
        currentPage(currentPage());
      }
   }
}

void MSPostScriptView::setProperties(void)
{
  int angle=0;
  switch(pageOrientation())
   {
   case Portrait:   angle=0;   break;
   case Landscape:  angle=90;  break;
   case UpsideDown: angle=180; break;
   case Seascape:   angle=270; break;
   }
     
  char buf[256];
  sprintf(buf, "%d %d %d %d %d %d %f %f", // %d %d %d %d",
          0,angle,0,0,(int)_urx, (int)_ury, _xdpi,_ydpi);

  XChangeProperty(display(), window(), GHOSTVIEW,
                  XA_STRING,8,PropModeReplace,
                  (unsigned char *)buf, strlen(buf));
  sprintf(buf,"Color %d %d", foreground(), background());
  XChangeProperty(display(), window(), GHOSTVIEW_COLORS,
                  XA_STRING,8,PropModeReplace,
                  (unsigned char *)buf, strlen(buf));

  XSync(display(),False); /* update the properties */
}

void MSPostScriptView::setEnviroment(void)
{
  static char buf[256];
  sprintf(buf,"GHOSTVIEW=%ld %ld",window(),redrawPixmap()->pixmap());
  putenv(buf);
}

void MSPostScriptView::configure(void)
{
//  updateRectangle();
  int border=(shadowThickness()+highlightThickness())*2;
  int requiredWidth = pageWidth()+border;
  int requiredHeight = pageHeight()+border;
  if (width()>=requiredWidth) hsb()->hide();
  else
   {
     hsb()->show();
     requiredHeight+=hsb()->height();
   }
  if(height()>=requiredHeight) vsb()->hide();
  else
   {
     vsb()->show();
     if (hsb()->mapped()==MSFalse)
      {
        requiredWidth+=vsb()->width();
        if (width()<requiredWidth) hsb()->show();
      }
   }
  adjustXPos();
  adjustYPos();
  updateHsb();
  updateVsb();
  if(vsb()->mapped()==MSTrue&&hsb()->mapped()==MSTrue)
   {
     clipWindow()->resize(vsb()->width(),hsb()->height());
     clipWindow()->moveTo(hsb()->x()+hsb()->width(),vsb()->y()+vsb()->height());
     clipWindow()->show();
   }
  else clipWindow()->hide();
}

void MSPostScriptView::updateHsb(void)
{
  if (hsb()->mapped()==MSTrue)
   {
     int w=width()-highlightThickness()*2;
     if(vsb()->mapped()==MSTrue) w-=vsb()->width();
     w=(w>0)?w:1;
     hsb()->width(w);
     hsb()->moveTo(highlightThickness(),height()-highlightThickness()-hsb()->height());
     hsb()->max(pageWidth());
     int viewSize=width()-highlightThickness()*2-shadowThickness()*2;
     if (vsb()->mapped()==MSTrue) viewSize-=vsb()->width();
     hsb()->viewSize(viewSize);
     hsb()->pageInc(viewSize);
   }
}

void MSPostScriptView::updateVsb(void)
{
  if (vsb()->mapped()==MSTrue)
   {
     int h =height()-highlightThickness()*2;
     if(hsb()->mapped()==MSTrue) h-=hsb()->height();
     h=(h>0)?h:1;
     vsb()->height(h);
     vsb()->moveTo(width()-highlightThickness()-vsb()->width(),highlightThickness());
     vsb()->max(pageHeight());
     int viewSize=height()-highlightThickness()*2-shadowThickness()*2;
     if (hsb()->mapped()==MSTrue) viewSize-=hsb()->height();
     vsb()->viewSize(viewSize);
     vsb()->pageInc(viewSize);
   }
}

void MSPostScriptView::hsbChanged(void)
{
  xPos(hsb()->value());
}

void MSPostScriptView::vsbChanged(void)
{
  yPos(vsb()->value());
}

void MSPostScriptView::writeFailed()
{
  if(_replyWin==0)
   {
     if(stopInterpreter()==5) ghostScriptExecError();
     else ghostScriptFileError("");
   }
  else
   {
     stopInterpreter();
     ghostScriptWriteError();
   }
}


int MSPostScriptView::stopInterpreter(void)
{
  int res =-1;
  pid_t ret_pid;
  if(_pid!= -1)
   {
     int stat_loc;
     kill(_pid, SIGTERM);
     ret_pid=waitpid(_pid,&stat_loc,0);
#ifdef MS_WAIT_RETURNS_STATUS
     stat_loc=(int)ret_pid;
#endif     
#ifdef MS_WAIT_MACROS_NEED_UNION_CAST
     union wait *status=(union wait*)&stat_loc;
#else
     int *status=&stat_loc;
#endif
     if(WIFEXITED((*status))) res=WEXITSTATUS((*status));
     _pid=-1;
     _replyWin=0;
   }
  if(_in!=0)  { delete _in; _in=0;}
  if(_out!=0) { delete _out; _out=0;}
  if(_err!=0) { delete _err; _err=0; }
  
  setBusyState(MSFalse);
  return res;
}

void MSPostScriptView::unloadFile(void)
{
  stopInterpreter();
  clearPixmap();
  if(_docFile!=0) fclose(_docFile);
  _docFile=0;
  if(_doc!=0) delete _doc;
  _doc=0;
}

MSPostScriptView::ErrorCode MSPostScriptView::loadFile(const MSString& file_, int startPage_)
{
  if(_docFile!=0) fclose(_docFile);
  _fileName = file_;
  _docFile = fopen(_fileName.string(), "r");
  if(_docFile==0)
   {
     clearPixmap();
     return BadFile;
   }
  
  if(_doc!=0) delete _doc;
  _doc= new MSPostScriptViewDocument();
  if(_doc->scanFile(_docFile) == MSFalse)
   {
     _doc->_doc=0;
   }
  stopInterpreter();
  if(_doc!=0)
   {
     MSPostScriptViewDocument::PageOrder orient;
     orient=_doc->defaultPageOrientation();
     if(orient==MSPostScriptViewDocument::NONE) orient=_doc->pageOrientation();
     switch(orient)
      {
      case MSPostScriptViewDocument::PORTRAIT:
        pageOrientation(MSP::Portrait);
        break;
      case MSPostScriptViewDocument::LANDSCAPE:
        pageOrientation(MSP::Landscape);
        break;
      default:
        break;
      }
   }
  currentPage(startPage_);
  return NoError;
}

  
MSBoolean MSPostScriptView::startInterpreter(MSBoolean passFile_)
{
  static char buf1[256];
  static char buf2[256];
  
  setProperties();
  int	std_in[2];
  int	std_out[2];
  int	std_err[2];
  int   argc=0;
  const char *argv[10];
  int res=0;
  

  strcpy(buf1,_gsInterpreter.string());
  strcpy(buf2,_fileName.string());
  argv[argc++] = buf1;
  argv[argc++] = "-sDEVICE=x11";
  argv[argc++] = "-dNOPAUSE";
  argv[argc++] = "-dQUIET";
  argv[argc++] = "-dSAFER";
  argv[argc++] = (passFile_==MSFalse)?"-":buf2; 
  argv[argc++] = NULL;

//  setEnviroment();
  
  if (pipe(std_in)==-1) res=-1;
  else if (pipe(std_out)==-1) res=-1;
  else if (pipe(std_err)==-1) res=-1;

  if(res==-1)
   { 
     MSMessageLog::errorMessage("MSPostScriptView::could not create pipe\n");
     return MSFalse;
   } 

  _pid=fork();
  if (_pid==-1)
   {
     MSMessageLog::errorMessage("MSPostScriptView: unable to fork process\n");
     return MSFalse;
   }
  else if (_pid==0)
   {
     close(std_out[0]);
     close(std_err[0]);

     dup2(std_out[1], 1);
     close(std_out[1]);

     dup2(std_err[1], 2);
     close(std_err[1]);

     setEnviroment();
     
     close(std_in[1]);
     dup2(std_in[0],0);
     close(std_in[0]);


     execvp(argv[0], (char* const*)argv);
     // we failed
     close(0);
     close(1);
     close(2);
     exit(5);
   }
  else
   {
     close(std_in[0]);
     _in= new MSPostScriptViewInChannel(this,"gsIn",std_in[1]);
     close(std_out[1]);
     _out=new MSPostScriptViewOutChannel(this,"gsOut",std_out[0]);
     close(std_err[1]);
     _err=new MSPostScriptViewOutChannel(this,"gsErr",std_err[0]);

   }
  return MSTrue;
}

void MSPostScriptView::reload(void)
{
  if(isFileLoaded()==MSTrue) loadFile(fileName());
}

void MSPostScriptView::reloadPage(void)
{
  if(isFileLoaded()==MSTrue) loadFile(fileName(), currentPage());
}

MSPostScriptView::ErrorCode MSPostScriptView::printToFile(const MSString& fileName_,
                                                          const MSIndexVector& pageIndex_)
{
  FILE *fp;
  fp=fopen(fileName_,"w");
  if(fp==0) return BadFile;
  printPages(fp,pageIndex_);
  fclose(fp);
  return NoError;
}


static MSBoolean printPipeBroken = MSFalse;
static void printError(SIGPARAM)
{
  printPipeBroken=MSTrue;
}

MSPostScriptView::ErrorCode MSPostScriptView::printToPrinter(const MSString& printerCmd_,
                                                             const MSIndexVector& pageIndex_)
{
  FILE *fp;
  fp=popen((char *)printerCmd_.string(),"w");
  if(fp==0) return BadFile;
  printPages(fp,pageIndex_);
  if(printPipeBroken==MSTrue)
   {
     printPipeBroken=MSFalse;
     return BadFile;
   }
  else
   {
     pclose(fp);
   }

  return NoError;
}


/*
 * Code below is taken almost verbatim from ghostview source distribution.
 */

/* length calculates string length at compile time */
/* can only be used with character constants */
#define lengthOfString(a) (sizeof(a)-1)
#define iscomment(a, b)	(strncmp(a, b, lengthOfString(b)) == 0)
#define DSCcomment(a) (a[0] == '%' && a[1] == '%')


void MSPostScriptView::printPages(FILE *fp, const MSIndexVector& pageIndex_)
{
  FILE *psfile;
  char text[PSLINELENGTH];
  char *comment;
  MSBoolean pages_written = MSFalse;
  MSBoolean pages_atend = MSFalse;
  int pages = 0;
  int page = 1;
  int i, j;
  long here;

  printPipeBroken=MSFalse;
  void (*oldsig)(SIGPARAM);
  oldsig = aplus_signal(SIGPIPE, printError);
  
  psfile = fopen(fileName(), "r");
  pages=pageIndex_.length();

  if(pages==0)
   {
     char buf[BUFSIZ];
     int bytes=0;
     while (bytes=read(fileno(psfile),buf,BUFSIZ))
      {
        bytes=write(fileno(fp),buf,bytes);
      }
     fclose(psfile);
     aplus_signal(SIGPIPE,oldsig);
     return; 
   }
     
  here = _doc->_doc->beginheader;
  while (comment = _doc->pscopyuntil(psfile, fp, here,
                               _doc->_doc->endheader, "%%Pages:"))
   {
     if(printPipeBroken==MSTrue)
      {
        fclose(psfile);
        aplus_signal(SIGPIPE,oldsig);
        return;
      }
        
     here = ftell(psfile);
     if (pages_written || pages_atend) {
       free(comment);
       continue;
     }
     sscanf(comment+lengthOfString("%%Pages:"), "%s", text);
     if (strcmp(text, "(atend)") == 0) {
       fputs(comment, fp);
       pages_atend = MSTrue;
     } else {
       switch (sscanf(comment+lengthOfString("%%Pages:"), "%*d %d", &i)) {
       case 1:
         fprintf(fp, "%%%%Pages: %d %d\n", pages, i);
         break;
       default:
         fprintf(fp, "%%%%Pages: %d\n", pages);
         break;
       }
       pages_written = MSTrue;
     }
     free(comment);
   }

  _doc->pscopy(psfile, fp, _doc->_doc->beginpreview, _doc->_doc->endpreview);
  _doc->pscopy(psfile, fp, _doc->_doc->begindefaults, _doc->_doc->enddefaults);
  _doc->pscopy(psfile, fp, _doc->_doc->beginprolog, _doc->_doc->endprolog);
  _doc->pscopy(psfile, fp, _doc->_doc->beginsetup, _doc->_doc->endsetup);
  if(printPipeBroken==MSTrue)
   {
     fclose(psfile);
     aplus_signal(SIGPIPE,oldsig);
     return;
   }

  for (i = 0; i < _doc->_doc->numpages; i++)
   {
     if (_doc->_doc->pageorder == MSPostScriptViewDocument::DESCEND)  j = (_doc->_doc->numpages - 1) - i;
     else   j = i;
     if(pages==0 || pageIndex_.indexOf(j)!=pageIndex_.length())
      {
        comment = _doc->pscopyuntil(psfile, fp, _doc->_doc->pages[i].begin,
                              _doc->_doc->pages[i].end, "%%Page:");
        fprintf(fp, "%%%%Page: %s %d\n",
                _doc->_doc->pages[i].label, page++);
        free(comment);
        _doc->pscopy(psfile, fp, -1, _doc->_doc->pages[i].end);
        if(printPipeBroken==MSTrue)
         {
           fclose(psfile);
           aplus_signal(SIGPIPE,oldsig);
           return;
         }
      }
   }

  here = _doc->_doc->begintrailer;
  while (comment = _doc->pscopyuntil(psfile, fp, here,
				 _doc->_doc->endtrailer, "%%Pages:")) {
    here = ftell(psfile);
    if (pages_written) {
      free(comment);
      continue;
    }
    switch (sscanf(comment+lengthOfString("%%Pages:"), "%*d %d", &i)) {
    case 1:
      fprintf(fp, "%%%%Pages: %d %d\n", pages, i);
      break;
    default:
      fprintf(fp, "%%%%Pages: %d\n", pages);
      break;
    }
    pages_written = MSTrue;
    free(comment);
  }
  aplus_signal(SIGPIPE,oldsig);
  fclose(psfile);
}


MSPostScriptViewDocument::documentmedia MSPostScriptViewDocument::papersizes[] = {
    "Letter",		 612,  792,
    "LetterSmall",	 612,  792,
    "Tabloid",		 792, 1224,
    "Ledger",		1224,  792,
    "Legal",		 612, 1008,
    "Statement",	 396,  612,
    "Executive",	 540,  720,
    "A3",		 842, 1190,
    "A4",		 595,  842,
    "A4Small",		 595,  842,
    "A5",		 420,  595,
    "B4",		 729, 1032,
    "B5",		 516,  729,
    "Folio",		 612,  936,
    "Quarto",		 610,  780,
    "10x14",		 720, 1008,
    NULL,		   0,    0
};

MSPostScriptViewDocument::MSPostScriptViewDocument()
{
  _doc = 0;
}

MSPostScriptViewDocument::~MSPostScriptViewDocument()
{
  if(_doc != 0) freeDoc(_doc);
}


/*
 *	psscan -- scan the PostScript file for document structuring comments.
 *
 *	This scanner is designed to retrieve the information necessary for
 *	the ghostview previewer.  It will scan files that conform to any
 *	version (1.0, 2.0, 2.1, or 3.0) of the document structuring conventions.
 *	It does not really care which version of comments the file contains.
 *	(The comments are largely upward compatible.)  It will scan a number
 *	of non-conforming documents.  (You could have part of the document
 *	conform to V2.0 and the rest conform to V3.0.  It would be similar
 *	to the DC-2 1/2+, it would look funny but it can still fly.)
 *
 *	This routine returns a pointer to the document structure.
 *	The structure contains the information relevant to previewing.
 *      These include EPSF flag (to tell if the file is a encapsulated figure),
 *      Page Media (for the Page Size), Bounding Box (to minimize backing
 *      pixmap size or determine window size for encapsulated PostScript), 
 *      Orientation of Paper (for default transformation matrix), and
 *      Page Order.  The title and CreationDate are also retrieved to
 *      help identify the document.
 *
 *      The following comments are examined:
 *
 *      Header section: 
 *      Must start with %!PS-Adobe-.  Version numbers ignored.
 *
 *      %!PS-Adobe-* [EPSF-*]
 *      %%BoundingBox: <int> <int> <int> <int>|(atend)
 *      %%CreationDate: <textline>
 *      %%Orientation: Portrait|Landscape|(atend)
 *      %%Pages: <uint> [<int>]|(atend)
 *      %%PageOrder: Ascend|Descend|Special|(atend)
 *      %%Title: <textline>
 *      %%DocumentMedia: <text> <real> <real> <real> <text> <text>
 *      %%DocumentPaperSizes: <text>
 *      %%EndComments
 *
 *      Note: Either the 3.0 or 2.0 syntax for %%Pages is accepted.
 *            Also either the 2.0 %%DocumentPaperSizes or the 3.0
 *            %%DocumentMedia comments are accepted as well.
 *
 *      The header section ends either explicitly with %%EndComments or
 *      implicitly with any line that does not begin with %X where X is
 *      a not whitespace character.
 *
 *      If the file is encapsulated PostScript the optional Preview section
 *      is next:
 *
 *      %%BeginPreview
 *      %%EndPreview
 *
 *      This section explicitly begins and ends with the above comments.
 *
 *      Next the Defaults section for version 3 page defaults:
 *
 *      %%BeginDefaults
 *      %%PageBoundingBox: <int> <int> <int> <int>
 *      %%PageOrientation: Portrait|Landscape
 *      %%PageMedia: <text>
 *      %%EndDefaults
 *
 *      This section explicitly begins and ends with the above comments.
 *
 *      The prolog section either explicitly starts with %%BeginProlog or
 *      implicitly with any nonblank line.
 *
 *      %%BeginProlog
 *      %%EndProlog
 *
 *      The Prolog should end with %%EndProlog, however the proglog implicitly
 *      ends when %%BeginSetup, %%Page, %%Trailer or %%EOF are encountered.
 *
 *      The Setup section is where the version 2 page defaults are found.
 *      This section either explicitly begins with %%BeginSetup or implicitly
 *      with any nonblank line after the Prolog.
 *
 *      %%BeginSetup
 *      %%PageBoundingBox: <int> <int> <int> <int>
 *      %%PageOrientation: Portrait|Landscape
 *      %%PaperSize: <text>
 *      %%EndSetup
 *
 *      The Setup should end with %%EndSetup, however the setup implicitly
 *      ends when %%Page, %%Trailer or %%EOF are encountered.
 *
 *      Next each page starts explicitly with %%Page and ends implicitly with
 *      %%Page or %%Trailer or %%EOF.  The following comments are recognized:
 *
 *      %%Page: <text> <uint>
 *      %%PageBoundingBox: <int> <int> <int> <int>|(atend)
 *      %%PageOrientation: Portrait|Landscape
 *      %%PageMedia: <text>
 *      %%PaperSize: <text>
 *
 *      The tralier section start explicitly with %%Trailer and end with %%EOF.
 *      The following comment are examined with the proper (atend) notation
 *      was used in the header:
 *
 *      %%Trailer
 *      %%BoundingBox: <int> <int> <int> <int>|(atend)
 *      %%Orientation: Portrait|Landscape|(atend)
 *      %%Pages: <uint> [<int>]|(atend)
 *      %%PageOrder: Ascend|Descend|Special|(atend)
 *      %%EOF
 *
 *
 *  + A DC-3 received severe damage to one of its wings.  The wing was a total
 *    loss.  There was no replacement readily available, so the mechanic
 *    installed a wing from a DC-2.
 */

MSBoolean MSPostScriptViewDocument::scanFile(FILE *file)
{
    struct document *doc;
    int bb_set = NONE;
    int pages_set = NONE;
    int page_order_set = NONE;
    int orientation_set = NONE;
    int page_bb_set = NONE;
    int page_media_set = NONE;
    int preread;		/* flag which tells the readline isn't needed */
    int i;
    unsigned int maxpages = 0;
    unsigned int nextpage = 1;	/* Next expected page */
    unsigned int thispage;
    int ignore = 0;		/* whether to ignore page ordinals */
    char *label;
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    long position;		/* Position of the current line */
    long beginsection;		/* Position of the beginning of the section */
    unsigned int line_len; 	/* Length of the current line */
    unsigned int section_len;	/* Place to accumulate the section length */
    char *next_char;		/* 1st char after text returned by gettext() */
    char *cp;
    struct documentmedia *dmp;
    int force_page=0;
    int seen_trailer=0;
    

    rewind(file);
    if (readline(line, sizeof line, file, &position, &line_len) == NULL) {
	MSMessageLog::warningMessage("MSPostScriptView Warning: empty file.\n");
	return MSFalse;
    }

    /* Header comments */

    if (iscomment(line,"%!PS-Adobe-")) {
	doc = (struct document *) malloc(sizeof(struct document));
	if (doc == NULL) {
        MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	}
	memset(doc, 0, sizeof(struct document));
        text[0]='\0';
	sscanf(line, "%*s %s", text);
	doc->epsf = iscomment(text, "EPSF-");
	doc->beginheader = position;
	section_len = line_len;
    } else {
	return MSFalse;
    }

    preread = 0;
    while (preread || readline(line, sizeof line, file, &position, &line_len)) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (line[0] != '%' ||
	    iscomment(line+1, "%EndComments") ||
	    line[1] == ' ' || line[1] == '\t' || line[1] == '\n' ||
	    !isprint(line[1])) {
	    break;
	} else if (line[1] != '%') {
	    /* Do nothing */
	} else if (doc->title == NULL && iscomment(line+2, "Title:")) {
	    doc->title = gettextline(line+lengthOfString("%%Title:"));
	} else if (doc->date == NULL && iscomment(line+2, "CreationDate:")) {
	    doc->date = gettextline(line+lengthOfString("%%CreationDate:"));
	} else if (bb_set == NONE && iscomment(line+2, "BoundingBox:")) {
	    sscanf(line+lengthOfString("%%BoundingBox:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		bb_set = ATEND;
	    } else {
		if (sscanf(line+lengthOfString("%%BoundingBox:"), "%d %d %d %d",
			   &(doc->boundingbox[LLX]),
			   &(doc->boundingbox[LLY]),
			   &(doc->boundingbox[URX]),
			   &(doc->boundingbox[URY])) == 4)
		    bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+lengthOfString("%%BoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			bb_set = 1;
			doc->boundingbox[LLX] = (int)fllx;
			doc->boundingbox[LLY] = (int)flly;
			doc->boundingbox[URX] = (int)furx;
			doc->boundingbox[URY] = (int)fury;
			if (fllx < doc->boundingbox[LLX])
			    doc->boundingbox[LLX]--;
			if (flly < doc->boundingbox[LLY])
			    doc->boundingbox[LLY]--;
			if (furx > doc->boundingbox[URX])
			    doc->boundingbox[URX]++;
			if (fury > doc->boundingbox[URY])
			    doc->boundingbox[URY]++;
		    }
		}
	    }
	} else if (orientation_set == NONE &&
		   iscomment(line+2, "Orientation:")) {
	    sscanf(line+lengthOfString("%%Orientation:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		orientation_set = ATEND;
	    } else if (strcmp(text, "Portrait") == 0) {
		doc->orientation = PORTRAIT;
		orientation_set = 1;
	    } else if (strcmp(text, "Landscape") == 0) {
		doc->orientation = LANDSCAPE;
		orientation_set = 1;
	    }
	} else if (page_order_set == NONE && iscomment(line+2, "PageOrder:")) {
	    sscanf(line+lengthOfString("%%PageOrder:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		page_order_set = ATEND;
	    } else if (strcmp(text, "Ascend") == 0) {
		doc->pageorder = ASCEND;
		page_order_set = 1;
	    } else if (strcmp(text, "Descend") == 0) {
		doc->pageorder = DESCEND;
		page_order_set = 1;
	    } else if (strcmp(text, "Special") == 0) {
		doc->pageorder = SPECIAL;
		page_order_set = 1;
	    }
	} else if (pages_set == NONE && iscomment(line+2, "Pages:")) {
	    sscanf(line+lengthOfString("%%Pages:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		pages_set = ATEND;
	    } else {
		switch (sscanf(line+lengthOfString("%%Pages:"), "%d %d",
			       &maxpages, &i)) {
		    case 2:
			if (page_order_set == NONE) {
			    if (i == -1) {
				doc->pageorder = DESCEND;
				page_order_set = 1;
			    } else if (i == 0) {
				doc->pageorder = SPECIAL;
				page_order_set = 1;
			    } else if (i == 1) {
				doc->pageorder = ASCEND;
				page_order_set = 1;
			    }
			}
		    case 1:
			if (maxpages > 0) {
			    doc->pages = (struct page *) calloc(maxpages,
							   sizeof(struct page));
			    if (doc->pages == NULL) {
                              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
			    }
			}
		}
	    }
	} else if (doc->nummedia == NONE &&
		   iscomment(line+2, "DocumentMedia:")) {
	    float w, h;
	    doc->media = (struct documentmedia *)
			 malloc(sizeof (struct documentmedia));
	    if (doc->media == NULL) {
              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	    }
	    doc->media[0].name = gettext(line+lengthOfString("%%DocumentMedia:"),
					 &next_char);
	    if (doc->media[0].name != NULL) {
		if (sscanf(next_char, "%f %f", &w, &h) == 2) {
		    doc->media[0].width = (int)(w + 0.5);
		    doc->media[0].height = (int)(h + 0.5);
		}
		if (doc->media[0].width != 0 && doc->media[0].height != 0)
		    doc->nummedia = 1;
		else
		    free(doc->media[0].name);
	    }
	    preread=1;
	    while (readline(line, sizeof line, file, &position, &line_len) &&
		   DSCcomment(line) && iscomment(line+2, "+")) {
		section_len += line_len;
		doc->media = (struct documentmedia *)
			     realloc(doc->media,
				     (doc->nummedia+1)*
				     sizeof (struct documentmedia));
		if (doc->media == NULL) {
                  MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
		}
		doc->media[doc->nummedia].name = gettext(line+lengthOfString("%%+"),
							 &next_char);
		if (doc->media[doc->nummedia].name != NULL) {
		    if (sscanf(next_char, "%f %f", &w, &h) == 2) {
			doc->media[doc->nummedia].width = (int)(w + 0.5);
			doc->media[doc->nummedia].height = (int)(h + 0.5);
		    }
		    if (doc->media[doc->nummedia].width != 0 &&
			doc->media[doc->nummedia].height != 0) doc->nummedia++;
		    else
			free(doc->media[doc->nummedia].name);
		}
	    }
	    section_len += line_len;
	    if (doc->nummedia != 0) doc->default_page_media = doc->media;
	} else if (doc->nummedia == NONE &&
		   iscomment(line+2, "DocumentPaperSizes:")) {

	    doc->media = (struct documentmedia *)
			 malloc(sizeof (struct documentmedia));
	    if (doc->media == NULL) {
              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	    }
	    doc->media[0].name = gettext(line+lengthOfString("%%DocumentPaperSizes:"),
					 &next_char);
	    if (doc->media[0].name != NULL) {
		doc->media[0].width = 0;
		doc->media[0].height = 0;
		for (dmp=papersizes; dmp->name != NULL; dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(doc->media[0].name, dmp->name) == 0) {
			free(doc->media[0].name);
			doc->media[0].name =
				(char *)malloc(strlen(dmp->name)+1);
			if (doc->media[0].name == NULL) {
                          MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
			}
			strcpy(doc->media[0].name, dmp->name);
			doc->media[0].width = dmp->width;
			doc->media[0].height = dmp->height;
			break;
		    }
		}
		if (doc->media[0].width != 0 && doc->media[0].height != 0)
		    doc->nummedia = 1;
		else
		    free(doc->media[0].name);
	    }
	    while (cp = gettext(next_char, &next_char)) {
		doc->media = (struct documentmedia *)
			     realloc(doc->media,
				     (doc->nummedia+1)*
				     sizeof (struct documentmedia));
		if (doc->media == NULL) {
                  MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
		}
		doc->media[doc->nummedia].name = cp;
		doc->media[doc->nummedia].width = 0;
		doc->media[doc->nummedia].height = 0;
		for (dmp=papersizes; dmp->name != NULL; dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(doc->media[doc->nummedia].name,
			       dmp->name) == 0) {
			free(doc->media[doc->nummedia].name);
			doc->media[doc->nummedia].name =
				(char *)malloc(strlen(dmp->name)+1);
			if (doc->media[doc->nummedia].name == NULL) {
                          MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
			}
			strcpy(doc->media[doc->nummedia].name, dmp->name);
			doc->media[doc->nummedia].name = dmp->name;
			doc->media[doc->nummedia].width = dmp->width;
			doc->media[doc->nummedia].height = dmp->height;
			break;
		    }
		}
		if (doc->media[doc->nummedia].width != 0 &&
		    doc->media[doc->nummedia].height != 0) doc->nummedia++;
		else
		    free(doc->media[doc->nummedia].name);
	    }
	    preread=1;
	    while (readline(line, sizeof line, file, &position, &line_len) &&
		   DSCcomment(line) && iscomment(line+2, "+")) {
		section_len += line_len;
		next_char = line + lengthOfString("%%+");
		while (cp = gettext(next_char, &next_char)) {
		    doc->media = (struct documentmedia *)
				 realloc(doc->media,
					 (doc->nummedia+1)*
					 sizeof (struct documentmedia));
		    if (doc->media == NULL) {
                      MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
		    }
		    doc->media[doc->nummedia].name = cp;
		    doc->media[doc->nummedia].width = 0;
		    doc->media[doc->nummedia].height = 0;
		    for (dmp=papersizes; dmp->name != NULL; dmp++) {
			/* Note: Paper size comment uses down cased paper size
			 * name.  Case insensitive compares are only used for
			 * PaperSize comments.
			 */
			if (strcasecmp(doc->media[doc->nummedia].name,
				   dmp->name) == 0) {
			    doc->media[doc->nummedia].width = dmp->width;
			    doc->media[doc->nummedia].height = dmp->height;
			    break;
			}
		    }
		    if (doc->media[doc->nummedia].width != 0 &&
			doc->media[doc->nummedia].height != 0) doc->nummedia++;
		    else
			free(doc->media[doc->nummedia].name);
		}
	    }
	    section_len += line_len;
	    if (doc->nummedia != 0) doc->default_page_media = doc->media;
	}
    }

    if (DSCcomment(line) && iscomment(line+2, "EndComments")) {
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
    }
    doc->endheader = position;
    doc->lenheader = section_len - line_len;

    /* Optional Preview comments for encapsulated PostScript files */ 

    beginsection = position;
    section_len = line_len;
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (doc->epsf && DSCcomment(line) && iscomment(line+2, "BeginPreview")) {
	doc->beginpreview = beginsection;
	beginsection = 0;
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndPreview"))) {
	    section_len += line_len;
	}
	section_len += line_len;
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
	doc->endpreview = position;
	doc->lenpreview = section_len - line_len;
    }

    /* Page Defaults for Version 3.0 files */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (DSCcomment(line) && iscomment(line+2, "BeginDefaults")) {
	doc->begindefaults = beginsection;
	beginsection = 0;
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndDefaults"))) {
	    section_len += line_len;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->default_page_orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+lengthOfString("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->default_page_orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->default_page_orientation = LANDSCAPE;
		}
	    } else if (page_media_set == NONE &&
		       iscomment(line+2, "PageMedia:")) {
		cp = gettext(line+lengthOfString("%%PageMedia:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    if (strcmp(cp, dmp->name) == 0) {
			doc->default_page_media = dmp;
			page_media_set = 1;
			break;
		    }
		}
		free(cp);
	    } else if (page_bb_set == NONE &&
		       iscomment(line+2, "PageBoundingBox:")) {
		if (sscanf(line+lengthOfString("%%PageBoundingBox:"), "%d %d %d %d",
			   &(doc->default_page_boundingbox[LLX]),
			   &(doc->default_page_boundingbox[LLY]),
			   &(doc->default_page_boundingbox[URX]),
			   &(doc->default_page_boundingbox[URY])) == 4)
		    page_bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+lengthOfString("%%PageBoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			page_bb_set = 1;
			doc->default_page_boundingbox[LLX] = (int)fllx;
			doc->default_page_boundingbox[LLY] = (int)flly;
			doc->default_page_boundingbox[URX] = (int)furx;
			doc->default_page_boundingbox[URY] = (int)fury;
			if (fllx < doc->default_page_boundingbox[LLX])
			    doc->default_page_boundingbox[LLX]--;
			if (flly < doc->default_page_boundingbox[LLY])
			    doc->default_page_boundingbox[LLY]--;
			if (furx > doc->default_page_boundingbox[URX])
			    doc->default_page_boundingbox[URX]++;
			if (fury > doc->default_page_boundingbox[URY])
			    doc->default_page_boundingbox[URY]++;
		    }
		}
	    }
	}
	section_len += line_len;
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
	doc->enddefaults = position;
	doc->lendefaults = section_len - line_len;
    }

    /* Document Prolog */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (!(DSCcomment(line) &&
	  (iscomment(line+2, "BeginSetup") ||
	   iscomment(line+2, "Page:") ||
	   iscomment(line+2, "Trailer") ||
	   iscomment(line+2, "EOF")))) {
	doc->beginprolog = beginsection;
	beginsection = 0;
	preread = 1;

	while ((preread ||
		readline(line, sizeof line, file, &position, &line_len)) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "EndProlog") ||
	          iscomment(line+2, "BeginSetup") ||
	          iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          iscomment(line+2, "EOF")))) {
	    if (!preread) section_len += line_len;
	    preread = 0;
	}
	section_len += line_len;
	if (DSCcomment(line) && iscomment(line+2, "EndProlog")) {
	    readline(line, sizeof line, file, &position, &line_len);
	    section_len += line_len;
	}
	doc->endprolog = position;
	doc->lenprolog = section_len - line_len;
    }

    /* Document Setup,  Page Defaults found here for Version 2 files */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (!(DSCcomment(line) &&
	  (iscomment(line+2, "Page:") ||
	   iscomment(line+2, "Trailer") ||
	   iscomment(line+2, "EOF")))) {
	doc->beginsetup = beginsection;
	beginsection = 0;
	preread = 1;
	while ((preread ||
		readline(line, sizeof line, file, &position, &line_len)) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "EndSetup") ||
	          iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          iscomment(line+2, "EOF")))) {
	    if (!preread) section_len += line_len;
	    preread = 0;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->default_page_orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+lengthOfString("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->default_page_orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->default_page_orientation = LANDSCAPE;
		}
	    } else if (page_media_set == NONE &&
		       iscomment(line+2, "PaperSize:")) {
		cp = gettext(line+lengthOfString("%%PaperSize:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(cp, dmp->name) == 0) {
			doc->default_page_media = dmp;
			page_media_set = 1;
			break;
		    }
		}
		free(cp);
	    } else if (page_bb_set == NONE &&
		       iscomment(line+2, "PageBoundingBox:")) {
		if (sscanf(line+lengthOfString("%%PageBoundingBox:"), "%d %d %d %d",
			   &(doc->default_page_boundingbox[LLX]),
			   &(doc->default_page_boundingbox[LLY]),
			   &(doc->default_page_boundingbox[URX]),
			   &(doc->default_page_boundingbox[URY])) == 4)
		    page_bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+lengthOfString("%%PageBoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			page_bb_set = 1;
			doc->default_page_boundingbox[LLX] = (int)fllx;
			doc->default_page_boundingbox[LLY] = (int)flly;
			doc->default_page_boundingbox[URX] = (int)furx;
			doc->default_page_boundingbox[URY] = (int)fury;
			if (fllx < doc->default_page_boundingbox[LLX])
			    doc->default_page_boundingbox[LLX]--;
			if (flly < doc->default_page_boundingbox[LLY])
			    doc->default_page_boundingbox[LLY]--;
			if (furx > doc->default_page_boundingbox[URX])
			    doc->default_page_boundingbox[URX]++;
			if (fury > doc->default_page_boundingbox[URY])
			    doc->default_page_boundingbox[URY]++;
		    }
		}
	    }
	}
	section_len += line_len;
	if (DSCcomment(line) && iscomment(line+2, "EndSetup")) {
	    readline(line, sizeof line, file, &position, &line_len);
	    section_len += line_len;
	}
	doc->endsetup = position;
	doc->lensetup = section_len - line_len;
    }

    /* Individual Pages */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }
    
newpage:
    while ((DSCcomment(line) && iscomment(line+2, "Page:")) || force_page==1) {
      if(force_page &&iscomment(line+2, "Trailer"))
       {
         seen_trailer=1;
         break;
       }
	if (maxpages == 0) {
	    maxpages = 1;
	    doc->pages = (struct page *) calloc(maxpages, sizeof(struct page));
	    if (doc->pages == NULL) {
              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	    }
	}
	label = gettext(line+lengthOfString("%%Page:"), &next_char);
	if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	if (nextpage == 1) {
	    ignore = thispage != 1;
	}
	if (!ignore && thispage != nextpage) {
	    free(label);
	    doc->numpages--;
	    goto continuepage;
	}
	nextpage++;
	if (doc->numpages == maxpages) {
	    maxpages++;
	    doc->pages = (struct page *)
			 realloc(doc->pages, maxpages*sizeof (struct page));
	    if (doc->pages == NULL) {
              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	    }
	}
	memset(&(doc->pages[doc->numpages]), 0, sizeof(struct page));
	page_bb_set = NONE;
	doc->pages[doc->numpages].label = label;
	if (beginsection) {
	    doc->pages[doc->numpages].begin = beginsection;
	    beginsection = 0;
	} else {
	    doc->pages[doc->numpages].begin = position;
	    section_len = line_len;
	}
continuepage:
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          iscomment(line+2, "PageTrailer") ||
	          iscomment(line+2, "EOF")))) {
	    section_len += line_len;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->pages[doc->numpages].orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+lengthOfString("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->pages[doc->numpages].orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->pages[doc->numpages].orientation = LANDSCAPE;
		}
	    } else if (doc->pages[doc->numpages].media == NULL &&
		       iscomment(line+2, "PageMedia:")) {
		cp = gettext(line+lengthOfString("%%PageMedia:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    if (strcmp(cp, dmp->name) == 0) {
			doc->pages[doc->numpages].media = dmp;
			break;
		    }
		}
		free(cp);
	    } else if (doc->pages[doc->numpages].media == NULL &&
		       iscomment(line+2, "PaperSize:")) {
		cp = gettext(line+lengthOfString("%%PaperSize:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(cp, dmp->name) == 0) {
			doc->pages[doc->numpages].media = dmp;
			break;
		    }
		}
		free(cp);
	    } else if ((page_bb_set == NONE || page_bb_set == ATEND) &&
		       iscomment(line+2, "PageBoundingBox:")) {
		sscanf(line+lengthOfString("%%PageBoundingBox:"), "%s", text);
		if (strcmp(text, "(atend)") == 0) {
		    page_bb_set = ATEND;
		} else {
		    if (sscanf(line+lengthOfString("%%PageBoundingBox:"), "%d %d %d %d",
			    &(doc->pages[doc->numpages].boundingbox[LLX]),
			    &(doc->pages[doc->numpages].boundingbox[LLY]),
			    &(doc->pages[doc->numpages].boundingbox[URX]),
			    &(doc->pages[doc->numpages].boundingbox[URY])) == 4)
			if (page_bb_set == NONE) page_bb_set = 1;
		    else {
			float fllx, flly, furx, fury;
			if (sscanf(line+lengthOfString("%%PageBoundingBox:"),
				   "%f %f %f %f",
				   &fllx, &flly, &furx, &fury) == 4) {
			    if (page_bb_set == NONE) page_bb_set = 1;
			    doc->pages[doc->numpages].boundingbox[LLX] = (int)fllx;
			    doc->pages[doc->numpages].boundingbox[LLY] = (int)flly;
			    doc->pages[doc->numpages].boundingbox[URX] = (int)furx;
			    doc->pages[doc->numpages].boundingbox[URY] = (int)fury;
			    if (fllx <
				    doc->pages[doc->numpages].boundingbox[LLX])
				doc->pages[doc->numpages].boundingbox[LLX]--;
			    if (flly <
				    doc->pages[doc->numpages].boundingbox[LLY])
				doc->pages[doc->numpages].boundingbox[LLY]--;
			    if (furx >
				    doc->pages[doc->numpages].boundingbox[URX])
				doc->pages[doc->numpages].boundingbox[URX]++;
			    if (fury >
				    doc->pages[doc->numpages].boundingbox[URY])
				doc->pages[doc->numpages].boundingbox[URY]++;
			}
		    }
		}
	    }
	}
        if(force_page && iscomment(line+2, "Trailer"))
         {
           seen_trailer=1;
           break;
         }
        force_page=0;
	section_len += line_len;
	doc->pages[doc->numpages].end = position;
	doc->pages[doc->numpages].len = section_len - line_len;
	doc->numpages++;
    }

    /****************************************************
     * striv
     * This is added to try to understand certain types of EPSF
     * files which don't have %%Page comments.
     */

    if(doc->epsf && seen_trailer==0)
     {
       force_page=1;
       goto newpage;
     }
   /**************************************************************/

    /* Document Trailer */

    if (beginsection) {
	doc->begintrailer = beginsection;
	beginsection = 0;
    } else {
	doc->begintrailer = position;
	section_len = line_len;
    }

    preread = 1;
    while ((preread ||
	    readline(line, sizeof line, file, &position, &line_len)) &&
	   !(DSCcomment(line) && iscomment(line+2, "EOF"))) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (!DSCcomment(line)) {
	    /* Do nothing */
	} else if (iscomment(line+2, "Page:")) {
	    free(gettext(line+lengthOfString("%%Page:"), &next_char));
	    if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	    if (!ignore && thispage == nextpage) {
		if (doc->numpages > 0) {
		    doc->pages[doc->numpages-1].end = position;
		    doc->pages[doc->numpages-1].len += section_len - line_len;
		} else {
		    if (doc->endsetup) {
			doc->endsetup = position;
			doc->endsetup += section_len - line_len;
		    } else if (doc->endprolog) {
			doc->endprolog = position;
			doc->endprolog += section_len - line_len;
		    }
		}
		goto newpage;
	    }
	} else if (bb_set == ATEND && iscomment(line+2, "BoundingBox:")) {
	    if (sscanf(line+lengthOfString("%%BoundingBox:"), "%d %d %d %d",
		       &(doc->boundingbox[LLX]),
		       &(doc->boundingbox[LLY]),
		       &(doc->boundingbox[URX]),
		       &(doc->boundingbox[URY])) != 4) {
		float fllx, flly, furx, fury;
		if (sscanf(line+lengthOfString("%%BoundingBox:"), "%f %f %f %f",
			   &fllx, &flly, &furx, &fury) == 4) {
		    doc->boundingbox[LLX] = (int)fllx;
		    doc->boundingbox[LLY] = (int)flly;
		    doc->boundingbox[URX] = (int)furx;
		    doc->boundingbox[URY] = (int)fury;
		    if (fllx < doc->boundingbox[LLX])
			doc->boundingbox[LLX]--;
		    if (flly < doc->boundingbox[LLY])
			doc->boundingbox[LLY]--;
		    if (furx > doc->boundingbox[URX])
			doc->boundingbox[URX]++;
		    if (fury > doc->boundingbox[URY])
			doc->boundingbox[URY]++;
		}
	    }
	} else if (orientation_set == ATEND &&
		   iscomment(line+2, "Orientation:")) {
	    sscanf(line+lengthOfString("%%Orientation:"), "%s", text);
	    if (strcmp(text, "Portrait") == 0) {
		doc->orientation = PORTRAIT;
	    } else if (strcmp(text, "Landscape") == 0) {
		doc->orientation = LANDSCAPE;
	    }
	} else if (page_order_set == ATEND && iscomment(line+2, "PageOrder:")) {
	    sscanf(line+lengthOfString("%%PageOrder:"), "%s", text);
	    if (strcmp(text, "Ascend") == 0) {
		doc->pageorder = ASCEND;
	    } else if (strcmp(text, "Descend") == 0) {
		doc->pageorder = DESCEND;
	    } else if (strcmp(text, "Special") == 0) {
		doc->pageorder = SPECIAL;
	    }
	} else if (pages_set == ATEND && iscomment(line+2, "Pages:")) {
	    if (sscanf(line+lengthOfString("%%Pages:"), "%*u %d", &i) == 1) {
		if (page_order_set == NONE) {
		    if (i == -1) doc->pageorder = DESCEND;
		    else if (i == 0) doc->pageorder = SPECIAL;
		    else if (i == 1) doc->pageorder = ASCEND;
		}
	    }
	}
    }
    section_len += line_len;
    if (DSCcomment(line) && iscomment(line+2, "EOF")) {
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
    }
    doc->endtrailer = position;
    doc->lentrailer = section_len - line_len;

#if 0
    section_len = line_len;
    preread = 1;
    while (preread ||
	   readline(line, sizeof line, file, &position, &line_len)) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (DSCcomment(line) && iscomment(line+2, "Page:")) {
	    free(gettext(line+lengthOfString("%%Page:"), &next_char));
	    if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	    if (!ignore && thispage == nextpage) {
		if (doc->numpages > 0) {
		    doc->pages[doc->numpages-1].end = position;
		    doc->pages[doc->numpages-1].len += doc->lentrailer +
						       section_len - line_len;
		} else {
		    if (doc->endsetup) {
			doc->endsetup = position;
			doc->endsetup += doc->lentrailer +
					 section_len - line_len;
		    } else if (doc->endprolog) {
			doc->endprolog = position;
			doc->endprolog += doc->lentrailer +
					  section_len - line_len;
		    }
		}
		goto newpage;
	    }
	}
    }
#endif
    _doc = doc;
    return MSTrue;
}

/*
 *	psfree -- free dynamic storage associated with document structure.
 */
void MSPostScriptViewDocument::freeDoc(struct document *doc)
{
    int i;

    if (doc) {
	for (i=0; i<doc->numpages; i++) {
	    if (doc->pages[i].label) free(doc->pages[i].label);
	}
	for (i=0; i<doc->nummedia; i++) {
	    if (doc->media[i].name) free(doc->media[i].name);
	}
	if (doc->title) free(doc->title);
	if (doc->date) free(doc->date);
	if (doc->pages) free(doc->pages);
	if (doc->media) free(doc->media);
	free(doc);
    }
}

/*
 * gettextine -- skip over white space and return the rest of the line.
 *               If the text begins with '(' return the text string
 *		 using gettext().
 */

char *MSPostScriptViewDocument::gettextline(char *line)
{
    char *cp;

    while (*line && (*line == ' ' || *line == '\t')) line++;
    if (*line == '(') {
	return gettext(line, NULL);
    } else {
	if (strlen(line) == 0) return NULL;
	cp = (char *) malloc(strlen(line));
	if (cp == NULL) {
          MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	}
	strncpy(cp, line, strlen(line)-1);
	cp[strlen(line)-1] = '\0';
	return cp;
    }
}

/*
 *	gettext -- return the next text string on the line.
 *		   return NULL if nothing is present.
 */

char * MSPostScriptViewDocument::gettext(char *line, char **next_char)
{
    char text[PSLINELENGTH];	/* Temporary storage for text */
    char *cp;
    int quoted=0;

    while (*line && (*line == ' ' || *line == '\t')) line++;
    cp = text;
    if (*line == '(') {
	int level = 0;
	quoted=1;
	line++;
	while (*line && !(*line == ')' && level == 0 )) {
	    if (*line == '\\') {
		if (*(line+1) == 'n') {
		    *cp++ = '\n';
		    line += 2;
		} else if (*(line+1) == 'r') {
		    *cp++ = '\r';
		    line += 2;
		} else if (*(line+1) == 't') {
		    *cp++ = '\t';
		    line += 2;
		} else if (*(line+1) == 'b') {
		    *cp++ = '\b';
		    line += 2;
		} else if (*(line+1) == 'f') {
		    *cp++ = '\f';
		    line += 2;
		} else if (*(line+1) == '\\') {
		    *cp++ = '\\';
		    line += 2;
		} else if (*(line+1) == '(') {
		    *cp++ = '(';
		    line += 2;
		} else if (*(line+1) == ')') {
		    *cp++ = ')';
		    line += 2;
		} else if (*(line+1) >= '0' && *(line+1) <= '9') {
		    if (*(line+2) >= '0' && *(line+2) <= '9') {
			if (*(line+3) >= '0' && *(line+3) <= '9') {
			    *cp++ = ((*(line+1) - '0')*8 + *(line+2) - '0')*8 +
				    *(line+3) - '0';
			    line += 4;
			} else {
			    *cp++ = (*(line+1) - '0')*8 + *(line+2) - '0';
			    line += 3;
			}
		    } else {
			*cp++ = *(line+1) - '0';
			line += 2;
		    }
		} else {
		    line++;
		    *cp++ = *line++;
		}
	    } else if (*line == '(') {
		level++;
		*cp++ = *line++;
	    } else if (*line == ')') {
		level--;
		*cp++ = *line++;
	    } else {
		*cp++ = *line++;
	    }
	}
    } else {
	while (*line && !(*line == ' ' || *line == '\t' || *line == '\n'))
	    *cp++ = *line++;
    }
    *cp = '\0';
    if (next_char) *next_char = line;
    if (!quoted && strlen(text) == 0) return NULL;
    cp = (char *) malloc(strlen(text)+1);
    if (cp == NULL) {
      MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
    }
    strcpy(cp, text);
    return cp;
}

/*
 *	readline -- Read the next line in the postScript file.
 *                  Automatically skip over data (as indicated by
 *                  %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		    comments.)
 *		    Also, skip over included documents (as indicated by
 *		    %%BeginDocument/%%EndDocument comments.)
 */

char * MSPostScriptViewDocument::readline(char *line, int size, FILE *fp,
                       long *position, unsigned int *line_len)
{
    char text[PSLINELENGTH];	/* Temporary storage for text */
    char save[PSLINELENGTH];	/* Temporary storage for text */
    char *cp;
    unsigned int num;
    unsigned int nbytes;
    int i;
    char buf[BUFSIZ];

    if (position) *position = ftell(fp);
    cp = fgets(line, size, fp);
    if (cp == NULL) line[0] = '\0';
    *line_len = strlen(line);
    if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	/* Do nothing */
    } else if (iscomment(line+7, "Document:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndDocument"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Feature:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFeature"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "File:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFile"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Font:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFont"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "ProcSet:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndProcSet"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Resource:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndResource"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Data:")) {
	text[0] = '\0';
	strcpy(save, line+7);
	if (sscanf(line+lengthOfString("%%BeginData:"), "%d %*s %s", &num, text) >= 1) {
	    if (strcmp(text, "Lines") == 0) {
		for (i=0; i < num; i++) {
		    cp = fgets(line, size, fp);
		    *line_len += cp ? strlen(line) : 0;
		}
	    } else {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, fp);
		    *line_len += BUFSIZ;
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, fp);
		*line_len += num;
	    }
	}
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndData"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Binary:")) {
	strcpy(save, line+7);
	if(sscanf(line+lengthOfString("%%BeginBinary:"), "%d", &num) == 1) {
	    while (num > BUFSIZ) {
		fread(buf, sizeof (char), BUFSIZ, fp);
		*line_len += BUFSIZ;
		num -= BUFSIZ;
	    }
	    fread(buf, sizeof (char), num, fp);
	    *line_len += num;
	}
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndBinary"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    }
    return cp;
}


/*
 *	pscopy -- copy lines of Postscript from a section of one file
 *		  to another file.
 *                Automatically switch to binary copying whenever
 *                %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		  comments are encountered.
 */

void MSPostScriptViewDocument::pscopy(FILE *from, FILE *to,
            long begin,			/* set negative to avoid initial seek */
            long end)
{
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    unsigned int num;
    int i;
    char buf[BUFSIZ];

    if (begin >= 0) fseek(from, begin, SEEK_SET);
    while (ftell(from) < end) {

	fgets(line, sizeof line, from);
	fputs(line, to);
        if(printPipeBroken==MSTrue) return;

	if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	    /* Do nothing */
	} else if (iscomment(line+7, "Data:")) {
	    text[0] = '\0';
	    if (sscanf(line+lengthOfString("%%BeginData:"),
		       "%d %*s %s", &num, text) >= 1) {
		if (strcmp(text, "Lines") == 0) {
		    for (i=0; i < num; i++) {
			fgets(line, sizeof line, from);
			fputs(line, to);
                        if(printPipeBroken==MSTrue) return;
		    }
		} else {
		    while (num > BUFSIZ) {
			fread(buf, sizeof (char), BUFSIZ, from);
			fwrite(buf, sizeof (char), BUFSIZ, to);
                        if(printPipeBroken==MSTrue) return;
			num -= BUFSIZ;
		    }
		    fread(buf, sizeof (char), num, from);
		    fwrite(buf, sizeof (char), num, to);
                    if(printPipeBroken==MSTrue) return;
		}
	    }
	} else if (iscomment(line+7, "Binary:")) {
	    if(sscanf(line+lengthOfString("%%BeginBinary:"), "%d", &num) == 1) {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, from);
		    fwrite(buf, sizeof (char), BUFSIZ, to);
                    if(printPipeBroken==MSTrue) return;
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, from);
		fwrite(buf, sizeof (char), num, to);
                if(printPipeBroken==MSTrue) return;
	    }
	}
    }
}

/*
 *	pscopyuntil -- copy lines of Postscript from a section of one file
 *		       to another file until a particular comment is reached.
 *                     Automatically switch to binary copying whenever
 *                     %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		       comments are encountered.
 */

char *MSPostScriptViewDocument::pscopyuntil(FILE *from,FILE *to,
                  long begin,  /* set negative to avoid initial seek */
                  long end, const char *comment)
{
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    unsigned int num;
    int comment_length;
    int i;
    char buf[BUFSIZ];
    char *cp;

    comment_length = strlen(comment);
    if (begin >= 0) fseek(from, begin, SEEK_SET);
    while (ftell(from) < end) {

	fgets(line, sizeof line, from);

	/* iscomment cannot be used here,
	 * because comment_length is not known at compile time. */
	if (strncmp(line, comment, comment_length) == 0) {
	    cp = (char *) malloc(strlen(line)+1);
	    if (cp == NULL) {
              MSTKTHROWEXCEPTION(MSOutOfMemory("MSPostScriptView Out Of Memory.\n"));
	    }
	    strcpy(cp, line);
	    return cp;
	}
	fputs(line, to);
	if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	    /* Do nothing */
	} else if (iscomment(line+7, "Data:")) {
	    text[0] = '\0';
	    if (sscanf(line+lengthOfString("%%BeginData:"),
		       "%d %*s %s", &num, text) >= 1) {
		if (strcmp(text, "Lines") == 0) {
		    for (i=0; i < num; i++) {
			fgets(line, sizeof line, from);
			fputs(line, to);
                        if(printPipeBroken==MSTrue) return NULL;
		    }
		} else {
		    while (num > BUFSIZ) {
			fread(buf, sizeof (char), BUFSIZ, from);
			fwrite(buf, sizeof (char), BUFSIZ, to);
                        if(printPipeBroken==MSTrue) return NULL;
			num -= BUFSIZ;
		    }
		    fread(buf, sizeof (char), num, from);
		    fwrite(buf, sizeof (char), num, to);
                    if(printPipeBroken==MSTrue) return NULL;
		}
	    }
	} else if (iscomment(line+7, "Binary:")) {
	    if(sscanf(line+lengthOfString("%%BeginBinary:"), "%d", &num) == 1) {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, from);
		    fwrite(buf, sizeof (char), BUFSIZ, to);
                    if(printPipeBroken==MSTrue) return NULL;
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, from);
		fwrite(buf, sizeof (char), num, to);
                if(printPipeBroken==MSTrue) return NULL;
	    }
	}
    }
    return NULL;
}

int MSPostScriptViewDocument::blank(char *line)
{
    char *cp = line;

    while (*cp == ' ' || *cp == '\t') cp++;
    return *cp == '\n' || (*cp == '%' && (line[0] != '%' || line[1] != '%'));
}

