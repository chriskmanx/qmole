#ifndef MSGCHEADER
#define MSGCHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDisplayServerHEADER
#include <MSGUI/MSDisplayServer.H>
#endif

#ifndef _XLIB_H_
#include <X11/Xlib.h>
#endif

#include <MSTypes/MSMutex.H>

class MSGC
{
public:
  class List;
  class Data
  {
  friend class List;
  public:
    MSDisplayServer *_server;
    GC               _gc;
    unsigned long    _mask;
    MSBoolean        _shared;
    char            *_dashes;
    int              _nDashes;
    XGCValues        _values;

    int              _count;
    
    Data(MSDisplayServer *server_,MSBoolean shared_,GC gc_,unsigned long mask_);
    ~Data(void);

    void dashes(const char *dashes_,int n_);
    void mask(unsigned long mask_);

    GC gc(void)                   const {return _gc;}
    unsigned long mask(void)      const {return _mask;}
    MSDisplayServer *server(void) const {return _server;}
    Display *display(void)        const {return _server->display();}
    MSBoolean shared(void)        const {return _shared;}
    const XGCValues& values(void) const {return _values;}
    const char *dashes(void)      const {return _dashes;}
    int nDashes(void)             const {return _nDashes;}

    void increment(void)   { _count++; }
    void decrement(void)   { if (--_count==0) delete this; }
    int  count(void) const { return _count; }
};

  class List 
  {
  protected:
    int        _size;
    int        _count;
    Data     **_array;

    int size(void) const  { return _size; }
    void reserve(int size_);
    
  public:
    List(int size_);
    ~List(void);

    Data *data(int i_)        const {return (i_>=0&&i_<size())?_array[i_]:0;}
    int count(void)           const {return _count;}
    void add(Data *data_);
    void remove(Data *data_);
  };
  
private:
  static List       _xgcList;
  Data             *_data;
  MSDisplayServer  *_server;
#ifdef MS_MULTI_THREAD
  static MSMutex    _msgcHashTableMutex;
#endif
  
  void create(MSBoolean shared_,XGCValues *values_,unsigned long mask_);
  void remove(void);
  Data *findMatch(XGCValues *values_,unsigned long mask_) const;
  MSBoolean isMatch(Data *data_,XGCValues *values_,unsigned long mask_) const;
  void clearData(void);
  void setData(XGCValues *,unsigned long mask_);
  void mask(unsigned long mask_) { _data->mask(mask_); }

public:
  MSGC(void);
  MSGC(MSDisplayServer*, MSBoolean shared_, XGCValues *values, unsigned long mask_);
  MSGC(const MSGC&);
  ~MSGC(void);

  MSGC& operator=(const MSGC&);

  void setGCValues(MSDisplayServer *, MSBoolean, XGCValues *, unsigned long);
  MSBoolean isValid(void);

  static List& xgcList(void)           {return _xgcList;}

  Data *data(void)               const {return _data;}
  MSDisplayServer *server(void)  const {return _server;}
  Display *display(void)         const {return _server->display();}
  MSBoolean shared(void)         const {return _data->_shared;}

  const XGCValues& values(void)  const {return _data->_values;}
  unsigned long mask(void)       const {return _data->_mask;}
  GC gc(void)                    const {return _data->_gc;}
  const char *dashes(void)       const {return _data->_dashes;}
  int nDashes(void)              const {return _data->_nDashes;}

  int function(void)             const {return _data->_values.function;}
  unsigned long planeMask(void)  const {return _data->_values.plane_mask;}
  unsigned long foreground(void) const {return _data->_values.foreground;}
  unsigned long background(void) const {return _data->_values.background;}
  int lineWidth(void)            const {return _data->_values.line_width;}
  int lineStyle(void)            const {return _data->_values.line_style;}
  int capStyle(void)             const {return _data->_values.cap_style;}
  int joinStyle(void)            const {return _data->_values.join_style;}
  int fillStyle(void)            const {return _data->_values.fill_style;}
  int fillRule(void)             const {return _data->_values.fill_rule;}
  int arcMode(void)              const {return _data->_values.arc_mode;}
  Pixmap tile(void)              const {return _data->_values.tile;}
  Pixmap stipple(void)           const {return _data->_values.stipple;}
  int tsXOrigin(void)            const {return _data->_values.ts_x_origin;}
  int tsYOrigin(void)            const {return _data->_values.ts_y_origin;}
  Font font(void)                const {return _data->_values.font;}
  int subwindowMode(void)        const {return _data->_values.subwindow_mode;}
  int graphicsExposures(void)    const {return _data->_values.graphics_exposures;}
  int clipXOrigin(void)          const {return _data->_values.clip_x_origin;}
  int clipYOrigin(void)          const {return _data->_values.clip_y_origin;}
  Pixmap clipMask(void)          const {return _data->_values.clip_mask;}
  int dashOffset(void)           const {return _data->_values.dash_offset;}

  void function(int func_);
  void planeMask(unsigned long mask_);
  void color(unsigned long fg_,unsigned long bg_);
  void foreground(unsigned long fg_);
  void background(unsigned long bg_);
  void lineWidth(int lw_);
  void lineStyle(int ls_);
  void capStyle(int cs_);
  void joinStyle(int js_);
  void fillStyle(int fs_);
  void fillRule(int fr_);
  void arcMode(int am_);
  void tile(Pixmap t_);
  void stipple(Pixmap s_);
  void tsXOrigin(int x_);
  void tsYOrigin(int y_);
  void font(Font fid_);
  void subwindowMode(int mode_);
  void graphicsExposures(int ge_);
  void clipXOrigin(int x_);
  void clipYOrigin(int y_);
  void clipMask(Pixmap mask_);
  void dashes(int dashOffset_,const char *dashList_,int n_);
  void clipRectangles(XRectangle *rects_,int n_,int ordering_);
};

#endif


