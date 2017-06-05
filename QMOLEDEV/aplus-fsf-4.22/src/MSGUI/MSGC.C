///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSGC.H>
#include <MSTypes/MSMessageLog.H>

MSGC::List MSGC::_xgcList(64);

#ifdef MS_MULTI_THREAD
MSMutex MSGC::_msgcHashTableMutex;
#endif

MSGC::Data::Data(MSDisplayServer *server_,MSBoolean shared_,GC gc_,unsigned long mask_)
{
  _server=server_;
  _shared=shared_;
  _gc=gc_;
  _mask=mask_;
  _dashes=0;
  _nDashes=0;
  // We want to get all the values here
  unsigned long allGCValues=GCFunction|GCPlaneMask|GCForeground|GCBackground|GCLineWidth|GCLineStyle|GCCapStyle|GCJoinStyle|GCFillStyle|GCFillRule|GCTile|GCStipple|GCTileStipXOrigin|GCTileStipYOrigin|GCFont|GCSubwindowMode|GCGraphicsExposures|GCClipXOrigin|GCClipYOrigin|GCDashOffset|GCArcMode;
  XGetGCValues(_server->display(),_gc,allGCValues,&_values);
  _count = 0;
}

MSGC::Data::~Data(void)
{
  if (_dashes!=0) delete [] _dashes;
  if (_gc!=0)     XFreeGC(display(),_gc);
}

void MSGC::Data::mask(unsigned long mask_)
{
  _mask=mask_;
  XGetGCValues(_server->display(),_gc,_mask,&_values);  
}

void MSGC::Data::dashes(const char *dashes_,int n_)
{
  if (dashes_!=0)
   {
     if (dashes()!=0) delete [] _dashes;
     _dashes=new char[n_+1];
     strncpy(_dashes,(char *)dashes_,n_);
     _dashes[n_]='\0';
     _nDashes=n_;
   }
}

MSGC::List::List(int size_)
{
  _size=size_;
  _count=0;
  _array=new Data*[size()];
  for (int i=0;i<size();i++) _array[i]=0;
}

MSGC::List::~List(void)
{
  for (int i=0;i<count();i++) _array[i]=0;
  delete [] _array;
  _array=0; 
  _size=0;
  _count=0;
}

void MSGC::List::reserve(int size_)
{
  if (size()<size_)
   {
     int newSize=(size()==0)?size_<<1:size()<<1;
     int i;
     Data **array=new Data*[newSize];
     for (i=0;i<size();i++) 
      {
        array[i]=_array[i];
        _array[i]=0;
      }
     for (i=size();i<newSize;i++) array[i]=0;
     delete [] _array;
     _array=array;
     _size=newSize;
   }
}

void MSGC::List::add(Data *data_)
{
  reserve(count()+1);
  _array[count()]=data_;
  _count++;
}

void MSGC::List::remove(Data *data_)
{
  for (int i=0;i<count();i++)
   {
     if (_array[i]==data_) 
      {
        for (int j=i;j<count()-1;j++) _array[j]=_array[j+1];
        _array[count()-1]=0;
        _count--;
	break;
      }
   }
}

MSGC::MSGC(void)
: _server(0), _data(0)
{}

MSGC::MSGC(MSDisplayServer* server_,MSBoolean shared_,XGCValues *values_,unsigned long mask_)
: _data(0)
{ setGCValues(server_, shared_, values_, mask_); }


MSGC::MSGC(const MSGC& msgc_)
: _server(msgc_._server), _data(0)
{
  MSGUARD(_msgcHashTableMutex);
  if(msgc_._data->shared() == MSTrue)
   {
     _data = msgc_._data;
     _data->increment();
   }
  else  create(MSFalse, (XGCValues *)&msgc_.values(), msgc_.mask());
}

MSGC::~MSGC(void)
{ 
  MSGUARD(_msgcHashTableMutex);
  clearData(); 
}

void MSGC::setGCValues(MSDisplayServer * server_, MSBoolean shared_,
                       XGCValues *values_,unsigned long mask_)
{
  _server = server_;
  if (shared_ == MSTrue) setData(values_, mask_);
  else 
   {
     MSGUARD(_msgcHashTableMutex);
     create(MSFalse, values_, mask_);
   }
}

MSBoolean MSGC::isValid(void)
{ return MSBoolean(_server != 0); }

MSGC& MSGC::operator=(const MSGC& msgc_)
{
  if(this != &msgc_)
   {
     MSGUARD(_msgcHashTableMutex);
     clearData();
     _server = msgc_._server;
     if(msgc_._data->shared() == MSTrue)
      {
	_data = msgc_._data;
	_data->increment();
      }                   
     else create(MSFalse, (XGCValues *)&msgc_.values(), msgc_.mask());
   }
  return *this;
}

// for shared gcs only
void MSGC::setData(XGCValues *values_,unsigned long mask_)
{
  MSGUARD(_msgcHashTableMutex);
  Data *d=findMatch(values_,mask_);
  if (d==0) create(MSTrue,values_,mask_);
  else if (data()!=d)
   {
     clearData();
     _data=d;
     data()->increment();     
   }
}

void MSGC::clearData(void)
{
  if(data() != 0)
   {
     if (data()->count()==1) remove();
     data()->decrement();
     _data=0;
   }
}


void MSGC::create(MSBoolean shared_,XGCValues *vals_,unsigned long mask_)
{
  clearData();
  GC gc=XCreateGC(display(),server()->root(),mask_,vals_);
  _data=new Data(server(),shared_,gc,mask_);
  data()->increment();
  xgcList().add(data());
}


MSGC::Data *MSGC::findMatch(XGCValues *v_,unsigned long mask_) const
{
  for (int i=MSGC::xgcList().count()-1;i>=0;i--) 
   {
     if (isMatch(xgcList().data(i),v_,mask_)==MSTrue) return xgcList().data(i);
   }
  return 0;
}

void MSGC::remove(void)
{
  xgcList().remove(data());
}  

// function is broken up into three sections for efficiency
// 1. most likely to be different
// 2. second most likely to be different
// 3. least likely to be different

MSBoolean MSGC::isMatch(Data *data_,XGCValues *v_,unsigned long m_) const
{
  if (m_==data_->mask()&&server()==data_->server()&&data_->shared()==MSTrue)
   {
     XGCValues v;
     XGetGCValues(data_->display(),data_->gc(),m_,&v);

     if (m_&GCForeground)        if (v.foreground!=v_->foreground) return MSFalse;
     if (m_&GCBackground)        if (v.background!=v_->background) return MSFalse;
     if (m_&GCFont)              if (v.font!=v_->font) return MSFalse;
     if (m_&GCFunction)          if (v.function!=v_->function) return MSFalse;
     if (m_&GCGraphicsExposures) if (v.graphics_exposures!=v_->graphics_exposures) 
                                    return MSFalse;     
     if (m_&GCLineWidth)  if (v.line_width!=v_->line_width) return MSFalse;
     if (m_&GCLineStyle)  if (v.line_style!=v_->line_style) return MSFalse;
     if (m_&GCCapStyle)   if (v.cap_style!=v_->cap_style) return MSFalse;
     if (m_&GCJoinStyle)  if (v.join_style!=v_->join_style) return MSFalse;
     if (m_&GCFillStyle)  if (v.fill_style!=v_->fill_style) return MSFalse;
     if (m_&GCFillRule)   if (v.fill_rule!=v_->fill_rule) return MSFalse;
     if (m_&GCArcMode)    if (v.arc_mode!=v_->arc_mode) return MSFalse;
     
     if (m_&GCTile)            if (v.tile!=v_->tile) return MSFalse;
     if (m_&GCStipple)         if (v.stipple!=v_->stipple) return MSFalse;
     if (m_&GCTileStipXOrigin) if (v.ts_x_origin!=v_->ts_x_origin) return MSFalse;
     if (m_&GCTileStipYOrigin) if (v.ts_y_origin!=v_->ts_y_origin) return MSFalse;
     if (m_&GCSubwindowMode)   if (v.subwindow_mode!=v_->subwindow_mode) return MSFalse;
     if (m_&GCClipXOrigin)     if (v.clip_x_origin!=v_->clip_x_origin) return MSFalse;
     if (m_&GCClipYOrigin)     if (v.clip_y_origin!=v_->clip_y_origin) return MSFalse;
     if (m_&GCClipMask)        if (v.clip_mask!=v_->clip_mask) return MSFalse;
     if (m_&GCPlaneMask)       if (v.plane_mask!=v_->plane_mask) return MSFalse;
     if (m_&GCDashOffset)      if (v.dash_offset!=v_->dash_offset) return MSFalse;
     if (m_&GCDashList)        if (v.dashes!=v_->dashes) return MSFalse;  

     return MSTrue;
   }
  return MSFalse;
}

void MSGC::function(int func_)
{
  if (data()->shared()==MSTrue&&function()!=func_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.function=func_;
     setData(&val,mask()|GCFunction); 
   }
  else
   {
     mask(mask()|GCFunction);
     XSetFunction(display(),gc(),func_);
   }
}

void MSGC::planeMask(unsigned long mask_)
{
  if (data()->shared()==MSTrue&&planeMask()!=mask_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.plane_mask=mask_;
     setData(&val,mask()|GCPlaneMask); 
   }
  else
   {
     mask(mask()|GCPlaneMask);
     XSetPlaneMask(display(),gc(),mask_);
   }
}

void MSGC::foreground(unsigned long fg_)
{
  if (data()->shared()==MSTrue&&foreground()!=fg_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.foreground=fg_;
     setData(&val,mask()|GCForeground); 
   }
  else
   {
     mask(mask()|GCForeground);
     XSetForeground(display(),gc(),fg_);
   }
}

void MSGC::background(unsigned long bg_)
{
  if (data()->shared()==MSTrue&&background()!=bg_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.background=bg_;
     setData(&val,mask()|GCBackground); 
   }
  else
   {
     mask(mask()|GCBackground);
     XSetBackground(display(),gc(),bg_);
   }
}

void MSGC::color(unsigned long fg_,unsigned long bg_)
{
  if (data()->shared()==MSTrue&&(foreground()!=fg_||background()!=bg_))
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.foreground=fg_;
     val.background=bg_;
     setData(&val,mask()|GCForeground|GCBackground); 
   }
  else
   {
     mask(mask()|GCForeground|GCBackground);
     XSetForeground(display(),gc(),fg_);
     XSetBackground(display(),gc(),bg_);
   }
}

void MSGC::lineWidth(int lw_)
{  
  if (data()->shared()==MSTrue&&lineWidth()!=lw_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.line_width=lw_;
     setData(&val,mask()|GCLineWidth); 
   }
  else
   {
     mask(mask()|GCLineWidth);
     XSetLineAttributes(display(),gc(),lw_,lineStyle(),capStyle(),joinStyle());
   }
}

void MSGC::lineStyle(int ls_)
{
  if (data()->shared()==MSTrue&&lineStyle()!=ls_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.line_style=ls_;
     setData(&val,mask()|GCLineStyle); 
   }
  else
   {
     mask(mask()|GCLineStyle);
     XSetLineAttributes(display(),gc(),lineWidth(),ls_,capStyle(),joinStyle());
   }
}

void MSGC::capStyle(int cs_)
{
  if (data()->shared()==MSTrue&&capStyle()!=cs_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.cap_style=cs_;
     setData(&val,mask()|GCCapStyle); 
   }
  else
   {
     mask(mask()|GCCapStyle);
     XSetLineAttributes(display(),gc(),lineWidth(),lineStyle(),cs_,joinStyle());
   }
}

void MSGC::joinStyle(int js_)
{
  if (data()->shared()==MSTrue&&joinStyle()!=js_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.join_style=js_;
     setData(&val,mask()|GCJoinStyle); 
   }
  else
   {
     mask(mask()|GCJoinStyle);
     XSetLineAttributes(display(),gc(),lineWidth(),lineStyle(),capStyle(),js_);
   }
}

void MSGC::fillStyle(int fs_)
{
  if (data()->shared()==MSTrue&&fillStyle()!=fs_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.fill_style=fs_;
     setData(&val,mask()|GCFillStyle); 
   }
  else
   {
     mask(mask()|GCFillStyle);
     XSetFillStyle(display(),gc(),fs_);
   }
}

void MSGC::fillRule(int fr_)
{
  if (data()->shared()==MSTrue&&fillRule()!=fr_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.fill_rule=fr_;
     setData(&val,mask()|GCFillRule); 
   }
  else
   {
     mask(mask()|GCFillRule);
     XSetFillRule(display(),gc(),fr_);
   }
}

void MSGC::arcMode(int am_)
{
  if (data()->shared()==MSTrue&&arcMode()!=am_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.arc_mode=am_;
     setData(&val,mask()|GCArcMode); 
   }
  else
   {
     mask(mask()|GCArcMode);
     XSetArcMode(display(),gc(),am_);
   }
}

void MSGC::tile(Pixmap t_)
{
  if (data()->shared()==MSTrue&&tile()!=t_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.tile=t_;
     setData(&val,mask()|GCTile); 
   }
  else
   {
     mask(mask()|GCTile);
     XSetTile(display(),gc(),t_);
   }
}

void MSGC::stipple(Pixmap s_)
{
  if (data()->shared()==MSTrue&&stipple()!=s_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.stipple=s_;
     setData(&val,mask()|GCStipple); 
   }
  else
   {
     mask(mask()|GCStipple);
     XSetStipple(display(),gc(),s_);
   }
}

void MSGC::tsXOrigin(int x_)
{
  if (data()->shared()==MSTrue&&tsXOrigin()!=x_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.ts_x_origin=x_;
     setData(&val,mask()|GCTileStipXOrigin); 
   }
  else
   {
     mask(mask()|GCTileStipXOrigin);
     XSetTSOrigin(display(),gc(),x_,tsYOrigin());
   }
}

void MSGC::tsYOrigin(int y_)
{
  if (data()->shared()==MSTrue&&tsYOrigin()!=y_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.ts_y_origin=y_;
     setData(&val,mask()|GCTileStipYOrigin); 
   }
  else
   {
     mask(mask()|GCTileStipYOrigin); 
     XSetTSOrigin(display(),gc(),tsXOrigin(),y_);
   }
}

void MSGC::font(Font fid_)
{
  if (data()->shared()==MSTrue&&font()!=fid_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.font=fid_;
     setData(&val,mask()|GCFont); 
   }
  else
   {
     mask(mask()|GCFont);
     XSetFont(display(),gc(),fid_);
   }
}

void MSGC::subwindowMode(int mode_)
{
  if (data()->shared()==MSTrue&&subwindowMode()!=mode_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.subwindow_mode=mode_;
     setData(&val,mask()|GCSubwindowMode); 
   }
  else
   {
     mask(mask()|GCSubwindowMode);
     XSetSubwindowMode(display(),gc(),mode_);
   }
}

void MSGC::graphicsExposures(int ge_)
{
  if (data()->shared()==MSTrue&&graphicsExposures()!=ge_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.graphics_exposures=ge_;
     setData(&val,mask()|GCGraphicsExposures); 
   }
  else
   {
     mask(mask()|GCGraphicsExposures);
     XSetGraphicsExposures(display(),gc(),ge_);
   }
}

void MSGC::clipXOrigin(int x_)
{
  if (data()->shared()==MSTrue&&clipXOrigin()!=x_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.clip_x_origin=x_;
     setData(&val,mask()|GCClipXOrigin); 
   }
  else
   {
     mask(mask()|GCClipXOrigin);
     XSetClipOrigin(display(),gc(),x_,clipYOrigin());
   }
}

void MSGC::clipYOrigin(int y_)
{
  if (data()->shared()==MSTrue&&clipYOrigin()!=y_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.clip_y_origin=y_;
     setData(&val,mask()|GCClipYOrigin); 
   }
  else
   {
     mask(mask()|GCClipYOrigin);
     XSetClipOrigin(display(),gc(),clipXOrigin(),y_);
   }
}

void MSGC::clipMask(Pixmap mask_)
{
  if (data()->shared()==MSTrue&&clipMask()!=mask_)
   {
     XGCValues val;
     XGetGCValues(display(),gc(),mask(),&val);
     val.clip_mask=mask_;
     setData(&val,mask()|GCClipMask); 
   }
  else
   {
     mask(mask()|GCClipMask);
     XSetClipMask(display(),gc(),mask_);
   }
}

void MSGC::dashes(int dashOffset_,const char *dashes_,int n_)
{
  if (data()->shared()==MSTrue)
   {
     MSMessageLog::warningMessage("MSGC error: setting dash list on a shared GC");
   }
  mask(mask()|GCDashList|GCDashOffset);
  XSetDashes(display(),gc(),dashOffset_,dashes_,n_);
  data()->dashes(dashes_,n_);
}

void MSGC::clipRectangles(XRectangle *rects_,int n_,int ordering_)
{
  if (data()->shared()==MSTrue)
   {
     MSMessageLog::warningMessage("MSGC error: setting clip rectangle on a shared GC");
   }
  XSetClipRectangles(display(),gc(),clipXOrigin(),clipYOrigin(),rects_,n_,ordering_);
}



