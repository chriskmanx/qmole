///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSMethodCallback.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusTableColumn.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/EnumTables.H>

extern A convertToPixels(const MSWidgetCommon *, A);
extern long dbg_tmstk;
extern A grc(A,int,int);

AplusTableColumn::AplusTableColumn(AplusTable *owner_) : MSTableColumn((MSTable *)owner_), _breakText(aplus_nl)
{
  _columnAlignment=MSLeft;
  _outStr=aplus_nl;
  _compMode = AplusReportAlgorithm::Sum;
  _algorithm = AplusReportSumAlgorithm::reportSum();
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
  if (owner()==0)
   {
     headingForeground(server()->defaultForeground());
     _headingFont=server()->defaultFont();
   }     
  else
   {
     headingForeground(((MSTable *)owner())->titleForeground());
     _headingFont=((MSTable *)owner())->titleFont();
   }
  
  callback(MSWidgetCallback::columnresize,
	   new MSMethodCallback<AplusTableColumn>(this,&AplusTableColumn::columnResizeCB));

}

AplusTableColumn::~AplusTableColumn(void)
{ dc(_outStr); }

void AplusTableColumn::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusTableColumn"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = ((AplusModel *)model())->aplusVar();
      A index = ave->index();
      A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  if (event_.type() == AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusTableColumn"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  if (event_.type() == AplusUpdateDataEvent::symbol())  // Size update Event
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusTableColumn"  << endl;
      V v = ((AplusModel *)model())->aplusVar();
      AVariableData *varData = ::pAVarDataFromV(v);
      if (varData)
	{
	  columnWidth(varData->colWidth());
	  setClipMode();
	}
    }
}

MSBoolean AplusTableColumn::verifyData(V,A a_)
{
  if (a_!=0&&QA(a_))
   {
     if (a_->t==Et) return MSTrue;
     else if (a_->r==1&&a_->t!=Ct) return MSTrue;
     else if (a_->t==Ct&&a_->r>=1&&a_->r<=2) return MSTrue;
   }
  return MSFalse;
}

void AplusTableColumn::addSenderNotify(MSEventSender *m_)
{
  if (dbg_tmstk) cout << "AplusTableColumn::addSenderNotify" << endl;
  INTERNAL_COUPLE(((AplusModel *) m_));

  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v)
    {
      setClipMode();
      if (isNull(getVarData()->bgA())==MSTrue)
	{
	  background(table()->background());
	}

      headingFont(titleFont());
   }
}

MSBoolean AplusTableColumn::validate(const char *string_, unsigned row_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  return validate(v, string_, row_);
}


MSBoolean AplusTableColumn::validate(V v_, const char *string_, unsigned row_)
{
  extern int safeAset(V,A,A,A);
  
  if (v_!=0) 
   {
     AInFunction *inFunc;
     A inData=aplus_nl;
     
     if ((inFunc=AplusModel::getInFunc(v_))!=0) inData=inFunc->invoke(v_,(char *)string_,(int)row_, 0);
     else inData=defaultInFunc(v_,string_);
     if (inData!=0 && isNull(inData)==MSFalse)
      {
	A index=grc((A)v_->a,row_,0);
        inData=(A)ic(inData);
	if (safeAset(v_,inData,index,0)==0)
	{
	  showError(qs);
	  dc(inData);
	  if (index!=0) dc(index);	
	  return MSFalse;
	}
	else 
         {
	   ((AplusModel*)model())->doneCB(v_,inData,index,0);
	   dc(inData);
	   if (index!=0) dc(index);	
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

A AplusTableColumn::defaultInFunc(V v_,const char *string_)
{
  char   *ptrchar=0;
  long    lnum=0;
  double  dnum=0.0;
  int     n=0;
  A       av,r=aplus_nl;
  
  if (v_!=0) 
   {
     av=(A)v_->a;
     switch(av->t)
      {
	case It:
  	lnum=strtol((char *)string_,&ptrchar,10); // Base 10
	if (ptrchar==(char *)string_) 
	 {
	   r=aplus_nl;
	   showError("Unknown Number: Integer Expected");
	 }
	else r=gi((int)lnum);
	break;
	
	case Ft:
	dnum=strtod((char *)string_,&ptrchar);
	if (ptrchar==(char *)string_) 
	 {
	   r=aplus_nl;
	   showError("Unknown Number: Float Expected");
	 }
	else r=gf((double)dnum);
	break;
	
	case Ct:
	n=(av->r==1?(int)av->d[0]:(int)av->d[1]); 
	r=gv(Ct,n);
	(void) memset((char *)r->p,' ',n);  // ' ' == 0x20 == 32 --> blankspace
	strncpy((char *)r->p,(char *)string_,strlen((char *)string_));
	break;
	
	case Et: // no attempt at conversion - nesting level ????
	r=gsv(0,(char *)string_);
	break;
	
	default:
	break;
      }
   }
  return (A) r;
}

const char *AplusTableColumn::formatOutput(MSString& str_, unsigned row_)
{
  static const char blank[]={" "};
  V v                       = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  ACharStrFunction *outFunc = AplusModel::getOutFunc(v);
  AVariableData    *vd      = ::pAVarDataFromV(v);
  invokeFunction(outFunc, row_);
  str_ = (Ct==_outStr->t) ? (char *)_outStr->p : blank;
  dc(_outStr);
  _outStr=aplus_nl;
  return str_;
}

void AplusTableColumn::invokeFunction(AOutFunction *outFunc_, unsigned row_)
{ invokeFunction(outFunc_,row_,-1); }

void AplusTableColumn::invokeFunction(AOutFunction *outFunc_, unsigned row_, unsigned col_)
{
  I *data = ((model()!=0)?((AplusModel*)model())->data():0);
  invokeFunction(outFunc_, row_, col_, data);
}

void AplusTableColumn::invokeFunction(AOutFunction *outFunc_, unsigned row_, unsigned col_, I *data_)
{
  V                 v          = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  unsigned long     type       = (model()!=0)?((AplusModel*)model())->a_type():0;
  int 		    charlength = (model()!=0)?((AplusModel*)model())->charLength():0;
  int 		    rank       = (model()!=0)?((AplusModel*)model())->rank():0;
  P p;              p.i        = data_;
  char *buf;

  if (outFunc_!=0 && row_<numRows())
   {
     int w=1;  // Always only one column
     int offset=row_*w;
     int n=0;
     A d;
     
     switch(type)
      {
	case It:
	  _outStr=(A)outFunc_->invoke(v,(int)p.i[offset],row_,col_);
	  break;
	case Ft:
	  _outStr=(A)outFunc_->invoke(v,(double)p.f[offset],row_,col_);
	  break;
	case Ct:
	  n=charlength;
	  offset=row_ * n;
          buf=new char[n+1];
	  strncpy(buf,p.c+(offset),n);
	  buf[n]= '\0';
	  _outStr=(A)outFunc_->invoke(v,(char *)buf,row_,col_);
          delete [] buf;
	  break;
	case Et:
          d=gs(Et);
	  *d->p=ic(p.a[offset]);
	  _outStr=(A)outFunc_->invoke(v,d,row_,col_);
          dc(d);
	  break;
      }
   }
}

void AplusTableColumn::invokeFunction(AFormatBreakFunction *outFunc_, unsigned row_, unsigned col_, A data_)
{
  V             v          = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  unsigned long type       = data_->t;
  P p;          p.i        = data_->p;
  unsigned len;
  char *buf;
  
  if (outFunc_!=0)
   {
     switch(type)
      {
	case It:
	  _outStr=(A)outFunc_->invoke(v,(int)p.i[0],row_,col_);
	  break;
	case Ft:
	  _outStr=(A)outFunc_->invoke(v,(double)p.f[0],row_,col_);
	  break;
	case Ct:
	  if (p.c && (len=strlen((char *)p.c))!=0)
	   {
	     buf = new char[len+1];
	     strcpy(buf, (char *)p.c);
	     buf[len]='\0';
	     _outStr=(A)outFunc_->invoke(v,buf,row_,col_);
	     delete [] buf;
	   }
	  break;
	case Et:
	  ic(data_);
	  _outStr=(A)outFunc_->invoke(v,data_,row_,col_);
	  dc(data_);
	  break;
      }
   }
}


MSBoolean AplusTableColumn::isCellProtected(unsigned row_)
{
  char *buf;
  AVariableData *varData=getVarData();
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  MSBoolean ro=(varData!=0)?varData->readOnly():MSFalse;
  AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);
  unsigned long     type       = (model()!=0)?((AplusModel*)model())->a_type():0;
  int 		    charlength = (model()!=0)?((AplusModel*)model())->charLength():0;
  int 		    rank       = (model()!=0)?((AplusModel*)model())->rank():0;
  P p;              p.i        = (model()!=0)?((AplusModel*)model())->data():0;

  if (roFunc!=0&&row_<numRows())
   {
     int w=rank==2?numColumns():1;
     int r=row_;
     int offset=r*w;
     int n =0;
     A d;
     
     switch(type)
      {
	case It:
	  ro=(MSBoolean) roFunc->invoke(v,(int)p.i[offset],r,0);   
 	  break;
	case Ft:
	  ro=(MSBoolean) roFunc->invoke(v,(double)p.f[offset],r,0);   
	  break;
	case Ct:
          n=charlength;
          offset=row_ * n;
          buf=new char[n+1];
          strncpy(buf,p.c+(offset),n);
          buf[n]= '\0';
	  ro=(MSBoolean) roFunc->invoke(v,(char *)buf,r,0);   
          delete [] buf;
          break;
	case Et:
          d=gs(Et);
	  *d->p=ic(p.a[offset]);
	  ro=(MSBoolean) roFunc->invoke(v,d,r,0);   
          dc(d);
	  break;
      }
   }
  return ro;
}


void AplusTableColumn::update(const MSIndexVector &v_)
{
  MSTableColumn::update(v_);
}


void AplusTableColumn::update(V v_,int r_,int,UpdateType t_)
{ 
  if (v_!=0)
   {
     if (t_==ShapeUpdate||t_==AppendUpdate) ((AplusTable *)table())->columnUpdate(column());
     else if (t_==ValueUpdate)
      {
        if (column()==0&&numRows()>=((AplusTable*)table())->numRows()) ((AplusTable*)table())->appendUpdate();
        if (r_==-1) ((AplusTable*)table())->cycleColumn(column());   
	else ((AplusTable*)table())->cycleRowColumn(r_,column());
      }
   }
}


Font AplusTableColumn::cellFont(unsigned row_)
{
  char *buf;
  Font fid; 
  V                 v          = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  unsigned long     type       = (model()!=0)?((AplusModel*)model())->a_type():0;
  int 		    charlength = (model()!=0)?((AplusModel*)model())->charLength():0;
  int 		    rank       = (model()!=0)?((AplusModel*)model())->rank():0;
  P p;              p.i        = (model()!=0)?((AplusModel*)model())->data():0;
  
  AFontFunction *fontFunc=AplusModel::getFontFunc(v);
  if (fontFunc!=0&&row_<numRows())
   {
     int w=rank==2?numColumns():1;
     int offset=row_*w;
     int n=0;
     A d;
     
     switch(type)
      {
	case It:
	  fid=(Font)fontFunc->invoke(v,(int)p.i[offset],row_,0);
	  break;
	case Ft:
	  fid=(Font)fontFunc->invoke(v,(double)p.f[offset],row_,0);
	  break;
	case Ct:
	  n=charlength;
	  offset=row_ * n;
          buf=new char[n+1];
	  strncpy(buf,p.c+(offset),n);
	  buf[n]= '\0';
	  fid=(Font)fontFunc->invoke(v,(char *)buf,row_,0);   
          delete [] buf;
	  break;
	case Et:
          d=gs(Et);
	  *d->p=ic(p.a[offset]);
	  fid=(Font)fontFunc->invoke(v,d,row_,0);  
          dc(d);
	  break;
      }
   }
  else fid=((AplusTable*)table())->getVFont(v);
  return fid;
}

void AplusTableColumn::update(V v_,A index_,A,I ravel_)
{ 
  if(index_==(A)MP(22)) update(v_,-1,-1,AppendUpdate);
  else if(!index_) update(v_,-1,-1,ShapeUpdate);
  else if(ravel_) // ravel update
   {
     A a=(A)v_->a;
     if(a->r==2&&a->n==1) 
      { 
        int n=(int)a->d[1];
        int k=(int)index_->p[0];
        int j=k/n;
        update(v_,j,k-n*j,ValueUpdate); 
      }
     else update(v_,-1,-1,ValueUpdate);
   }
  else 
   {
     A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
     A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
     if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
      {     
        if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
        else 
         {      
           for(int i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
         }
      }
     else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
      { 
        for(int i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
      }
     else 
      {
	for(int j=0;j<(int)r->n;j++)
	 {
	   for(int i=0;i<(int)c->n;i++)
	    {
	      update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
	    }
	 }
      }
   }
}

Font AplusTableColumn::titleFont(void)
{
  if (model() && ((AplusModel*)model())->aplusVar()!=0) 
   {
     AVariableData *avar = getVarData();
     if (avar && isNull(avar->titleAFont())==MSFalse) return avar->titleFont();
     else return table()->titleFont();
   }
  return font();
}

void AplusTableColumn::setBg(unsigned long bg_)
{
  if (isNull(getVarData()->bgA())==MSTrue) background(bg_);
}

void AplusTableColumn::setFont(Font fid_)
{
  if (isNull(getVarData()->fontA())==MSTrue) font(fid_);
}

unsigned long AplusTableColumn::cellForeground(unsigned row_)
{
  AplusModel *m=(AplusModel *)model();
  unsigned long fgcolor=foreground();
  if(m)
  {
    V v = m->aplusVar();
    AColorFunction *fgFunc=AplusModel::getFgFunc(v);
    P p;
    p.i = m->data();
    char *buf;
    
    if (fgFunc!=0&&row_<numRows())
    {
      int w=(2==m->rank())?numColumns():1;
      int offset=row_*w;
      A d;
      
      switch(m->a_type())
      {
      case It:
	fgcolor=(unsigned long)fgFunc->invoke(v,(int)p.i[offset],row_,0);   
	break;
      case Ft:
	fgcolor=(unsigned long)fgFunc->invoke(v,(double)p.f[offset],row_,0);
	break;
      case Ct:
	w=m->charLength();
	offset=row_ * w;
	buf=new char[w+1];
	memcpy(buf,p.c+(offset),w);
	buf[w]= '\0';
	fgcolor=(unsigned long) fgFunc->invoke(v,(char *)buf,row_,0);   
	delete [] buf;
	break;
      case Et:
	d=gs(Et);
	*d->p=ic(p.a[offset]);
	fgcolor=(unsigned long) fgFunc->invoke(v,d,row_,0);
	dc(d);
	break;
      }
    }
  }
  return fgcolor;
}
  
unsigned long AplusTableColumn::cellBackground(unsigned row_)
{
  AplusModel *m=(AplusModel *)model();
  unsigned long bgcolor;

  // Inherit bg from table for `tableField has ( `bg;() )
  if (isNull(getVarData()->bgA())==MSTrue)
    bgcolor=table()->background();
  else
    bgcolor=background();

  if(m)
  {
    V v = m->aplusVar();
    AColorFunction *bgFunc=AplusModel::getBgFunc(v);
    P p;
    p.i = m->data();
    char *buf;
    
    if (bgFunc!=0&&row_<numRows())
    {
      int w=(2==m->rank())?numColumns():1;
      int offset=row_*w;
      A d;
      
      switch(m->a_type())
      {
      case It:
	bgcolor=(unsigned long)bgFunc->invoke(v,(int)p.i[offset],row_,0);   
	break;
      case Ft:
	bgcolor=(unsigned long)bgFunc->invoke(v,(double)p.f[offset],row_,0);
	break;
      case Ct:
	w=m->charLength();
	offset=row_ * w;
	buf=new char[w+1];
	memcpy(buf,p.c+(offset),w);
	buf[w]= '\0';
	bgcolor=(unsigned long) bgFunc->invoke(v,(char *)buf,row_,0);   
	delete [] buf;
	break;
      case Et:
	d=gs(Et);
	*d->p=ic(p.a[offset]);
	bgcolor=(unsigned long) bgFunc->invoke(v,d,row_,0);
	dc(d);
	break;
      }
    }
  }
  return bgcolor;
}
  

MSUnsignedLongVector AplusTableColumn::getCycleColors(int row_) const
{
  char *buf;
  A r=aplus_nl;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  ACycleFunction *cycleFunc = AplusModel::getCycleFunc(v);
  
  if (cycleFunc && cycleFunc->func()!=0&&(row_<numRows()||row_==0))
   {
     P p; p.i=((AplusModel *)model())->data();
     int rank = ((AplusModel *)model())->rank();
     int type = ((AplusModel *)model())->a_type();
     int w=rank==2?numColumns():1;
     int offset=row_*w;
     int n=0;
     
     switch(type)
      {
	case It:
	  r=cycleFunc->invoke(v,(int)p.i[offset],row_,0);
	  break;
	case Ft:
	  r=cycleFunc->invoke(v,(double)p.f[offset],row_,0);
	  break;
	case Ct:
	  n=((AplusModel*)model())->charLength();
	  offset=row_*n;
          buf=new char[n+1];
	  strncpy(buf,p.c+(offset),n);
	  buf[n]= '\0';
	  r=cycleFunc->invoke(v,(char *)buf,row_,0);   
          delete [] buf;
	  break;
	case Et:
	  if (((AplusModel*)model())->numElmts()>0)
           {
             A d=gs(Et);
	     *d->p=ic(p.a[offset]);
             r=cycleFunc->invoke(v,d,row_,0);   
             dc(d);
	   }
	  break;
      }
   }
  A pixelColors = (isNull(r)==MSFalse) ? convertToPixels(table(),r) : r;
  MSUnsignedLongVector uv;

  if (isNull(pixelColors)==MSFalse)
   {
     for (unsigned i = 0; i < pixelColors->n; i++) 
       uv << (unsigned long) pixelColors->p[i];
     dc(pixelColors);
   }
  return uv;
}

  
//
//  Report Stuff
//

MSBoolean AplusTableColumn::breakCriteria(unsigned row_)  // FIX THIS
{
  if (breakCriteriaFunc()->func()!=0)
   {
     invokeFunction(breakCriteriaFunc(),row_);
     if (_outStr->t!=It)
      {
	dc(_outStr);
	_outStr=aplus_nl;
	showError("Non-integer (boolean) value in breakCriteria function");
	return MSFalse;
      }
     else
      {
	P p; p.i=_outStr->p;
	MSBoolean r = (p.i[0]==0)?MSFalse:MSTrue;
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
   }
  else
   {
     if (isNumericColumn()) return MSFalse;
     else
      {
	// Don't break on first row
	// Break if the row is a duplicate OR last row
	// else, don't break
	return (row_!=0)?
	(isDuplicate(row_)==MSFalse)||row_==((numRows())-1)?MSTrue:MSFalse:MSFalse;
      }
   }
  
}

MSBoolean AplusTableColumn::isDuplicate(unsigned row_)
{
  if (row_==0) return MSFalse;

  MSString str;
  char *str1=(char *)formatOutput(str,row_);
  char *tmp=0;
  if (str1!=0)
   {
     tmp=new char[strlen(str1)+1];
     strcpy(tmp,str1);
   }
  char *str2=(char *)formatOutput(str,row_-1);
  if (tmp&&str2&&strcmp(tmp,str2)==0)
  {
    delete tmp;
    return MSTrue;
  }
  if (tmp) delete tmp;
  if (tmp==0&&str2==0) return MSTrue;
  return MSFalse;
}

unsigned long AplusTableColumn::style(unsigned row_)
{
  if (styleFunc()->func()!=0)
   {
     invokeFunction(styleFunc(),row_);
     unsigned style = GUIEnum.formatStyle(_outStr);
     dc(_outStr);
     _outStr=aplus_nl;
     return style;
   }
  else return style();
}

const char *AplusTableColumn::reportFont(unsigned row_)
{
  if (reportFontFunc()->func()!=0)
   {
     invokeFunction(reportFontFunc(),row_);
     _reportFontStringBuffer = (char *) _outStr->p;
     dc(_outStr);
     _outStr=aplus_nl;
     return _reportFontStringBuffer;
   }
  else return reportFont();
}

double AplusTableColumn::fgGrayScale(unsigned row_)
{
  if (fgGrayScaleFunc()->func()!=0)
   {
     invokeFunction(fgGrayScaleFunc(),row_);
     if (_outStr->t == Ft || _outStr->t==It)
      {
	double r;
	P p; p.i = _outStr->p;
	if (_outStr->t==Ft) r = p.f[0];
	else r = ((double)p.i[0]) / 100.0;
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
     else
     showError("Non-numeric value in fgGrayScale Function");
   }
  return fgGrayScale();
}

double AplusTableColumn::bgGrayScale(unsigned row_)
{
  if (bgGrayScaleFunc()->func()!=0)
   {
     invokeFunction(bgGrayScaleFunc(),row_);
     if (_outStr->t == Ft || _outStr->t==It)
      {
	double r;
	P p; p.i = _outStr->p;
	if (_outStr->t==Ft) r = p.f[0];
	else r = ((double)p.i[0]) / 100.0;
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
     else
     showError("Non-numeric value in bgGrayScale Function");
   }
  return bgGrayScale();
}

double AplusTableColumn::breakFgGrayScale(unsigned row_)
{
  if (breakFgGrayScaleFunc()->func()!=0)
   {
     invokeFunction(breakFgGrayScaleFunc(),row_);
     if (_outStr->t == Ft || _outStr->t==It)
      {
	double r;
	P p; p.i = _outStr->p;
	if (_outStr->t==Ft) r = p.f[0];
	else r = ((double)p.i[0]) / 100.0;
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
     else
     showError("Non-numeric value in breakFgGrayScale Function");
   }
  return breakFgGrayScale();
}

double AplusTableColumn::breakBgGrayScale(unsigned row_)
{
  if (breakBgGrayScaleFunc()->func()!=0)
   {
     invokeFunction(breakBgGrayScaleFunc(),row_);
     if (_outStr->t == Ft || _outStr->t==It)
      {
	double r;
	P p; p.i = _outStr->p;
	if (_outStr->t==Ft) r = p.f[0];
	else r = ((double)p.i[0]) / 100.0;
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
     else
     showError("Non-numeric value in breakBgGrayScale Function");
   }
  return breakBgGrayScale();
}

unsigned long AplusTableColumn::breakStyle(unsigned row_)
{
  if (breakStyleFunc()->func()!=0)
   {
     invokeFunction(breakStyleFunc(),row_);
     unsigned style = GUIEnum.formatStyle(_outStr);
     dc(_outStr);
     _outStr=aplus_nl;
     return style;
   }
  else return breakStyle();
}

int AplusTableColumn::breakOffset(unsigned row_)
{
  if (breakOffsetFunc()->func()!=0)
   {
     invokeFunction(breakOffsetFunc(),row_);
     if (_outStr->t == It)
      {
	int r;
	r = (int) _outStr->p[0];
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
   }
  return breakOffset();
}

int AplusTableColumn::breakLeading(unsigned row_)
{
  if (breakLeadingFunc()->func()!=0)
   {
     invokeFunction(breakLeadingFunc(),row_);
     if (_outStr->t == It)
      {
	int r;
	r = (int) _outStr->p[0];
	dc(_outStr);
	_outStr=aplus_nl;
	return r;
      }
   }
  return breakLeading();
}

const char *AplusTableColumn::breakFont(unsigned row_)
{
  if (breakFontFunc()->func()!=0)
   {
     invokeFunction(breakFontFunc(),row_);
     _breakFontStringBuffer = (char *) _outStr->p;
     dc(_outStr);
     _outStr=aplus_nl;
     return _breakFontStringBuffer;
   }
  else return breakFont();
}

MSBoolean AplusTableColumn::isNumericColumn(void)
{
  AplusModel *m = (AplusModel *) model();

  if (m)
   {
     if (m->a_type()==Ft||m->a_type()==It) return MSTrue;
     return MSFalse;
   }
  return MSFalse;
}


void AplusTableColumn::breakProcess(MSIndexVector & i_)
{
  if (breakProcessFunc()->func()!=0)
   {
     MSIndexVector t=MSIndexVector::take(i_,-2);
     invokeFunction(breakProcessFunc(),t(0),t(1));
     if (isNull(_outStr)==MSFalse)
      {
	_breakProcessStringBuffer=(char *) _outStr->p;
	breakString()<<_breakProcessStringBuffer;
	dc(_outStr);
	_outStr=aplus_nl;
      }
   }
  else
   {
     if (isNumericColumn())
      {
	MSFloat aFloat;
	aFloat = computeIndexedSum(i_);
	MSString buf;
	breakString()<<aFloat.format(buf, format());
      }
   }
}

const char *AplusTableColumn::formatBreak(MSString& str_, unsigned row_, unsigned col_)
{
  if (formatBreakFunc()->func()!=0)
   {
     A data = AplusConvert::asA(breakString()(row_));
     invokeFunction(formatBreakFunc(),row_,col_,data);
     dc(data);
     if (isNull(_outStr)==MSFalse)
      {
 	str_=(char *)_outStr->p;
 	dc(_outStr);
 	_outStr=aplus_nl;
      }
   }
  else
   {
     if (isNumericColumn())
       {
	 if (row_<breakString().length())
	   {
	     str_=breakString()(row_);
	   }
       }
     else
      {
	unsigned index=reportTable()->breakIndex()(row_);
	if (col_==column()&&breakString().length()>0&&breakIndex().indexOf(index)<breakIndex().length())
	 {
	   int i=row_<breakString().length()?row_:breakString().length()-1;
	   str_=breakString()(i);
	 }
      }
   }
  return str_.string();
}

double AplusTableColumn::computeColumnTotal(void)
{
  if (isNumericColumn())
   {
     double sum=_algorithm->computeValue((A)((AplusModel*)model())->a());
     return sum;
   }
  return 0.0;
}

double AplusTableColumn::computeIndexedSum(MSIndexVector &i_)
{
  if (isNumericColumn())
   {
     MSIndexVector t=MSIndexVector::take(i_,-2);
     double sum=_algorithm->computeValue((A)((AplusModel*)model())->a(), t(0), t(1));
     return sum;
   }
  return 0.0;
}

A ABreakFunction::callFunc(V v_, A a_, int row_, int col_,A p_)
{
  A i=0;
  if (row_!=-1||col_!=-1)
  {
    i = (A)gv(It,2);
    P p; p.i=i->p;
    p.i[0]=row_;
    p.i[1]=col_;
  }
  A r=(_func!=0)?(A)(*_func)(_arg,a_,i,p_,v_):(A)gsv(0,"");
  if (i!=0) dc((A)i);
  if (a_!=0) dc((A)a_);
  return r;
}

void AplusTableColumn::compMode(unsigned compMode_)
{
  _compMode = compMode_;
  switch(_compMode)
   {
   case AplusReportAlgorithm::Sum:
     _algorithm = AplusReportSumAlgorithm::reportSum();
     break;
   case AplusReportAlgorithm::Max:
     _algorithm = AplusReportMaxAlgorithm::reportMax();
     break;
   case AplusReportAlgorithm::Min:
     _algorithm = AplusReportMinAlgorithm::reportMin();
     break;
   case AplusReportAlgorithm::Avg:
     _algorithm = AplusReportAvgAlgorithm::reportAvg();
     break;
   case AplusReportAlgorithm::StdDev:
     _algorithm = AplusReportStdDevAlgorithm::reportStdDev();
     break;
   case AplusReportAlgorithm::Variance:
     _algorithm = AplusReportVarianceAlgorithm::reportVariance();
     break;
   }
}

void AplusTableColumn::updateFont(Font oldfont_)
{
  if (model()!=0 && ((AplusModel*)model())->aplusVar()!=0)
   {
     // If we're using a functional font attr, use cell 0
     // as the base font size.  This is a limitation on
     // size calculation.
     Font f = cellFont(0);
     if (font()!=f) _fontID=f;
   }

  // The code below is practically copied from MSTableColumn::updateFont()
  // in order to turn off the MStk's feature of setting the heading font
  // from the column font if the heading font has not been set yet.
  //
  if (model()!=0)
   {
     MSWidget::updateFont(oldfont_);
     MSBoolean wasFrozen=table()->frozen();
     table()->freeze();
     fontStruct((XFontStruct *)server()->fontStruct(font()));
     table()->calculateRowHeight();
     table()->adjustNumVisible();
     if (wasFrozen==MSFalse) table()->unfreeze();
   }
}

void AplusTableColumn::updateForeground(unsigned long oldfg_)
{
  MSWidget::updateForeground(oldfg_);
  redraw();
}

void AplusTableColumn::forceEval(void) const
{
  if(model()!=0)
    {
      ((AplusModel*)model())->a();
    }
}


void AplusTableColumn::columnResizeCB(void)
{
  AplusModel *pModel = (AplusModel *)model();
  if (pModel!=0)
    {
      AVariableData *pVarData = pModel->pAVarData();
      if (pVarData!=0)
	{
	  pVarData->colWidth(columnWidth(),MSFalse); // update colWidth and DO NOT send an event to the view
	  activateCallback("columnresizecb");
	}
    }
}


void AplusTableColumn::setClipMode(void)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel!=0)
    {
      V v=pModel->aplusVar();
      if (v!=0)
	{
	  AVariableData *varData = ::pAVarDataFromV(v);
	  if (varData->stars()==MSTrue)
	    {
	      clipMode(MSClipStars);
	    }
	  else
	    {
	      clipMode(MSNoClipping);
	    }
	}
    }
}


unsigned AplusTableColumn::numRows(void) const
{
  return(model()!=0)?((AplusModel*)model())->numRows():0;
}


unsigned AplusTableColumn::numColumns(void) const
{
  return(model()!=0)?((AplusModel*)model())->numCols():0;
}


const MSSymbol& AplusTableColumn::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusTableColumn::symbol(void)
{
  static MSSymbol sym("AplusTableColumn");
  return sym;
}
