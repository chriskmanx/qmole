///////////////////////////////////////////////////////////////////////////////
//
// Copyrightg (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusTrace.H>
#include <AplusGUI/AplusGraph.H>

extern long dbg_tmstk;
extern A grc(A,int,int);
extern int safeAset(V,::A,::A,::A);

const int   EnumHashTableSize=64;
static const int DefaultTraceSymbolSize=11;

MSBoolean    AplusTraceSet::_initialized=MSFalse;
MSHashTable  AplusTraceSet::_enumHashTable(EnumHashTableSize);
MSHashTable  AplusTraceSet::_constraintEnumHashTable(EnumHashTableSize);
MSHashTable  AplusTraceSet::_styleEnumHashTable(EnumHashTableSize);
MSHashTable  AplusTraceSet::_symbolEnumHashTable(EnumHashTableSize);

AplusTrace::AplusTrace(MSTraceSet *traceSet_,int col_,const MSSymbol& tag_): MSTrace(traceSet_,col_,tag_)
{
  _gradient=MSFalse;
}

AplusTrace::~AplusTrace(void)
{}

V AplusTrace::aplusVar(void)
{
  V v=0;
  AplusTraceSet *ats=(AplusTraceSet *) traceSet();
  if (ats) v=((AplusModel *)ats->model())->aplusVar();
  return v;
}

AplusTraceSet::AplusTraceSet(MSGraph *owner_) : MSTraceSet(owner_)
{
  if (_initialized==MSFalse)
   {
     _initialized=MSTrue;
     initEnumHashTable();
     initStringEnumHashTable();
   }
  _outStr=aplus_nl;
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusTraceSet::~AplusTraceSet(void)
{ dc(_outStr); }


void AplusTraceSet::initEnumHashTable(void)
{
  enumHashTable()->notFound((unsigned long)0);
  enumHashTable()->add(""               ,(void*)MSNone);
  enumHashTable()->add("traceStyle" 	,(void*)_TraceStyle);
  enumHashTable()->add("traceSymbol"	,(void*)_Symbol);
  enumHashTable()->add("traceConstraint",(void*)_Constraint);
  enumHashTable()->add("traceLineStyle" ,(void*)_LineStyle);
  enumHashTable()->add("none"		,(void*)MSNone);
  enumHashTable()->add("line"		,(void*)MSG::Line);
  enumHashTable()->add("outline"	,(void*)MSG::Outline);
  enumHashTable()->add("scatter"	,(void*)MSG::Scatter);
  enumHashTable()->add("linescatter"    ,(void*)(MSG::Line | MSG::Scatter));
  enumHashTable()->add("ls"  		,(void*)(MSG::Line | MSG::Scatter));
  enumHashTable()->add("area"		,(void*)MSG::Area);
  enumHashTable()->add("bar" 		,(void*)MSG::Bar);
  enumHashTable()->add("c"   		,(void*)MSG::Close);
  enumHashTable()->add("close"      	,(void*)MSG::Close);
  enumHashTable()->add("highlow"    	,(void*)MSG::HL);
  enumHashTable()->add("hl"         	,(void*)MSG::HL);
  enumHashTable()->add("highlowoc"  	,(void*)MSG::HLOC);
  enumHashTable()->add("ohlc"       	,(void*)MSG::HLOC);
  enumHashTable()->add("highlowc"   	,(void*)MSG::HLC);
  enumHashTable()->add("hlc"        	,(void*)MSG::HLC);
  enumHashTable()->add("candle"     	,(void*)MSG::Candle);
  enumHashTable()->add("stack"      	,(void*)MSG::Stack);
  enumHashTable()->add("step"       	,(void*)MSG::Step);
  enumHashTable()->add("stepscatter"	,(void*)(MSG::Step | MSG::Scatter));
  enumHashTable()->add("ss"  		,(void*)(MSG::Step | MSG::Scatter));
  enumHashTable()->add("pie" 		,(void*)MSG::Pie);
  enumHashTable()->add("osc" 		,(void*)MSG::Osc);
  enumHashTable()->add("fill"		,(void*)MSG::Fill);
  enumHashTable()->add("segment"	,(void*)MSG::Segment);
  enumHashTable()->add("text"   	,(void*)MSG::Text);
  enumHashTable()->add("marketprofile"  ,(void*)MSG::MarketProfile);
  enumHashTable()->add("colorprofile"   ,(void*)MSG::ColorProfile);
  enumHashTable()->add("mp"		,(void*)MSG::MarketProfile);
  enumHashTable()->add("cp"		,(void*)MSG::ColorProfile);
  enumHashTable()->add("circle"         ,(void*)MSG::Circle);
  enumHashTable()->add("circlecross"    ,(void*)(MSG::Cross | MSG::Circle));
  enumHashTable()->add("circlediamond"  ,(void*)(MSG::Diamond | MSG::Circle));
  enumHashTable()->add("circletriangle" ,(void*)(MSG::Triangle | MSG::Circle));
  enumHashTable()->add("circlefilled"   ,(void*)(MSG::Circle | MSG::Fill));
  enumHashTable()->add("cross"          ,(void*)MSG::Cross);
  enumHashTable()->add("diamond"      	,(void*)MSG::Diamond);
  enumHashTable()->add("diamondcross" 	,(void*)(MSG::Diamond | MSG::Cross));
  enumHashTable()->add("diamondfilled"	,(void*)(MSG::Diamond | MSG::Fill));
  enumHashTable()->add("square"       	,(void*)MSG::Square);
  enumHashTable()->add("squarecross"  	,(void*)(MSG::Square | MSG::Cross));
  enumHashTable()->add("squarecircle" 	,(void*)(MSG::Square | MSG::Circle));
  enumHashTable()->add("squarefilled" 	,(void*)(MSG::Square | MSG::Fill));
  enumHashTable()->add("star"         	,(void*)(MSG::Cross | MSG::X));
  enumHashTable()->add("triangle"     	,(void*)MSG::Triangle);
  enumHashTable()->add("trianglefilled" ,(void*)(MSG::Triangle | MSG::Fill));
  enumHashTable()->add("xsym"           ,(void*)MSG::X);
  enumHashTable()->add("x"		,(void*)MSG::HoldX);
  enumHashTable()->add("y"		,(void*)MSG::HoldY);
}

void AplusTraceSet::initStringEnumHashTable(void)
{
  constraintEnumHashTable()->notFound((unsigned long)0);

  constraintEnumHashTable()->add((unsigned long)MSNone,(void*)"none");  
  constraintEnumHashTable()->add(MSG::HoldY           ,(void*)"y");  
  constraintEnumHashTable()->add(MSG::HoldX  	      ,(void*)"x");

  styleEnumHashTable()->notFound((unsigned long)0);
  styleEnumHashTable()->add((unsigned long) MSNone   ,(void*)"none");  
  styleEnumHashTable()->add(MSG::Line                ,(void*)"line");
  styleEnumHashTable()->add(MSG::Outline             ,(void*)"outline");
  styleEnumHashTable()->add(MSG::Scatter             ,(void*)"scatter");
  styleEnumHashTable()->add(MSG::Line | MSG::Scatter ,(void*)"ls");
  styleEnumHashTable()->add(MSG::Area                ,(void*)"area");
  styleEnumHashTable()->add(MSG::Bar                 ,(void*)"bar");
  styleEnumHashTable()->add(MSG::Close               ,(void*)"close");
  styleEnumHashTable()->add(MSG::HL                  ,(void*)"hl");
  styleEnumHashTable()->add(MSG::HLOC                ,(void*)"ohlc");
  styleEnumHashTable()->add(MSG::HLC                 ,(void*)"hlc");
  styleEnumHashTable()->add(MSG::Candle              ,(void*)"candle");
  styleEnumHashTable()->add(MSG::Pie                 ,(void*)"pie");
  styleEnumHashTable()->add(MSG::Stack               ,(void*)"stack");
  styleEnumHashTable()->add(MSG::Step                ,(void*)"step");
  styleEnumHashTable()->add(MSG::Step | MSG::Scatter ,(void*)"stepscatter");
  styleEnumHashTable()->add(MSG::Osc           	     ,(void*)"osc");
  styleEnumHashTable()->add(MSG::Fill                ,(void*)"fill");
  styleEnumHashTable()->add(MSG::Segment             ,(void*)"segment");
  styleEnumHashTable()->add(MSG::Text                ,(void*)"text");
  styleEnumHashTable()->add(MSG::MarketProfile       ,(void*)"marketprofile");
  styleEnumHashTable()->add(MSG::ColorProfile        ,(void*)"colorprofile");


  symbolEnumHashTable()->notFound((unsigned long)0);
  symbolEnumHashTable()->add((unsigned long) MSNone      ,(void*)"none");  
  symbolEnumHashTable()->add(MSG::Circle        	 ,(void*)"circle");
  symbolEnumHashTable()->add(MSG::Circle | MSG::Cross    ,(void*)"circlecross");
  symbolEnumHashTable()->add(MSG::Circle | MSG::Diamond  ,(void*)"circlediamond");
  symbolEnumHashTable()->add(MSG::Circle | MSG::Triangle ,(void*)"circletriangle");
  symbolEnumHashTable()->add(MSG::Circle | MSG::Fill     ,(void*)"circlefilled");
  symbolEnumHashTable()->add(MSG::Cross         	 ,(void*)"cross");
  symbolEnumHashTable()->add(MSG::Diamond       	 ,(void*)"diamond");
  symbolEnumHashTable()->add(MSG::Diamond | MSG::Cross   ,(void*)"diamondcross");
  symbolEnumHashTable()->add(MSG::Diamond | MSG::Fill    ,(void*)"diamondfilled");
  symbolEnumHashTable()->add(MSG::Square        	 ,(void*)"square");
  symbolEnumHashTable()->add(MSG::Square | MSG::Cross    ,(void*)"squarecross");
  symbolEnumHashTable()->add(MSG::Square | MSG::Circle   ,(void*)"squarecircle");
  symbolEnumHashTable()->add(MSG::Square | MSG::Fill     ,(void*)"squarefilled");
  symbolEnumHashTable()->add(MSG::Cross | MSG::X       	 ,(void*)"star");
  symbolEnumHashTable()->add(MSG::Triangle      	 ,(void*)"triangle");
  symbolEnumHashTable()->add(MSG::Triangle | MSG::Fill   ,(void*)"trianglefilled");
  symbolEnumHashTable()->add(MSG::X          	         ,(void*)"xsym");
}

A AplusTraceSet::enumSymbols(const char *str_)
{ 
  A		r=aplus_nl;
  TraceEnum	k;

  if ((k=(TraceEnum)(unsigned long)enumHashTable()->lookup(str_))>=0&&
      (k>=_TraceStyle&&k<=_Constraint))
   {
     switch(k)
      {
      case _TraceStyle:
	r=gv(Et,20);
	r->p[0]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Line                )));
	r->p[1]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Outline             )));
	r->p[2]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Scatter             )));
	r->p[3]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Line | MSG::Scatter )));
	r->p[4]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Bar                 )));
	r->p[5]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Area                )));
	r->p[6]=MS(si((char *) styleEnumHashTable()->lookup(MSG::Close               )));
	r->p[7]=MS(si((char *) styleEnumHashTable()->lookup(MSG::HL                  )));
	r->p[9]=MS(si((char *) styleEnumHashTable()->lookup(MSG::HLOC                )));
	r->p[8]=MS(si((char *) styleEnumHashTable()->lookup(MSG::HLC                 )));
	r->p[10]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Candle              )));
	r->p[10]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Pie                 )));
	r->p[11]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Stack               )));
	r->p[12]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Step                )));
	r->p[13]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Step | MSG::Scatter )));
	r->p[14]=MS(si((char *)styleEnumHashTable()->lookup(MSG::MarketProfile       )));
	r->p[15]=MS(si((char *)styleEnumHashTable()->lookup(MSG::ColorProfile        )));
	r->p[16]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Fill                )));
	r->p[17]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Segment             )));
	r->p[18]=MS(si((char *)styleEnumHashTable()->lookup(MSG::Text                )));
	r->p[19]=MS(si((char *)styleEnumHashTable()->lookup((unsigned long)MSNone    )));
	break;
	
      case _Symbol:
	r=gv(Et,12);
	r->p[0]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Square	       )));
	r->p[1]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Square | MSG::Fill   )));
	r->p[2]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Circle	       )));
	r->p[3]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Circle | MSG::Fill   )));
	r->p[4]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Diamond	       )));
	r->p[5]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Diamond | MSG::Fill  )));
	r->p[6]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Cross		       )));
	r->p[7]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::X		       )));
	r->p[8]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Cross | MSG::X       )));
	r->p[9]=MS(si((char *) symbolEnumHashTable()->lookup(MSG::Triangle	       )));
	r->p[10]=MS(si((char *)symbolEnumHashTable()->lookup(MSG::Triangle | MSG::Fill )));
	r->p[11]=MS(si("char"));
	break;

      case _Constraint:
	r=gv(Et,3);
	r->p[0]=MS(si((char *) constraintEnumHashTable()->lookup((unsigned long) MSNone)));
	r->p[1]=MS(si((char *) constraintEnumHashTable()->lookup(MSG::HoldX            )));
	r->p[2]=MS(si((char *) constraintEnumHashTable()->lookup(MSG::HoldY            )));
	break;			
	
      case _LineStyle:
	r=gv(Et,4);
	r->p[0]=MS(si((char *)graph()->stringEnumHashTable()->lookup(AplusGraph::Solid)));
	r->p[1]=MS(si((char *)graph()->stringEnumHashTable()->lookup(AplusGraph::Dash2)));
	r->p[2]=MS(si((char *)graph()->stringEnumHashTable()->lookup(AplusGraph::DotDash2)));
	r->p[3]=MS(si((char *)graph()->stringEnumHashTable()->lookup(AplusGraph::Dot2)));
	break;
      }
   }
  else graph()->enumError()->showError(((AplusModel*)model())->aplusVar(),str_);
  return r;
}



////////////////////////////////////////////////////////////////////////////////////////////////////
// The following functions set the functions attributes
////////////////////////////////////////////////////////////////////////////////////////////////////

void AplusTraceSet::lineStyleFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  lineStyleFunc()->func(func_);
  lineStyleFunc()->arg(ac);
  lineStyleFuncInvoke();
}

void AplusTraceSet::lineWidthFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  lineWidthFunc()->func(func_);
  lineWidthFunc()->arg(ac);
  lineWidthFuncInvoke();
}
void AplusTraceSet::pieOffsetFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  pieOffsetFunc()->func(func_);
  pieOffsetFunc()->arg(ac);
  pieOffsetFuncInvoke();
}
void AplusTraceSet::gradientFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  gradientFunc()->func(func_);
  gradientFunc()->arg(ac);
  gradientFuncInvoke();
}
void AplusTraceSet::fillColorFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  AColorFunction *f=(AColorFunction *) fillColorFunc();
  f->func(func_);
  f->arg(ac);
  fillColorFuncInvoke();
}
void AplusTraceSet::lineColorFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  AColorFunction *f=(AColorFunction *) lineColorFunc();
  f->func(func_);
  f->arg(ac);
  lineColorFuncInvoke();
}
void AplusTraceSet::legendFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  legendFunc()->func(func_);
  legendFunc()->arg(ac);
  legendFuncInvoke();
}
void AplusTraceSet::traceSymbolFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]);
  traceSymbolFunc()->func(func_);
  traceSymbolFunc()->arg(ac);
  traceSymbolFuncInvoke();
}
void AplusTraceSet::traceSymbolSizeFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]); 
  traceSymbolSizeFunc()->func(func_); 
  traceSymbolSizeFunc()->arg(ac);
  traceSymbolSizeFuncInvoke();
}
void AplusTraceSet::altXaxisFunc(AFunc func_,A fc_)
{
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]); 
  altXaxisFunc()->func(func_); 
  altXaxisFunc()->arg(ac);
  altXaxisFuncInvoke();
}
void AplusTraceSet::altYaxisFunc(AFunc func_,A fc_)
{
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]); 
  altYaxisFunc()->func(func_); 
  altYaxisFunc()->arg(ac);
  altYaxisFuncInvoke();
}
void AplusTraceSet::traceStyleFunc(AFunc func_,A fc_)
{ 
  AClientData *ac=new AClientData((A)fc_->p[0],(A)fc_->p[1]); 
  traceStyleFunc()->func(func_); 
  traceStyleFunc()->arg(ac);
  traceStyleFuncInvoke();
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// The following functions invoke and set the attributes
////////////////////////////////////////////////////////////////////////////////////////////////////

void AplusTraceSet::lineStyleFuncInvoke(void)
{
  if (lineStyleFunc()->func()!=0)
   {
     A sym=aplus_nl;

     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedLongVector styleVector;
     MSUnsignedVector weightVector;
     unsigned long style,weight;
     AplusGraph::LineStyle s;
     
     if (rank==1)
      {
	sym=(A) lineStyleFunc()->invoke(v,a,-1,-1,aplus_nl);
	if ((s=(AplusGraph::LineStyle)(unsigned long)graph()->enumHashTable()->lookup((char *)XS(*sym->p)->n))>0&&
	    graph()->styleConvert(s,style,weight)==MSTrue)
	 {
	   styleVector<<(unsigned long) style;
	   weightVector<<(unsigned) weight;
	 }
	else graph()->enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym->p[0])->n);
	dc(sym);
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   sym=(A) lineStyleFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   if ((s=(AplusGraph::LineStyle)(unsigned long)graph()->enumHashTable()->lookup((char *)XS(*sym->p)->n))>0&&
	       graph()->styleConvert(s,style,weight)==MSTrue)
	    {
	      styleVector<<(unsigned long) style;
	      weightVector<<(unsigned) weight;
	    }
	   else graph()->enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym->p[0])->n);
	   dc(sym);
	 }
      }
     if (styleVector.length()>0)
     {
       freeze();
       lineStyle(styleVector);
       unfreeze();
     }
     if (weightVector.length()>0) lineWeight(weightVector);
   }
}

void AplusTraceSet::lineWidthFuncInvoke(void)
{
  if (lineWidthFunc()->func()!=0)
   {
     int ret;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedVector uv;

     if (rank==1)
      {
	ret=(int) lineWidthFunc()->intInvoke(v,a,-1,-1,aplus_nl);
	uv<<(unsigned) (ret<0?0:ret);
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   ret=lineWidthFunc()->intInvoke(v,a,-1,i+1,aplus_nl);
	   uv<<(unsigned) (ret<0?0:ret);
	 }
      }
     if (uv.length()>0) lineWidth(uv);
   }
}

void AplusTraceSet::pieOffsetFuncInvoke(void)
{
  if (pieOffsetFunc()->func()!=0)
   {
     int ret;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSFloatVector fv;

     if (rank==1)
      {
	ret= pieOffsetFunc()->intInvoke(v,a,-1,-1,aplus_nl);
	fv<<(double) (ret<0?0:ret);
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   ret=pieOffsetFunc()->intInvoke(v,a,-1,i+1,aplus_nl);
	   fv<<(double) (ret<0?0:ret);
	 }
      }
     if (fv.length()>0) pieOffsets(fv);
   }
}

void AplusTraceSet::gradientFuncInvoke(void)
{
  if (gradientFunc()->func()!=0)
   {
     int ret;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedVector uv;

     if (rank==1)
      {
	ret=(int) gradientFunc()->intInvoke(v,a,-1,-1,aplus_nl);
	uv<<(unsigned) (ret<0?0:ret);
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   ret=gradientFunc()->intInvoke(v,a,-1,i+1,aplus_nl);
	   uv<<(unsigned) (ret<0?0:ret);
	 }
      }
     if (uv.length()>0) gradient(uv);
   }
}

void AplusTraceSet::fillColorFuncInvoke(void)
{
  AColorFunction *f=(AColorFunction *) fillColorFunc();
  if (f->func()!=0)
   {
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedLongVector uv;
     AplusTrace *t=trace(0);

     if (rank!=1) 
      {
	for(int i=0;i<traceList().count();i++)
         {
           uv<<(unsigned long) f->invoke(v,a,-1,i+1,aplus_nl);
         }
      }
     else if (t!=0&&t->style()==MSG::Pie)
      {
	for(int i=0;i<dataCount();i++)
         {
           uv<<(unsigned long) f->invoke(v,a,-1,i+1,aplus_nl);
         }
      }
     else uv<<(unsigned long) f->invoke(v,a,-1,-1,aplus_nl);
     if (uv.length()>0) MSTraceSet::fillColor(uv);
   }
}

void AplusTraceSet::lineColorFuncInvoke(void)
{
  AColorFunction *f=(AColorFunction *)lineColorFunc();
  if (f->func()!=0)
   {
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedLongVector uv;

     if (rank!=1) 
      {
	for(int i=0;i<traceList().count();i++)
         {
           uv<<(unsigned long) f->invoke(v,a,-1,i+1,aplus_nl);
         }
      }
     else uv<<(unsigned long) f->invoke(v,a,-1,-1,aplus_nl);
     if (uv.length()>0) MSTraceSet::lineColor(uv);
   }
}

void AplusTraceSet::legendFuncInvoke(void)
{
  A outStr=aplus_nl;
  if (legendFunc()->func()!=0)
   {
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();

     if (rank==1)
      {
	outStr=(A) legendFunc()->invoke(v,a,-1,-1,aplus_nl);
	legend((char *)outStr->p);
      }
     else
      {
	MSStringVector sv;
	for(int i=0;i<traceList().count();i++)
	 {
	   outStr=(A) legendFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   sv<<MSString((char *) outStr->p);
	 }
	legend(sv);
      }
   }
}

void AplusTraceSet::traceStyleFuncInvoke(void)
{
  if (traceStyleFunc()->func()!=0)
   {
     A sym=aplus_nl;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedLongVector traceStyleVector;
     unsigned long s;
     
     if (rank==1)
      {
	sym=(A) traceStyleFunc()->invoke(v,a,-1,-1,aplus_nl);
	traceStyleVector<<convertTraceStyle(sym);
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   sym=(A) traceStyleFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   s=convertTraceStyle(sym);
	   traceStyleVector<<s;
	   if (i==0&&s>=MSG::Close) // If one of the 4-column styles,ignore the rest
	   {
	     style(s);
	     return;
	   }
	 }
      }
     style(traceStyleVector);
     // invoke line width functions
   }
}

void AplusTraceSet::traceSymbolFuncInvoke(void)
{
  if (traceSymbolFunc()->func()!=0)
   {
     A sym=aplus_nl;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     MSUnsignedLongVector symbolVector;
     MSStringVector textVector;
     unsigned long traceSym;
     MSString stringSymbol;
     
     if (rank==1)
      {
	sym=(A) traceSymbolFunc()->invoke(v,a,-1,-1,aplus_nl);
	convertTraceSymbol(sym,traceSym,stringSymbol);
	symbolVector<<traceSym;
	textVector<<stringSymbol;
      }
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   sym=(A) traceSymbolFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   convertTraceSymbol(sym,traceSym,stringSymbol);
	   symbolVector<<traceSym;
	   textVector<<stringSymbol;
	 }
      }
     MSTraceSet::symbol(symbolVector);
     textSymbol(textVector);
   }
}

void AplusTraceSet::traceSymbolSizeFuncInvoke(void)
 {
  if (traceSymbolSizeFunc()->func()!=0)
   {
     MSUnsignedVector sizeVector;
     unsigned ret=DefaultTraceSymbolSize;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();
     int r;
     if (rank==1)
     {
       r=(int) (traceSymbolSizeFunc()->intInvoke(v,a,-1,-1,aplus_nl));
       sizeVector<<(unsigned) ((r<=0)?ret:r);
     }
     
     else
      {
	for(int i=0;i<traceList().count();i++)
	 {
	   r=(int) (traceSymbolSizeFunc()->intInvoke(v,a,-1,i+1,aplus_nl));
	   sizeVector<<(unsigned) ((r<=0)?ret:r);
	 }
      }
     if (sizeVector.length()>0) symbolSize(sizeVector);
   }
}

void AplusTraceSet::altXaxisFuncInvoke(void)
{
  if (altXaxisFunc()->func()!=0)
   {
     A sym;
     unsigned long axis;
     MSUnsignedLongVector axisVector;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();

     if (rank==1)
      {
	sym=altXaxisFunc()->invoke(v,a,-1,-1,aplus_nl);
	if (convertXAxis(sym,axis)==MSTrue) axisVector<<axis;
      }
     else
      {
	for (int i=0;i<traceList().count();i++)
	 {
	   sym=altXaxisFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   if (convertXAxis(sym,axis)==MSTrue) axisVector<<axis;
	 }
      }
     if (axisVector.length()>0) xAxis(axisVector, MSInit);
   }
}

void AplusTraceSet::altYaxisFuncInvoke(void)
{
  if (altYaxisFunc()->func()!=0)
   {
     A sym;
     unsigned long axis;
     MSUnsignedLongVector axisVector;
     V v=((AplusModel*)model())->aplusVar();
     A a=(A)((AplusModel*)model())->a();
     int rank=((AplusModel*)model())->rank();

     if (rank==1)
      {
	sym=altYaxisFunc()->invoke(v,a,-1,-1,aplus_nl);
	if (convertYAxis(sym,axis)==MSTrue) axisVector<<axis;
      }
     else
      {
	for (int i=0;i<traceList().count();i++)
	 {
	   sym=altYaxisFunc()->invoke(v,a,-1,i+1,aplus_nl);
	   if (convertYAxis(sym,axis)==MSTrue) axisVector<<axis;
	 }
      }
     if (axisVector.length()>0) yAxis(axisVector, MSInit);
   }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// The follow functions converts A symbols to the proper MSTK enums/values
////////////////////////////////////////////////////////////////////////////////////////////////////

unsigned long AplusTraceSet::convertTraceStyle(A sym_)
{
  long style=-1;
  V v=((AplusModel*)model())->aplusVar();
  
  if (isNull(sym_)==MSFalse)
    {
      if (QS(sym_))
	{
	  style=(long)(enumHashTable()->lookup((char *)XS(sym_)->n));
	}	
      else if (sym_->t==Et&&sym_->n>0&&QS(*sym_->p))  
	{
	  style=(long)(enumHashTable()->lookup((char *)XS(*sym_->p)->n));
	}
      
      if (style<MSNone || style>MSG::ColorProfile)
	{
	  graph()->enumError()->showError(v,(char *)XS(sym_->p[0])->n); 
	  style=(long)MSG::Line;
	}
    }

  return style==-1?MSG::Line:(unsigned long)style;
}


void AplusTraceSet::convertTraceSymbol(A sym_,unsigned long&traceSym_,MSString&string_)
{
  unsigned long symbol=MSG::Cross;
  
  if (QS(sym_))
   {
     symbol=(unsigned long) enumHashTable()->lookup((char *)XS(sym_)->n);
     string_=(char *)XS(sym_)->n;
   }
  else if (sym_->t==Et&&sym_->n>0&&QS(*sym_->p))  
   {
     symbol=(unsigned long) enumHashTable()->lookup((char *)XS(*sym_->p)->n);
     string_=(char *)XS(*sym_->p)->n;
     dc(sym_);
   }
  else if (sym_->t==Ct&&sym_->n>0)
   {
     string_=(char *)sym_->p;
     dc(sym_);
     symbol=MSG::Text;
   }
  traceSym_=(symbol<MSG::Cross || symbol>(MSG::Triangle | MSG::Fill))?MSG::Text:symbol;
}

MSBoolean AplusTraceSet::convertYAxis(A sym_,unsigned long&axis_)
{
  if (isNull(sym_)==MSFalse && QA(sym_) && QS(*sym_->p))
   {
     char *style=(char *)XS(sym_->p[0])->n;
     axis_=(strcmp(style,"Y")==0)?MSRight:(strcmp(style,"y")==0)?MSLeft:-1;
     V v=((AplusModel*)model())->aplusVar();
     if (axis_<0) graph()->enumError()->showError(v,(char *)XS(sym_->p[0])->n);
    return MSTrue;
   }
  return MSFalse;
}

MSBoolean AplusTraceSet::convertXAxis(A sym_,unsigned long&axis_)
{
  if (isNull(sym_)==MSFalse && QA(sym_) && QS(*sym_->p))
   {
     char *style=(char *)XS(sym_->p[0])->n;
     axis_=(strcmp(style,"X")==0)?MSTop:(strcmp(style,"x")==0)?MSLeft:-1;
     V v=((AplusModel*)model())->aplusVar();
     if (axis_<0) graph()->enumError()->showError(v,(char *)XS(sym_->p[0])->n);
     return MSTrue;
   }
  return MSFalse;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

A AplusTraceSet::selected(void)  // Call selectRow and selectColumn
{
  A index=0;
  A av=(A)((AplusModel *)model())->aplusVar()->a;  
  int row=selectRow();
  int col=selectCol();

  if (row!=-1&&col!=-1)
    {
      if (av->r==2)
       {
         index=gv(It,2);
         index->p[0]=(I)row;
         index->p[1]=(I)col+1;
      }
      else 
       {
         index=gv(It,1);
         index->p[0]=(I)col+1;
      }
   }
   return index;
}

void AplusTraceSet::selected(A index_)
{
  if (isNull(index_)==MSFalse && (index_->t==It||index_->t==Ft))
   {
     P p; p.i=index_->p; 
     A av=(A)((AplusModel *)model())->aplusVar()->a;  
     int row=-1,col=0;
     if (av->r==2&&index_->n==2)
      { 
	row=(int)(index_->t==Ft?p.f[0]:p.i[0]);
	col=(int)(index_->t==Ft?p.f[1]:p.i[1]);
	if (col<1||col>numColumns()) row=-1;
      }
     else if (av->r==1)
      {
	row=(int)(index_->t==Ft?p.f[0]:p.i[0]);
      }	
     if (row>0&&col<(numColumns())&&row<dataCount())
      { 
	selected(row,col-1);  
	graph()->highlightPoint(this);
      }
   }
}

A AplusTraceSet::constraintSym(void)
{
  A a=gs(Et); 
  *a->p=MS(si((char *)constraintEnumHashTable()->lookup(constraint())));
  return a;
}

void AplusTraceSet::constraint(A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     unsigned long k; 
     if ((k=(Constraint)(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>=0&&
	 (k>=MSNone&&k<=MSG::HoldY)) 
      { 
	MSTraceSet::constraint(k); 
      } 
     else graph()->enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
} 


A AplusTraceSet::coordinate(void)
{ 
  return computeAdjustedData(xOffset(),yOffset());
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Virtual functions from MSTraceSet to access data
////////////////////////////////////////////////////////////////////////////////////////////////////


int AplusTraceSet::numRows(void) const
{
  return(model()!=0)?((AplusModel*)model())->numRows():0;
}

int AplusTraceSet::numColumns(void) const
{
  A av=((AplusModel*)model())->a();
  return (av->r<=1?1:av->d[1]<2?2:av->d[1]);
}

int AplusTraceSet::dataCount(void) const
{
  A av=(A)((AplusModel*)model())->a();
  return (av->r<=1)?av->n:av->r==2&&av->n==0?0:av->d[0];
}

double AplusTraceSet::x(int index_) const
{
  double r=index_;
  A av=(A)((AplusModel*)model())->aplusVar()->a;  

  if (av->r==2) 
   {
     P p; p.i=av->p;
     int offset=index_*(int)av->d[1];
     r=(av->t==It)?(double)p.i[offset]:p.f[offset];
   }
  return r; 
}

double AplusTraceSet::y(int index_,int col_) const
{ 
  double r=0.0;
  A av=(A)((AplusModel*)model())->aplusVar()->a;  
  P p; p.i=av->p;

  if (av->r==1)
   {
     r=(av->t==It)?(double)p.i[index_]:p.f[index_];     
   }
  if (av->r==2)
   {
     int offset=index_*(int)av->d[1]+col_+1;
     r=(av->t==It)?(double)p.i[offset]:p.f[offset];
   }
  return r; 
}

unsigned long AplusTraceSet::traceLineColor(int row_,int col_) const
{
  unsigned long ret=foreground();
  V v=((AplusModel*)model())->aplusVar();
  int rank=((AplusModel*)model())->rank();
  A a=(A)((AplusModel*)model())->a();
  AColorFunction *f=(AColorFunction *) lineColorFunc();  // Need to cast away constness
  AplusTrace *t=trace(col_);
  
  if (t&&t->gradient()==MSTrue&&f->func()!=0)
  {
    int c=rank==1?-1:col_;
    double fv=t->y(row_);
    ret=(unsigned long) f->invoke(v,(a->t==It)?(int)fv:fv,row_,c,aplus_nl);
  }
  else if (t) ret=MSTraceSet::lineColor(col_);
  return ret;
}

unsigned long AplusTraceSet::traceFillColor(int row_,int col_) const
{
  unsigned long ret=foreground();
  V v=((AplusModel*)model())->aplusVar();
  int rank=((AplusModel*)model())->rank();
  A a=(A)((AplusModel*)model())->a();
  AColorFunction *f=(AColorFunction *) fillColorFunc();  // Need to cast away constness
  AplusTrace *t=trace(col_);
  
  if (t&&(t->gradient()==MSTrue||t->style()==Pie)&&f->func()!=0)
  {
    int c=rank==1?-1:col_;
    double fv=t->y(row_);
    ret=(unsigned long) f->invoke(v,(a->t==It)?(int)fv:fv,row_,c,aplus_nl);
  }
  else if (t) ret=MSTraceSet::fillColor(col_);
  return ret;
}

void AplusTraceSet::validate(int row_,int col_,double x_,double y_)
{
  A 		av=(A)((AplusModel*)model())->aplusVar()->a;
  V             v=((AplusModel*)model())->aplusVar();
  int 		i,setCols;
  A 		data,index,cols;

  if (av->r==1) // simple vector
   {
     A i=grc(av,row_,0);
     A d=(av->t==It)?gi((int)y_):gf(y_);
     if (safeAset(v,d,i,0)==0) showError(qs); 
     dc(i);
   }
  else if (av->r==2) // simple matrix
   {
     unsigned long traceStyle=trace(col_)->style();
     col_++;
     if (traceStyle>=MSG::Candle && traceStyle<=MSG::HLC)
      {
	setCols=numColumns()-1;
	data=gv(av->t,setCols);
	P p; p.i=data->p;
	for (i=0;i<setCols;i++)
	 {
	   (av->t==It)?(p.i[i]=(I)y_):(p.f[i]=y_);
	 }
	index=gv(Et,2);
	cols=gv(It,setCols);
	p.i=index->p;	
	p.a[0]=(A)gi(row_);
	p.a[1]=cols;
	p.i=cols->p;
	for (i=0;i<setCols;i++) p.i[i]=col_+i;
      }
     else if (constraint()==MSNone || dataCount()==1)
      {
	data=gv(av->t,2);
	P p; p.i=data->p;
	(av->t==It)?(p.i[0]=(I)x_,p.i[1]=(I)y_):(p.f[0]=x_,p.f[1]=y_);
	index=gv(Et,2);
	A cols=gv(It,2); // 1 one for x,1 row for y
	p.i=index->p;	
	p.a[0]=(A)gi(row_);
	p.a[1]=cols;
	p.i=cols->p;
	p.i[0]=0;
	p.i[1]=col_;
      }
     else if (constraint()==MSG::HoldX)
      {
	data=(av->t==It)?gi((int)y_):gf(y_);
	index=grc(av,row_,col_);
      }
     else if (constraint()==MSG::HoldY)
      {
	data=(av->t==It)?gi((int)x_):gf(x_);
	index=grc(av,0,col_);
      }
     if (safeAset(v,data,index,0)==0) showError(qs); 
     dc(index);
   }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Widget Methods
////////////////////////////////////////////////////////////////////////////////////////////////////

void AplusTraceSet::updateData(void)
{
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v)
   {
     AplusTrace *pTrace=0;
     int         cols=numColumns()>1?numColumns()-1:numColumns();
     int         numTraces=traceList().count();

     freeze();
     for (int i=numTraces-1;i>-1;i--)
      {
	if ((pTrace=(AplusTrace *)trace(i))!=0)
	 {
	   if (pTrace->virtualCol()>=cols)
	    {
	      traceList().remove(pTrace); 
	      graph()->traceList().remove(pTrace); 
	      delete pTrace;
	    }
	   else pTrace->virtualCol(i);
	 }  
      }
     if (cols>numTraces)
      {
	for (int i=numTraces;i<cols;i++)
	 {
	   pTrace=new AplusTrace(this,i,tag());
	   graph()->traceList().add(pTrace);
	   traceList().add(pTrace); 
	 }   
      }   

     // Invoke all functional attributes
     legendFuncInvoke();
     lineColorFuncInvoke();
     fillColorFuncInvoke();
     lineStyleFuncInvoke();
     lineWidthFuncInvoke();
     pieOffsetFuncInvoke();
     gradientFuncInvoke();
     traceStyleFuncInvoke();
     traceSymbolFuncInvoke();
     traceSymbolSizeFuncInvoke();
     altXaxisFuncInvoke();
     altYaxisFuncInvoke();

     unfreeze();
     computeExtents();
     lastDataCount(dataCount());
     graph()->updateLegendStatus(MSTrue);
     graph()->redraw();
   }
}

void AplusTraceSet::update(V v_,A index_,A,I ravel_)
{ 
  if(!index_) update(v_,-1,-1,ShapeUpdate);
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
     if(isNull(c)==MSTrue) // c is aplus_nl-all cols are updated
      {     
        if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
        else if(r->n>0) update(v_,(int)r->p[r->n-1],-1,ValueUpdate);
      }
     else if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
     else if (r->n>0) update(v_,(int)r->p[r->n-1],(int)c->p[c->n-1],ValueUpdate);
   }
}

void AplusTraceSet::update(V v_,int r_,int c_,UpdateType t_)
{ 
  if (v_!=0&&v_==((AplusModel*)model())->aplusVar()) 
   {
     if (t_==ShapeUpdate)
     {
       updateTraceStyle(trace(0)!=0?trace(0)->style():MSG::Line);
       graph()->legend()->redraw();
       graph()->redraw();
     }
     else if (t_==SizeUpdate) graph()->redraw();
     else if (t_==ValueUpdate)
     {
       MSIndexVector i;
       if (c_==-1&&r_==-1) {}
       else if (c_==-1) { i<<(r_*(numColumns()));}
       else if (r_==-1) {}
       else i<<(r_*(numColumns()));
       MSTraceSet::update(i);
     }
   }
}

void AplusTraceSet::addSenderNotify(MSEventSender *m_)
{ INTERNAL_COUPLE(((AplusModel *) m_)); }

void AplusTraceSet::receiveEvent(MSEvent&event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) cout<<"Received UpdateEvent in AplusTraceSet" <<endl;
     AplusEvent *ave=(AplusEvent *)&event_;
     V v=((AplusModel *)model())->aplusVar();
     A index=ave->index();
     A pick=ave->pick();
     I ravel=ave->ravel();;
     update(v,index,pick,ravel);
   }
  if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout<<"Received VerifyEvent in AplusTraceSet" <<endl;

     AplusVerifyEvent *ave=(AplusVerifyEvent *)&event_;
     ave->result(verifyData(ave->aplusVar(),ave->a()));
   }
}

MSBoolean AplusTraceSet::verifyData(V,A a_)
{
  MSBoolean r=MSFalse;
  if (0!=a_ && QA(a_) && (a_->t==Ft|| a_->t==It)&&a_->r<3) // (Yv or Ym)
   {
     r=MSTrue;
   }
  return r;
}

void AplusTraceSet::selectable(MSBoolean b_)
{ MSTraceSet::selectable(b_); }

const char *AplusTraceSet::formatText(MSString& str_, unsigned index_)
{
  if (textBuffer().length()>index_)
   {
     str_ = textBuffer()(index_).asString();
   }
  return str_;
}

unsigned AplusTraceSet::textLength(void) const
{ return _textBuffer.length(); }

// Relying on s to set the title of the traceset
void AplusTraceSet::validateText(const MSStringVector)
{ activateCallback("textactivate"); }

void AplusTraceSet::updateTitle(void)
{
  for(unsigned i=0;i<traceList().count();i++)
   {
     if (trace(i)->style()==MSG::Text)
      {
	graph()->redrawImmediately();
	break;
      }
   }
}

void AplusTraceSet::textBuffer(const MSStringVector t_)
{
  if (_textBuffer!=t_)
   {
     _textBuffer=t_;
     updateTitle();
   }
}

void AplusTraceSet::gradient(const MSUnsignedVector x_)
{
  for (int i=0;i<traceList().count();i++) trace(i)->gradient(x_(i%x_.length())==1?MSTrue:MSFalse);
  graph()->updateLegendStatus(MSTrue); 
  graph()->redrawImmediately(); 
}


const char *AplusTraceSet::formatOutput(MSString& str_, unsigned row_, unsigned col_)
{
  static const char blank[]={" "};
  V v                       = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  ACharStrFunction *outFunc = AplusModel::getOutFunc(v);
  AVariableData    *vd      = ::pAVarDataFromV(v);
  invokeFunction(outFunc, row_, col_);
  str_ = (Ct==_outStr->t) ? (char *)_outStr->p : blank;
  dc(_outStr);
  _outStr=aplus_nl;

  return str_;
}


// copied from AplusTableColumn; possibly needs to be cleaned up
void AplusTraceSet::invokeFunction(AOutFunction *outFunc_, unsigned row_, unsigned col_)
{
  I *data = ((model()!=0)?((AplusModel*)model())->data():0);
  V                 v          = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  unsigned long     type       = (model()!=0)?((AplusModel*)model())->a_type():0;
  int 		    charlength = (model()!=0)?((AplusModel*)model())->charLength():0;
  int 		    rank       = (model()!=0)?((AplusModel*)model())->rank():0;
  P p;              p.i        = data;
  char *buf;

  if (outFunc_!=0)
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


::A AplusTraceSet::computeAdjustedData(double xOffset_, double yOffset_) const
{
  ::A r=aplus_nl;
  P p,pp;
  V v = 0;
  int rows=dataCount(),cols,offset=0;

  if (rows>0)
    {
      v = ((AplusModel *)model())->aplusVar();
      pp.i = ((::A)v->a)->p;

      cols=numColumns();
      if (((::A)v->a)->r>1)	// if this is a matrix
	{
	  r=gm(Ft,rows,cols);
	  p.i=r->p;
	  for (int i=0; i<rows; i++)
	    {
	      for (int j=0; j<cols; j++)
		{
		  p.f[offset]=(((::A)v->a)->t==It)?pp.i[offset]:pp.f[offset];
		  p.f[offset++]+= (j==0)?xOffset_:-yOffset_;
		}
	    }
	}
      else if (((::A)v->a)->r==1) // if it's a vector
	{
	  r=gv(Ft,rows);
	  p.i=r->p;
	  for (int i=0; i < rows; i++)
	    {
	      p.f[offset]=(((::A)v->a)->t==It)?pp.i[offset]:pp.f[offset];
	      p.f[offset++]+=-yOffset_;
	    }
	}
    }

  return r;
}


MSBoolean AplusTraceSet::moveTraceValidate(double xOffset_, double yOffset_)
{
  ::A r=computeAdjustedData(xOffset_,yOffset_);

  if (isNull(r)==MSTrue)
    {
      return MSTrue;
    }

  V v=((AplusModel *)model())->aplusVar();
  if (safeAset(v,r,0,0)==0)
    {
      showError(qs);
      return MSFalse; 
    }

  return MSTrue;
}


MSFloatMatrix AplusTraceSet::asFloatMatrix(void) const
{
  unsigned cols = numColumns();
  unsigned rows = numRows();
  MSFloatMatrix fm(rows, cols);

  for (unsigned i=0; i<cols; i++)
    {
      for (unsigned j=0; j<rows; j++)
	{
	  fm(j,i) = y(j,i);
	}
    }

  return fm;
}


const MSSymbol& AplusTraceSet::widgetType(void) const
{
  return symbol();
}

MSBoolean AplusTraceSet::isProtected(void) const
{

  V v =((AplusModel *)model())->aplusVar();
  A a =((AplusModel *)model())->a();

  AVariableData *varData = pAVarDataFromV(v);  
  AReadOnlyFunction *roFunc=((AplusModel *)model())->getReadOnlyFunc(v);

  if ( roFunc!=0 )
    return (MSBoolean)roFunc->invoke(v,a);
  else if ( varData && varData->readOnly()==MSTrue )
    return MSTrue;
  else
    return MSFalse;
}

const MSSymbol& AplusTraceSet::symbol(void)
{
  static MSSymbol sym("AplusTraceSet");
  return sym;
}
