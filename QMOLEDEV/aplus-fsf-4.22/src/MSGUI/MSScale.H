#ifndef MSScaleHEADER
#define MSScaleHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPrimitive.H>
#include <MSGUI/MSTextField.H>
#include <MSGUI/MSLabelOut.H>
#include <MSGUI/MSLabelFormat.H>
#include <MSTypes/MSInt.H>
#include <MSTypes/MSFloat.H>
#include <MSTypes/MSFormat.H>
#include <MSIPC/MSIntervalTimer.H>
#include <MSIPC/MSRegularTimer.H>
   
class MSGUIExport MSScale : public MSPrimitive
{
friend class ValueWin;
public:
  MSScale(MSWidget *,const char *title_=0);
  MSScale(MSWidget *,const MSStringVector&);
  MSScale(MSWidget *,MSFloat& model_,const char *title_=0);
  MSScale(MSWidget *,MSFloat& model_,const MSStringVector&);
  MSScale(MSWidget *,MSInt& model_,const char *title_=0);
  MSScale(MSWidget *,MSInt& model_,const MSStringVector&);
  ~MSScale(void);

  virtual void naturalSize(void);
  virtual void redraw(void);
  virtual void print(const char *file_=0);
  virtual void sliderSize(int);
  virtual int sliderSize(void) const;

  virtual double currentValue(void);
  virtual double currentValue(void) const;

  unsigned long titleAlignment(void)     const {return _titleAlignment;}
  const MSStringVector& subtitle(void)   const {return _subtitle;}
  Font subtitleFont(void)                const {return _subtitleFont;}
  unsigned long subtitleForeground(void) const {return _subtitleForeground;}
  unsigned long subtitleAlignment(void)  const {return _subtitleAlignment;}

  const MSStringVector& mintitle(void)   const {return _mintitle;}
  unsigned long mintitleForeground(void) const {return _mintitleForeground;}
  unsigned long mintitleAlignment(void)  const {return _mintitleAlignment;}
  Font mintitleFont(void)                const {return _mintitleFont;}

  const MSStringVector& maxtitle(void)   const {return _maxtitle;}
  unsigned long maxtitleForeground(void) const {return _maxtitleForeground;}
  unsigned long maxtitleAlignment(void)  const {return _maxtitleAlignment;}
  Font maxtitleFont(void)                const {return _maxtitleFont;}

  Font labelFont(void)                   const {return _labelFont;}
  unsigned long labelForeground(void)    const {return _labelForeground;}
  double labelInc(void)                  const {return _labelInc;}
  unsigned long labelAlignment(void)     const {return _labelAlignment;}
  MSLabelOutPtr labelOut(void)           const {return _labelOut; }

  double valueMin(void)                  const {return _valueMin;}
  double valueMax(void)                  const {return _valueMax;}
  double valueInc(void)                  const {return _valueInc;}
  double valuePageInc(void)              const {return _valuePageInc;}
  double valueChange(void)               const {return _valueChange;}
  unsigned long valueForeground(void)    const {return _valueWin->foreground();}
  unsigned long valueAlignment(void)     const {return _valueAlignment;}
  Font valueFont(void)                   const {return _valueWin->font();}
  MSBoolean valueSensitive(void)         const {return _valueWin->sensitive();}
  
  int majorTickSize(void)                const {return _majorTickSize;}
  int minorTickSize(void)                const {return _minorTickSize;}
  int minorTickCount(void)               const {return _minorTickCount;}

  unsigned long sliderForeground(void)   const {return _slider->foreground();}
  unsigned long sliderBackground(void)   const {return _slider->background();}

  void titleAlignment(unsigned long);       
					       
  void subtitle(const MSStringVector& );      
  void subtitleAlignment(unsigned long);
  void subtitleForeground(unsigned long);    
  void subtitleForeground(const char *);    
  void subtitleFont(Font);            
  void subtitleFont(const char *);            
					       
  void mintitle(const MSStringVector&);      
  void mintitleAlignment(unsigned long);
  void mintitleForeground(unsigned long);    
  void mintitleForeground(const char *);    
  void mintitleFont(Font);                   
  void mintitleFont(const char *);                   
					       
  void maxtitle(const MSStringVector&);      
  void maxtitleAlignment(unsigned long);         
  void maxtitleForeground(unsigned long);    
  void maxtitleForeground(const char *);    
  void maxtitleFont(Font);                   
  void maxtitleFont(const char *);                   
					       
  void labelFont(Font);  		       
  void labelFont(const char *);
  void labelAlignment(unsigned long);             
  void labelForeground(unsigned long);       
  void labelForeground(const char *);         
  void labelInc(double);                     
  void labelOut(MSLabelOut *);		       
  void labelOut(MSLabelOutPtr);
  void sliderForeground(unsigned long);      
  void sliderForeground(const char *);        
  void sliderBackground(unsigned long);      
  void sliderBackground(const char *);        

  void valueMin(double);               
  void valueMax(double);               
  void valueInc(double);               
  void valuePageInc(double);           
  void valueChange(double);            
  void valueAlignment(unsigned long);      
  void valueForeground(unsigned long); 
  void valueForeground(const char *);
  void valueFont(Font);                
  void valueFont(const char *);
  void valueSensitive(MSBoolean);
  void majorTickSize(int);             
  void minorTickSize(int);             
  void minorTickCount(int);               

  void model(MSFloat&);
  void model(const MSFloat&);
  void model(MSInt&);
  void model(const MSInt&);

  MSModel& viewModel(void)              { return *_model; }
  const MSModel& viewModel(void) const  { return *_model; }
  
  const MSSymbol& modelType(void) const        { return _modelType; } 

  MSScale& operator=(const MSFloat& x_)        {value(x_); return *this;}
  MSScale& operator=(double x_)                {value(x_); return *this;}

  MSScale& operator=(const MSInt& x_)          {value(x_); return *this;}
  MSScale& operator=(int x_)                   {value(x_); return *this;}
  operator double()                      const {return (double)asFloat();} 
//  operator int()                         const {return (int)asInt();} 

  MSInt asInt(void) const;
  MSFloat asFloat(void) const;
  
  const MSFormat& format(void) const;
  void format(const MSFormat&);
  
  const char *formatValue(MSString &,double);

  unsigned long addEditorKeyCallback( const char*,MSKeyCallback*);
  void removeEditorKeyCallback(unsigned long);
  void removeEditorKeyCallback(const char*);
  
  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

  class MSGUIExport Slider:public MSWidgetCommon
  {
  friend class MSScale;
  public:
    Slider(MSWidget *);
    ~Slider(void);

    virtual void drawSliderEtch(void);
    virtual void adjustSize(int,int);
    virtual void moveTo(int,int);
    virtual void redraw(void);
    int offset(void)            const {return highlightThickness()+shadowThickness();}
  protected:
    MSScale *scale(void)              {return (MSScale *)owner();}
    GC gc(void)                       {return _gc;}

    void setValueWinXCoord(int);
    void setValueWinYCoord(int);

    virtual void configure(void);
    virtual void button1Press(const XEvent *);
  private:
    GC  _gc;
  };

  class MSGUIExport ValueWin:public MSWidgetCommon
  {
  public:
    ValueWin(MSWidget *);
    ~ValueWin(void);

    virtual void updateValue(const char *);

    MSScale *scale(void)        const {return (MSScale *)owner();}
    int textHeight(void)        const {return _fontInfo->ascent+_fontInfo->descent;}
    int offset(void)            const {return _offset;}
    int x_org(void)             const {return _x_org;}
    int y_org(void)             const {return _y_org;}
    GC gc(void)                 const {return _gc;}
    XFontStruct *fontInfo(void) const {return _fontInfo;}
    void offset(int x_) 	      {_offset=x_;}
    void x_org(int x_) 		      {_x_org=x_;}
    void y_org(int x_) 		      {_y_org=x_;}
  private:
    int           _minWidth;
    int           _offset;
    int           _x_org;
    int           _y_org;
    GC	       	  _gc;
    XFontStruct  *_fontInfo;
    
    virtual void redraw(void);
    virtual void updateFont(Font);
    virtual void updateForeground(unsigned long);
  };

  class MSGUIExport Editor:public MSTextField
  {
  friend class MSScale;
  public:
    Editor(MSWidget *);
    ~Editor(void);
    virtual void activate(void);
    virtual void escape(void);
  };

  class MSGUIExport DelayTimer:public MSRegularTimer
  {
  public:
    DelayTimer(MSScale *,unsigned long);
    ~DelayTimer(void);
    virtual void process(void);
  protected:
    MSScale *_scale;
  };

  class MSGUIExport RepeatTimer:public MSIntervalTimer
  {
  public:
    RepeatTimer(MSScale *,unsigned long);
    ~RepeatTimer(void);
    virtual void process(void);
  protected:
    MSScale *_scale;
  };

friend class Slider;
friend class Editor;
friend class DelayTimer;
friend class RepeatTimer;
friend class MSHScale;
friend class MSVScale;

protected:
  GC gc(void) 			              {return _gc;}
  MSLabelFormat& outFmt(void)                 {return _outFormat;}
  DelayTimer *delayTimer(void)          const {return _delayTimer;}
  RepeatTimer *repeatTimer(void)        const {return _repeatTimer;}
  ValueWin *valueWin(void)              const {return _valueWin;}
  Slider *slider(void)                  const {return _slider;}
  Editor *editor(void)                  const {return _editor;}
  MSRect& sliderAreaRect(void)                {return _sliderAreaRect;}
  const MSRect& sliderAreaRect(void)    const {return _sliderAreaRect;}
  int x_org(void)                       const {return sliderAreaRect().x();}
  int x_end(void)                       const {return x_org()+sliderAreaRect().width();}
  int y_org(void)                       const {return sliderAreaRect().y();}
  int y_end(void)                       const {return y_org()+sliderAreaRect().height();}
  double incData(void)                  const {return _incData;}
  double base(void)                     const {return _base;}
  double scale(void)                    const {return _scale;}
  int titleHeight(void)                 const {return _titleHeight;}
  int titleWidth(void)                  const {return _titleWidth;}
  int mintitleWidth(void)	        const {return _mintitleWidth;}
  int mintitleHeight(void)		const {return _mintitleHeight;}
  int maxtitleWidth(void)	        const {return _maxtitleWidth;}
  int maxtitleHeight(void)		const {return _maxtitleHeight;}
  int topOffset(void)                   const {return _topOffset;}
  int bottomOffset(void)                const {return _bottomOffset;}
  int leftOffset(void)                  const {return _leftOffset;}
  int rightOffset(void)                 const {return _rightOffset;}
  int naturalScaleSize(void)            const {return _naturalScaleSize; }
  MSBoolean internalUpdateStatus()      const {return _internalUpdateStatus;}

  void value(const MSFloat&);
  void value(const MSInt&);
  void value(double);
  void value(int);
  void base(double x_)                        {_base=x_;}
  void scale(double x_)                       {_scale=x_;}
  void incData(double x_)                     {_incData=x_;}
  void titleHeight(int x_)                    {_titleHeight=x_;}
  void titleWidth(int x_)                     {_titleWidth=x_;}
  void naturalScaleSize(int x_)               {_naturalScaleSize=x_; }
  void internalUpdateStatus(MSBoolean x_)     {_internalUpdateStatus=x_;}
  void setValue(double);                      
  void mintitleHeight(int x_)                 {_mintitleHeight=x_;} 
  void maxtitleHeight(int x_)                 {_maxtitleHeight=x_;}
  void mintitleWidth(int x_)                  {_mintitleWidth=x_;}
  void maxtitleWidth(int x_)                  {_maxtitleWidth=x_;}
  void topOffset(int x_)                      {_topOffset=x_;}
  void bottomOffset(int x_)                   {_bottomOffset=x_;}
  void leftOffset(int x_)                     {_leftOffset=x_;}
  void rightOffset(int x_)                    {_rightOffset=x_;}
  void labelOffset(int x_)		      {_labelOffset=x_;}
  void labelSpacing(int x_)		      {_labelSpacing=x_;}
  void sliderWidth(int x_)                    {_slider->width(x_);}
  void sliderHeight(int x_)                   {_slider->height(x_);}
                                              
  int labelOffset(void)  		      {return _labelOffset;}
  int labelSpacing(void)  		      {return _labelSpacing;}
  int sliderWidth(void)                       {return _slider->width();}
  int sliderHeight(void)                      {return _slider->height();}
  const char *formatLabel(MSString &buffer_,double x_)
  {return labelOut()->formatOutput(buffer_,x_);}

  virtual double pixelToValue(int);
  virtual int valueToPixel(double);
  virtual int incFactor(int,int);

  void drawTitles(void);

  void processDelayTimer(void);
  void processRepeatTimer(void);
  virtual void update(const MSIndexVector&);
  virtual MSBoolean assignValue(double);
  
  virtual void drawSliderEtch(void);
  virtual void sliderRedrawNotify();
  virtual void drawTickLabels(void);
  virtual void drawSliderTitles(void);
  virtual void moveSlider(const XEvent *);
  virtual void editValue(const char *);
  void drawSliderValue(void);
  void startDelayTimer(void);
  void startRepeatTimer(void);
  void stopRepeatTimer(void);
  void updateSliderValue(void);
  void startTimedUpdate(void);
  virtual void drawSubWindows(void);
  virtual void drawSliderArea(void);
  virtual void computeLabelOffset(void);
  virtual void computeSliderAreaSize(void);
  virtual void computeSliderScale(void);
  virtual void computeTickInc(void);
  virtual void computeValueWinSize(void);
  
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);
  virtual void buttonPress(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *);
  virtual void editorActivate(void);
  virtual void editorEscape(void);
  virtual MSBoolean validate(const char *);
  virtual void computeSize(void);
  virtual void configure(void);
  virtual void firstMapNotify(void);
  virtual void setSliderPosition(int);
  virtual void updateTitle(void);
  virtual void updateFormat(void);
  virtual void updateData(void);
  void init(void);

  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);

  virtual void home(void);
  virtual void end(void);
  virtual void up(void);
  virtual void down(void);
  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void left(void);
  virtual void right(void);

private:
  GC	 	      	_gc;
  unsigned long      	_titleAlignment;
  
  MSStringVector 	_subtitle;
  Font 			_subtitleFont;
  unsigned long  	_subtitleForeground;
  unsigned long		_subtitleAlignment;

  MSStringVector        _mintitle;
  unsigned long 	_mintitleForeground;
  unsigned long		_mintitleAlignment;
  int			_mintitleWidth;
  int			_mintitleHeight;
  Font	 		_mintitleFont;

  MSStringVector 	_maxtitle;
  unsigned long 	_maxtitleForeground;
  unsigned long		_maxtitleAlignment;
  int			_maxtitleWidth;
  int			_maxtitleHeight;
  Font	 		_maxtitleFont;

  unsigned long 	_labelForeground;
  Font                 	_labelFont;
  double               	_labelInc;
  unsigned long		_labelAlignment;
  MSLabelOutPtr         _labelOut;
  int  		        _labelOffset;
  int  		        _labelSpacing;
  
  double               	_valueMin;
  double               	_valueMax;
  double               	_valueInc;
  double               	_valuePageInc;
  double               	_valueChange;
  unsigned long		_valueAlignment;

  int			_majorTickSize;
  int			_minorTickSize;
  int			_minorTickCount;
  
  Slider	       *_slider;
  ValueWin	       *_valueWin;
  double               	_scale;
  double               	_base;
  double               	_incData;
  int	 		_titleHeight;
  int	 		_titleWidth;
  int 			_topOffset;
  int 			_bottomOffset;
  int			_leftOffset;
  int			_rightOffset;
  int     		_naturalScaleSize;
  MSRect               	_sliderAreaRect;
  DelayTimer           *_delayTimer;
  RepeatTimer          *_repeatTimer;
  Editor               *_editor;
  MSBoolean            	_repeatOn;
  MSBoolean            	_internalUpdateStatus;
  static MSLabelFormat 	_outFormat;
  static MSLabelOutPtr 	_defaultLabelOut;
  MSFormat              _format;   
  MSSymbol              _modelType;
};

inline const MSFormat& MSScale::format(void) const 
{ return _format; }       
inline void MSScale::format(const MSFormat& format_)
{ _format=format_; }       

#endif
