#ifndef MSToggleMenuItemHEADER
#define MSToggleMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuItem.H>

class MSGUIExport MSToggleMenuItem : public MSMenuItem 
{
public:
  MSToggleMenuItem(MSMenu *);
  MSToggleMenuItem(MSMenu *,const char *,char mnemonic_=0,int tag_=0);
  MSToggleMenuItem(MSMenu *,const MSString&,char mnemonic_=0,int tag_=0);
  MSToggleMenuItem(MSMenu *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_=0);
  MSToggleMenuItem(MSMenu *,const MSString &,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
		   const MSPixmap &insensitivePixmap_,char mnemonic_=0,int tag_=0);
  MSToggleMenuItem(MSMenu *,const char *,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
		   const MSPixmap &insensitivePixmap_,char mnemonic_=0,int tag_=0);
	
  ~MSToggleMenuItem(void);

  void armedPixmap(const MSPixmap&);
  const MSPixmap *armedPixmap(void) const;
  
  void showDisarmState(MSBoolean);
  MSBoolean showDisarmState(void) const;

  void selectColor(unsigned long);
  void selectColor(const char *);
  unsigned long selectColor(void) const;

  // methods for builder support
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  
protected:
  MSPixmap       *_armedPixmap;
  MSBoolean       _showDisarmState;
  MSGC            _selectMSGC;
  unsigned long   _selectColor;  
  
  void init(void);
  void initArmedPixmap(const MSPixmap &);
  
  virtual const MSPixmap *currentPixmap(void) const;
  virtual void computeSize(void);
  virtual int computeIndentation(void);
  virtual int symbolHeight(void) const;
  virtual int symbolWidth(void) const;
};

inline const MSPixmap *MSToggleMenuItem::armedPixmap(void) const
{ return _armedPixmap; }

inline MSBoolean MSToggleMenuItem::showDisarmState(void) const
{ return _showDisarmState; }

inline void MSToggleMenuItem::showDisarmState(MSBoolean showDisarmState_) 
{  _showDisarmState = showDisarmState_; }

inline unsigned long MSToggleMenuItem::selectColor(void) const
{ return _selectColor; }
#endif
