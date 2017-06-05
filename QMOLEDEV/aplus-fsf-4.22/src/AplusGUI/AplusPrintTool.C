///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSDisplayPrint.H>
#include <AplusGUI/AplusParagraph.H>
#include <AplusGUI/AplusPrintColumn.H>
#include <AplusGUI/AplusRulePrintItem.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusGraph.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/AplusPrintTool.H>

extern long dbg_tmstk;

AplusPrintTool::AplusPrintTool(MSWidget *owner_)
  : MSWidgetView(owner_), MSPrintTool(), _headers(aplus_nl), _footers(aplus_nl), _banner(aplus_nl), _pageNumber(aplus_nl)
{ 
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusPrintTool::~AplusPrintTool(void)
{
  dc(_headers);
  dc(_footers);
  dc(_banner);
  dc(_pageNumber);
}

void AplusPrintTool::addSenderNotify(MSEventSender *sender_)
{
  INTERNAL_COUPLE(((AplusModel *)sender_));
}

void AplusPrintTool::receiveEvent(MSEvent &event_)
{
  if (event_.type()==AplusEvent::symbol())
   {
     if (dbg_tmstk) showError("Received UpdateEvent in AplusPrintTool",2); // print info message
   }
  if (event_.type()==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) showError("Received VerifyEvent in AplusPrintTool",2); // print info message

     AplusVerifyEvent *ave = (AplusVerifyEvent *)&event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}


MSBoolean AplusPrintTool::verifyData(V v_, ::A a_)
{
  if (a_==0)
    {
      return MSFalse;
    }
  else if (a_->t==Ct || a_->t==Et)		// a single paragraph item or a list of items
    {
      return MSTrue;
    }
  else
    {
      return MSFalse;
    }
}


void AplusPrintTool::constructReport(void)
{
  removeAllHeaders();
  constructHeaders();
  removeAll();
  constructBody();
  removeAllFooters();
  constructFooters();
  constructBanner();
  constructPageNumber();
}


void AplusPrintTool::constructBody(void)
{
  V v=model()->aplusVar();
  ::A a=model()->a();

  if (verifyData(v,a)==MSFalse)
    {
      if (dbg_tmstk)  showError("Invalid report format"); // print error message
      return;
    }

  constructPrintManager(this, a);
}


void AplusPrintTool::constructPrintManager(MSPrintManager *pManager_, ::A aItems_)
{
  if (aItems_->t==Ct)		// it's a single text item
    {
      pManager_->addParagraph(AplusConvert::asMSStringVector(aItems_));
    }
  else	// it's a list or vector of print items
    {
      int i, n=aItems_->n;
      ::A *p=(::A *)aItems_->p;
      
      for (i=0; i<n; i++)	// iterate over all symbols in the print item vector
	{
	  if (QA(p[i]))
	    {
	      constructPrintItem(pManager_, p[i]);
	    }
	  else if (QS(p[i]))
	    {
	      constructPrintItem(pManager_, (S)XS(p[i]));
	    }
	}
    }
}


void AplusPrintTool::constructPrintItem(MSPrintManager *pManager_, ::A aItem_)
{
  switch (aItem_->t)
    {
    case Ct:
      pManager_->addParagraph(AplusConvert::asMSStringVector(aItem_));
      break;

    case Et:
      if (isNull(aItem_)==MSTrue) // null item => this is a page break
	{
	  pManager_->addPageBreak();
	}
      else if (aItem_->n==1 && QS(*aItem_->p))	// this is a single print item
	{
	  constructPrintItem(pManager_, (S)XS(*aItem_->p));
	}
      else	// this is a print column
	{
	  if (AplusPrintColumn::verifyData(0,aItem_)==MSFalse)
	    {
	      if (dbg_tmstk)  showError("Invalid print column format in report",1); // print warning message
	      return;
	    }

	  MSPrintColumn *pColumn = new MSPrintColumn(pManager_, aItem_->n);
	  constructPrintManager(pColumn, aItem_);
	}
      break;

    default:
      if (dbg_tmstk) showError("Invalid print item format in report",1); // print warning message
      break;
    }
}


void AplusPrintTool::constructPrintItem(MSPrintManager *pManager_, S sym_)
{
  if (sym_==si(""))	// empty symbol => this is a page break
    {
      pManager_->addPageBreak();
    }
  else	// handle all other print items
    {
      V vItem = (V)getVFromSym(Cx,sym_); // if no V is present for sym_, a dummy one will be created for it
      //ASSERTION:  vItem!=0
      if (vItem->attr!=0)
	{
	  MSWidgetView *printItem = ((AVariableData *)vItem->attr)->pWidgetView();
	  // This situation (vItem->attr!=0, but printItem==0) can happen if there has been an attempt to
	  // bind the variable to a non-existing widget class.  In that case, attr member would be created
	  // for the A+ variable, but _pWidgetView pointer would be zero.  This seems to be a side effect
	  // of s.is function in A+ and should probably be addressed there, but for the time being we'll
	  // fix it here.
	  //
	  if (printItem==0)
	    {
	      if (dbg_tmstk)
		{
		  MSString err("`");
		  err << sym_->n << " has not been bound to any print item class"; 
		  showError(err.string(),1); // print warning message
		}
	      return;
	    }

	  const MSSymbol& type(printItem->widgetType());
	  
	  // check for the type of the print item and add it to the print manager
	  if (type==AplusParagraph::symbol())
	    {
	      AplusParagraph& paragraph = *(AplusParagraph *)printItem;
	      // ASSERTION:  paragraph's model and A object are valid
	      paragraph.text(AplusConvert::asMSStringVector(paragraph.model()->a())); // set the paragraph's text
	      pManager_->addParagraph(paragraph);
	    }
	  else if (type==AplusPrintColumn::symbol())
	    {
	      AplusPrintColumn *pColumn=(AplusPrintColumn *)printItem;
	      pManager_->addPrintItem(pColumn);
	      pColumn->removeAll();
	      pColumn->constructColumn();
	    }
	  else if (type==AplusTable::symbol())
	    {
	      pManager_->addPrintItem((AplusTable *)printItem);
	    }
	  else if (type==AplusGraph::symbol())
	    {
	      AplusGraph *pGraph = (AplusGraph *)printItem;
	      // clear the print width to fix a bug in MStk (version 2.5.3), where width calculations get
	      // screwed up if graph is in a print column and the column widths are changed
	      pGraph->printWidth(0);
	      pManager_->addPrintItem(pGraph);
	    }
	  else if (type==AplusRulePrintItem::symbol())
	    {
	      // MStk does not allow the same print item to be used in several places in a report;
	      // since a rule item is something that one may want reuse, create a new one every time
	      pManager_->addPrintItem(new AplusRulePrintItem(*(AplusRulePrintItem *)printItem));
	    }
	  else
	    {
	      if (dbg_tmstk)
		{
		  MSString err("`");
		  err << sym_->n << " has invalid print item type";
		  showError(err.string(),1); // print warning message
		}
	    }
	}
      else
	{
	  if (dbg_tmstk)
	    {
	      MSString err("`");
	      err << sym_->n << " has not been bound to any print item class"; 
	      showError(err.string(),1); // print warning message
	    }
	}
    }
}


void AplusPrintTool::constructHeaders(void)
{
  if (verifyData(getVfromA(_headers),_headers)==MSFalse) // should we have a separate function, verifyHeaders()?
    {
      if (dbg_tmstk)  showError("Invalid header format in report",1); // print warning message
      return;
    }

  if (_headers->t==Ct)		// it's a single text item
    {
      addHeader(AplusConvert::asMSStringVector(_headers));
    }
  else	// it's a list or vector of print items
    {
      int i, n=_headers->n;
      ::A *p=(::A *)_headers->p;
      ::A aItem;

      for (i=0; i<n; i++)	// iterate over all symbols in the print item vector
	{
	  aItem=p[i];
	  if (QA(aItem))
	    {
	      if (aItem->t==Ct)	// a paragraph item
		{
		  addHeader(AplusConvert::asMSStringVector(aItem));
		}
	      else if (aItem->t==Et)
		{
		  if (isNull(aItem)==MSFalse)
		    {
		      if (aItem->n==1 && QS(*aItem->p))   // a single print item
			{
			  constructHeaderItem((S)XS(*aItem->p));
			}
		      else		// a print column
			{
			  MSPrintColumn *pColumn = new MSPrintColumn(aItem->n);
			  addHeader(pColumn);
			  constructPrintManager(pColumn, aItem);
			}
		    }
		}
	    }
	  else if (QS(aItem))
	    {
	      constructHeaderItem((S)XS(aItem));
	    }
	}
    }
}

void AplusPrintTool::constructHeaderItem(S sym_)
{
  V vItem = (V)getVFromSym(Cx,sym_);
  //ASSERTION: vItem!=0
  if (vItem->attr!=0)
    {
      MSWidgetView *printItem = ((AVariableData *)vItem->attr)->pWidgetView();
      // See the comment in a simliar section of code in constructPrintItem() method for explanation
      // of why we need to check for null widget pointer here
      if (printItem==0)
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The header item `");
	      err << sym_->n << " has not been bound to any class"; 
	      showError(err.string(),1); // print warning message
	    }
	  return;
	}
      
      const MSSymbol& type(printItem->widgetType());
	      
      // check for the type of the print item and add it as a header
      if (type==AplusParagraph::symbol())
	{
	  AplusParagraph& paragraph = *(AplusParagraph *)printItem;
	  // ASSERTION:  paragraph's model and A object are valid
	  paragraph.text(AplusConvert::asMSStringVector(paragraph.model()->a())); // set the paragraph's text
	  addHeader(paragraph);
	}
      else if (type==AplusPrintColumn::symbol())
	{
	  AplusPrintColumn *pColumn=(AplusPrintColumn *)printItem;
	  addHeader(pColumn);
	  pColumn->removeAll();
	  pColumn->constructColumn();
	}
      else if (type==AplusRulePrintItem::symbol())
	{
	  // currently, as of version 2.5.3 of MStk, a rule item cannot be used as a header or footer;
	  // this will be fixed in MStk 2.6; for now we fake it here by creating a column with a single rule item
	  // inside, but this will have to be synch'ed up with MStk when 2.6 comes out
	  MSPrintColumn *pColumn = new MSPrintColumn(1);
	  pColumn->addPrintItem(new AplusRulePrintItem(*(AplusRulePrintItem *)printItem));
	  addHeader(pColumn);
	}
      else
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The header item `");
	      err << sym_->n << " has invalid type";
	      showError(err.string(),1); // print warning message
	    }
	}
    }
  else
    {
      if (dbg_tmstk)
	{
	  MSString err("The header item `");
	  err << sym_->n << " has not been bound to any class";
	  showError(err.string(),1); // print warning message
	}
    }
}


void AplusPrintTool::constructFooters(void)
{
  if (verifyData(getVfromA(_footers),_footers)==MSFalse) // should we have a separate function, verifyFooters()?
    {
      if (dbg_tmstk)  showError("Invalid footer format in report",1); // print warning message
      return;
    }

  if (_footers->t==Ct)		// it's a single text item
    {
      addFooter(AplusConvert::asMSStringVector(_footers));
    }
  else	// it's a list or vector of print items
    {
      int i, n=_footers->n;
      ::A *p=(::A *)_footers->p;
      ::A aItem;

      for (i=0; i<n; i++)	// iterate over all symbols in the print item vector
	{
	  aItem=p[i];
	  if (QA(aItem))
	    {
	      if (aItem->t==Ct)	// a paragraph item
		{
		  addFooter(AplusConvert::asMSStringVector(aItem));
		}
	      else if (aItem->t==Et)
		{
		  if (isNull(aItem)==MSFalse)
		    {
		      if (aItem->n==1 && QS(*aItem->p))   // a single print item
			{
			  constructFooterItem((S)XS(*aItem->p));
			}
		      else		// a print column
			{
			  MSPrintColumn *pColumn = new MSPrintColumn(aItem->n);
			  addFooter(pColumn);
			  constructPrintManager(pColumn, aItem);
			}
		    }
		}
	    }
	  else if (QS(aItem))
	    {
	      constructFooterItem((S)XS(aItem));
	    }
	}
    }
}

  
void AplusPrintTool::constructFooterItem(S sym_)
{
  V vItem = (V)getVFromSym(Cx,sym_);
  //ASSERTION: vItem!=0
  if (vItem->attr!=0)
    {
      MSWidgetView *printItem = ((AVariableData *)vItem->attr)->pWidgetView();
      // See the comment in a simliar section of code in constructPrintItem() method for explanation
      // of why we need to check for null widget pointer here
      if (printItem==0)
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The footer item `");
	      err << sym_->n << " has not been bound to any class"; 
	      showError(err.string(),1); // print warning message
	    }
	  return;
	}
      
      const MSSymbol& type(printItem->widgetType());
	      
      // check for the type of the print item and add it as a footer
      if (type==AplusParagraph::symbol())
	{
	  AplusParagraph& paragraph = *(AplusParagraph *)printItem;
	  // ASSERTION:  paragraph's model and A object are valid
	  paragraph.text(AplusConvert::asMSStringVector(paragraph.model()->a())); // set the paragraph's text
	  addFooter(paragraph);
	}
      else if (type==AplusPrintColumn::symbol())
	{
	  AplusPrintColumn *pColumn=(AplusPrintColumn *)printItem;
	  addFooter(pColumn);
	  pColumn->removeAll();
	  pColumn->constructColumn();
	}
      else if (type==AplusRulePrintItem::symbol())
	{
	  // currently, as of version 2.5.3 of MStk, a rule item cannot be used as a header or footer;
	  // this will be fixed in MStk 2.6; for now we fake it here by creating a column with a single rule item
	  // inside, but this will have to be synch'ed up with MStk when 2.6 comes out
	  MSPrintColumn *pColumn = new MSPrintColumn(1);
	  pColumn->addPrintItem(new AplusRulePrintItem(*(AplusRulePrintItem *)printItem));
	  addFooter(pColumn);
	}
      else
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The footer `");
	      err << sym_->n << " has invalid type";
	      showError(err.string(),1); // print warning message
	    }
	}
    }
  else
    {
      if (dbg_tmstk)
	{
	  MSString err("The footer item `");
	  err << sym_->n << " has not been bound to any class";
	  showError(err.string(),1); // print warning message
	}
    }
}


void AplusPrintTool::constructBanner(void)
{
  V vBanner=getVfromA(_banner);

  if (verifyData(vBanner,_banner)==MSFalse) // should we have a separate function verifyBanner()?
    {
      if (dbg_tmstk)  showError("Invalid banner format in report",1); // warning
      return;
    }

  if (_banner->n==1 && _banner->t==Et && QS(*_banner->p)) // it's a single paragraph
    {
      S sym=(S)XS(*_banner->p);
      V v = (V)getVFromSym(Cx,sym);
      //ASSERTION: vItem!=0
      if (v->attr!=0)
	{
	  MSWidgetView *printItem = ((AVariableData *)v->attr)->pWidgetView();
	  // See the comment in a simliar section of code in constructPrintItem() method for explanation
	  // of why we need to check for null widget pointer here
	  if (printItem==0)
	    {
	      if (dbg_tmstk)
		{
		  MSString err("The banner `");
		  err << sym->n << " has not been bound to reportparagraph"; 
		  showError(err.string(),1); // print warning message
		}
	      return;
	    }
	      
	  // check for the type of the print item and add it as a banner
	  if (printItem->widgetType()==AplusParagraph::symbol())
	    {
	      AplusParagraph& paragraph = *(AplusParagraph *)printItem;
	      // ASSERTION:  paragraph's model and A object are valid
	      paragraph.text(AplusConvert::asMSStringVector(paragraph.model()->a())); // set the paragraph's text
	      MSPrintTool::banner(paragraph);
	    }
	  else
	    {
	      if (dbg_tmstk)
		{
		  MSString err("The banner `");
		  err << sym->n << " is not bound to reportparagraph";
		  showError(err.string(),1); // print warning message
		}
	    }
	}
      else
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The banner `");
	      err << sym->n << " has not been bound to reportparagraph";
	      showError(err.string(),1); // print warning message
	    }
	}

    }
  else
    {
      MSStringVector b(AplusConvert::asMSStringVector(_banner));
      MSPrintTool::banner(b);
    }
}


void AplusPrintTool::constructPageNumber(void)
{
  V vPageNumber=getVfromA(_pageNumber);

  if (verifyData(vPageNumber,_pageNumber)==MSFalse) // should we have a separate function verifyPageNumber()?
    {
      if (dbg_tmstk)  showError("Invalid page number format in report",1);
      return;
    }

  if (_pageNumber->n==1 && _pageNumber->t==Et && QS(*_pageNumber->p)) // it's a single paragraph
    {
      S sym=(S)XS(*_pageNumber->p);
      V v = (V)getVFromSym(Cx,sym);
      //ASSERTION: vItem!=0
      if (v->attr!=0)
	{
	  MSWidgetView *printItem = ((AVariableData *)v->attr)->pWidgetView();
	  // See the comment in a simliar section of code in constructPrintItem() method for explanation
	  // of why we need to check for null widget pointer here
	  if (printItem==0)
	    {
	      if (dbg_tmstk)
		{
		  MSString err("The page number `");
		  err << sym->n << " has not been bound to reportparagraph"; 
		  showError(err.string(),1); // print warning message
		}
	      return;
	    }
	      
	  // check for the type of the print item and add it as a pageNumber
	  if (printItem->widgetType()==AplusParagraph::symbol())
	    {
	      AplusParagraph& paragraph = *(AplusParagraph *)printItem;
	      // ASSERTION:  paragraph's model and A object are valid
	      paragraph.text(AplusConvert::asMSStringVector(paragraph.model()->a())); // set the paragraph's text
	      MSPrintTool::pageNumber(paragraph);
	    }
	  else
	    {
	      if (dbg_tmstk)
		{
		  MSString err("The page number `");
		  err << sym->n << " is not bound to reportparagraph";
		  showError(err.string(),1); // print warning message
		}
	    }
	}
      else
	{
	  if (dbg_tmstk)
	    {
	      MSString err("The page number `");
	      err << sym->n << " has not been bound to reportparagraph";
	      showError(err.string(),1); // print warning message
	    }
	}

    }
  else
    {
      MSStringVector b(AplusConvert::asMSStringVector(_pageNumber));
      MSPrintTool::pageNumber(b);
    }
}


const MSSymbol& AplusPrintTool::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusPrintTool::symbol(void)
{
  static MSSymbol sym("AplusPrintTool");
  return sym;
}
