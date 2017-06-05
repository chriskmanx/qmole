///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSGraph.H>
#include <MSGUI/MSPrintManager.H>
#include <MSGUI/MSParagraph.H>
#include <MSGUI/MSPrintColumn.H>
#include <MSGUI/MSReportTable.H>
#include <MSGUI/MSReport.H>
#include <MSTypes/MSMessageLog.H>

MSParagraph MSPrintManager::_defaultParagraph;

MSPrintManager::MSPrintManager(void) 
{
  _parent=0;
}

MSPrintManager::MSPrintManager(MSPrintManager *parent_) 
{
  _parent=parent_;
  _parent->childList().add(this);
}

MSPrintManager::~MSPrintManager(void)
{ removeAll(); }

MSPrintManager *MSPrintManager::topLevel(void)
{
  MSPrintManager *pPrintBase=(MSPrintManager *)this;
  while (pPrintBase->parent()!=0) pPrintBase=pPrintBase->parent();
  return pPrintBase;
}

const MSPrintManager *MSPrintManager::topLevel(void) const
{
  MSPrintManager *pPrintBase=(MSPrintManager *)this;
  while (pPrintBase->parent()!=0) pPrintBase=pPrintBase->parent();
  return pPrintBase;
}

void MSPrintManager::removeAllParagraphs(void)
{
  unsigned i,n=paragraphList().count();
  for (i=n-1;i<n;i--)
   {
     MSPrintItem *printItem=paragraphList().array(i);
     if (printItemList().remove(printItem)==MSTrue)
      {
	paragraphList().remove(printItem);
	delete printItem;
      }
   }
}

void MSPrintManager::removeAll(void)
{
  removeAllParagraphs();
  unsigned n=printItemList().count();
  for (unsigned i=n-1;i<n;i--)
   {
     if (item(i)->isDestroyable()==MSTrue) delete item(i);
     else item(i)->printManager(0);
   }
  printItemList().removeAll();
}

MSBoolean MSPrintManager::removePrintItem(const MSSymbol& tag_)
{
  MSBoolean status=MSFalse;
  unsigned i,n=printItemList().count();
  for (i=0;i<n;i++)
   {
     MSPrintItem *printItem=item(i);
     if (tag_==printItem->printTag())
      {
        if (printItem->isDestroyable()==MSTrue) delete printItem;
	status=printItemList().remove(printItem);
	paragraphList().remove(printItem);
      }
   }
  if (status==MSFalse)
   {
     n=childList().count();
     for (i=0;i<n;i++)
      {
        status=childList().array(i)->removePrintItem(tag_);
      }
   }
  return status;
}

MSBoolean MSPrintManager::removePrintItem(MSPrintItem *item_)
{
  MSBoolean status=MSFalse;
  paragraphList().remove(item_);
  if ((status=printItemList().remove(item_))!=MSTrue)
   {
     for (unsigned i=0,n=childList().count();i<n;i++)
      {
        status=childList().array(i)->removePrintItem(item_);
      }
   }
  if (status==MSTrue&&item_->isDestroyable()==MSTrue) delete item_;
  return status;
}

MSPrintItem& MSPrintManager::addPageBreak(void)
{
  MSPrintItem *item=new MSPageBreakPrintItem();
  addPrintItem(item);
  return *item;
}

MSRulePrintItem& MSPrintManager::addRule(int width_)
{
  MSRulePrintItem *item=new MSRulePrintItem(width_);
  addPrintItem(item);
  return *item;
}

MSParagraph& MSPrintManager::addParagraph(const MSParagraph& paragraph_)
{
  MSParagraph *paragraph=new MSParagraph(paragraph_);
  addPrintItem(paragraph);
  _paragraphList.add(paragraph);
  return *paragraph;
}

MSParagraph& MSPrintManager::addParagraph(const MSStringVector& paragraph_)
{return addParagraph(MSParagraph(paragraph_));}

MSPrintColumn& MSPrintManager::addPrintItem(MSPrintColumn* aPrintColumn_)
{
  aPrintColumn_->parent(this);
  _printItemList.add(aPrintColumn_);
  return *aPrintColumn_;
}

MSReportTable& MSPrintManager::addPrintItem(MSReportTable *table_)
{
  table_->printManager(this);
  _printItemList.add(table_);
  return *table_;
}

MSGraph& MSPrintManager::addPrintItem(MSGraph *graph_)
{
  graph_->printManager(this);
  _printItemList.add(graph_);
  return *graph_;
}

MSPrintItem& MSPrintManager::addPrintItem(MSPrintItem *printItem_)
{
  _printItemList.add(printItem_);
  return *printItem_;
}

const MSParagraph& MSPrintManager::paragraph(unsigned i_) const
{
  if (i_<paragraphList().count()) return *((MSParagraph *)paragraphList().array(i_));
  MSMessageLog::warningMessage("Warning: MSPrintManager::paragraph index out of range\n");
  return defaultParagraph();
}

MSParagraph& MSPrintManager::paragraph(unsigned i_)
{
  if (i_<paragraphList().count()) return *((MSParagraph *)paragraphList().array(i_));
  MSMessageLog::warningMessage("Warning: MSPrintManager::paragraph index out of range\n");
  return defaultParagraph();
}

const MSParagraph& MSPrintManager::paragraph(const MSSymbol& tag_) const
{
  unsigned i,n=paragraphList().count();
  for (i=0;i<n;i++) if (tag_==paragraph(i).tag()) return paragraph(i);
  n=childList().count();
  for (i=0;i<n;i++)
   {
     const MSParagraph& par=childList().array(i)->paragraph(tag_);
     if (&par!=&defaultParagraph()) return par;
   }
  if (parent()==0) MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

MSParagraph& MSPrintManager::paragraph(const MSSymbol& tag_)
{
  unsigned i,n=paragraphList().count();
  for (i=0;i<n;i++) if (tag_==paragraph(i).tag()) return paragraph(i);
  n=childList().count();
  for (i=0;i<n;i++)
   {
     MSParagraph& par=childList().array(i)->paragraph(tag_);
     if (&par!=&defaultParagraph()) return par;
   }
  if (parent()==0) MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return defaultParagraph();
}

const MSPrintItem& MSPrintManager::printItem(const MSSymbol& tag_) const
{
  unsigned i,n=printItemList().count();
  for (i=0;i<n;i++) if (tag_==item(i)->printTag()) return *item(i);
  n=childList().count();
  for (i=0;i<n;i++)
   {
     MSPrintItem& aPrintItem=childList().array(i)->printItem(tag_);
     if (&aPrintItem!=(MSPrintItem*)(&defaultParagraph())) return aPrintItem;
   }
  if (parent()==0) MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return *((MSPrintItem*)&defaultParagraph());
}

MSPrintItem &MSPrintManager::printItem(const MSSymbol& tag_)
{
  unsigned i,n=printItemList().count();
  for (i=0;i<n;i++) if (tag_==item(i)->printTag()) return *item(i);
  n=childList().count();
  for (i=0;i<n;i++)
   {
     MSPrintItem& aPrintItem=childList().array(i)->printItem(tag_);
     if (&aPrintItem!=(MSPrintItem*)(&defaultParagraph())) return aPrintItem;
   }
  if (parent()==0)   MSMessageLog::warningMessage("Warning: paragraph \"%s\" not found\n",tag_.symbolName());
  return *((MSPrintItem*)&defaultParagraph());
}

MSParagraph& operator<<(MSPrintManager& printManager_,const MSParagraph& paragraph_)
{return printManager_.addParagraph(paragraph_);}
MSParagraph& operator<<(MSPrintManager& printManager_,const MSStringVector& aStringVector_)
{return printManager_.addParagraph(aStringVector_);}
MSPrintColumn& operator<<(MSPrintManager& printManager_,MSPrintColumn* column_)
{return printManager_.addPrintItem(column_);}
MSReportTable& operator<<(MSPrintManager& printManager_,MSReportTable *table_)
{return printManager_.addPrintItem(table_);}
MSGraph& operator<<(MSPrintManager& printManager_,MSGraph *graph_)
{return printManager_.addPrintItem(graph_);}
MSPrintItem& operator<<(MSPrintManager& printManager_,MSPrintItem *item_)
{return printManager_.addPrintItem(item_);}

