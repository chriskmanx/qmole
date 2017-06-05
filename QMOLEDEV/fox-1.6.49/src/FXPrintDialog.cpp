/********************************************************************************
*                                                                               *
*                        P r i n t   J o b   D i a l o g                        *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXPrintDialog.cpp,v 1.54 2006/01/22 17:58:37 fox Exp $                   *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "FXHash.h"
#include "fxascii.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXStat.h"
#include "FXFile.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObjectList.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCPrint.h"
#include "FXGIFIcon.h"
#include "FXRecentFiles.h"
#include "FXFrame.h"
#include "FXSeparator.h"
#include "FXLabel.h"
#include "FXTextField.h"
#include "FXButton.h"
#include "FXRadioButton.h"
#include "FXPacker.h"
#include "FXHorizontalFrame.h"
#include "FXVerticalFrame.h"
#include "FXMatrix.h"
#include "FXSpinner.h"
#include "FXGroupBox.h"
#include "FXDialogBox.h"
#include "FXScrollArea.h"
#include "FXList.h"
#include "FXListBox.h"
#include "FXComboBox.h"
#include "FXMessageBox.h"
#include "FXInputDialog.h"
#include "FXFileDialog.h"
#include "FXPrintDialog.h"
#ifdef HAVE_CUPS_H
#include <cups/cups.h>
#include <cups/ppd.h>
#endif
#include "icons.h"

/*
  Notes:

  - Registry paper data base is of the form:

      [PAPER]
      0 = "[US Letter] 612 792 72 72 72 72"
      1 = "[US Legal] 612 1008 72 72 72 72"
      2 = "[A3] 841.88976 1190.5512 80 80 80 80"
      3 = "[A4] 595.27559 841.88976 80 80 80 80"
      4 = "[A5] 420.94488 595.27559 80 80 80 80"
      5 = "[B4] 730 1034 60 60 60 60"
      6 = "[B5] 500 710 80 80 80 80"
      7 = "[B5 Japan] 517 730 80 80 80 80"
      8 = "[Half Letter] 612 397 72 72 72 72"
      9 = "[Executive] 523 758 72 72 72 72"
      10 = "[Tabloid/Ledger] 794 1227 72 72 72 72"
      11 = "[Monarch] 280 542 10 10 10 10"
      12 = "[SuperB] 843 1227 80 80 80 80"
      13 = "[Envelope Commercial] 298 686 5 5 5 5"
      14 = "[Envelope Monarch] 280 542 5 5 5 5"
      15 = "[Envelope DL] 312 625 5 5 5 5"
      16 = "[Envelope C5] 460 650 5 5 5 5"
      17 = "[Euro Postcard] 298 420 5 5 5 5"

    The paper media are described in terms of points, i.e. 1/72 of an inch.
    The description comprises name, stock width, height, and left, right, top,
    bottom margins.

  - Inputs print job:
    Destination Printer, or file (browse to file)
    Orientation
    Paper size [A4(210x297mm) B5, Letter(8.5x11in), Legal, Executive, ?...
    Print all pages, or range of pages [first/last]
    Page collate order [first page first, last page first, even/odd]
    Number of copies
    Color/gray/blackwhite
    Custom paper dimensions
    Standard paper dimensions:

                  Page size          Print area         Top/Bottom   Sides

    Letter        8.5 x 11 in        8.2 x 10.6 in      0.25 in       0.25 in
    A4            210 x 297 mm       200 x 287 mm       5 mm         5 mm
    Legal         8.5 x 14 in        8.2 x 13.6 in      0.2 in       0.15 in
    #10 Envelope  4.13 x 9.5 in      3.8 x 9.2 in       0.16 in      0.14 in
                  104.8 x 241.3 mm   96.5 x 233.7 mm    4.1 mm       3.6 mm

    Known paper sizes:

       PAPERSIZE    X"         Y"         X cm      Y cm
       -----------------------------------------------------
       11x17        11"        17"        27.94     43.18
       a0           33.0556"   46.7778"   83.9611   118.816
       a10          1.02778"   1.45833"   2.61056   3.70417
       a1           23.3889"   33.0556"   59.4078   83.9611
       a2           16.5278"   23.3889"   41.9806   59.4078
       a3           11.6944"   16.5278"   29.7039   41.9806
       a4           8.26389"   11.6944"   20.9903   29.7039
       a5           5.84722"   8.26389"   14.8519   20.9903
       a6           4.125"     5.84722"   10.4775   14.8519
       a7           2.91667"   4.125"     7.40833   10.4775
       a8           2.05556"   2.91667"   5.22111   7.40833
       a9           1.45833"   2.05556"   3.70417   5.22111
       archA        9"         12"        22.86     30.48
       archB        12"        18"        30.48     45.72
       archC        18"        24"        45.72     60.96
       archD        24"        36"        60.96     91.44
       archE        36"        48"        91.44     121.92
       b0           39.3889"   55.6667"   100.048   141.393
       b1           27.8333"   39.3889"   70.6967   100.048
       b2           19.6944"   27.8333"   50.0239   70.6967
       b3           13.9167"   19.6944"   35.3483   50.0239
       b4           9.84722"   13.9167"   25.0119   35.3483
       b5           6.95833"   9.84722"   17.6742   25.0119
       flsa         8.5"       13"        21.59     33.02
       flse         8.5"       13"        21.59     33.02
       halfletter   5.5"       8.5"       13.97     21.59
       ledger       17"        11"        43.18     27.94
       legal        8.5"       14"        21.59     35.56
       letter       8.5"       11"        21.59     27.94
       note         7.5"       10"        19.05     25.4

       Paper Keywords and paper size in points
       =======================================

       Letter           612x792
       LetterSmall      612x792
       Tabloid          792x1224
       Ledger          1224x792
       Legal            612x1008
       Statement        396x612
       Executive        540x720
       A0               2384x3371
       A1              1685x2384
       A2              1190x1684
       A3               842x1190
       A4               595x842
       A4Small          595x842
       A5               420x595
       B4               729x1032
       B5               516x729
       Envelope         ???x???
       Folio            612x936
       Quarto           610x780
       10x14            720x1008


*/

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXPrintDialog) FXPrintDialogMap[]={
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_TO_PRINTER,FXPrintDialog::onCmdToPrinter),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_TO_PRINTER,FXPrintDialog::onUpdToPrinter),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_TO_FILE,FXPrintDialog::onCmdToFile),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_TO_FILE,FXPrintDialog::onUpdToFile),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_BROWSE_FILE,FXPrintDialog::onCmdBrowse),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_BROWSE_FILE,FXPrintDialog::onUpdBrowse),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_PROPERTIES,FXPrintDialog::onCmdProps),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_PROPERTIES,FXPrintDialog::onUpdProps),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_PORTRAIT,FXPrintDialog::onCmdPortrait),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_PORTRAIT,FXPrintDialog::onUpdPortrait),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_LANDSCAPE,FXPrintDialog::onCmdLandscape),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_LANDSCAPE,FXPrintDialog::onUpdLandscape),
  FXMAPFUNCS(SEL_COMMAND,FXPrintDialog::ID_PAGES_ALL,FXPrintDialog::ID_PAGES_RANGE,FXPrintDialog::onCmdPages),
  FXMAPFUNCS(SEL_UPDATE,FXPrintDialog::ID_PAGES_ALL,FXPrintDialog::ID_PAGES_RANGE,FXPrintDialog::onUpdPages),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_COLOR_PRINTER,FXPrintDialog::onCmdColor),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_COLOR_PRINTER,FXPrintDialog::onUpdColor),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_GRAY_PRINTER,FXPrintDialog::onCmdGray),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_GRAY_PRINTER,FXPrintDialog::onUpdGray),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_NUM_COPIES,FXPrintDialog::onCmdNumCopies),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_NUM_COPIES,FXPrintDialog::onUpdNumCopies),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_PAGES_FIRST,FXPrintDialog::onCmdFirstPage),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_PAGES_FIRST,FXPrintDialog::onUpdFirstPage),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_PAGES_LAST,FXPrintDialog::onCmdLastPage),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_PAGES_LAST,FXPrintDialog::onUpdLastPage),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_COLLATE_NORMAL,FXPrintDialog::onCmdCollateNormal),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_COLLATE_NORMAL,FXPrintDialog::onUpdCollateNormal),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_COLLATE_REVERSED,FXPrintDialog::onCmdCollateReversed),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_COLLATE_REVERSED,FXPrintDialog::onUpdCollateReversed),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_MEDIA,FXPrintDialog::onCmdMedia),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_MEDIA,FXPrintDialog::onUpdMedia),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_PRINTER_NAME,FXPrintDialog::onCmdPrinterName),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_PRINTER_NAME,FXPrintDialog::onUpdPrinterName),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_FILE_NAME,FXPrintDialog::onCmdFileName),
  FXMAPFUNC(SEL_UPDATE,FXPrintDialog::ID_FILE_NAME,FXPrintDialog::onUpdFileName),
  FXMAPFUNC(SEL_COMMAND,FXPrintDialog::ID_ACCEPT,FXPrintDialog::onCmdAccept),
  };


// Object implementation
FXIMPLEMENT(FXPrintDialog,FXDialogBox,FXPrintDialogMap,ARRAYNUMBER(FXPrintDialogMap))


// Separator item
FXPrintDialog::FXPrintDialog(FXWindow* owner,const FXString& caption,FXuint opts,FXint x,FXint y,FXint w,FXint h):
  FXDialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,0,0,0,0,4,4){
  FXchar key[20],name[100];
  //const FXchar *paper;

  FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,10,10,10,10, 10,10);

  // Close and cancel buttons
  FXHorizontalFrame* buttons=new FXHorizontalFrame(contents,LAYOUT_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,0,0,0,0);
  new FXButton(buttons,tr("&Print"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,20,20);
  new FXButton(buttons,tr("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,20,20);
#ifdef HAVE_CUPS_H
  FXLabel *label=new FXLabel(buttons,tr("Using CUPS"),NULL,LAYOUT_CENTER_Y|LAYOUT_LEFT,0,0,0,0,0,0);
  label->setTextColor(label->getShadowColor());
#endif

  // Print destination
  FXGroupBox *dest=new FXGroupBox(contents,tr("Print Destination"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5, 10,5);

  sendtoprinter=new FXRadioButton(dest,tr("Pr&inter:"),this,ID_TO_PRINTER,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  FXHorizontalFrame* printdest=new FXHorizontalFrame(dest,LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0, 10,10);
  printername=new FXComboBox(printdest,25,this,ID_PRINTER_NAME,COMBOBOX_NORMAL|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  printername->setNumVisible(4);
  new FXButton(printdest,tr("Properties..."),NULL,this,ID_PROPERTIES,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT|LAYOUT_FIX_WIDTH,0,0,100,0, 10,10);

  new FXFrame(dest,LAYOUT_SIDE_TOP|LAYOUT_FIX_HEIGHT,0,0,0,10);

  sendtofile=new FXRadioButton(dest,tr("&File:"),this,ID_TO_FILE,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  FXHorizontalFrame* filedest=new FXHorizontalFrame(dest,LAYOUT_SIDE_TOP|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0, 10,10);
  filename=new FXTextField(filedest,25,this,ID_FILE_NAME,FRAME_THICK|FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_FILL_X);
  new FXButton(filedest,tr("&Browse..."),NULL,this,ID_BROWSE_FILE,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT|LAYOUT_FIX_WIDTH,0,0,100,0, 10,10);

  FXHorizontalFrame* bottom=new FXHorizontalFrame(contents,LAYOUT_BOTTOM|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0);

  // Left
  FXVerticalFrame *left=new FXVerticalFrame(bottom,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0, 10,10);

  // Pages
  FXGroupBox *pages=new FXGroupBox(left,tr("Pages"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  printall=new FXRadioButton(pages,tr("Print &All"),this,ID_PAGES_ALL,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  printeven=new FXRadioButton(pages,tr("&Even Pages"),this,ID_PAGES_EVEN,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  printodd=new FXRadioButton(pages,tr("&Odd Pages"),this,ID_PAGES_ODD,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  printrange=new FXRadioButton(pages,tr("Print &Range:"),this,ID_PAGES_RANGE,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  FXMatrix *pagerange=new FXMatrix(pages,2,MATRIX_BY_ROWS|LAYOUT_FILL_X|LAYOUT_SIDE_TOP,0,0,0,0, 20,0,0,0);
  new FXLabel(pagerange,tr("From:"),NULL,LAYOUT_CENTER_Y|LAYOUT_RIGHT|JUSTIFY_RIGHT);
  new FXLabel(pagerange,tr("To:"),NULL,LAYOUT_CENTER_Y|LAYOUT_RIGHT|JUSTIFY_RIGHT);
  firstpage=new FXSpinner(pagerange,4,this,ID_PAGES_FIRST,FRAME_THICK|FRAME_SUNKEN|LAYOUT_RIGHT);
  lastpage=new FXSpinner(pagerange,4,this,ID_PAGES_LAST,FRAME_THICK|FRAME_SUNKEN|LAYOUT_RIGHT);
  firstpage->setRange(1,10000000);
  lastpage->setRange(1,10000000);

  FXGroupBox *colors=new FXGroupBox(left,tr("Colors"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  printincolor=new FXRadioButton(colors,tr("Print in Color"),this,ID_COLOR_PRINTER,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  printinblacknwhite=new FXRadioButton(colors,tr("Print in Black and White"),this,ID_GRAY_PRINTER,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);

  // Right
  FXVerticalFrame *right=new FXVerticalFrame(bottom,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0, 10,10);

  // Make some icons
  landscapeIcon=new FXGIFIcon(getApp(),landscape);
  portraitIcon=new FXGIFIcon(getApp(),portrait);

  // Layout
  FXGroupBox *copies=new FXGroupBox(right,tr("Copies"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  new FXLabel(copies,tr("Number of copies to print:"),NULL,LAYOUT_CENTER_Y|LAYOUT_RIGHT|JUSTIFY_RIGHT|LAYOUT_SIDE_LEFT);
  numberofcopies=new FXSpinner(copies,4,this,ID_NUM_COPIES,FRAME_THICK|FRAME_SUNKEN|LAYOUT_CENTER_Y|LAYOUT_SIDE_LEFT);
  numberofcopies->setRange(1,1000);

  // Collate
  FXGroupBox *collate=new FXGroupBox(right,tr("Collate Order"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  firstpagefirst=new FXRadioButton(collate,tr("First Page First"),this,ID_COLLATE_NORMAL,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);
  lastpagefirst=new FXRadioButton(collate,tr("Last Page First"),this,ID_COLLATE_REVERSED,LAYOUT_SIDE_TOP|ICON_BEFORE_TEXT);

  // Layout
  FXGroupBox *orientation=new FXGroupBox(right,tr("Layout"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  new FXLabel(orientation,FXString::null,portraitIcon,LAYOUT_SIDE_LEFT|LAYOUT_CENTER_Y|LAYOUT_RIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_CENTER_Y);
  orientportrait=new FXRadioButton(orientation,tr("Portrait"),this,ID_PORTRAIT,LAYOUT_SIDE_LEFT|ICON_BEFORE_TEXT|JUSTIFY_CENTER_Y|LAYOUT_CENTER_Y);
  orientlanscape=new FXRadioButton(orientation,tr("Landscape"),this,ID_LANDSCAPE,LAYOUT_SIDE_RIGHT|ICON_BEFORE_TEXT|JUSTIFY_CENTER_Y|LAYOUT_CENTER_Y);
  new FXLabel(orientation,FXString::null,landscapeIcon,LAYOUT_SIDE_RIGHT|LAYOUT_CENTER_Y|LAYOUT_RIGHT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_CENTER_Y);

  // Paper
  FXGroupBox *paperdims=new FXGroupBox(right,tr("Paper Size"),GROUPBOX_TITLE_LEFT|FRAME_RIDGE|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_FILL_Y,0,0,0,0, 10,10,5,5);
  media=new FXListBox(paperdims,this,ID_MEDIA,FRAME_SUNKEN|FRAME_THICK|LISTBOX_NORMAL|LAYOUT_FILL_X|LAYOUT_SIDE_TOP);
  media->setNumVisible(6);

  // Fill up with some defaults
  sendtoprinter->setCheck(TRUE);
  printall->setCheck(TRUE);
  firstpagefirst->setCheck(TRUE);
  printinblacknwhite->setCheck(TRUE);
  orientportrait->setCheck(TRUE);

  // Initial focus on printer name
  printername->setFocus();

  // Make sure we have at least a couple
  if(!getApp()->reg().readStringEntry("PAPER","0",NULL)){
    getApp()->reg().writeStringEntry("PAPER","0","[US Letter] 612 792 72 72 72 72");
    getApp()->reg().writeStringEntry("PAPER","1","[US Legal] 612 1008 72 72 72 72");
    getApp()->reg().writeStringEntry("PAPER","2","[A4] 595.27559 841.88976 80 80 80 80");
    getApp()->reg().writeStringEntry("PAPER","3","[A3] 841.88976 1190.5512 80 80 80 80");
    }

  // List paper sizes
  for(int i=0; ; i++){
    sprintf(key,"%d",i);
    if(getApp()->reg().readFormatEntry("PAPER",key,"[%[^]]] %*f %*f %*f %*f %*f %*f",name)!=1) break;
    media->appendItem(name);
    }

  // These things are always reset
  printer.firstpage=1;
  printer.lastpage=1;
  printer.currentpage=1;
  printer.frompage=1;
  printer.topage=1;
  printer.numcopies=1;

  // The rest initialized from registry
  printer.mediasize=1;
  printer.mediawidth=612.0;
  printer.mediaheight=792.0;
  printer.leftmargin=72.0;
  printer.rightmargin=72.0;
  printer.topmargin=72.0;
  printer.bottommargin=72.0;

  // Get media
  printer.mediasize=getApp()->reg().readIntEntry("PRINTER","media",0);

  // Obtain size
  sprintf(key,"%d",printer.mediasize);

  // Parse size
  getApp()->reg().readFormatEntry("PAPER",key,"[%[^]]] %lf %lf %lf %lf %lf %lf",name,&printer.mediawidth,&printer.mediaheight,&printer.leftmargin,&printer.rightmargin,&printer.topmargin,&printer.bottommargin);
  FXTRACE((100,"Media=\"%s\" w=%g h=%g lm=%g rm=%g tm=%g bm=%g\n",name,printer.mediawidth,printer.mediaheight,printer.leftmargin,printer.rightmargin,printer.topmargin,printer.bottommargin));

  // Initial flags
  printer.flags=0;

  // Landscape or portrait
  if(getApp()->reg().readIntEntry("PRINTER","landscape",FALSE))
    printer.flags|=PRINT_LANDSCAPE;
  else
    printer.flags&=~PRINT_LANDSCAPE;

  // Print to file or printer
  if(getApp()->reg().readIntEntry("PRINTER","printtofile",FALSE))
    printer.flags|=PRINT_DEST_FILE;
  else
    printer.flags&=~PRINT_DEST_FILE;

  // Color prints
  if(getApp()->reg().readIntEntry("PRINTER","color",FALSE))
    printer.flags|=PRINT_COLOR;
  else
    printer.flags&=~PRINT_COLOR;

  // Initial name
  if(printer.flags&PRINT_DEST_FILE)
    printer.name=getApp()->reg().readStringEntry("PRINTER","file","output.ps");
  else
    printer.name=getApp()->reg().readStringEntry("PRINTER","printer","");
  }


// Build a list of printers
void FXPrintDialog::create(){
  FXDialogBox::create();
#ifndef WIN32
#ifdef HAVE_CUPS_H

  // Use CUPS to determine list of printers
  cups_dest_t *dests;
  int          num_dests;

  // Obtain list of destinations
  num_dests=cupsGetDests(&dests);

  // Fill list of printers
  for(int d=0; d<num_dests; d++){
    printername->appendItem(dests[d].name);
    if(printer.name==dests[d].name) printername->setCurrentItem(d);
    }

#else

  char name[1000];
  FILE *pc;
  char buf[1000];
  int i;

  // Open printcap file, found as per registry setting
  // You may have change this setting for your particular system
  pc=fopen(getApp()->reg().readStringEntry("SETTINGS","printcap","/etc/printcap"),"r");

  // Cannot open the printcap file
  if(!pc) return;

  name[0]=0;

  // Parse printcap one line at a time
  while(fgets(buf,1000,pc)){

    // Extra info if printcap has been generated by Printtool
    if(strncmp(buf,"##PRINTTOOL3##",14)==0){
      if(sscanf(buf,"%*s %*s %*s %*s %*s %*s %s",name)!=1) name[0]=0;
      continue;
      }

    // Blank line
    if(buf[0]=='#' || Ascii::isSpace(buf[0])){
      continue;
      }

    // Snarf printer name (we read until the ':' or the '|' which separates aliases)
    for(i=0; i<1000 && buf[i]!=0 && buf[i]!=':' && buf[i]!='|'; i++);
    buf[i]=0;

    // Append human-readable info, if any
    if(name[0]){
      strcat(buf," (");
      strcat(buf,name);
      strcat(buf,")");
      }

    // Add printer name
    if(strlen(buf)) printername->appendItem(buf);

    name[0]=0;
    //if(printer.name==dests[d].name) printername->setCurrentItem(d);
    }

  // Close the file
  fclose(pc);

#endif
#else

  // What operating system are we running?
  OSVERSIONINFO osvi;
  osvi.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
  GetVersionEx(&osvi);
  FXuint p;

  // Determine list of printers on Windows NT
  if(osvi.dwPlatformId==VER_PLATFORM_WIN32_NT){
    DWORD dwFlags=PRINTER_ENUM_LOCAL|PRINTER_ENUM_CONNECTIONS;
    DWORD dwBytesNeeded,dwNumPrinters;
    EnumPrinters(dwFlags,NULL,4,NULL,0,&dwBytesNeeded,&dwNumPrinters);
    PRINTER_INFO_4 *prtinfo;
    if(FXMALLOC(&prtinfo,BYTE,dwBytesNeeded)){
      if(EnumPrinters(dwFlags,NULL,4,(LPBYTE)prtinfo,dwBytesNeeded,&dwBytesNeeded,&dwNumPrinters)){
	for(p=0; p<dwNumPrinters; p++){
          printername->appendItem(prtinfo[p].pPrinterName);
          if(printer.name==prtinfo[p].pPrinterName) printername->setCurrentItem(p);
          }
	}
      FXFREE(&prtinfo);
      }
    }

  // Determine list of printers on Windows 9x
  else if(osvi.dwPlatformId==VER_PLATFORM_WIN32_WINDOWS){
    DWORD dwFlags=PRINTER_ENUM_LOCAL;
    DWORD dwBytesNeeded,dwNumPrinters;
    EnumPrinters(dwFlags,NULL,5,NULL,0,&dwBytesNeeded,&dwNumPrinters);
    PRINTER_INFO_5 *prtinfo;
    if(FXMALLOC(&prtinfo,BYTE,dwBytesNeeded)){
      if(EnumPrinters(dwFlags,NULL,5,(LPBYTE)prtinfo,dwBytesNeeded,&dwBytesNeeded,&dwNumPrinters)){
        for(p=0; p<dwNumPrinters; p++){
          printername->appendItem(prtinfo[p].pPrinterName);
          if(printer.name==prtinfo[p].pPrinterName) printername->setCurrentItem(p);
          }
	}
      FXFREE(&prtinfo);
      }
    }

#endif
  }


// Send to printer
long FXPrintDialog::onCmdToPrinter(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","printtofile",FALSE);
  printer.name=printername->getText();
  printer.flags&=~PRINT_DEST_FILE;
  FXTRACE((100,"Print to printer: %s\n",printer.name.text()));

// #ifdef HAVE_CUPS_H
//   int num_dests;
//   cups_dest_t *dests;
//   num_dests=cupsGetDests(&dests);
//   FXTRACE((1,"num_dests=%d\n",num_dests));
//   for(int i=0; i<num_dests; i++){
//     FXTRACE((1,"printer name=%s instance=%s default=%d\n",dests[i].name,dests[i].instance,dests[i].is_default));
//     const char* ppdfilename;
//     ppdfilename=cupsGetPPD(dests[i].name);
//     if(ppdfilename){
//       ppd_file_t* ppd;
//       ppd=ppdOpenFile(ppdfilename);
//       if(ppd){
//         FXTRACE((1,"got ppd\n"));
//         FXTRACE((1,"ppd->jcl_begin=%d\n",ppd->jcl_begin));
//         FXTRACE((1,"ppd->jcl_ps=%d\n",ppd->jcl_ps));
//         FXTRACE((1,"ppd->jcl_end=%d\n",ppd->jcl_end));
//         FXTRACE((1,"ppd->num_fonts=%d\n",ppd->num_fonts));
//         for(int f=0; f<ppd->num_fonts; f++) FXTRACE((1,"ppd->fonts[%d]=%s\n",f,ppd->fonts[f]));
//         FXTRACE((1,"ppd->manufacturer=%s\n",ppd->manufacturer));
//         FXTRACE((1,"ppd->modelname=%s\n",ppd->modelname));
//         FXTRACE((1,"ppd->nickname=%s\n",ppd->nickname));
//         FXTRACE((1,"ppd->shortnickname=%s\n",ppd->shortnickname));
//         FXTRACE((1,"ppd->ttrasterizer=%s\n",ppd->ttrasterizer));
//         FXTRACE((1,"ppd->product=%s\n",ppd->product));
//         FXTRACE((1,"ppd->patches=%s\n",ppd->patches));
//         FXTRACE((1,"ppd->lang_version=%s\n",ppd->lang_version));
//         FXTRACE((1,"ppd->lang_encoding=%s\n",ppd->lang_encoding));
//         FXTRACE((1,"ppd->landscape=%d\n",ppd->landscape));
//         FXTRACE((1,"ppd->language_level=%d\n",ppd->language_level));
//         FXTRACE((1,"ppd->model_number=%d\n",ppd->model_number));
//         FXTRACE((1,"ppd->manual_copies=%d\n",ppd->manual_copies));
//         FXTRACE((1,"ppd->throughput=%d\n",ppd->throughput));
//         FXTRACE((1,"ppd->variable_sizes=%d\n",ppd->variable_sizes));
//         FXTRACE((1,"ppd->num_sizes=%d\n",ppd->num_sizes));
//         for(int s=0; s<ppd->num_sizes; s++){
//           ppd_size_t *sz=&ppd->sizes[s];
//           FXTRACE((1,"sz->marked=%d\n",sz->marked));
//           FXTRACE((1,"sz->name=%s\n",sz->name));
//           FXTRACE((1,"sz->width=%f\n",sz->width));
//           FXTRACE((1,"sz->length=%f\n",sz->length));
//           FXTRACE((1,"sz->top=%f\n",sz->top));
//           FXTRACE((1,"sz->bottom=%f\n",sz->bottom));
//           FXTRACE((1,"sz->left=%f\n",sz->left));
//           FXTRACE((1,"sz->right=%f\n",sz->right));
//           }
//         ppdClose(ppd);
//         }
//       }
//     }
//   const char *defdest;
//   defdest=cupsGetDefault();
//   FXTRACE((1,"default destination=%s\n",defdest));
// #endif
  return 1;
  }


// Update send to printer
long FXPrintDialog::onUpdToPrinter(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_DEST_FILE)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Send to file
long FXPrintDialog::onCmdToFile(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","printtofile",TRUE);
  printer.name=filename->getText();
  printer.flags|=PRINT_DEST_FILE;
  FXTRACE((100,"Print to file: %s\n",printer.name.text()));
  return 1;
  }


// Update send to file
long FXPrintDialog::onUpdToFile(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_DEST_FILE)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Close dialog with an accept
long FXPrintDialog::onCmdAccept(FXObject* sender,FXSelector sel,void* ptr){
  if(printer.flags&PRINT_DEST_FILE){
    if(FXStat::exists(printer.name)){
      FXuint answer=FXMessageBox::question(this,MBOX_YES_NO_CANCEL,tr("Overwrite file?"),tr("Overwrite existing file %s?"),printer.name.text());
      if(answer==MBOX_CLICKED_CANCEL) return 1;
      if(answer==MBOX_CLICKED_NO){
        return FXDialogBox::onCmdCancel(sender,sel,ptr);
        }
      }
    }
  return FXDialogBox::onCmdAccept(sender,sel,ptr);
  }


// Browse output file
long FXPrintDialog::onCmdBrowse(FXObject*,FXSelector,void*){
  FXString name=getApp()->reg().readStringEntry("PRINTER","file","output.ps");
  name=FXFileDialog::getSaveFilename(this,tr("Select Output File"),name,tr("All Files (*)\nPostscript Files (*.ps,*.eps)"),0);
  if(name.empty()) return 1;
  getApp()->reg().writeStringEntry("PRINTER","file",name.text());
  if(printer.flags&PRINT_DEST_FILE){
    printer.name=name;
    FXTRACE((100,"Print to file: %s\n",printer.name.text()));
    }
  return 1;
  }


// Update browse output file
long FXPrintDialog::onUpdBrowse(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_DEST_FILE)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Printer properties panel
long FXPrintDialog::onCmdProps(FXObject*,FXSelector,void*){   // FIXME this needs a dialog to add/remove printers
#ifndef WIN32
  FXString command="lpr -P%s -#%d";
  if(FXInputDialog::getString(command,this,tr("Printer Command"),"Specify the printer command, for example:\n\n  \"lpr -P%s -#%d\" or \"lp -d%s -n%d\"\n\nThis will print \"%d\" copies to printer \"%s\".")){
    getApp()->reg().writeStringEntry("PRINTER","command",command.text());
    }
#else
#endif
  return 1;
  }


// Update printer properties panel
long FXPrintDialog::onUpdProps(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_DEST_FILE)?FXSEL(SEL_COMMAND,ID_DISABLE):FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// File name
long FXPrintDialog::onCmdFileName(FXObject*,FXSelector,void*){
  FXString name=filename->getText();
  getApp()->reg().writeStringEntry("PRINTER","file",name.text());
  if(printer.flags&PRINT_DEST_FILE){
    printer.name=name;
    FXTRACE((100,"Print to file: %s\n",printer.name.text()));
    }
  return 1;
  }


// Update File name
long FXPrintDialog::onUpdFileName(FXObject* sender,FXSelector,void*){
  FXString name;
  if(printer.flags&PRINT_DEST_FILE){
    name=printer.name;
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  else{
    name=getApp()->reg().readStringEntry("PRINTER","file","output.ps");
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&name);
  return 1;
  }


// Printer name
long FXPrintDialog::onCmdPrinterName(FXObject*,FXSelector,void*){
  FXString name=printername->getText();
  getApp()->reg().writeStringEntry("PRINTER","printer",name.text());
  if(!(printer.flags&PRINT_DEST_FILE)){
    printer.name=name;
    FXTRACE((100,"Print to printer: %s\n",printer.name.text()));
//
//   // If using CUPS, we list the available media types for this printer
// #ifndef WIN32
// #ifdef HAVE_CUPS_H
//   const char* ppd_file;
//   ppd_file_t* ppd;
//   ppd_file=cupsGetPPD(printer.name.text());
//   if(!ppd_file) return 1;
//   ppd=ppdOpenFile(ppd_file);
//   if(!ppd) return 1;
//   media->clearItems();
//   FXTRACE((100,"num_sizes=%d\n",ppd->num_sizes));
//   for(int s=0; s<ppd->num_sizes; s++){
//     FXTRACE((100,"ppd->sizes[%d].name=%s\n",s,ppd->sizes[s].name));
//     media->appendItem(ppd->sizes[s].name);
//     }
//   ppdClose(ppd);
// #endif
// #endif
    }
  return 1;
  }


// Update Printer name
long FXPrintDialog::onUpdPrinterName(FXObject* sender,FXSelector,void*){
  FXString name;
  if(printer.flags&PRINT_DEST_FILE){
    name=getApp()->reg().readStringEntry("PRINTER","printer","");
    sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    }
  else{
    name=printer.name;
    sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&name);
  return 1;
  }


// Portrait
long FXPrintDialog::onCmdPortrait(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","landscape",FALSE);
  printer.flags&=~PRINT_LANDSCAPE;
  return 1;
  }


// Update portrait
long FXPrintDialog::onUpdPortrait(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_LANDSCAPE)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Landscape
long FXPrintDialog::onCmdLandscape(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","landscape",TRUE);
  printer.flags|=PRINT_LANDSCAPE;
  return 1;
  }


// Update landscape
long FXPrintDialog::onUpdLandscape(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_LANDSCAPE)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Pages
long FXPrintDialog::onCmdPages(FXObject*,FXSelector sel,void*){
  FXuint sid=FXSELID(sel);
  switch(sid){
    case ID_PAGES_ALL:
      printer.flags&=~(PRINT_PAGES_EVEN|PRINT_PAGES_ODD|PRINT_PAGES_RANGE);
      printer.frompage=printer.firstpage;
      printer.topage=printer.lastpage;
      break;
    case ID_PAGES_EVEN:
      printer.flags|=PRINT_PAGES_EVEN;
      printer.flags&=~(PRINT_PAGES_ODD|PRINT_PAGES_RANGE);
      printer.frompage=(printer.firstpage+1)&~1;    // Get even
      printer.topage=printer.lastpage&~1;
      break;
    case ID_PAGES_ODD:
      printer.flags|=PRINT_PAGES_ODD;
      printer.flags&=~(PRINT_PAGES_EVEN|PRINT_PAGES_RANGE);
      printer.frompage=printer.firstpage|1;         // Get odd?
      printer.topage=(printer.lastpage-1)|1;
      break;
    case ID_PAGES_RANGE:
      printer.flags|=PRINT_PAGES_RANGE;
      printer.flags&=~(PRINT_PAGES_EVEN|PRINT_PAGES_ODD);
      printer.frompage=printer.firstpage;
      printer.topage=printer.lastpage;
      break;
    }
  if(printer.frompage>printer.lastpage) printer.frompage=printer.lastpage;
  if(printer.frompage<printer.firstpage) printer.frompage=printer.firstpage;
  if(printer.topage>printer.lastpage) printer.topage=printer.lastpage;
  if(printer.topage<printer.firstpage) printer.topage=printer.firstpage;
  return 1;
  }


// Update Pages
long FXPrintDialog::onUpdPages(FXObject* sender,FXSelector sel,void*){
  FXuint ch=ID_UNCHECK;
  FXuint sid=FXSELID(sel);
  switch(sid){
    case ID_PAGES_ALL:
      if(!(printer.flags&(PRINT_PAGES_EVEN|PRINT_PAGES_ODD|PRINT_PAGES_RANGE))) ch=ID_CHECK;
      break;
    case ID_PAGES_EVEN:
      if(printer.flags&PRINT_PAGES_EVEN) ch=ID_CHECK;
      break;
    case ID_PAGES_ODD:
      if(printer.flags&PRINT_PAGES_ODD) ch=ID_CHECK;
      break;
    case ID_PAGES_RANGE:
      if(printer.flags&PRINT_PAGES_RANGE) ch=ID_CHECK;
      break;
    }
  sender->handle(this,FXSEL(SEL_COMMAND,ch),NULL);
  return 1;
  }


// Color
long FXPrintDialog::onCmdColor(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","color",TRUE);
  printer.flags|=PRINT_COLOR;
  return 1;
  }


// Update color
long FXPrintDialog::onUpdColor(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_COLOR)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Gray
long FXPrintDialog::onCmdGray(FXObject*,FXSelector,void*){
  getApp()->reg().writeIntEntry("PRINTER","color",FALSE);
  printer.flags&=~PRINT_COLOR;
  return 1;
  }


// Update gray
long FXPrintDialog::onUpdGray(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_COLOR)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Number of copies
long FXPrintDialog::onCmdNumCopies(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&printer.numcopies);
  return 1;
  }


// Update number of copies
long FXPrintDialog::onUpdNumCopies(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&printer.numcopies);
  return 1;
  }


// First page
long FXPrintDialog::onCmdFirstPage(FXObject* sender,FXSelector,void*){
  FXint pg;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&pg);
  FXASSERT(pg>0);
  if((FXuint)pg<printer.firstpage) pg=printer.firstpage;
  if((FXuint)pg>printer.lastpage) pg=printer.lastpage;
  printer.frompage=pg;
  return 1;
  }


// Update first page
long FXPrintDialog::onUpdFirstPage(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&printer.frompage);
  sender->handle(this,(printer.flags&PRINT_PAGES_RANGE)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Last page
long FXPrintDialog::onCmdLastPage(FXObject* sender,FXSelector,void*){
  FXint pg;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&pg);
  FXASSERT(pg>0);
  if((FXuint)pg<printer.firstpage) pg=printer.firstpage;
  if((FXuint)pg>printer.lastpage) pg=printer.lastpage;
  printer.topage=pg;
  return 1;
  }


// Update last page
long FXPrintDialog::onUpdLastPage(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&printer.topage);
  sender->handle(this,(printer.flags&PRINT_PAGES_RANGE)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Collate Normal
long FXPrintDialog::onCmdCollateNormal(FXObject*,FXSelector,void*){
  printer.flags&=~PRINT_COLLATE_REVERSED;
  return 1;
  }


// Update Collate Normal
long FXPrintDialog::onUpdCollateNormal(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_COLLATE_REVERSED)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
  return 1;
  }


// Collate Reversed
long FXPrintDialog::onCmdCollateReversed(FXObject*,FXSelector,void*){
  printer.flags|=PRINT_COLLATE_REVERSED;
  return 1;
  }


// Update Collate Reversed
long FXPrintDialog::onUpdCollateReversed(FXObject* sender,FXSelector,void*){
  sender->handle(this,(printer.flags&PRINT_COLLATE_REVERSED)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  return 1;
  }


// Standard paper dimensions
long FXPrintDialog::onCmdMedia(FXObject*,FXSelector,void*){
  FXchar key[20],name[100];
  printer.mediasize=media->getCurrentItem();
  getApp()->reg().writeIntEntry("PRINTER","media",printer.mediasize);
  sprintf(key,"%d",printer.mediasize);
  getApp()->reg().readFormatEntry("PAPER",key,"[%[^]]] %lf %lf %lf %lf %lf %lf",name,&printer.mediawidth,&printer.mediaheight,&printer.leftmargin,&printer.rightmargin,&printer.topmargin,&printer.bottommargin);
  FXTRACE((100,"Media=\"%s\" w=%g h=%g lm=%g rm=%g tm=%g bm=%g\n",name,printer.mediawidth,printer.mediaheight,printer.leftmargin,printer.rightmargin,printer.topmargin,printer.bottommargin));
  return 1;
  }


// Update Standard paper dimensions
long FXPrintDialog::onUpdMedia(FXObject*,FXSelector,void*){
  if((FXint)printer.mediasize<=media->getNumItems()) media->setCurrentItem(printer.mediasize);
  return 1;
  }


// Set printer to print to
void FXPrintDialog::setPrinter(const FXPrinter& pr){
  printer=pr;
  }


// Obtain printer to print to
void FXPrintDialog::getPrinter(FXPrinter& pr){
  pr=printer;
  }


// Save data
void FXPrintDialog::save(FXStream& store) const {
  FXDialogBox::save(store);
  }


// Load data
void FXPrintDialog::load(FXStream& store){
  FXDialogBox::load(store);
  }


// Cleanup
FXPrintDialog::~FXPrintDialog(){
  delete landscapeIcon;
  delete portraitIcon;
  landscapeIcon=(FXIcon*)-1L;
  portraitIcon=(FXIcon*)-1L;
  }

}


