/********************************************************************************
*                                                                               *
*                                  Chart  Test                                  *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* $Id: chart.cpp,v 1.7 2006/01/22 18:01:13 fox Exp $                            *
********************************************************************************/
#include "fx.h"
#include "FXChart.h"
#include "icons.h"

/*
  Notes:

*/

/*******************************************************************************/


// Mini application object
class ChartWindow : public FXMainWindow {
  FXDECLARE(ChartWindow)
protected:
  FXChart           *chart;
  FXMenuBar         *menubar;
  FXStatusBar       *statusbar;
  FXMenuPane        *filemenu;
  FXBMPImage        *image;
protected:
  ChartWindow(){}
private:
  ChartWindow(const ChartWindow&);
  ChartWindow &operator=(const ChartWindow&);
public:
  long onCmdChart(FXObject*,FXSelector,void*);
public:
  enum {
    ID_CHART=FXMainWindow::ID_LAST
    };
public:
  ChartWindow(FXApp *a);
  virtual void create();
  virtual ~ChartWindow();
  };



/*******************************************************************************/

// Map
FXDEFMAP(ChartWindow) ChartWindowMap[]={
  FXMAPFUNC(SEL_COMMAND,  ChartWindow::ID_CHART,     ChartWindow::onCmdChart),
  };


// Object implementation
FXIMPLEMENT(ChartWindow,FXMainWindow,ChartWindowMap,ARRAYNUMBER(ChartWindowMap))



// Make some windows
ChartWindow::ChartWindow(FXApp* a):FXMainWindow(a,"Chart Test",NULL,NULL,DECOR_ALL,20,20,700,460){

  // Menubar
  menubar=new FXMenuBar(this,FRAME_RAISED|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
  statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|FRAME_RAISED|STATUSBAR_WITH_DRAGCORNER);

  // File menu
  filemenu=new FXMenuPane(this);
  new FXMenuCommand(filemenu,"&Quit\tCtl-Q",NULL,getApp(),FXApp::ID_QUIT);
  new FXMenuTitle(menubar,"&File",NULL,filemenu);

  // Container
  FXHorizontalFrame *container=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED);

  // Well for chart
  FXHorizontalFrame *chartwell=new FXHorizontalFrame(container,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0, 0,0);

  // Chart
  chart=new FXChart(chartwell,this,ID_CHART,LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Pattern
  image=new FXBMPImage(getApp(),marble);

  FillStyle fs;
  fs.style=FILLSTYLE_HORIZONTAL;
  fs.hatch=STIPPLE_NONE;
  fs.image=image;
  fs.color=FXRGB(128,255,255);
  fs.backcolor=FXRGB(128,128,255);
  fs.lower=FXRGB(255,255,255);
  fs.upper=FXRGB(0,0,255);
  chart->setFillStyle(fs);
  }


// Create image
void ChartWindow::create(){
  FXMainWindow::create();
  image->create();
  }


// Command from chart
long ChartWindow::onCmdChart(FXObject*,FXSelector,void*){
  return 1;
  }


// Clean up
ChartWindow::~ChartWindow(){
  delete filemenu;
  delete image;
  }


/*******************************************************************************/


// Start the whole thing
int main(int argc,char *argv[]){

  // Make application
  FXApp application("ChartWindow","FoxTest");

  // Open display
  application.init(argc,argv);

  // Main window
  ChartWindow* window=new ChartWindow(&application);

  // Create app
  application.create();

  // Show it
  window->show(PLACEMENT_SCREEN);

  // Run
  return application.run();
  }
