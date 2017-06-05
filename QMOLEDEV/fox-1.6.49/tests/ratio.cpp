#include "fx.h"


/*
  Example proportional layout using FXSpring widgets
*/
int main(int argc,char **argv){

  // Create application object
  FXApp application("Ratio","FoxTest");

  // Initialize and open display
  application.init(argc,argv);

  // Create main window
  FXMainWindow *main=new FXMainWindow(&application,"Ratio",NULL,NULL,DECOR_ALL,0,0,400,200,8,8,8,8,6,6);
  
  // Add quit button and connect it to application
  new FXButton(main,"&Quit",NULL,&application,FXApp::ID_QUIT,FRAME_RAISED|FRAME_THICK|LAYOUT_SIDE_BOTTOM|LAYOUT_CENTER_X,0,0,0,0,20,20,2,2);
  
  // Label above it
  new FXLabel(main,"FXSpring can be used to keep widgets at fixed size ratios.\n\nResize the window to see how it behaves!",NULL,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

  // Layout manager to place the springs  
  FXHorizontalFrame *horz=new FXHorizontalFrame(main,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);

  // First spring is set to behave normally in Y direction,
  // but to have a ratio 1 for the X direction
  FXSpring *spring1=new FXSpring(horz,LAYOUT_FILL_X|LAYOUT_FILL_Y,1,0, 0,0,0,0, 0,0,0,0);
  FXLabel *label1=new FXLabel(spring1,"1",NULL,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  label1->setBackColor(FXRGB(255,0,0));

  // Second spring has ratio 2 in the X direction
  FXSpring *spring2=new FXSpring(horz,LAYOUT_FILL_X|LAYOUT_FILL_Y,2,0, 0,0,0,0, 0,0,0,0);
  FXLabel *label2=new FXLabel(spring2,"2",NULL,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  label2->setBackColor(FXRGB(0,255,0));

  // Second spring has ratio 3 in the X direction
  FXSpring *spring3=new FXSpring(horz,LAYOUT_FILL_X|LAYOUT_FILL_Y,3,0, 0,0,0,0, 0,0,0,0);
  FXLabel *label3=new FXLabel(spring3,"3",NULL,LAYOUT_FILL_X|LAYOUT_FILL_Y);
  label3->setBackColor(FXRGB(0,0,255));

  application.create();

  main->show(PLACEMENT_SCREEN);

  return application.run();
  }
