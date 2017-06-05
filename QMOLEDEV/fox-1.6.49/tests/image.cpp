/********************************************************************************
*                                                                               *
*                                   Image Test                                  *
*                                                                               *
********************************************************************************/
#include "fx.h"
#ifdef HAVE_PNG_H
#include "FXPNGImage.h"
#endif
#ifdef HAVE_JPEG_H
#include "FXJPGImage.h"
#endif
#ifdef HAVE_TIFF_H
#include "FXTIFImage.h"
#endif
#include "FXICOImage.h"
#include "FXTGAImage.h"
#include "FXRGBImage.h"

#include "FXGradientBar.h"


FXColor grey_ramp[512*50];                 // Created images
FXColor red_ramp[512*50];
FXColor green_ramp[512*50];
FXColor blue_ramp[512*50];


// Event Handler Object
class ImageWindow : public FXMainWindow {

  // Macro for class hierarchy declarations
  FXDECLARE(ImageWindow)

private:

  FXCanvas        *canvas;                    // Canvas to draw into
  FXColorWell     *backwell;                  // Color Well for background
  FXColorWell     *borderwell;                // Color Well for border
  FXColorWell     *textwell;                  // Color Well for text
  FXImage         *grey;
  FXImage         *red;
  FXImage         *green;
  FXImage         *blue;
  FXImage         *grey_nodither;
  FXImage         *red_nodither;
  FXImage         *green_nodither;
  FXImage         *blue_nodither;
  FXImage         *picture;                   // Complete picture
  FXFont          *font;                      // Font for text

protected:
  ImageWindow(){}

public:

  // Message handlers
  long onCanvasRepaint(FXObject*,FXSelector,void*);
  long onCmdWell(FXObject*,FXSelector,void*);
  long onCmdRestore(FXObject*,FXSelector,void*);

public:

  // Messages for our class
  enum{
    ID_CANVAS=FXMainWindow::ID_LAST,
    ID_WELL,
    ID_RESTORE,
    ID_LAST
    };

public:

  // ImageWindow constructor
  ImageWindow(FXApp* a);

  // Initialize
  virtual void create();

  // ImageWindow destructor
  virtual ~ImageWindow();
  };



// Message Map for the Scribble App class
FXDEFMAP(ImageWindow) ImageWindowMap[]={

  //____Message_Type______________ID_______________Message_Handler___
  FXMAPFUNC(SEL_PAINT,   ImageWindow::ID_CANVAS,  ImageWindow::onCanvasRepaint),
  FXMAPFUNC(SEL_COMMAND, ImageWindow::ID_WELL,    ImageWindow::onCmdWell),
  FXMAPFUNC(SEL_COMMAND, ImageWindow::ID_RESTORE, ImageWindow::onCmdRestore),
  };



// Macro for the ScribbleApp class hierarchy implementation
FXIMPLEMENT(ImageWindow,FXMainWindow,ImageWindowMap,ARRAYNUMBER(ImageWindowMap))



// Construct ImageWindow
ImageWindow::ImageWindow(FXApp* a):FXMainWindow(a,"Image Application",NULL,NULL,DECOR_ALL,0,0,800,600){
  FXint x,y;
  FXVerticalFrame *canvasFrame;
  FXVerticalFrame *buttonFrame;
  FXHorizontalFrame *contents;

  FXColorDialog *colordlg=new FXColorDialog(this,"Color Dialog");

  FXHorizontalFrame *hf=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);

  //new FXGradientBar(this,NULL,0,GRADIENTBAR_VERTICAL|GRADIENTBAR_CONTROLS_LEFT|FRAME_SUNKEN|LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y,0,0,0,0, 15,15,15,15);
  new FXGradientBar(this,NULL,0,GRADIENTBAR_VERTICAL|GRADIENTBAR_CONTROLS_LEFT|GRADIENTBAR_CONTROLS_RIGHT|FRAME_SUNKEN|LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y,0,0,0,0, 15,15,15,15);
  //new FXGradientBar(this,NULL,0,GRADIENTBAR_VERTICAL|GRADIENTBAR_CONTROLS_RIGHT|FRAME_SUNKEN|LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y,0,0,0,0, 15,15,15,15);
  //new FXGradientBar(this,NULL,0,GRADIENTBAR_VERTICAL|FRAME_SUNKEN|LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y,0,0,0,0, 15,15,15,15);

  //new FXGradientBar(this,NULL,0,GRADIENTBAR_HORIZONTAL|GRADIENTBAR_CONTROLS_BOTTOM|FRAME_SUNKEN|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0, 15,15,15,15);
  FXGradientBar *gb=new FXGradientBar(this,NULL,0,GRADIENTBAR_HORIZONTAL|GRADIENTBAR_CONTROLS_TOP|GRADIENTBAR_CONTROLS_BOTTOM|FRAME_SUNKEN|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0, 15,15,15,15);
  //new FXGradientBar(this,NULL,0,GRADIENTBAR_HORIZONTAL|GRADIENTBAR_CONTROLS_TOP|GRADIENTBAR_CONTROLS_BOTTOM|FRAME_SUNKEN|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0, 15,15,15,15);
  //new FXGradientBar(this,NULL,0,GRADIENTBAR_HORIZONTAL|FRAME_SUNKEN|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0, 15,15,15,15);
  

  new FXRadioButton(hf,"Linear blend",gb,FXGradientBar::ID_BLEND_LINEAR,LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXRadioButton(hf,"Power law blend",gb,FXGradientBar::ID_BLEND_POWER,LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXRadioButton(hf,"Sine blend",gb,FXGradientBar::ID_BLEND_SINE,LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXRadioButton(hf,"Increasing blend",gb,FXGradientBar::ID_BLEND_INCREASING,LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXRadioButton(hf,"Decreasing blend",gb,FXGradientBar::ID_BLEND_DECREASING,LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXColorWell(hf,FXRGB(0,0,0),gb,FXGradientBar::ID_LOWER_COLOR,LAYOUT_CENTER_Y);
  new FXColorWell(hf,FXRGB(0,0,0),gb,FXGradientBar::ID_UPPER_COLOR,LAYOUT_CENTER_Y);
  new FXButton(hf,"Recenter",NULL,gb,FXGradientBar::ID_RECENTER,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXButton(hf,"Split",NULL,gb,FXGradientBar::ID_SPLIT,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXButton(hf,"Merge",NULL,gb,FXGradientBar::ID_MERGE,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);
  new FXButton(hf,"Uniform",NULL,gb,FXGradientBar::ID_UNIFORM,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|ICON_BEFORE_TEXT);

  contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // LEFT pane to contain the canvas
  canvasFrame=new FXVerticalFrame(contents,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,10,10);

    // Label above the canvas
    new FXLabel(canvasFrame,"Canvas Frame",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);

    new FXRuler(canvasFrame,NULL,0,RULER_MARKERS|RULER_NUMBERS|RULER_ARROW|RULER_TICKS_CENTER|LAYOUT_FILL_X);
    
    // Horizontal divider line
    new FXHorizontalSeparator(canvasFrame,SEPARATOR_GROOVE|LAYOUT_FILL_X);

    // Drawing canvas
    canvas=new FXCanvas(canvasFrame,this,ID_CANVAS,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT);


  // RIGHT pane for the buttons
  buttonFrame=new FXVerticalFrame(contents,FRAME_SUNKEN|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,10,10);

    // Label above the buttons
    new FXLabel(buttonFrame,"Button Frame",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);

    // Horizontal divider line
    new FXHorizontalSeparator(buttonFrame,SEPARATOR_RIDGE|LAYOUT_FILL_X);

    new FXLabel(buttonFrame,"&Background\nColor well",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);
    backwell=new FXColorWell(buttonFrame,FXRGB(255,255,255),this,ID_WELL,LAYOUT_CENTER_X|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,100,30);

    new FXLabel(buttonFrame,"B&order\nColor well",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);
    borderwell=new FXColorWell(buttonFrame,FXRGB(0,0,0),this,ID_WELL,LAYOUT_CENTER_X|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,100,30);

    new FXLabel(buttonFrame,"&Text\nColor well",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);
    textwell=new FXColorWell(buttonFrame,FXRGB(0,0,0),this,ID_WELL,LAYOUT_CENTER_X|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,100,30);

    // Button to draw
    new FXButton(buttonFrame,"&Colors...\tPop the color dialog",NULL,colordlg,FXWindow::ID_SHOW,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

    // Button to draw
    new FXButton(buttonFrame,"Save Image...\tRead back image and save to file",NULL,this,ID_RESTORE,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

    // Exit button
    new FXButton(buttonFrame,"E&xit\tQuit ImageApp",NULL,getApp(),FXApp::ID_QUIT,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

  // Create images with dithering
  grey=new FXImage(getApp(),grey_ramp,IMAGE_DITHER|IMAGE_SHMI|IMAGE_SHMP,512,50);
  red=new FXImage(getApp(),red_ramp,IMAGE_DITHER|IMAGE_SHMI|IMAGE_SHMP,512,50);
  green=new FXImage(getApp(),green_ramp,IMAGE_DITHER|IMAGE_SHMI|IMAGE_SHMP,512,50);
  blue=new FXImage(getApp(),blue_ramp,IMAGE_DITHER|IMAGE_SHMI|IMAGE_SHMP,512,50);

  // Create image with nearest color instead of dithering
  grey_nodither=new FXImage(getApp(),grey_ramp,IMAGE_NEAREST|IMAGE_SHMI|IMAGE_SHMP,512,50);
  red_nodither=new FXImage(getApp(),red_ramp,IMAGE_NEAREST|IMAGE_SHMI|IMAGE_SHMP,512,50);
  green_nodither=new FXImage(getApp(),green_ramp,IMAGE_NEAREST|IMAGE_SHMI|IMAGE_SHMP,512,50);
  blue_nodither=new FXImage(getApp(),blue_ramp,IMAGE_NEAREST|IMAGE_SHMI|IMAGE_SHMP,512,50);

  // Result image
//  picture=new FXBMPImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXXPMImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXGIFImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXPNGImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXRGBImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXPCXImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXTGAImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXICOImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,64,64);
//  picture=new FXJPGImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
//  picture=new FXTIFImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);
  picture=new FXGIFImage(getApp(),NULL,IMAGE_SHMI|IMAGE_SHMP,850,600);

  // Fill the ramps
  for(x=0; x<512; x++){
    for(y=0; y<50; y++){
      grey_ramp[y*512+x]=FXRGB(x/2,x/2,x/2);
      }
    for(y=0; y<50; y++){
      red_ramp[y*512+x]=FXRGB(x/2,0,0);
      }
    for(y=0; y<50; y++){
      green_ramp[y*512+x]=FXRGB(0,x/2,0);
      }
    for(y=0; y<50; y++){
      blue_ramp[y*512+x]=FXRGB(0,0,x/2);
      }
    }

  // Make font
  font=new FXFont(getApp(),"times",36,FXFont::Bold);

  // Make a tip
  new FXToolTip(getApp());
  }


// Destroy ImageWindow
ImageWindow::~ImageWindow(){
  delete grey;
  delete red;
  delete green;
  delete blue;
  delete grey_nodither;
  delete red_nodither;
  delete green_nodither;
  delete blue_nodither;
  delete picture;
  delete font;
  }


// Create and initialize
void ImageWindow::create(){

  // Create the windows
  FXMainWindow::create();

  // Create images
  grey->create();
  red->create();
  green->create();
  blue->create();
  grey_nodither->create();
  red_nodither->create();
  green_nodither->create();
  blue_nodither->create();

  picture->create();

  // Font too
  font->create();

  // Make it appear
  show(PLACEMENT_SCREEN);

  // First time repaint
  canvas->update();

  }


// Handle the clear message
long ImageWindow::onCanvasRepaint(FXObject*,FXSelector,void* ptr){
  FXEvent *event=(FXEvent*)ptr;
  FXuint pat;

  // We caused a redraw, so redo it all
  if(event->synthetic){
    FXDCWindow dc(picture);

    // Erase the canvas, color comes from well
    dc.setForeground(backwell->getRGBA());

    dc.fillRectangle(0,0,picture->getWidth(),picture->getHeight());

    // Draw images
    dc.drawImage(grey,10,10);
    dc.drawImage(grey_nodither,10,60);
    dc.drawImage(red,10,130);
    dc.drawImage(red_nodither,10,180);
    dc.drawImage(green,10,250);
    dc.drawImage(green_nodither,10,300);
    dc.drawImage(blue,10,370);
    dc.drawImage(blue_nodither,10,420);

    // Draw patterns
    dc.setFillStyle(FILL_OPAQUESTIPPLED);
    dc.setForeground(FXRGB(0,0,0));
    dc.setBackground(FXRGB(255,255,255));
    for(pat=STIPPLE_0; pat<=STIPPLE_16; pat+=1){
      dc.setStipple((FXStipplePattern)pat);
      dc.fillRectangle(10+(512*pat)/17,490,31,50);
      }
    dc.setFillStyle(FILL_SOLID);

    // Draw borders
    dc.setForeground(borderwell->getRGBA());
    dc.drawRectangle(10,10,512,50);
    dc.drawRectangle(10,60,512,50);

    dc.drawRectangle(10,130,512,50);
    dc.drawRectangle(10,180,512,50);

    dc.drawRectangle(10,250,512,50);
    dc.drawRectangle(10,300,512,50);

    dc.drawRectangle(10,370,512,50);
    dc.drawRectangle(10,420,512,50);

    dc.drawRectangle(10,490,512,50);

    // Draw text
    dc.setFont(font);
    dc.setForeground(textwell->getRGBA());
    dc.drawText(540,60,"Grey",4);
    dc.drawText(540,180,"Red",3);
    dc.drawText(540,300,"Green",5);
    dc.drawText(540,420,"Blue",4);
    dc.drawText(540,540,"Patterns",8);
    }

  // Now repaint the screen
  FXDCWindow sdc(canvas,event);

  // Clear whole thing
  sdc.setForeground(backwell->getRGBA());
  sdc.fillRectangle(0,0,canvas->getWidth(),canvas->getHeight());

  // Draw using drawImage, which is simple
  //sdc.drawImage(picture,0,0);

  // Or use drawArea, which is more flexible
  sdc.drawArea(picture,0,0,picture->getWidth(),picture->getHeight(),0,0);

  return 1;
  }


// Color well got changed
long ImageWindow::onCmdWell(FXObject*,FXSelector,void*){
  canvas->update();
  return 1;
  }


// Restore image from off-screen pixmap
long ImageWindow::onCmdRestore(FXObject*,FXSelector,void*){
  FXFileDialog savedialog(this,"Save BMP");
  savedialog.setDirectory(".");
  if(savedialog.execute()){
    FXFileStream outfile;
    if(outfile.open(savedialog.getFilename(),FXStreamSave)){
      picture->restore();
      picture->savePixels(outfile);
      outfile.close();
      }
    }
  return 1;
  }


// Here we begin
int main(int argc,char *argv[]){

  // Make application
  FXApp application("Image","FoxText");

  // Start app
  application.init(argc,argv);

  // Make window
  new ImageWindow(&application);

  // Create the application's windows
  application.create();

  // Run the application
  return application.run();
  }




