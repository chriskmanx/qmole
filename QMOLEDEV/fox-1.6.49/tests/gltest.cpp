/********************************************************************************
*                                                                               *
*                         OpenGL Application coding sample                      *
*                                                                               *
********************************************************************************/
#include "fx.h"
#include "fx3d.h"


#ifdef HAVE_GL_H


// Timer setting (in milliseconds)
const FXuint TIMER_INTERVAL = 100;


/*******************************************************************************/


// Settings dialog thanks to Sander Jansen <sander@knology.net>
class GLSettingsDialog : public FXDialogBox {
  FXDECLARE(GLSettingsDialog)
private:
  GLSettingsDialog(){}
public:
  GLSettingsDialog(FXWindow* owner,FXGLVisual *vis);
  };



/*******************************************************************************/


// Implementation
FXIMPLEMENT(GLSettingsDialog,FXDialogBox,NULL,0)


// Construct a dialog box
GLSettingsDialog::GLSettingsDialog(FXWindow* owner,FXGLVisual *vis):FXDialogBox(owner,"OpenGL Info",DECOR_STRETCHABLE|DECOR_TITLE|DECOR_BORDER,0,0,600){
  FXTabBook *tabbook;
  FXVerticalFrame *frame1;
  FXGroupBox *driverbox, *limitsbox, *glextbox, *displaybox;
  FXMatrix *v_matrix, *v_matrix2;
  FXHorizontalFrame *options;
  GLint	intval;
  GLint	intvals[2];
  char	*token, *text, *tmp;

  FXVerticalFrame *m_master=new FXVerticalFrame(this, LAYOUT_FILL_X|LAYOUT_FILL_Y, 0,0,0,0, 0,0,0,0);

  tabbook=new FXTabBook(m_master,NULL,0,PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);

  //
  // Tab 1: general info
  //
  new FXTabItem(tabbook, "General", NULL);
  frame1 = new FXVerticalFrame(tabbook, LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_THICK|FRAME_RAISED,0,0,0,0);

  // Opengl version information
  driverbox=new FXGroupBox(frame1, "Driver", GROUPBOX_NORMAL|FRAME_RIDGE|LAYOUT_FILL_X);
  v_matrix=new FXMatrix(driverbox, 2, MATRIX_BY_COLUMNS);

  new FXLabel(v_matrix,"Vendor: ",NULL,LABEL_NORMAL);
  new FXLabel(v_matrix,FXStringFormat("%s",glGetString(GL_VENDOR)),NULL,LABEL_NORMAL);

  new FXLabel(v_matrix,"Renderer: ",NULL,LABEL_NORMAL);
  new FXLabel(v_matrix,FXStringFormat("%s",glGetString(GL_RENDERER)),NULL,LABEL_NORMAL);

  new FXLabel(v_matrix,"GL Version: ",NULL,LABEL_NORMAL);
  new FXLabel(v_matrix,FXStringFormat("%s",glGetString(GL_VERSION)),NULL,LABEL_NORMAL);

  new FXLabel(v_matrix,"GLU Version: ",NULL,LABEL_NORMAL);
  new FXLabel(v_matrix,FXStringFormat("%s",gluGetString(GLU_VERSION)),NULL,LABEL_NORMAL);

  // Opengl implementation-dependent stuff
  limitsbox=new FXGroupBox(frame1, "Implementation limits", GROUPBOX_NORMAL|FRAME_RIDGE|LAYOUT_FILL_X);
  v_matrix2=new FXMatrix(limitsbox, 2, MATRIX_BY_COLUMNS);

  glGetIntegerv(GL_MAX_VIEWPORT_DIMS, intvals);
  new FXLabel(v_matrix2, "Maximum viewport size: ");
  new FXLabel(v_matrix2, FXStringFormat("%d x %d", intvals[0], intvals[1]));

  glGetIntegerv(GL_MAX_TEXTURE_SIZE, &intval);
  new FXLabel(v_matrix2, "Maximum texture size (w or h): ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  glGetIntegerv(GL_MAX_LIGHTS, &intval);
  new FXLabel(v_matrix2, "Maximum number of lights: ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  glGetIntegerv(GL_MAX_CLIP_PLANES, &intval);
  new FXLabel(v_matrix2, "Maximum number of clipping planes: ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  glGetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH, &intval);
  new FXLabel(v_matrix2, "Maximum modelview-matrix stack depth: ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  glGetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH, &intval);
  new FXLabel(v_matrix2, "Maximum projection-matrix stack depth: ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  glGetIntegerv(GL_MAX_ATTRIB_STACK_DEPTH, &intval);
  new FXLabel(v_matrix2, "Maximum attribute stack depth: ");
  new FXLabel(v_matrix2, FXStringFormat("%d", intval));

  options=new FXHorizontalFrame(frame1,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,0,0,0,0);

  // Display mode info
  displaybox=new FXGroupBox(options,"Display Mode",GROUPBOX_NORMAL|FRAME_RIDGE|LAYOUT_FILL_Y);
  FXMatrix *mat=new FXMatrix(displaybox, 2, MATRIX_BY_COLUMNS);

  new FXLabel(mat,"Hardware-accelerated",NULL,LABEL_NORMAL);
  if(vis->isAccelerated())
    new FXLabel(mat,"yes",NULL,LABEL_NORMAL);
  else
    new FXLabel(mat,"no",NULL,LABEL_NORMAL);

  new FXLabel(mat,"Double Buffering",NULL,LABEL_NORMAL);
  if(vis->isDoubleBuffer())
    new FXLabel(mat,"yes",NULL,LABEL_NORMAL);
  else
    new FXLabel(mat,"no",NULL,LABEL_NORMAL);

  new FXLabel(mat,"Stereo View",NULL,LABEL_NORMAL);
  if(vis->isStereo())
    new FXLabel(mat,"yes",NULL,LABEL_NORMAL);
  else
    new	FXLabel(mat,"no",NULL,LABEL_NORMAL);

  new FXLabel(mat,"Buffer-swap by copy",NULL,LABEL_NORMAL);
  if(vis->isBufferSwapCopy())
    new FXLabel(mat,"yes",NULL,LABEL_NORMAL);
  else
    new FXLabel(mat,"no",NULL,LABEL_NORMAL);

  new FXLabel(mat,"Color Depth",NULL,LABEL_NORMAL);
  new FXLabel(mat,FXStringFormat("%d",
    vis->getActualRedSize()+vis->getActualGreenSize()+vis->getActualBlueSize()+vis->getActualAlphaSize()));

  new FXLabel(mat,"Depth Buffer Size",NULL,LABEL_NORMAL);
  new FXLabel(mat,FXStringFormat("%d",vis->getActualDepthSize()),NULL,LABEL_NORMAL);

  new FXLabel(mat,"Stencil Buffer Size",NULL,LABEL_NORMAL);
  new FXLabel(mat,FXStringFormat("%d",vis->getActualStencilSize()),NULL,LABEL_NORMAL);

  new FXLabel(mat,"RGBA",NULL,LABEL_NORMAL);
  new FXLabel(mat,FXStringFormat("%d-%d-%d-%d",vis->getActualRedSize(),vis->getActualGreenSize(),vis->getActualBlueSize(),vis->getActualAlphaSize()),NULL,LABEL_NORMAL);

  new FXLabel(mat,"Accum RGBA",NULL,LABEL_NORMAL);
  new FXLabel(mat,FXStringFormat("%d-%d-%d-%d",vis->getActualAccumRedSize(),vis->getActualAccumGreenSize(),vis->getActualAccumBlueSize(),vis->getActualAccumAlphaSize()),NULL,LABEL_NORMAL);

  // List of extensions
  glextbox= new FXGroupBox(options,"Available Extensions",GROUPBOX_NORMAL|FRAME_RIDGE|LAYOUT_FILL_Y|LAYOUT_FILL_X);
  FXVerticalFrame *listframe=new FXVerticalFrame(glextbox,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN|FRAME_THICK,0,0,0,0, 0,0,0,0);

  FXList *extensionList=new FXList(listframe,NULL,0,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y);

  // Get OpenGL extensions
  tmp=(char*)glGetString(GL_EXTENSIONS);
  if(tmp){
    text=strdup(tmp);
    token=strtok(text," ");
    while(token){
      extensionList->appendItem(FXStringFormat("[GL] %s",token));
      token=strtok(NULL," ");
      }
    free(text);
    }

  // Get GLU extensions
#ifdef GLU_VERSION_1_1
  tmp=(char*)gluGetString(GLU_EXTENSIONS);
  if(tmp){
    text=strdup(tmp);
    token=strtok(text," ");
    while(token!=NULL){
      extensionList->appendItem(FXStringFormat("[GLU] %s",token));
      token=strtok(NULL," ");
      }
    free(text);
    }
#endif

  // Button frame
  FXHorizontalFrame *control=new FXHorizontalFrame(m_master, LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X);

  new FXButton(control,"OK",NULL,this,ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X|LAYOUT_CENTER_Y,0,0,0,0, 20,20,3,3);
  }



/*******************************************************************************/


// Event Handler Object
class GLTestWindow : public FXMainWindow {
  FXDECLARE(GLTestWindow)

private:

  FXGLCanvas      *glcanvas;                  // GL Canvas to draw into
  int              spinning;                  // Is box spinning
  double           angle;                     // Rotation angle of box
  FXGLVisual      *glvisual;                  // OpenGL visual

protected:
  GLTestWindow(){}

public:

  // We define additional ID's, starting from the last one used by the base class+1.
  // This way, we know the ID's are all unique for this particular target.
  enum{
    ID_CANVAS=FXMainWindow::ID_LAST,
    ID_SPIN,
    ID_SPINFAST,
    ID_STOP,
    ID_TIMEOUT,
    ID_CHORE,
    ID_OPENGL
    };

  // Message handlers
  long onMouseDown(FXObject*,FXSelector,void*);
  long onMouseUp(FXObject*,FXSelector,void*);
  long onMouseMove(FXObject*,FXSelector,void*);
  long onExpose(FXObject*,FXSelector,void*);
  long onConfigure(FXObject*,FXSelector,void*);
  long onCmdSpin(FXObject*,FXSelector,void*);
  long onUpdSpin(FXObject*,FXSelector,void*);
  long onCmdStop(FXObject*,FXSelector,void*);
  long onUpdStop(FXObject*,FXSelector,void*);
  long onTimeout(FXObject*,FXSelector,void*);
  long onChore(FXObject*,FXSelector,void*);
  long onCmdSpinFast(FXObject*,FXSelector,void*);
  long onUpdSpinFast(FXObject*,FXSelector,void*);
  long onCmdOpenGL(FXObject*,FXSelector,void*);

public:

  // GLTestWindow constructor
  GLTestWindow(FXApp* a);

  // Initialize
  void create();

  // Draw scene
  void drawScene();

  // GLTestWindow destructor
  virtual ~GLTestWindow();
  };



// Message Map
FXDEFMAP(GLTestWindow) GLTestWindowMap[]={

  //________Message_Type_________ID_____________________Message_Handler_______
  FXMAPFUNC(SEL_PAINT,     GLTestWindow::ID_CANVAS,   GLTestWindow::onExpose),
  FXMAPFUNC(SEL_CONFIGURE, GLTestWindow::ID_CANVAS,   GLTestWindow::onConfigure),
  FXMAPFUNC(SEL_COMMAND,   GLTestWindow::ID_SPIN,     GLTestWindow::onCmdSpin),
  FXMAPFUNC(SEL_UPDATE,    GLTestWindow::ID_SPIN,     GLTestWindow::onUpdSpin),
  FXMAPFUNC(SEL_COMMAND,   GLTestWindow::ID_SPINFAST, GLTestWindow::onCmdSpinFast),
  FXMAPFUNC(SEL_UPDATE,    GLTestWindow::ID_SPINFAST, GLTestWindow::onUpdSpinFast),
  FXMAPFUNC(SEL_COMMAND,   GLTestWindow::ID_STOP,     GLTestWindow::onCmdStop),
  FXMAPFUNC(SEL_UPDATE,    GLTestWindow::ID_STOP,     GLTestWindow::onUpdStop),
  FXMAPFUNC(SEL_TIMEOUT,   GLTestWindow::ID_TIMEOUT,  GLTestWindow::onTimeout),
  FXMAPFUNC(SEL_CHORE,     GLTestWindow::ID_CHORE,    GLTestWindow::onChore),
  FXMAPFUNC(SEL_COMMAND,   GLTestWindow::ID_OPENGL,   GLTestWindow::onCmdOpenGL),
  };



// Implementation
FXIMPLEMENT(GLTestWindow,FXMainWindow,GLTestWindowMap,ARRAYNUMBER(GLTestWindowMap))



// Construct a GLTestApp
GLTestWindow::GLTestWindow(FXApp* a):FXMainWindow(a,"OpenGL Test Application",NULL,NULL,DECOR_ALL,0,0,800,600){
  FXVerticalFrame *glcanvasFrame;
  FXVerticalFrame *buttonFrame;
  FXComposite *glpanel;
  FXHorizontalFrame *frame;

  frame=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);

  // LEFT pane to contain the glcanvas
  glcanvasFrame=new FXVerticalFrame(frame,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,10,10);

  // Label above the glcanvas
  new FXLabel(glcanvasFrame,"OpenGL Canvas Frame",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);

  // Horizontal divider line
  new FXHorizontalSeparator(glcanvasFrame,SEPARATOR_GROOVE|LAYOUT_FILL_X);

  // Drawing glcanvas
  glpanel=new FXVerticalFrame(glcanvasFrame,FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0, 0,0,0,0);

  // A Visual to drag OpenGL
  glvisual=new FXGLVisual(getApp(),VISUAL_DOUBLEBUFFER|VISUAL_STEREO);

  // Drawing glcanvas
  glcanvas=new FXGLCanvas(glpanel,glvisual,this,ID_CANVAS,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT);

  // RIGHT pane for the buttons
  buttonFrame=new FXVerticalFrame(frame,LAYOUT_FILL_Y|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,10,10);

  // Label above the buttons
  new FXLabel(buttonFrame,"Button Frame",NULL,JUSTIFY_CENTER_X|LAYOUT_FILL_X);

  // Horizontal divider line
  new FXHorizontalSeparator(buttonFrame,SEPARATOR_RIDGE|LAYOUT_FILL_X);

  new FXButton(buttonFrame,"&OpenGL Info\tDisplay OpenGL Capabilities",NULL,this,ID_OPENGL,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);
  // Button to print
  new FXButton(buttonFrame,"Spin &Timer\tSpin using interval timers\nNote the app blocks until the interal has elapsed...",NULL,this,ID_SPIN,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);
  new FXButton(buttonFrame,"Spin &Chore\tSpin as fast as possible using chores\nNote even though the app is very responsive, it never blocks;\nthere is always something to do...",NULL,this,ID_SPINFAST,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

  // Button to print
  new FXButton(buttonFrame,"&Stop Spin\tStop this mad spinning, I'm getting dizzy",NULL,this,ID_STOP,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

  // Exit button
  new FXButton(buttonFrame,"&Exit\tExit the application",NULL,getApp(),FXApp::ID_QUIT,FRAME_THICK|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_TOP|LAYOUT_LEFT,0,0,0,0,10,10,5,5);

  // Make a tooltip
  new FXToolTip(getApp());

  // Initialize private variables
  spinning=0;
  angle=0.0;
  }


// Destructor
GLTestWindow::~GLTestWindow(){
  getApp()->removeTimeout(this,ID_TIMEOUT);
  getApp()->removeChore(this,ID_CHORE);
  delete glvisual;
  }



// Create and initialize
void GLTestWindow::create(){
  FXMainWindow::create();
  show(PLACEMENT_SCREEN);
  }



// Widget has been resized
long GLTestWindow::onConfigure(FXObject*,FXSelector,void*){
  if(glcanvas->makeCurrent()){
    glViewport(0,0,glcanvas->getWidth(),glcanvas->getHeight());
    glcanvas->makeNonCurrent();
    }
  return 1;
  }



// Widget needs repainting
long GLTestWindow::onExpose(FXObject*,FXSelector,void*){
  drawScene();
  return 1;
  }



//  Rotate the boxes when a timer message is received
long GLTestWindow::onTimeout(FXObject*,FXSelector,void*){
  angle += 2.;
  if(angle > 360.) angle -= 360.;
  drawScene();
  getApp()->addTimeout(this,ID_TIMEOUT,TIMER_INTERVAL);
  return 1;
  }


// Rotate the boxes when a chore message is received
long GLTestWindow::onChore(FXObject*,FXSelector,void*){
  angle += 2.;
  if(angle > 360.) angle -= 360.;
  drawScene();
  getApp()->addChore(this,ID_CHORE);
  return 1;
  }


// Start the boxes spinning
long GLTestWindow::onCmdSpin(FXObject*,FXSelector,void*){
  spinning=1;
  getApp()->addTimeout(this,ID_TIMEOUT,TIMER_INTERVAL);
  return 1;
  }



// Enable or disable the spin button
long GLTestWindow::onUpdSpin(FXObject* sender,FXSelector,void*){
  FXButton* button=(FXButton*)sender;
  spinning ? button->disable() : button->enable();
  return 1;
  }


// Start the boxes spinning
long GLTestWindow::onCmdSpinFast(FXObject*,FXSelector,void*){
  spinning=1;
  getApp()->addChore(this,ID_CHORE);
  return 1;
  }


// Enable or disable the spin button
long GLTestWindow::onUpdSpinFast(FXObject* sender,FXSelector,void*){
  FXButton* button=(FXButton*)sender;
  spinning ? button->disable() : button->enable();
  return 1;
  }


// If boxes are spinning, stop them
long GLTestWindow::onCmdStop(FXObject*,FXSelector,void*){
  getApp()->removeTimeout(this,ID_TIMEOUT);
  getApp()->removeChore(this,ID_CHORE);
  spinning=0;
  return 1;
  }


// Enable or disable the stop button
long GLTestWindow::onUpdStop(FXObject* sender,FXSelector,void*){
  FXButton* button=(FXButton*)sender;
  spinning ? button->enable() : button->disable();
  return 1;
  }



// Draws a simple box using the given corners
void drawBox(GLfloat xmin, GLfloat ymin, GLfloat zmin, GLfloat xmax, GLfloat ymax, GLfloat zmax) {
  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.,0.,-1.);
    glVertex3f(xmin, ymin, zmin);
    glVertex3f(xmin, ymax, zmin);
    glVertex3f(xmax, ymin, zmin);
    glVertex3f(xmax, ymax, zmin);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(1.,0.,0.);
    glVertex3f(xmax, ymin, zmin);
    glVertex3f(xmax, ymax, zmin);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.,0.,1.);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymax, zmax);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(-1.,0.,0.);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymax, zmax);
    glVertex3f(xmin, ymin, zmin);
    glVertex3f(xmin, ymax, zmin);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.,1.,0.);
    glVertex3f(xmin, ymax, zmin);
    glVertex3f(xmin, ymax, zmax);
    glVertex3f(xmax, ymax, zmin);
    glVertex3f(xmax, ymax, zmax);
  glEnd();

  glBegin(GL_TRIANGLE_STRIP);
    glNormal3f(0.,-1.,0.);
    glVertex3f(xmax, ymin, zmax);
    glVertex3f(xmax, ymin, zmin);
    glVertex3f(xmin, ymin, zmax);
    glVertex3f(xmin, ymin, zmin);
  glEnd();
  }


// Draw the GL scene
void GLTestWindow::drawScene(){
  const GLfloat lightPosition[]={15.,10.,5.,1.};
  const GLfloat lightAmbient[]={.1f,.1f,.1f,1.};
  const GLfloat lightDiffuse[]={.9f,.9f,.9f,1.};
  const GLfloat redMaterial[]={1.,0.,0.,1.};
  const GLfloat blueMaterial[]={0.,0.,1.,1.};

  GLdouble width = glcanvas->getWidth();
  GLdouble height = glcanvas->getHeight();
  GLdouble aspect = height>0 ? width/height : 1.0;

  // Make context current
  glcanvas->makeCurrent();

  glViewport(0,0,glcanvas->getWidth(),glcanvas->getHeight());

  glClearColor(1.0,1.0,1.0,1.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glEnable(GL_DEPTH_TEST);

  glDisable(GL_DITHER);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(30.,aspect,1.,100.);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(5.,10.,15.,0.,0.,0.,0.,1.,0.);

  glShadeModel(GL_SMOOTH);
  glLightfv(GL_LIGHT0, GL_POSITION, lightPosition);
  glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmbient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiffuse);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  glMaterialfv(GL_FRONT, GL_AMBIENT, blueMaterial);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, blueMaterial);

  glPushMatrix();
  glRotated(angle, 0., 1., 0.);
  drawBox(-1, -1, -1, 1, 1, 1);

  glMaterialfv(GL_FRONT, GL_AMBIENT, redMaterial);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, redMaterial);

  glPushMatrix();
  glTranslated(0.,1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPushMatrix();
  glTranslated(0.,-1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPushMatrix();
  glRotated(90., 1., 0., 0.);
  glTranslated(0.,1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPushMatrix();
  glRotated(90., -1., 0., 0.);
  glTranslated(0.,1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPushMatrix();
  glRotated(90., 0., 0., 1.);
  glTranslated(0.,1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPushMatrix();
  glRotated(90., 0., 0., -1.);
  glTranslated(0.,1.75,0.);
  glRotated(angle, 0., 1., 0.);
  drawBox(-.5,-.5,-.5,.5,.5,.5);
  glPopMatrix();

  glPopMatrix();

  // Swap if it is double-buffered
  if(glvisual->isDoubleBuffer()){
    glcanvas->swapBuffers();
    }

  // Make context non-current
  glcanvas->makeNonCurrent();
  }


// Pop a dialog showing OpenGL properties
long GLTestWindow::onCmdOpenGL(FXObject*,FXSelector,void*){
  glcanvas->makeCurrent();
  GLSettingsDialog sd((FXWindow*)this,glvisual);
  glcanvas->makeNonCurrent();
  sd.execute();
  return 1;
  }


// Here we begin
int main(int argc,char *argv[]){

  // Make application
  FXApp application("GLTest","FoxTest");

  // Open the display
  application.init(argc,argv);

  // Make window
  new GLTestWindow(&application);

  // Create the application's windows
  application.create();

  // Run the application
  return application.run();
  }


#else


// Here we begin
int main(int argc,char *argv[]){
  fxmessage("The FOX Library was compiled without OpenGL\n");
  return 0;
  }

#endif
