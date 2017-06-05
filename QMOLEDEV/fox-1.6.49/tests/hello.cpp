#include "fx.h"


/*

  This is a classical programming example.  Doing it graphically
  using FOX is, as you see, not a whole lot more involved than
  its command-line equivalent!

  Note the following things:

    - Each FOX application needs one (and only one) application
      object (FXApp).

    - Before doing anything, the application object needs to be
      initialized.  You need to pass argc and argv so that certain
      command line arguments may be filtered out by FOX (e.g. -display).

    - You need to create at least one toplevel window; in this case,
      that is FXMainWindow.

    - FOX widgets are nested simply by creating them in the right order.
      Here, we create FXButton as a child of "main."

    - A single call to FXApp::create() will create X windows for each
      widget.  Until calling create(), a widget exists only at the client,
      and has no associated X window yet.

    - Finally, FXApp::run() will start the main event loop.  This will
      only return when the application is done.

*/

#include "FXExpression.h"

// The entry point where the program starts running
int main(int argc,char **argv){

  // Each FOX GUI program needs one, and only one, application object.
  // The application objects coordinates some common stuff shared between
  // all the widgets; for example, it dispatches events, keeps track of
  // all the windows, and so on.
  // We pass the "name" of the application, and its "vendor", the name
  // and vendor are used to search the registry database (which stores
  // persistent information e.g. fonts and colors).
  FXApp application("Hello","FoxTest");

  // Here we initialize the application.  We pass the command line arguments
  // because FOX may sometimes need to filter out some of the arguments.
  // This opens up the display as well, and reads the registry database
  // so that persistent settings are now available.
  application.init(argc,argv);

  // This creates the main window. We pass in the title to be displayed
  // above the window, and possibly some icons for when its iconified.
  // The decorations determine stuff like the borders, close buttons,
  // drag handles, and so on the Window Manager is supposed to give this
  // window.
  FXMainWindow *main=new FXMainWindow(&application,"Hello",NULL,NULL,DECOR_ALL);

  // Here we create a button.  The button has a label on it, but no icon in
  // this case.  An '&' followed by a letter introduces a hot-key so you can
  // invoke this button from the keyboard.
  // The button sends an ID_QUIT message to the application object, which
  // in its default implementation causes the program to quit.
  new FXButton(main,"&Hello, World!",NULL,&application,FXApp::ID_QUIT);

  // This "realizes" the widget tree.  This is necessary because GUI's are
  // a client-server system, i.e. there are actually two programs involved,
  // a client (in this case, "hello world"), and a server (The X11 server or
  // Windows GDI).  We can build our C++ widgets but something extra is needed
  // to tell the server that we want windows on the screen.  Besides windows,
  // there are various other resources that need to be created, such as icons,
  // fonts, and so on.  This call recurses through the entire widget tree and
  // creates them all, insofar as it can know about them.
  application.create();

  // Pretty self-explanatory:- this shows the window, and places it in the
  // middle of the screen.
  main->show(PLACEMENT_SCREEN);

  // Now, we actually run the application.  This does not return until we
  // quit. The function run() is a simple loop which gets events from the
  // user, executes them, and then waits for the next event, and so on until
  // we hit the button.
  return application.run();
  }
