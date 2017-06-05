package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X create notify event. */
public final class CreateNotify extends Event {

  private int parentID;
  private int windowID;
  private int x;
  private int y;
  private int width;
  private int height;
  private int borderWidth;
  private boolean overrideRedirect;

  public CreateNotify (Display display, ResponseInputStream in) {
    super (display, in); 
    parentID = in.readInt32 ();
    windowID = in.readInt32 ();
    x = in.readInt16 ();
    y = in.readInt16 ();
    width = in.readInt16 ();
    height = in.readInt16 ();
    borderWidth = in.readInt16 ();
    overrideRedirect = in.readBool ();
    in.skip (9);
  }

}
