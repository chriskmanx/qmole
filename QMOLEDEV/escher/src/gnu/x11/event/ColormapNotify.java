package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X colormap notify event. */
public final class ColormapNotify extends Event {

  private int windowID;
  private int colormapID;
  private boolean isNew;
  private int state;

  public ColormapNotify (Display display, ResponseInputStream in) {
    super(display, in);
    windowID = in.readInt32();
    colormapID = in.readInt32();
    isNew = in.readBool();
    state = in.readInt8();
    in.skip(18);
  }

}
