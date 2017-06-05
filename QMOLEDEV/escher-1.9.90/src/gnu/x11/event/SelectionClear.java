package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X selection clear event. */
public final class SelectionClear extends Event {

  private int time;
  private int ownerID;
  private int selectionAtomID;

  public SelectionClear (Display display, ResponseInputStream in) {
    super(display, in);
    time = in.readInt32();
    ownerID = in.readInt32();
    selectionAtomID = in.readInt32();
    in.skip(16);
  }
}
