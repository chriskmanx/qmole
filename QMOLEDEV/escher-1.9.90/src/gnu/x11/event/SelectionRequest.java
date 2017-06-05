package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** X selection request event. */
public final class SelectionRequest extends Event {

  private int time;
  private int ownerWindowID;
  private int requestorWindowID;
  private int selectionAtomID;
  private int targetAtomID;
  private int propertyAtomID;

  public SelectionRequest (Display display, ResponseInputStream in) {
    super(display, in);
    time = in.readInt32();
    ownerWindowID = in.readInt32();
    requestorWindowID = in.readInt32();
    selectionAtomID = in.readInt32();
    targetAtomID = in.readInt32();
    propertyAtomID = in.readInt32();
    in.skip(4);
  }
}
