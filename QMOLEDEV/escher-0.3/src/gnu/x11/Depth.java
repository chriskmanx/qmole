package gnu.x11;


/** X depth. */
public class Depth {

  public int depth;
  public Visual[] visual_types;

  public Depth (ResponseInputStream in) {
    depth = in.read_int8 ();
    in.skip(1);
    int visual_count = in.read_int16 ();
    in.skip (4);
    visual_types = new Visual [visual_count];
    for (int i = 0; i < visual_count; i++) {
      visual_types [i] = new Visual (in);
    }
  }

  public String toString () {
    return "#Depth"
      + "\n  depth: " + depth
      + "\n  visual-count: " + visual_types.length;
  }
}
