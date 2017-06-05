package gnu.x11.extension.glx;

import gnu.x11.Display;
import gnu.x11.ResponseInputStream;


/** GLX pbuffer clobber event. */
public class PbufferClobberEvent extends gnu.x11.event.Event {
  public static final int code = 0;


  private int event_type;
  private int drawable_type;
  private int drawable_id;
  private int buffer_mask;
  private int aux_buffer;
  private int x;
  private int y;
  private int width;
  private int height;
  private int count;

  public PbufferClobberEvent (Display display, ResponseInputStream i) { 
    super (display, i); 
    event_type = i.readInt16 ();
    drawable_id = i.readInt32 ();
    buffer_mask = i.readInt32 ();
    aux_buffer = i.readInt16 ();
    x = i.readInt16 ();
    y = i.readInt16 ();
    width = i.readInt16 ();
    height = i.readInt16 ();
    count = i.readInt16 ();
  }


  public int drawable_id () {
    return drawable_id;
  }

  public int buffer_mask () {
    return buffer_mask;
  }

  public int aux_buffer () {
    return aux_buffer;
  }

  public int x () {
    return x;
  }

  public int y () {
    return y;
  }

  public int width () {
    return width;
  }

  public int height () {
    return height;
  }

  public int count () {
    return count;
  }

  public final static int DAMAGED = 0x8017;
  public final static int SAVED = 0x8018;


  /** 
   * @return valid:
   * {@link #DAMAGED},
   * {@link #SAVED}
   */
  public int event_type () {
    return event_type;
  }


  public final static int WINDOW = 0x8019;
  public final static int PBUFFER = 0x801A;


  /** 
   * @return valid:
   * {@link #WINDOW},
   * {@link #PBUFFER}
   */
  public int drawable_type () {
    return event_type;
  }

}  
