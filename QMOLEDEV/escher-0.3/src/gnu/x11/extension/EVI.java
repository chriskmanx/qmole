package gnu.x11.extension;

import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Visual;


/**
 * Extended Visual Information Extension. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/evi.ps.gz">here</a>.
 */
public class EVI extends Extension {
  public static final String [] MINOR_OPCODE_STRINGS = {
    "GetVersion",               // 0
    "GetVisualInfo"             // 1
  };


  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 0;


  public int server_major_version, server_minor_version;


  // evi opcode 0 - get version
  /**
   * @see <a href="XeviQueryVersion.html">XeviQueryVersion</a>
   */
  public EVI (Display display) throws NotFoundException { 
    super (display, "Extended-Visual-Information", MINOR_OPCODE_STRINGS); 

    // check version before any other operations
    /* Note that the specification says the request includes a major and a
     * minor version, but most implementation (xfree86 3.3/4.0) does not. 
     * Which one is bugged?
     */
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        server_major_version = i.read_int16 ();
        server_minor_version = i.read_int16 ();
        i.skip (20);
      }
    }
  }


  /** EVI visual info. */
  public static class VisualInfo {

    public int core_visual_id;
    public int screen;
    public int level;
    public int transparency_type;
    public int transparency_value;
    public int min_hw_colormaps;
    public int max_hw_colormaps;
    public int num_colormap_conflicts;

    VisualInfo (ResponseInputStream i) {
      core_visual_id = i.read_int32 ();
      screen = i.read_int8 ();
      level = i.read_int8 ();
      transparency_type = i.read_int8 ();
      i.skip (1);
      transparency_value = i.read_int32 ();
      min_hw_colormaps = i.read_int8 ();
      max_hw_colormaps = i.read_int8 ();
      num_colormap_conflicts = i.read_int8 ();
    }
    
  }


  /** Reply of {@link #visual_info(Visual[])} */
  public static class VisualInfoReply {

    public int n_conflicts;
    public VisualInfo [] items;

    VisualInfoReply (ResponseInputStream i) {
      int n_info = i.read_int32 ();
      n_conflicts = i.read_int32 ();
      i.skip (16);
      items = new VisualInfo [n_info];
      for (int j = 0; j < n_info; j++) {
        items [j] = new VisualInfo (i);
      }
    }
  }


  // evi opcode 1 - get visual info
  /**
   * @see <a href="XeviGetVisualInfo.html">XeviGetVisualInfo</a>
   */
  public VisualInfoReply visual_info (Visual [] visuals) {
    VisualInfoReply reply;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 1, 2 + visuals.length);
      o.write_int32 (visuals.length);
      for (int i = 0; i < visuals.length; i++)
        o.write_int32 (visuals [i].id);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.read_reply (o);
        in.skip (8);
        reply = new VisualInfoReply (in);
        
      }
    }
    return reply;
  }


  public String more_string () {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version;
  }
}
