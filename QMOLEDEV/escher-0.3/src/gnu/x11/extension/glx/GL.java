  package gnu.x11.extension.glx;

import gnu.x11.Drawable;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/**
 * GLX rendering context. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/gl-1.2.1.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/gl-design.ps.gz"
 * >design</a>).
 *
 * <p>There are a few differences with C binding:
 * <ul>
 * 
 * <li><code>boolean</code> instead of <code>GL_TRUE</code> and <code>GL_FALSE</code>;
 *
 * <li>symbol names starting with numeric character (with <code>GL_</code>
 * removed) is prefixed with <code>X</code> (e.g. <code>GL_2D</code>
 * becomes {@link #X2D});
 *
 * <li>trivial vector wrappers such as <code>glEdgeFlagv</code> and
 * <code>glIndexdv</code> are not implemented.
 *
 * </ul>
 */
public class GL extends gnu.x11.Resource implements GLConstant {

  /**
   * A helper for sending large render requests.
   */
  private class LargeRenderRequest {

    /**
     * The output stream for the request.
     */
    private RequestOutputStream out;

    /**
     * True when th request must be sent as large request, false otherwise. 
     */
    private boolean render_large;

    /**
     * Indicates if we are currently writing the large parameter. In this case
     * we need to check for splitting up the value.
     */
    private boolean large_param;

    /**
     * The overall number of request parts.
     */
    private int request_total;

    /**
     * The current request number.
     */
    private int request_number;

    /**
     * Stores the total length of the large parameter.
     */
    private int large_param_length;

    /**
     * Prepares the LargeRenderRequest for sending a new request. This starts
     * the request and writes the render opcode and render request length.
     *
     * @param o the output stream
     * @param l the length of the request
     * @param large_param_length the length of the large parameters in bytes
     */
    void begin (RequestOutputStream o, int opcode, int small_params_length,
                int large_param_length) {
      out = o;
      this.large_param_length = large_param_length;
      int length_total = small_params_length + large_param_length;
      int pad = RequestOutputStream.pad (length_total);
      int render_command_length = 4 + length_total + pad; 
      render_large =  2 + render_command_length / 4 > o.buffer.length;
      if (render_large) {
        request_total = large_param_length / (o.buffer.length - 16) + 1;
        pad = RequestOutputStream.pad (small_params_length);
        int l = 6 + (small_params_length + pad) / 4;
        out.begin_request(glx.major_opcode, 2, l);
        out.write_int32 (tag);
        request_number = 1;
        out.write_int16 (request_number);
        out.write_int16 (request_total);
        out.write_int32 (small_params_length);
        out.write_int32 (length_total);
        out.write_int32 (opcode);
        large_param = false;
      } else {
        out.begin_request(glx.major_opcode, 1, 2 + render_command_length / 4);
        out.write_int32 (tag);
        out.write_int16 (render_command_length);
        out.write_int16 (opcode);
      }
    }

    /**
     * Signals the beginning of the large parameter.
     */
    void begin_large_parameter () {
      if (request_number > 1) {
        // Update the length fields before sending the previous request.
        int index = out.index;
        int ni = out.index - 16;
        int pi = RequestOutputStream.pad (ni);
        out.index = 2;
        out.write_int16 (4 + (ni + pi) / 4);
        out.index = 12;
        out.write_int32 (ni);
        out.index = index;
      }

      request_number++;

      out.begin_request(glx.major_opcode, 2, 0); // Length written later.
      out.write_int32 (tag);
      out.write_int16 (request_number);
      out.write_int16 (request_total);
      out.write_int32 (0); // ni, written later.
    }

    void write_float32 (float val) {
      if (render_large && large_param && ! out.fits (4)) {
        begin_large_parameter ();
      }
      out.write_float (val);
    }

    void write_float64 (double val) {
      if (render_large && large_param && ! out.fits (8)) {
        begin_large_parameter ();
      }
      out.write_double (val);
    }

    void write_int32 (int val) {
      if (render_large && large_param && ! out.fits (4)) {
        begin_large_parameter ();
      }
      out.write_int32 (val);
    }

    void write_int16 (int val) {
      if (render_large && large_param && ! out.fits (2)) {
        begin_large_parameter ();
      }
      out.write_int16 (val);
    }

    void write_int8 (byte val) {
      if (render_large && large_param && ! out.fits (1)) {
        begin_large_parameter ();
      }
      out.write_int8 (val);
    }

    void write_pad (int p) {
      if (render_large && large_param && ! out.fits (p)) {
        begin_large_parameter ();
      }
      out.write_pad (p);
    }

    void write_bool (boolean val) {
      if (render_large && large_param && ! out.fits (1)) {
        begin_large_parameter ();
      }
      out.write_bool (val);
    }

  }

  /**
   * Predefined context.
   *
   * @see gnu.x11.Window#NONE
   */
  public static final GL NONE0 = new GL (0);


  public GLX glx;
  public int tag;


  /**
   * A helper for sending large render requests.
   */
  private LargeRenderRequest large_render_request;
 
  private int render_mode;
  private String version_string_cache;


  /** Predefined. */
  public GL (int id) { super (id); }
  

  // glx opcode 1 - render

  /**
   * Starts a render request. First this makes sure that the current
   * request is a render request, and that the buffer has enough room
   * for the new render command. Then it writes the opcode and length
   * to the request and returns the stream for the caller to complete the
   * render command.
   *
   * If the current request is a render request, but the buffer hasn't got
   * enough room for the new command, or the current request is no GLX render
   * command, the buffer is flushed and a new request
   * is started. 
   *
   * @param opcode the opcode
   * @param the command length
   *
   * @return the output stream
   */
  private void begin_render_request (RequestOutputStream o, int opcode,
                                     int length) {

    synchronized (o) {
      if (o.index == 0             // No request started so far.
          || o.opcode () != 1      // Wrong opcode.
          || ! o.fits (length)) {  // Doesn't fit.
        o.begin_request (glx.major_opcode, 1, 2 + length / 4);
        o.write_int32 (tag);
      }
      o.write_int16 (length);
      o.write_int16 (opcode);
    }
  }

  // glx opcode 3 - create context
  /**
   * @see <a href="glXCreateContext.html">glXCreateContext</a>
   */
  public GL (GLX glx, int visual_id, int screen_no,
             GL share_list, boolean direct) {
    
    super (glx.display);
    this.glx = glx;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 3, 6);
      o.write_int32 (id);
      o.write_int32 (visual_id);
      o.write_int32 (screen_no);
      o.write_int32 (share_list.id);
      o.write_bool (direct);
      o.skip (3);
      o.send ();
    }
  }


  // glx opcode 4 - destroy context  
  /**
   * @see <a href="glXDestroyContext.html">glXDestroyContext</a>
   */
  public void destroy () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 4, 2);
      o.write_int32 (id);
      o.send ();
    }
  }


  // glx opcode 5 - make current
  /**
   * @see <a href="glXMakeCurrent.html">glXMakeCurrent</a>
   */
  public void make_current (GLXDrawable drawable) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 5, 4);
      o.write_int32 (drawable.id ());
      o.write_int32 (id);
      o.write_int32 (tag);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        tag = i.read_int32 ();
        i.skip (20);
      }
    }
  }  


  // glx opcode 6 - is direct
  /**
   * @see <a href="glXIsDirect.html">glXIsDirect</a>
   */
  public boolean direct () {

    boolean direct;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 6, 2);
      o.write_int32 (id);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        direct = i.read_bool ();
        i.skip (23);
      }
    }
    return direct;
  }  


  // glx opcode 8 - wait gl
  /**
   * @see <a href="glXWaitGL.html">glXWaitGL</a>
   */
  public void wait_gl () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 8, 2);
      o.write_int32 (tag);
      // Need to flush here.
      o.flush ();
    }
  }


  // glx opcode 9 - wait x
  /**
   * @see <a href="glXWaitX.html">glXWaitX</a>
   */
  public void wait_x () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 9, 2);
      o.write_int32 (tag);
      // Need to flush here.
      o.flush ();
    }
  }


  // glx opcode 10 - copy context
  /**
   * @see <a href="glXCopyContext.html">glXCopyContext</a>
   */
  public void copy (GL dst, int mask) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 10, 5);
      o.write_int32 (id);
      o.write_int32 (dst.id);
      o.write_int32 (mask);
      o.write_int32 (tag);
      o.send ();
    }
  }


  // glx opcode 11 - swap buffers
  /**
   * @see <a href="glXSwapBuffers.html">glXSwapBuffers</a>
   */
  public void swap_buffers (Drawable drawable) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 11, 3);
      o.write_int32 (tag);
      o.write_int32 (drawable.id);
      o.send ();
    }
  }


  // glx opcode 12 - use x font
  /**
   * @see <a href="glXUseXFont.html">glXUseXFont</a>
   */
  public void use_x_font (gnu.x11.Font font, int first, 
                          int count, int base) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 12, 6);
      o.write_int32 (tag);
      o.write_int32 (font.id);
      o.write_int32 (first);
      o.write_int32 (count);
      o.write_int32 (base);
      o.send ();
    }
  }


  // glx opcode 101 - new list
  /**
   * @see <a href="glNewList.html">glNewList</a>
   */
  public void new_list (int list, int mode) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode,101, 4);
      o.write_int32 (tag);
      o.write_int32 (list);
      o.write_int32 (mode);
      o.send ();
    }
  }


  // glx opcode 102 - end list
  /**
   * @see <a href="glEndList.html">glEndList</a>
   */
  public void end_list () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 102, 2);
      o.write_int32 (tag);
      o.send ();
    }
  }


  // glx opcode 103 - delete lists
  /**
   * @see <a href="glDeleteLists.html">glDeleteLists</a>
   */
  public void delete_lists (int list, int range) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 103, 4);
      o.write_int32 (tag);
      o.write_int32 (list);
      o.write_int32 (range);
      o.send ();
    }
  }


  // glx opcode 104 - generate lists
  /**
   * @see <a href="glGenLists.html">glGenLists</a>
   */
  public int gen_lists (int range) {
    int ret;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 104, 3);
      o.write_int32 (tag);
      o.write_int32 (range);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        ret = i.read_int32 ();
        i.skip (20);
      }
    }
    return ret;
  }
  

  // glx opcode 105 - feedback buffer
  /**
   * @see <a href="glFeedbackBuffer.html">glFeedbackBuffer</a>
   */
  public void feedback_buffer (int size, int type) {
    render_mode = FEEDBACK;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 105, 4);
      o.write_int32 (tag);
      o.write_int32 (size);
      o.write_int32 (type);
      o.send ();
    }
  }


  // glx opcode 106 - selection buffer
  /**
   * @see <a href="glSelectionBuffer.html">glSelectionBuffer</a>
   */
  public void selection_buffer (int size) {
    render_mode = SELECT;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 106, 3);
      o.write_int32 (tag);
      o.write_int32 (size);
      o.send ();
    }
  }

  public class RenderModeData {
  
    public int ret_val;
    public int [] selection_data;
    public float [] feedback_data;
  }

  // glx opcode 107 - render mode
  /**
   * @see <a href="glRenderMode.html">glRenderMode</a>
   */
  public RenderModeData render_mode (int mode) {

    RenderModeData d = new RenderModeData ();
    int new_mode = render_mode;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 107, 3);
      o.write_int32 (tag);
      o.write_int32 (mode);
      if (render_mode == RENDER)
        o.send ();
      else {
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          d.ret_val = i.read_int32 ();
          int num_data = i.read_int32 ();
          new_mode = i.read_int32 ();
          i.skip (12);
          if (render_mode == FEEDBACK) {
            d.feedback_data = new float [num_data];
            for (int j = 0; j < num_data; j++)
              d.feedback_data [j] = i.read_float32 ();
          } else if (render_mode == SELECT) {
            d.selection_data = new int [num_data];
            for (int j = 0; j < num_data; j++)
              d.selection_data [j] = i.read_int32 ();
          } else {
            assert false : "Should not get here";
          }
        }
        
      }
    }

    return d;
  }


  // glx opcode 108 - finish
  /**
   * @see <a href="glFinish.html">glFinish</a>
   */
  public void finish () {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 108, 2);
      o.write_int32 (tag);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        // We wait for the response to make sure it is really finished.
        // There is no interesting data in there.
        i.skip (32);
      }
    }
  }


  // glx opcode 109 - pixel storef
  /**
   * @see <a href="glPixelStoref.html">glPixelStoref</a>
   */
  public void pixel_storef (int pname, int param) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 109, 4);
      o.write_int32 (tag);
      o.write_int32 (pname);
      o.write_int32 (param);
      o.send ();
    }
  }


  // glx opcode 110 - pixel storei
  /**
   * @see <a href="glPixelStorei.html">glPixelStorei</a>
   */
  public void pixel_storei (int pname, int param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 11, 4);
      o.write_int32 (tag);
      o.write_int32 (pname);
      o.write_int32 (param);
      o.send ();
    }
  }


  // glx opcode 112 - get booleanv
  /**
   * @see <a href="glGetBooleanv.html">glGetBooleanv</a>
   */
  public boolean [] booleanv (int pname) {
    boolean [] v;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 112, 3);
      o.write_int32 (tag);
      o.write_int32 (pname);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (12);
        int n = i.read_int32 ();
        if (n == 1) {
          v = new boolean [1];
          v [0] = i.read_bool ();
          i.skip (15);
        } else {
          i.skip (16);
          v = new boolean [n];
          for (int j = 0; j < n; j++) {
            v [j] = i.read_bool ();
          }
          i.skip (RequestOutputStream.pad (n));
        }
      }
    }
    return v;
  }


  // glx opcode 113 - get clip plane
  /**
   * @see <a href="glGetClipPlane.html">glGetClipPlane</a>
   */
  public double [] clip_plane (int plane) {
    return get_dv1 (113, plane);
  }


  // glx opcode 114 - get doublev
  /**
   * @see <a href="glGetDoublev.html">glGetDoublev</a>
   */
  public double [] doublev (int pname) {
    return get_dv1 (114, pname);
  }


  // glx opcode 115 - get error
  /**
   * @return valid:
   * @see #error_string()
   * @see <a href="glGetError.html">glGetError</a>
   */
  public int error () {
    int err;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 115, 2);
      o.write_int32 (tag);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        err = i.read_int32 ();
        i.skip (20);
      }
    }
    return err;
  }


  // glx opcode 116 - get floatv
  /**
   * @see <a href="glGetFloatv.html">glGetFloatv</a>
   */
  public float [] floatv (int pname) {
    return get_fv1 (116, pname);
  }


  // glx opcode 117 - get integerv
  /**
   * @see <a href="glGetIntegerv.html">glGetIntegerv</a>
   */
  public int [] integerv (int pname) {
    return get_iv1 (117, pname);
  }


  // glx opcode 118 - get lightfv
  /**
   * @see <a href="glGetLightfv.html">glGetLightfv</a>
   */
  public float [] lightfv (int light, int pname) {
    return get_fv2 (118, light, pname);
  }


  // glx opcode 119 - get lightiv
  /**
   * @see <a href="glGetLightiv.html">glGetLightiv</a>
   */
  public int [] lightiv (int light, int pname) {
    return get_iv2 (119, light, pname);
  }


  // glx opcode 120 - get mapdv
  /**
   * @see <a href="glGetMapdv.html">glGetMapdv</a>
   */
  public double [] mapdv (int target, int query) {
    return get_dv2 (120, target, query);
  }


  // glx opcode 121 - get mapfv
  /**
   * @see <a href="glGetMapfv.html">glGetMapfv</a>
   */
  public float [] mapfv (int target, int query) {
    return get_fv2 (121, target, query);
  }


  // glx opcode 122 - get mapiv
  /**
   * @see <a href="glGetMapiv.html">glGetMapiv</a>
   */
  public int [] mapiv (int target, int query) {
    return get_iv2 (122, target, query);
  }


  // glx opcode 123 - get materialfv
  /**
   * @see <a href="glGetMaterialfv.html">glGetMaterialfv</a>
   */
  public float [] materialfv (int face, int pname) {
    return get_fv2 (123, face, pname);
  }


  // glx opcode 124 - get materialiv
  /**
   * @see <a href="glGetMaterialiv.html">glGetMaterialiv</a>
   */
  public int [] materialiv (int face, int pname) {
    return get_iv2 (124, face, pname);
  }


  // glx opcode 125 - get pixel mapfv
  /**
   * @see <a href="glGetPixelMapfv.html">
   * glGetPixelMapfv</a>
   */
  public float [] pixel_mapfv (int map) {
    return get_fv1 (125, map);
  }


  // glx opcode 126 - get pixel mapiv
  /**
   * @see <a href="glGetPixelMapiv.html">
   * glGetPixelMapiv</a>
   */
  public int [] pixel_mapuiv (int map) {
    return get_iv1 (126, map);
  }


  // glx opcode 127 - get pixel mapusv
  /**
   * @see <a href="glGetPixelMapusv.html">
   * glGetPixelMapusv</a>
   */
  public int [] pixel_mapusv (int map) {
    RequestOutputStream o = display.out;
    int [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 127, 3);
      o.write_int32 (tag);
      o.write_int32 (map);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new int [n];
        if (n == 1) {
          ret [0] = in.read_int16 ();
          in.skip (14);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_int16 ();
          in.pad(RequestOutputStream.pad (n));
        }
      }
    }
    return ret;
  }


  // glx opcode 129 - get string
  /**
   * @see <a href="glGetString.html">glGetString</a>
   */
  public String string (int name) {
    if (name == VERSION && version_string_cache != null)
      return version_string_cache;

    RequestOutputStream o = display.out;
    String str;
    synchronized (o) {
      o.begin_request(glx.major_opcode, 129, 3);
      o.write_int32 (tag);
      o.write_int32 (name);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.read_reply (o);
        in.skip (12);
        int n = in.read_int32 ();
        in.skip (16);
        str = in.read_string8 (n);
        in.skip (RequestOutputStream.pad (n));
      }
    }

    if (name == VERSION) 
      version_string_cache = str;

    return str;
  }


  // glx opcode 130 - get tex envfv
  /**
   * @see <a href="glGetTexEnvfv.html">glGetTexEnvfv</a>
   */
  public float [] tex_envfv (int target, int pname) {
    return get_fv2 (130, target, pname);
  }


  // glx opcode 131 - get tex enviv
  /**
   * @see <a href="glGetTexEnviv.html">glGetTexEnviv</a>
   */
  public int [] tex_enviv (int target, int pname) {
    return get_iv2 (131, target, pname);
  }


  // glx opcode 132 - get tex gendv
  /**
   * @see <a href="glGetTexGendv.html">glGetTexGendv</a>
   */
  public double [] tex_gendv (int target, int pname) {
    return get_dv2 (132, target, pname);
  }


  // glx opcode 133 - get tex genfv
  /**
   * @see <a href="glGetTexGenfv.html">glGetTexGenfv</a>
   */
  public float [] tex_genfv (int target, int pname) {
    return get_fv2 (133, target, pname);
  }


  // glx opcode 134 - get tex geniv
  /**
   * @see <a href="glGetTexGeniv.html">glGetTexGeniv</a>
   */
  public int [] tex_geniv (int target, int pname) {
    return get_iv2 (134, target, pname);
  }


  // glx opcode 136 - get tex parameterfv
  /**
   * @see <a href="glGetTexParameterfv.html">glGetTexParameterfv</a>
   */
  public float [] tex_parameterfv (int target, int pname) {
    return get_fv2 (136, target, pname);
  }


  // glx opcode 137 - get tex parameteriv
  /**
   * @see <a href="glGetTexParameteriv.html">glGetTexParameteriv</a>
   */
  public int [] tex_parameteriv (int target, int pname) {
    return get_iv2 (138, target, pname);
  }


  // glx opcode 138 - get tex level parameterfv
  /**
   * @see <a href="glGetTexLevelParameterfv.html">
   * glGetTexLevelParameterfv</a>
   */
  public float [] tex_level_parameterfv (int target, int level, int pname) {
    RequestOutputStream o = display.out;
    float [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 138, 5);
      o.write_int32 (tag);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (pname);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new float [n];
        if (n == 1) {
          ret [0] = in.read_float32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_float32 ();
        }
      }
    }
    return ret;
  }


  // glx opcode 139 - get tex level parameteriv
  /**
   * @see <a href="glGetTexLevelParameteriv.html">
   * glGetTexLevelParameteriv</a>
   */
  public int [] tex_level_parameteriv (int target, int level, int pname) {
    RequestOutputStream o = display.out;
    int [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 139, 5);
      o.write_int32 (tag);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (pname);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new int [n];
        if (n == 1) {
          ret [0] = in.read_int32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_int32 ();
        }
      }
    }
    return ret;
  }

  
  // glx opcode 141 - is list
  /**
   * @see <a href="glXIsList.html">glXIsList</a>
   */
  public boolean list (int list) {
    RequestOutputStream o = display.out;
    boolean ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 141, 3);
      o.write_int32 (tag);
      o.write_int32 (list);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.skip (8);
        ret = i.read_bool();
        i.skip (20);
      }
    }
    return ret;
  }


  // glx opcode 142 - flush
  /**
   * @see <a href="glFlush.html">glFlush</a>
   */
  public void flush () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 142, 2);
      o.write_int32 (tag);
      o.flush ();
    }
  }


  /** Reply of {@link #textures_resident(int[])}. */
  public static class TexturesResidentReply {

    public boolean all_resident;

    public boolean [] residences;

    TexturesResidentReply (ResponseInputStream in, int count) {
      all_resident = in.read_bool();
      in.skip (20);
      residences = new boolean [count];
      for (int i = 0; i < count; i++)
        residences [i] = in.read_bool ();
    }

  }


  // glx opcode 143 - are textures resident
  /**
   * @see <a href="glAreTexturesResident.html">glAreTexturesResident</a>
   */
  public TexturesResidentReply textures_resident (int [] textures) {
    int n = textures.length;
    RequestOutputStream o = display.out;
    TexturesResidentReply ret;
    synchronized (o) {
      // The spec says request length == 1, but this seems wrong.
      o.begin_request(glx.major_opcode, 143, 3 + n);
      o.write_int32 (tag);
      o.write_int32 (n);
      for (int i = 0; i < n; i++)
        o.write_int32 (textures[i]);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (8);
        ret = new TexturesResidentReply (in, n);
        in.pad (RequestOutputStream.pad (n));
      }
    }
    return ret;
  }


  // glx opcode 144 - delete textures
  /**
   * @see <a href="glDeleteTextures.html">glDeleteTextures</a>
   */
  public void delete_textures (int [] textures) {
    int n = textures.length;
    RequestOutputStream o = display.out;
    synchronized (o) {
      // The spec says request length == 1, but this seems wrong.
      o.begin_request(glx.major_opcode, 144, 3 + n);
      o.write_int32 (tag);
      o.write_int32 (n);
      for (int i = 0; i < n; i++)
        o.write_int32 (textures[i]);
    }
  }


  // glx opcode 145 - generate textures
  /**
   * @see <a href="glGenTextures.html">glGenTextures</a>
   */
  public int [] gen_textures (int n) {
    RequestOutputStream o = display.out;
    int [] textures;
    synchronized (o) {
      o.begin_request (glx.major_opcode, 145, 3);
      o.write_int32 (tag);
      o.write_int32 (n);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (32);
        textures = new int [n];
        for (int i = 0; i < n; i++)
          textures [i] = in.read_int32 ();
      }
    }
    return textures;
  }


  // glx opcode 146 - is texture
  /**
   * @see <a href="glXIsTexture.html">glXIsTexture</a>
   */
  public boolean texture (int texture) {
    RequestOutputStream o = display.out;
    boolean ret;
    synchronized (o) {
      o.begin_request(glx.major_opcode, 146, 3);
      o.write_int32 (tag);
      o.write_int32 (texture);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (8);
        ret = in.read_bool();
        in.skip (20);
      }
    }
    return ret;
  }


  // glx opcode 148 - get color table parameterfv
  /**
   * @see <a href="glGetColorTableParameterfv.html">
   * glGetColorTableParameterfv</a>
   */
  public float [] color_table_parameterfv (int target, int pname) {
    return get_fv2 (148, target, pname);
  }

  // glx opcode 149 - get color table parameteriv
  /**
   * @see <a href="glGetColorTableParameteriv.html">
   * glGetColorTableParameteriv</a>
   */
  public int [] color_table_parameteriv (int target, int pname) {
    return get_iv2 (149, target, pname);
  }


  // glx opcode 151 - get convolution parameterfv
  /**
   * @see <a href="glGetConvolutionParameterfv.html">
   * glGetConvolutionParameterfv</a>
   */
  public float [] convolution_parameterfv (int target, int pname) {
    return get_fv2 (151, target, pname);
  }


  // glx opcode 152 - get convolution parameteriv
  /**
   * @see <a href="glGetConvolutionParameteriv.html">
   * glGetConvolutionParameteriv</a>
   */
  public int [] convolution_parameteriv (int target, int pname) {
    return get_iv2 (152, target, pname);
  }


  // glx opcode 155 - get histogram parameterfv
  /**
   * @see <a href="glGetHistogramParameterfv.html">
   * glGetHistogramParameterfv</a>
   */
  public float [] histogram_parameterfv (int target, int pname) {
    return get_fv2 (155, target, pname);
  }


  // glx opcode 156 - get histogram parameteriv
  /**
   * @see <a href="glGetHistogramParameteriv.html">
   * glGetHistogramParameteriv</a>
   */
  public int [] histogram_parameteriv (int target, int pname) {
    return get_iv2 (156, target, pname);
  }


  // glx opcode 158 - get minmax parameterfv
  /**
   * @see <a href="glGetMinmaxParameterfv.html">
   * glGetMinmaxParameterfv</a>
   */
  public float [] minmax_parameterfv (int target, int pname) {
    return get_fv2 (158, target, pname);
  }


  // glx opcode 159 - get minmax parameteriv
  /**
   * @see <a href="glGetMinmaxParameteriv.html">
   * glGetMinmaxParameteriv</a>
   */
  public int [] minmax_parameteriv (int target, int pname) {
    return get_iv2 (159, target, pname);
  }


  // glx render opcode 1 - call list
  /**
   * @see <a href="glCallList.html">glCallList</a>
   */
  public void call_list (int list) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 1, 8);
      o.write_int32 (list);
    }
  }
    

  // glx render opcode 2 - call lists
  /**
   * @see <a href="glCallLists.html">glCallLists</a>
   */
  public void call_lists (int type, Object lists) {
    int length;
    switch (type) {
    case BYTE:                  // fall through
    case UNSIGNED_BYTE: 
      length = ((byte []) lists).length;
      break;

    case SHORT:                 // fall through
    case UNSIGNED_SHORT:        // fall through
    case X2_BYTES: 
      length = ((int []) lists).length * 2;
      break;

    case INT:                   // fall through
    case UNSIGNED_INT:          // fall through
    case X4_BYTES: 
      length = ((int []) lists).length * 4;
      break;

    case FLOAT: 
      length = ((float []) lists).length * 4;
      break;
    case X3_BYTES:
      length = ((int []) lists).length * 3;
      break;
    default:
      return;
    }

    int p = RequestOutputStream.pad (length);
    int req_length = 12 + length + p;
    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 2, 12, length + p);

      large_render_request.write_int32 (length);
      large_render_request.write_int32 (type);

      large_render_request.begin_large_parameter();

      switch (type) {
      case BYTE:                  // fall through
      case UNSIGNED_BYTE: 
        byte [] array1 = (byte []) lists;
        for (int i = 0; i < array1.length; i++)
          large_render_request.write_int8 (array1 [i]);
        break;

      case SHORT:                 // fall through
      case UNSIGNED_SHORT:        // fall through
      case X2_BYTES: 
        int [] array2 = (int []) lists;
        for (int i = 0; i < array2.length; i++)
          large_render_request.write_int16 (array2 [i]);
        break;

      case INT:                   // fall through
      case UNSIGNED_INT:          // fall through
      case X4_BYTES: 
        int [] array3 = (int []) lists;
        for (int i = 0; i < array3.length; i++)
          large_render_request.write_int32 (array3 [i]);
        break;

      case FLOAT: 
        float [] array4 = (float []) lists;
        for (int i = 0; i < array4.length; i++)
          large_render_request.write_float32 (array4 [i]);
        break;
      case X3_BYTES:
        byte [] array5 = (byte []) lists;
        for (int i = 0; i < array5.length; i++)
          large_render_request.write_int8 (array5 [i]);
        break;
      default:
        return;
      }
      large_render_request.write_pad (p);
    }

  }


  // glx render opcode 3 - list base
  /**
   * @see <a href="glListBase.html">glListBase</a>
   */
  public void list_base (int base) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 3, 8);
      o.write_int32 (base);
    }
  }


  // glx render opcode 4 - begin
  /**
   * @see <a href="glBegin.html">glBegin</a>
   */
  public void begin (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4, 8);
      o.write_int16 (8);
      o.write_int16 (4);
      o.write_int32 (mode);
    }
  }

  // glx render opcode 5 - bitmap
  /**
   * @see <a href="glBitmap.html">glBitmap</a>
   */
  public void bitmap (int width, int height, float xorig, float yorig,
                      float xmove, float ymove, byte [] bitmap) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 5, 48, bitmap.length);
      large_render_request.write_int8 ((byte) 0); // Unused.
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_int16 (0); // Unused

      // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.write_int32 (width);
      large_render_request.write_int32 (height);
      large_render_request.write_float32 (xorig);
      large_render_request.write_float32 (yorig);
      large_render_request.write_float32 (xmove);
      large_render_request.write_float32 (ymove);
      large_render_request.begin_large_parameter();
      for (int i = 0; i < bitmap.length; i++)
        large_render_request.write_int8 (bitmap [i]);
      large_render_request.write_pad(RequestOutputStream.pad(bitmap.length));
    }
  }


  // glx render opcode 6 - color3bv
  /**
   * @see <a href="glColor3b.html">glColor3b</a>
   */
  public void color3b (int red, int green, int blue) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 6, 8);
      o.write_int8 (red);    
      o.write_int8 (green);    
      o.write_int8 (blue);
      o.write_pad (1);
    }
  }


  // glx render opcode 7 - color3dv
  /**
   * @see <a href="glColor3d.html">glColor3d</a>
   */
  public void color3d (double red, double green, double blue) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 7, 28);
      o.write_double (red);
      o.write_double (green);
      o.write_double (blue);
    }
  }


  // glx render opcode 8 - color3fv
  /**
   * @see <a href="glColor3f.html">glColor3f</a>
   */
  public void color3f (float red, float green, float blue) {
    render_3f (8, red, green, blue);
  }


  // glx render opcode 9 - color3iv
  /**
   * @see <a href="glColor3i.html">glColor3i</a>
   */
  public void color3i (int red, int green, int blue) {
    render_3i (9, red, green, blue);
  }


  // glx render opcode 10 - color3sv
  /**
   * @see <a href="glColor3s.html">glColor3s</a>
   */
  public void color3s (int red, int green, int blue) {
    render_3s (10, red, green, blue);
  }


  // glx render opcode 11 - color3ubv
  /**
   * @see <a href="glColor3ub.html">glColor3ub</a>
   */
  public void color3ub (byte red, byte green, byte blue) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 11, 8);
      o.write_int8 (red);    
      o.write_int8 (green);    
      o.write_int8 (blue);
      o.write_pad (1);
    }
  }


  // glx render opcode 12 - color3uiv
  /**
   * @see <a href="glColor3ui.html">glColor3ui</a>
   */
  public void color3ui (int red, int green, int blue) {
    render_3i (12, red, green, blue);
  }


  // glx render opcode 13 - color3usv
  /**
   * @see <a href=5.html">glColor3us</a>
   */
  public void color3us (int red, int green, int blue) {
    render_3s (13, red, green, blue);
  }


  // glx render opcode 14 - color4bv
  /**
   * @see <a href="glColor4b.html">glColor4b</a>
   */
  public void color4b (byte red, byte green, byte blue, byte alpha) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 14, 8);
      o.write_int8 (red);
      o.write_int8 (green);
      o.write_int8 (blue);
      o.write_int8 (alpha);
    }
  }


  // glx render opcode 15 - color4dv
  /**
   * @see <a href="glColor4d.html">glColor4d</a>
   */
  public void color4d (double red, double green, 
                       double blue, double alpha) {
    render_4d (15, red, green, blue, alpha);
  }


  // glx render opcode 16 - color4fv
  /**
   * @see <a href="glColor4f.html">glColor4f</a>
   */
  public void color4f (float red, float green, float blue, float alpha) {
    render_4f (16, red, green, blue, alpha);
  }


  // glx render opcode 17 - color4iv
  /**
   * @see <a href="glColor4i.html">glColor4i</a>
   */
  public void color4i (int red, int green, int blue, int alpha) {
    render_4i (17, red, green, blue, alpha);
  }


  // glx render opcode 18 - color4sv
  /**
   * @see <a href="glColor4s.html">glColor4s</a>
   */
  public void color4s (int red, int green, int blue, int alpha) {
    render_4s (18, red, green, blue, alpha);
  }


  // glx render opcode 19 - color4ubv
  /**
   * @see <a href="glColor4ub.html">glColor4ub</a>
   */
  public void color4ub (boolean red, boolean green, boolean blue,
                        boolean alpha) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 19, 8);
      o.write_bool (red);    
      o.write_bool (green);    
      o.write_bool (blue);
      o.write_bool (alpha);
    }
  }


  // glx render opcode 20 - color4uiv
  /**
   * @see <a href="glColor4ui.html">glColor4ui</a>
   */
  public void color4ui (int red, int green, int blue, int alpha) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 20, 20);
      o.write_int32 (red);    
      o.write_int32 (green);    
      o.write_int32 (blue);
      o.write_int32 (alpha);
    }
  }


  // glx render opcode 21 - color4usv
  /**
   * @see <a href=5.html">glColor4us</a>
   */
  public void color4us (int red, int green, int blue, int alpha) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 12, 12);
      o.write_int16 (red);    
      o.write_int16 (green);    
      o.write_int16 (blue);
      o.write_int16 (alpha);
    }
  }


  // glx render opcode 22 - edge flagv
  /**
   * @see <a href="glEdgeFlag.html">glEdgeFlag</a>
   */
  public void edge_flag (boolean flag) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 22, 8);
      o.write_bool (flag);
      o.write_pad (3);
    }
  }


  // glx render opcode 23 - end
  /**
   * @see <a href="glEnd.html">glEnd</a>
   */
  public void end () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 23, 4);
    }
  }

  // glx render opcode 24 - indexdv
  /**
   * @see <a href="glIndexd.html">glIndexd</a>
   */
  public void indexd (double c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 24, 12);
      o.write_double (c);
    }
  }


  // glx render opcode 25 - indexfv
  /**
   * @see <a href="glIndexf.html">glIndexf</a>
   */
  public void indexf (float c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 25, 8);
      o.write_float (c);
    }
  }


  // glx render opcode 26 - indexiv
  /**
   * @see <a href="glIndexi.html">glIndexi</a>
   */
  public void indexi (int c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 26, 8);
      o.write_int32 (c);
    }
  }


  // glx render opcode 27 - indexsv
  /**
   * @see <a href="glIndexs.html">glIndexs</a>
   */
  public void indexs (int c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 27, 8);
      o.write_int16 (c);
      o.write_pad (2);
    }
  }


  // glx render opcode 28 - normal3bv
  /**
   * @see <a href="glNormal3b.html">glNormal3b</a>
   */
  public void normal3b (boolean x, boolean y, boolean z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 28, 8);
      o.write_bool (x);
      o.write_bool (y);
      o.write_bool (z);
      o.write_pad (1);
    }
  }


  // glx render opcode 29 - normal3dv
  /**
   * @see <a href="glNormal3d.html">glNormal3d</a>
   */
  public void normal3d (double x, double y, double z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 29, 28);
      o.write_double (x);
      o.write_double (y);
      o.write_double (z);
    }
  } 


  // glx render opcode 30 - normal3fv
  /**
   * @see <a href="glNormal3f.html">glNormal3f</a>
   */
  public void normal3f (float x, float y, float z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 30, 16);
      o.write_float (x);
      o.write_float (y);
      o.write_float (z);
    }
  } 


  // glx render opcode 31 - normal3iv
  /**
   * @see <a href="glNormal3i.html">glNormal3i</a>
   */
  public void normal3i (int x, int y, int z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 31, 16);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (z);
    }
  } 


  // glx render opcode 32 - normal3sv
  /**
   * @see <a href="glNormal3s.html">glNormal3s</a>
   */
  public void normal3s (int x, int y, int z) {
    render_3s (32, x, y, z);
  } 


  // glx render opcode 33 - raster pos2dv
  /**
   * @see <a href="glRasterPos2d.html">glRasterPos2d</a>
   */
  public void raster_pos2d (double x, double y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 33, 20);
      o.write_double (x);
      o.write_double (y);
    }
  } 


  // glx render opcode 34 - raster pos2fv
  /**
   * @see <a href="glRasterPos2f.html">glRasterPos2f</a>
   */
  public void raster_pos2f (float x, float y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 34, 12);
      o.write_float (x);
      o.write_float (y);
    }
  } 


  // glx render opcode 35 - raster pos2iv
  /**
   * @see <a href="glRasterPos2i.html">glRasterPos2i</a>
   */
  public void raster_pos2i (int x, int y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 35, 12);
      o.write_int32 (x);
      o.write_int32 (y);
    }
  } 


  // glx render opcode 36 - raster pos2sv
  /**
   * @see <a href="glRasterPos2s.html">glRasterPos2s</a>
   */
  public void raster_pos2s (int x, int y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 36, 8);
      o.write_int16 (x);
      o.write_int16 (y);
    }
  } 


  // glx render opcode 37 - raster pos3dv
  /**
   * @see <a href="glRasterPos3d.html">glRasterPos3d</a>
   */
  public void raster_pos3d (double x, double y, double z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 37, 28);
      o.write_double (x);
      o.write_double (y);
      o.write_double (z);
    }
  } 


  // glx render opcode 38 - raster pos3fv
  /**
   * @see <a href="glRasterPos3f.html">glRasterPos3f</a>
   */
  public void raster_pos3f (float x, float y, float z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 38, 16);
      o.write_float (x);
      o.write_float (y);
      o.write_float (z);
    }
  } 


  // glx render opcode 39 - raster pos3iv
  /**
   * @see <a href="glRasterPos3i.html">glRasterPos3i</a>
   */
  public void raster_pos3i (int x, int y, int z) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 39, 16);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (z);
    }
  } 


  // glx render opcode 40 - raster pos3sv
  /**
   * @see <a href="glRasterPos3s.html">glRasterPos3s</a>
   */
  public void raster_pos3s (int x, int y, int z) {
    render_3s (40, x, y, z);
  } 


  // glx render opcode 41 - raster pos4dv
  /**
   * @see <a href="glRasterPos4d.html">glRasterPos4d</a>
   */
  public void raster_pos4d (double x, double y, double z, double w) {
    render_4d (41, x, y, z, w);
  } 


  // glx render opcode 42 - raster pos4fv
  /**
   * @see <a href="glRasterPos4f.html">glRasterPos4f</a>
   */
  public void raster_pos4f (float x, float y, float z, float w) {
    render_4f (42, x, y, z, w);
  } 


  // glx render opcode 43 - raster pos4iv
  /**
   * @see <a href="glRasterPos4i.html">glRasterPos4i</a>
   */
  public void raster_pos4i (int x, int y, int z, int w) {
    render_4i (43, x, y, z, w);
  } 


  // glx render opcode 44 - raster pos4sv
  /**
   * @see <a href="glRasterPos4s.html">glRasterPos4s</a>
   */
  public void raster_pos4s (int x, int y, int z, int w) {
    render_4s (44, x, y, z, w);
  } 


  // glx render opcode 45 - rectdv
  /**
   * @see <a href="glRectd.html">glRectd</a>
   */
  public void rectd (double x1, double x2, double y1, double y2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 45, 36);
      o.write_double (x1);
      o.write_double (x2);
      o.write_double (y1);
      o.write_double (y2);
    }
  } 


  // glx render opcode 46 - rectfv
  /**
   * @see <a href="glRectf.html">glRectf</a>
   */
  public void rectf (float x1, float x2, float y1, float y2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 46, 20);
      o.write_float (x1);
      o.write_float (x2);
      o.write_float (y1);
      o.write_float (y2);
    }
  } 


  // glx render opcode 47 - rectiv
  /**
   * @see <a href="glRecti.html">glRecti</a>
   */
  public void recti (int x1, int x2, int y1, int y2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 47, 20);
      o.write_int32 (x1);
      o.write_int32 (x2);
      o.write_int32 (y1);
      o.write_int32 (y2);
    }
  } 


  // glx render opcode 48 - rectsv
  /**
   * @see <a href="glRects.html">glRects</a>
   */
  public void rects (int x1, int x2, int y1, int y2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 48, 12);
      o.write_int16 (x1);
      o.write_int16 (x2);
      o.write_int16 (y1);
      o.write_int16 (y2);
    }
  } 


  // glx render opcode 49 - texture coord1dv
  /**
   * @see <a href="glTexCoord1d.html">glTexCoord1d</a>
   */
  public void tex_coord1d (double s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 49, 12);
      o.write_double (s);
    }
  } 


  // glx render opcode 50 - texture coord1fv
  /**
   * @see <a href="glTexCoord1f.html">glTexCoord1f</a>
   */
  public void tex_coord1f (float s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 50, 8);
      o.write_float (s);
    }
  } 


  // glx render opcode 51 - texture coord1iv
  /**
   * @see <a href="glTexCoord1i.html">glTexCoord1i</a>
   */
  public void tex_coord1i (int s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 51, 8);
      o.write_int32 (s);
    }
  } 


  // glx render opcode 52 - texture coord1sv
  /**
   * @see <a href="glTexCoord1f.html">glTexCoord1f</a>
   */
  public void tex_coord1s (int s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 52, 8);
      o.write_int16 (s);
      o.write_pad (2);
    }
  } 


  // glx render opcode 53 - texture coord2dv
  /**
   * @see <a href="glTexCoord2d.html">glTexCoord2d</a>
   */
  public void tex_coord2d (double s, double t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 53, 20);
      o.write_double (s);
      o.write_double (t);
    }
  } 


  // glx render opcode 54 - texture coord2fv
  /**
   * @see <a href="glTexCoord2f.html">glTexCoord2f</a>
   */
  public void tex_coord2f (float s, float t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 54, 12);
      o.write_float (s);
      o.write_float (t);
    }
  } 


  // glx render opcode 55 - texture coord2iv
  /**
   * @see <a href="glTexCoord2i.html">glTexCoord2i</a>
   */
  public void tex_coord2i (int s, int t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 55, 12);
      o.write_int32 (s);
      o.write_int32 (t);
    }
  } 


  // glx render opcode 56 - texture coord2sv
  /**
   * @see <a href="glTexCoord2f.html">glTexCoord2f</a>
   */
  public void tex_coord2s (int s, int t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 56, 8);
      o.write_int16 (s);
      o.write_int16 (t);
    }
  } 


  // glx render opcode 57 - texture coord3dv
  /**
   * @see <a href="glTexCoord3d.html">glTexCoord3d</a>
   */
  public void tex_coord3d (double s, double t, double r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 57, 28);
      o.write_double (s);
      o.write_double (t);
      o.write_double (r);
    }
  } 


  // glx render opcode 58 - texture coord3fv
  /**
   * @see <a href="glTexCoord3f.html">glTexCoord3f</a>
   */
  public void tex_coord3f (float s, float t, float r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 58, 16);
      o.write_float (s);
      o.write_float (t);
      o.write_float (r);
    }
  } 


  // glx render opcode 59 - texture coord3iv
  /**
   * @see <a href="glTexCoord3i.html">glTexCoord3i</a>
   */
  public void tex_coord3i (int s, int t, int r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 59, 16);
      o.write_int32 (s);
      o.write_int32 (t);
      o.write_int32 (r);
    }
  } 


  // glx render opcode 60 - texture coord3sv
  /**
   * @see <a href="glTexCoord3f.html">glTexCoord3f</a>
   */
  public void tex_coord3s (int s, int t, int r) {
    render_3s (60, s, t, r);
  } 


  // glx render opcode 61 - texture coord4dv
  /**
   * @see <a href="glTexCoord4d.html">glTexCoord4d</a>
   */
  public void tex_coord4d (double s, double t, double r, double q) {
    render_4d (61, s, t, r, q);
  } 


  // glx render opcode 62 - texture coord4fv
  /**
   * @see <a href="glTexCoord4f.html">glTexCoord4f</a>
   */
  public void tex_coord4f (float s, float t, float r, float q) {
    render_4f (62, s, t, r, q);
  } 


  // glx render opcode 63 - texture coord4iv
  /**
   * @see <a href="glTexCoord4i.html">glTexCoord4i</a>
   */
  public void tex_coord4i (int s, int t, int r, int q) {
    render_4i (63, s, t, r, q);
  } 


  // glx render opcode 64 - texture coord4sv
  /**
   * @see <a href="glTexCoord4f.html">glTexCoord4f</a>
   */
  public void tex_coord4s (int s, int t, int r, int q) {
    render_4s (64, s, t, r, q);
  } 


  // glx render opcode 65 - vertex2dv
  /**
   * @see <a href="glVertex2d.html">glVertex2d</a>
   */
  public void vertex2d (double x, double y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 65, 20);
      o.write_double (x);
      o.write_double (y);
    }
  }


  // glx render opcode 66 - vertex2fv
  /**
   * @see <a href="glVertex2f.html">glVertex2f</a>
   */
  public void vertex2f (float x, float y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 66, 12);
      o.write_float (x);
      o.write_float (y);
    }
  }


  // glx render opcode 67 - vertex2iv
  /**
   * @see <a href="glVertex2i.html">glVertex2i</a>
   */
  public void vertex2i (int x, int y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 67, 12);
      o.write_int32 (x);
      o.write_int32 (y);
    }
  }


  // glx render opcode 68 - vertex2sv
  /**
   * @see <a href="glVertex2s.html">glVertex2s</a>
   */
  public void vertex2s (int x, int y) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 68, 8);
      o.write_int16 (x);
      o.write_int16 (y);
    }
  }


  // glx render opcode 69  - vertex3dv
  /**
   * @see <a href="glVertex3d.html">glVertex3d</a>
   */
  public void vertex3d (double x, double y, double z) {
    render_3d (69, x, y, z);
  }


  // glx render opcode 70 - vertex3fv
  /**
   * @see <a href="glVertex3f.html">glVertex3f</a>
   */
  public void vertex3f (float x, float y, float z) {
    render_3f (70, x, y, z);
  }


  // glx render opcode 71 - vertex3iv
  /**
   * @see <a href="glVertex3i.html">glVertex3i</a>
   */
  public void vertex3i (int x, int y, int z) {
    render_3i (71, x, y, z);
  }


  // glx render opcode 72 - vertex3sv
  /**
   * @see <a href="glVertex3s.html">glVertex3s</a>
   */
  public void vertex3s (int x, int y, int z) {
    render_3s(72, x, y, z);
  }


  // glx render opcode 73  - vertex4dv
  /**
   * @see <a href="glVertex4d.html">glVertex4d</a>
   */
  public void vertex4d (double x, double y, double z, double w) {
    render_4d (73, x, y, z, w);
  }


  // glx render opcode 74 - vertex4fv
  /**
   * @see <a href="glVertex4f.html">glVertex4f</a>
   */
  public void vertex4f (float x, float y, float z, float w) {
    render_4f (74, x, y, z, w);
  }


  // glx render opcode 75 - vertex4iv
  /**
   * @see <a href="glVertex4i.html">glVertex4i</a>
   */
  public void vertex4i (int x, int y, int z, int w) {
    render_4i (75, x, y, z, w);
  }


  // glx render opcode 76 - vertex4sv
  /**
   * @see <a href="glVertex4s.html">glVertex4s</a>
   */
  public void vertex4s (int x, int y, int z, int w) {
    render_4s (76, x, y, z, w);
  }


  // glx render opcode 77 - clip plane
  /**
   * @see <a href="glClipPlane.html">glClipPlane</a>
   */
  public void clip_plane (int plane, double [] equation) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 77, 40);
      o.write_double (equation [0]);
      o.write_double (equation [1]);
      o.write_double (equation [2]);
      o.write_double (equation [3]);
      o.write_int32 (plane);
    }
  }


  // glx render opcode 78 - color material
  /**
   * @see <a href="glColorMaterial.html">glColorMaterial</a>
   */
  public void color_material (int face, int mode) {
    render_2i (78, face, mode);
  }


  // glx render opcode 79 - cull face
  /**
   * @see <a href="glCullFace.html">glCullFace</a>
   */
  public void cull_face (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 79, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 80 - fogf
  /**
   * @see <a href="glFogf.html">glFogf</a>
   */
  public void fogf (int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 80, 12);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 81 - fogfv
  /**
   * @see <a href="glFogfv.html">glFogfv</a>
   */
  public void fogfv (int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case FOG_MODE:              // fall through
    case FOG_DENSITY:           // fall through
    case FOG_START:             // fall through
    case FOG_END:               // fall through
    case FOG_INDEX: n = 1; break;      
    case FOG_COLOR: n = 4; break;
    }
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 81, 8 + 4 * n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 82 - fogi
  /**
   * @see <a href="glFogi.html">glFogi</a>
   */
  public void fogi (int pname, int param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 82, 12);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 83 - fogiv
  /**
   * @see <a href="glFogiv.html">glFogiv</a>
   */
  public void fogiv (int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case FOG_MODE:              // fall through
    case FOG_DENSITY:           // fall through
    case FOG_START:             // fall through
    case FOG_END:               // fall through
    case FOG_INDEX: n = 1; break;
    case FOG_COLOR: n = 4; break;
    }
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 83, 8 + 4 * n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  /**
   * @see <a href="glFrontFace.html">glFrontFace</a>
   */
  public void front_face (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 84, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 85 - hint
  /**
   * @see <a href="glHint.html">glHint</a>
   */
  public void hint (int target, int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 85, 12);
      o.write_int32 (target);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 86 - lightf
  /**
   * @see <a href="glLightf.html">glLightf</a>
   */
  public void lightf (int light, int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 86, 16);
      o.write_int32 (light);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 87 - lightfv
  /**
   * @see <a href="glLightfv.html">glLightfv</a>
   */
  public void lightfv (int light, int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case SPOT_EXPONENT:         // fall through
    case SPOT_CUTOFF:           // fall through
    case CONSTANT_ATTENUATION:  // fall through
    case LINEAR_ATTENUATION:    // fall through
    case QUADRATIC_ATTENUATION: n = 1; break;
    case SPOT_DIRECTION: n = 3; break;
    case AMBIENT:               // fall through
    case DIFFUSE:               // fall through
    case SPECULAR:              // fall through
    case POSITION: n = 4; break;
    }
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 87, 12 + 4 * n);
      o.write_int32 (light);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 88 - lighti
  /**
   * @see <a href="glLighti.html">glLighti</a>
   */
  public void lighti (int light, int pname, int param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 88, 16);
      o.write_int32 (light);
      o.write_int32 (pname);
      o.write_int32 (param);
    }
  }


  // glx render opcode 89 - lightiv
  /**
   * @see <a href="glLightiv.html">glLightiv</a>
   */
  public void lightiv (int light, int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case SPOT_EXPONENT:         // fall through
    case SPOT_CUTOFF:           // fall through
    case CONSTANT_ATTENUATION:  // fall through
    case LINEAR_ATTENUATION:    // fall through
    case QUADRATIC_ATTENUATION: n = 1; break;
    case SPOT_DIRECTION: n = 3; break;
    case AMBIENT:               // fall through
    case DIFFUSE:               // fall through
    case SPECULAR:              // fall through
    case POSITION: n = 4; break;
    }
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 89, 12 + 4 * n);
      o.write_int32 (light);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 90 - light modelf
  /**
   * @see <a href="glLightModelf.html">glLightModelf</a>
   */
  public void light_modelf (int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 90, 12);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 91 - light modelfv
  /**
   * @see <a href="glLightModelfv.html">glLightModelfv</a>
   */
  public void light_modelfv (int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case LIGHT_MODEL_COLOR_CONTROL: // fall through
    case LIGHT_MODEL_LOCAL_VIEWER: // fall through
    case LIGHT_MODEL_TWO_SIDE: n = 1; break;
    case LIGHT_MODEL_AMBIENT: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 91, 8 + 4 * n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 92 - light modeli
  /**
   * @see <a href="glLightModeli.html">glLightModeli</a>
   */
  public void light_modeli (int pname, int param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 92, 12);
      o.write_int32 (pname);
      o.write_int32 (param);
    }
  }


  // glx render opcode 93 - light modeliv
  /**
   * @see <a href="glLightModeliv.html">glLightModeliv</a>
   */
  public void light_modeliv (int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case LIGHT_MODEL_COLOR_CONTROL: // fall through
    case LIGHT_MODEL_LOCAL_VIEWER: // fall through
    case LIGHT_MODEL_TWO_SIDE: n = 1; break;
    case LIGHT_MODEL_AMBIENT: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 93, 8 + 4 * n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 94 - line stipple
  /**
   * @see <a href="glLineStipple.html">glLineStipple</a>
   */
  public void line_stipple (int factor, int pattern) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 94, 12);
      o.write_int32 (factor);
      o.write_int16 (pattern);
      o.write_pad (2);
    }
  }


  // glx render opcode 95 - line width
  /**
   * @see <a href="glLineWidth.html">glLineWidth</a>
   */
  public void line_width (float width) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 95, 8);
      o.write_float (width);
    }
  }


  // glx render opcode 96 - materialf
  /**
   * @see <a href="glMaterialf.html">glMaterialf</a>
   */
  public void materialf (int face, int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 96, 16);
      o.write_int32 (face);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 97 - materialfv
  /**
   * @see <a href="glMaterialfv.html">glMaterialfv</a>
   */
  public void materialfv (int face, int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case SHININESS: n = 1; break;
    case COLOR_INDEXES: n = 3; break;
    case AMBIENT:               // fall through
    case DIFFUSE:               // fall through
    case SPECULAR:              // fall through
    case EMISSION:              // fall through
    case AMBIENT_AND_DIFFUSE: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 96, 12 + 4 * n);
      o.write_int32 (face);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 98 - materiali
  /**
   * @see <a href="glMateriali.html">glMateriali</a>
   */
  public void materiali (int face, int pname, int param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 98, 16);
      o.write_int32 (face);
      o.write_int32 (pname);
      o.write_int32 (param);
    }
  }


  // glx render opcode 99 - materialiv
  /**
   * @see <a href="glMaterialiv.html">glMaterialiv</a>
   */
  public void materialiv (int face, int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case SHININESS: n = 1; break;
    case COLOR_INDEXES: n = 3; break;
    case AMBIENT:               // fall through
    case DIFFUSE:               // fall through
    case SPECULAR:              // fall through
    case EMISSION:              // fall through
    case AMBIENT_AND_DIFFUSE: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 96, 12 + 4 * n);
      o.write_int32 (face);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 100 - point size
  /**
   * @see <a href="glPointSize.html">glPointSize</a>
   */
  public void point_size (float size) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 100, 8);
      o.write_float (size);
    }
  }


  // glx render opcode 101 - polygon mode
  /**
   * @see <a href="glPolygonMode.html">glPolygonMode</a>
   */
  public void polygon_mode (int face, int mode) {
    render_2i (101, face, mode);
  }

  // glx render opcode 102 - polygon stipple
  /**
   * @see <a href="glPolygonMode.html">glPolygonStipple</a>
   */
  public void polygon_stipple (byte [] mask) {    

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 102, 24, mask.length);
      large_render_request.write_int8 ((byte) 0);  // swap bytes
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_pad (2);

      // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.begin_large_parameter();
      for (int i = 0; i < mask.length; i++)
        large_render_request.write_int8(mask [i]);
    }
  }


  // glx render opcode 103 - scissor
  /**
   * @see <a href="glScissor.html">glScissor</a>
   */
  public void scissor (int x, int y, int width, int height) {
    render_4i (103, x, y, width, height);
  } 

  
  // glx render opcode 104 - shade model
  /**
   * @see <a href="glShadeModel.html">glShadeModel</a>
   */
  public void shade_model (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 104, 8);
      o.write_int32 (mode);
    }
  }

  // glx render opcode 105 - texture parameterf
  /**
   * @see <a href="glTexParameterf.html">glTexParameterf</a>
   */
  public void tex_parameterf (int target, int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 105, 16);
      o.write_int32 (target);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 106 - texture parameterfv
  /**
   * @see <a href="glTexParameterfv.html">glTexParameterfv</a>
   */
  public void tex_parameterfv (int target, int pname, 
    float [] params) {

    int n = 0;
    
    switch (pname) {    
    case TEXTURE_MIN_FILTER:    // fall through
    case TEXTURE_MAG_FILTER:    // fall through
    case TEXTURE_WRAP_S:        // fall through
    case TEXTURE_WRAP_T:        // fall through
    case TEXTURE_PRIORITY: n = 1; break;
    case TEXTURE_BORDER_COLOR: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 106, 12 + 4 * n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 107 - texture parameteri
  /**
   * @see <a href="glTexParameteri.html">glTexParameteri</a>
   */
  public void tex_parameteri (int target, int pname, int param) {
    render_3i (107, target, pname, param);
  }


  // glx render opcode 108 - texture parameteriv
  /**
   * @see <a href="glTexParameteriv.html">glTexParameteriv</a>
   */
  public void tex_parameteriv (int target, int pname, int [] params) {

    int n = 0;
    
    switch (pname) {    
    case TEXTURE_MIN_FILTER:    // fall through
    case TEXTURE_MAG_FILTER:    // fall through
    case TEXTURE_WRAP_S:        // fall through
    case TEXTURE_WRAP_T:        // fall through
    case TEXTURE_PRIORITY: n = 1; break;
    case TEXTURE_BORDER_COLOR: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 108, 12 + 4 * n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 109 - tex image 1d
  /**
   * @see <a href="glTexImage1d.html">glTexImage1D</a>
   */
  public void tex_image_1d (int target, int level, int internal_format,
    int width, int border, int format, int type, byte [] pixels) {   

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin (o, 109, 52, pixels.length);
      large_render_request.write_int8 ((byte) 0);  // swap bytes
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_pad (2);

      // FIXME GL_ABGR_EXT?

      // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.write_int32 (target);
      large_render_request.write_int32 (level);
      large_render_request.write_int32 (internal_format);
      large_render_request.write_int32 (width);
      large_render_request.write_pad (4);
      large_render_request.write_int32 (border);
      large_render_request.write_int32 (format);
      large_render_request.write_int32 (type);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < pixels.length; i++)
        large_render_request.write_int8 (pixels [i]);
      large_render_request.write_pad (RequestOutputStream.pad(pixels.length));
    }
  }


  // glx render opcode 110 - tex image 2d
  /**
   * @see <a href="glTexImage2d.html">glTexImage2D</a>
   */
  public void tex_image_2d (int target, int level, int internal_format,
    int width, int height, int border, int format, int type, 
    byte[] pixels) {   

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin (o, 110, 56, pixels.length);
      large_render_request.write_int8 ((byte) 0);  // swap bytes
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_pad (2);

      // FIXME GL_ABGR_EXT?

      // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.write_int32 (target);
      large_render_request.write_int32 (level);
      large_render_request.write_int32 (internal_format);
      large_render_request.write_int32 (width);
      large_render_request.write_int32 (height);
      large_render_request.write_int32 (border);
      large_render_request.write_int32 (format);
      large_render_request.write_int32 (type);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < pixels.length; i++)
        large_render_request.write_int8 (pixels [i]);
      large_render_request.write_pad (RequestOutputStream.pad(pixels.length));
    }
  }


  // glx render opcode 111 - texture envf
  /**
   * @see <a href="glTexEnvf.html">glTexEnvf</a>
   */
  public void tex_envf (int target, int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 111, 16);
      o.write_int32 (target);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 112 - texture envfv
  /**
   * @see <a href="glTexEnvfv.html">glTexEnvfv</a>
   */
  public void tex_envfv (int target, int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case TEXTURE_ENV_MODE: n = 1; break;
    case TEXTURE_ENV_COLOR: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 112, 12 + 4 * n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 113 - texture envi
  /**
   * @see <a href="glTexEnvi.html">glTexEnvi</a>
   */
  public void tex_envi (int target, int pname, int param) {
    render_3i (113, target, pname, param);
  }


  // glx render opcode 114 - texture enviv
  /**
   * @see <a href="glTexEnviv.html">glTexEnviv</a>
   */
  public void tex_enviv (int target, int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case TEXTURE_ENV_MODE: n = 1; break;
    case TEXTURE_ENV_COLOR: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 114, 12 + 4 * n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 115 - texture gend
  /**
   * @see <a href="glTexGend.html">glTexGend</a>
   */
  public void tex_gend (int coord, int pname, double param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 115, 20);
      o.write_int32 (coord);
      o.write_int32 (pname);
      o.write_double (param);
    }
  }


  // glx render opcode 116 - texture gendv
  /**
   * @see <a href="glTexGendv.html">glTexGendv</a>
   */
  public void tex_gendv (int coord, int pname, double [] params) {
    int n = 0;

    switch (pname) {
    case TEXTURE_GEN_MODE: n = 1; break;
    case OBJECT_PLANE:        // fall through
    case EYE_PLANE: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 116, 12 + 8 * n);
      o.write_int32 (coord);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_double (params [i]);
    }
  }


  // glx render opcode 117 - texture genf
  /**
   * @see <a href="glTexGenf.html">glTexGenf</a>
   */
  public void tex_genf (int coord, int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 117, 16);
      o.write_int32 (coord);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 118 - texture genfv
  /**
   * @see <a href="glTexGenfv.html">glTexGenfv</a>
   */
  public void tex_genfv (int coord, int pname, float [] params) {
    int n = 0;

    switch (pname) {
    case TEXTURE_GEN_MODE: n = 1; break;
    case OBJECT_PLANE:        // fall through
    case EYE_PLANE: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 118, 12 + 4 * n);
      o.write_int32 (coord);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params [i]);
    }
  }


  // glx render opcode 119 - texture geni
  /**
   * @see <a href="glTexGeni.html">glTexGeni</a>
   */
  public void tex_geni (int coord, int pname, int param) {
    render_3i (119, coord, pname, param);
  }


  // glx render opcode 120 - texture geniv
  /**
   * @see <a href="glTexGeniv.html">glTexGeniv</a>
   */
  public void tex_geniv (int coord, int pname, int [] params) {
    int n = 0;

    switch (pname) {
    case TEXTURE_GEN_MODE: n = 1; break;
    case OBJECT_PLANE:          // fall through
    case EYE_PLANE: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 120, 12 + 4 * n);
      o.write_int32 (coord);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params [i]);
    }
  }


  // glx render opcode 121 - init names
  /**
   * @see <a href="glInitNames.html">glInitNames</a>
   */
  public void init_names () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 121, 4);
    }
  }


  // glx render opcode 122 - load name
  /**
   * @see <a href="glLoadName.html">glLoadName</a>
   */
  public void load_name (int name) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 122, 8);
      o.write_int32 (name);
    }
  }


  // glx render opcode 123 - pass through
  /**
   * @see <a href="glPassThrough.html">glPassThrough</a>
   */
  public void pass_through  (float token) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 123, 8);
      o.write_float (token);
    }
  }


  // glx render opcode 124 - pop name
  /**
   * @see <a href="glPopName.html">glPopName</a>
   */
  public void pop_name () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 124, 4);
    }
  }


  // glx render opcode 125 - push name
  /**
   * @see <a href="glPushName.html">glPushName</a>
   */
  public void push_name (int name) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 125, 8);
      o.write_int32 (name);
    }
  }


  // glx render opcode 126 - draw buffer
  /**
   * @see <a href="glClear.html">glDrawBuffer</a>
   */
  public void draw_buffer (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 126, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 127 - clear
  /**
   * @see <a href="glClear.html">glClear</a>
   */
  public void clear (int mask) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 127, 8);
      o.write_int32 (mask);
    }
  }


  // glx render opcode 128 - clear accum
  /**
   * @see <a href="glClearAccum.html">glClearAccum</a>
   */
  public void clear_accum (float red, float green, float blue, float alpha) {
    render_4f (128, red, green, blue, alpha);
  }


  // glx render opcode 129 - clear index
  /**
   * @see <a href="glClearIndex.html">glClearIndex</a>
   */
  public void clear_index (float c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 129, 8);
      o.write_float (c);
    }
  }


  // glx render opcode 130 - clear color
  /**
   * @see <a href="glClearColor.html">glClearColor</a>
   */
  public void clear_color (float red, float green, float blue, float alpha) {
    render_4f (130, red, green, blue, alpha);
  }


  // glx render opcode 131 - clear stencil
  /**
   * @see <a href="glClearStencil.html">glClearStencil</a>
   */
  public void clear_stencil (int s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 131, 8);
      o.write_int32 (s);
    }
  }


  // glx render opcode 132 - clear depth
  /**
   * @see <a href="glClearDepth.html">glClearDepth</a>
   */
  public void clear_depth (double depth) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 132, 12);
      o.write_double (depth);
    }
  }


  // glx render opcode 133 - stencil mask
  /**
   * @see <a href="glStencilMask.html">glStencilMask</a>
   */
  public void stencil_mask (int mask) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 133, 8);
      o.write_int32 (mask);
    }
  }


  // glx render opcode 134 - color mask
  /**
   * @see <a href="glColorMask.html">glColorMask</a>
   */
  public void color_mask (boolean red, boolean green, boolean blue,
                          boolean alpha) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 134, 8);
      o.write_bool (red);
      o.write_bool (green);
      o.write_bool (blue);
      o.write_bool (alpha);
    }
  }


  // glx render opcode 135 - depth mask
  /**
   * @see <a href="glDepthMask.html">glDepthMask</a>
   */
  public void depth_mask (boolean flag) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 135, 8);
      o.write_bool (flag);
      o.write_pad (3);
    }
  }


  // glx render opcode 136 - index mask
  /**
   * @see <a href="glIndexMask.html">glIndexMask</a>
   */
  public void index_mask (int mask) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 136, 8);
      o.write_int32 (mask);
    }
  }


  // glx render opcode 137 - accum
  /**
   * @see <a href="glAccum.html">glAccum</a>
   */
  public void accum (int op, float value) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 137, 12);
      o.write_int32 (op);
      o.write_float (value);
    }
  }
  


  // glx render opcode 138 - disable
  /**
   * @see <a href="glDisable.html">glDisable</a>
   */
  public void disable (int capability) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 138, 8);
      o.write_int32 (capability);
    }
  }


  // glx render opcode 139 - enable
  /**
   * @see <a href="glEnable.html">glEnable</a>
   */
  public void enable (int capability) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 139, 8);
      o.write_int32 (capability);
    }
  }


  // glx render opcode 141 - pop attrib
  /**
   * @see <a href="glPopAttrib.html">glPopAttrib</a>
   */
  public void pop_attrib () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 141, 4);
    }
  }


  // glx render opcode 142 - push attrib
  /**
   * @see <a href="glPushAttrib.html">glPushAttrib</a>
   */
  public void push_attrib (int mask) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 142, 8);
      o.write_int32 (mask);
    }
  }


  // glx render opcode 143 - map1d
  /**
   * @see <a href="glMap1d.html">glMap1d</a>
   */
  public void map1d (int target, double u1, double u2, int stride, int order,
                     double [] points) {

    int k = 0;

    switch (target) {
    case MAP1_INDEX:            // fall through
    case MAP1_TEXTURE_COORD_1: k = 1; break;
    case MAP1_TEXTURE_COORD_2: k = 2; break;
    case MAP1_NORMAL:           // fall through
    case MAP1_TEXTURE_COORD_3:  // fall through
    case MAP1_VERTEX_3: k = 3; break;
    case MAP1_COLOR_4:          // fall through
    case MAP1_TEXTURE_COORD_4:  // fall through
    case MAP1_VERTEX_4: k = 4; break;
    }


    int n = order * k * 8;

    byte[] data = new byte [n];
    for (int i=0; i<order; i++) {
      for (int j=0; j<k; j++) {
        write_double (data, i * 8, points [i*stride + j]);
      }
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 143, 28, n);
      large_render_request.write_float64 (u1);
      large_render_request.write_float64 (u2);
      large_render_request.write_int32 (target);
      large_render_request.write_int32 (order);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < data.length; i++)
        large_render_request.write_int8 (data [i]);
      large_render_request.write_pad (RequestOutputStream.pad(data.length));
    }
  }


  // glx render opcode 144 - map1f
  /**
   * @see <a href="glMap1f.html">glMap1f</a>
   */
  public void map1f (int target, float u1, float u2, int stride, int order,
                     float [] points) {

    int k = 0;

    switch (target) {
    case MAP1_INDEX:            // fall through
    case MAP1_TEXTURE_COORD_1: k = 1; break;
    case MAP1_TEXTURE_COORD_2: k = 2; break;
    case MAP1_NORMAL:           // fall through
    case MAP1_TEXTURE_COORD_3:  // fall through
    case MAP1_VERTEX_3: k = 3; break;
    case MAP1_COLOR_4:          // fall through
    case MAP1_TEXTURE_COORD_4:  // fall through
    case MAP1_VERTEX_4: k = 4; break;
    }


    int n = order * k * 4;

    byte[] data = new byte [n];
    for (int i=0; i<order; i++) {
      for (int j=0; j<k; j++) {
        write_float32 (data, i * 8, points [i*stride + j]);
      }
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 144, 20, n);
      large_render_request.write_int32 (target);
      large_render_request.write_float32 (u1);
      large_render_request.write_float32 (u2);
      large_render_request.write_int32 (order);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < data.length; i++)
        large_render_request.write_int8 (data [i]);
      large_render_request.write_pad (RequestOutputStream.pad(data.length));
    }
  }


  // glx render opcode 145 - map2d
  /**
   * @see <a href="glMap2d.html">glMap2d</a>
   */
  public void map2d (int target, double u1, double u2, int ustride, int uorder,
                     double v1, double v2, int vstride, int vorder,
                     double [] points) {

    int k = 0;

    switch (target) {
    case MAP2_INDEX:            // fall through
    case MAP2_TEXTURE_COORD_1: k = 1; break;
    case MAP2_TEXTURE_COORD_2: k = 2; break;
    case MAP2_NORMAL:           // fall through
    case MAP2_TEXTURE_COORD_3:  // fall through
    case MAP2_VERTEX_3: k = 3; break;
    case MAP2_COLOR_4:          // fall through
    case MAP2_TEXTURE_COORD_4:  // fall through
    case MAP2_VERTEX_4: k = 4; break;
    }

    int n = vorder * uorder * k * 8;

    byte[] data = new byte [n];
    for (int i=0; i<vorder; i++) {
      for (int j=0; j<uorder; j++) {
        for (int m=0; m<k; m++) {
          write_double (data, i * 8, points [i*ustride + j*vstride + m]);
        }
      }
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 145, 48, n);
      large_render_request.write_float64 (u1);
      large_render_request.write_float64 (u2);
      large_render_request.write_float64 (v1);
      large_render_request.write_float64 (v2);
      large_render_request.write_int32 (target);
      large_render_request.write_int32 (uorder);
      large_render_request.write_int32 (vorder);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < data.length; i++)
        large_render_request.write_int8 (data [i]);
      large_render_request.write_pad (RequestOutputStream.pad(data.length));
    }

  }


  // glx render opcode 146 - map2f
  /**
   * @see <a href="glMap2f.html">glMap2f</a>
   */
  public void map2f (int target, float u1, float u2, int ustride, int uorder,
                     float v1, float v2, int vstride, int vorder,
                     float [] points) {

    int k = 0;

    switch (target) {
    case MAP2_INDEX:            // fall through
    case MAP2_TEXTURE_COORD_1: k = 1; break;
    case MAP2_TEXTURE_COORD_2: k = 2; break;
    case MAP2_NORMAL:           // fall through
    case MAP2_TEXTURE_COORD_3:  // fall through
    case MAP2_VERTEX_3: k = 3; break;
    case MAP2_COLOR_4:          // fall through
    case MAP2_TEXTURE_COORD_4:  // fall through
    case MAP2_VERTEX_4: k = 4; break;
    }

    int n = vorder * uorder * k * 4;

    byte[] data = new byte [n];
    for (int i=0; i<vorder; i++) {
      for (int j=0; j<uorder; j++) {
        for (int m=0; m<k; m++) {
          write_float32 (data, i * 8, points [i*ustride + j*vstride + m]);
        }
      }
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 146, 32, n);
      large_render_request.write_int32 (target);
      large_render_request.write_float32 (u1);
      large_render_request.write_float32 (u2);
      large_render_request.write_int32 (uorder);
      large_render_request.write_float32 (v1);
      large_render_request.write_float32 (v2);
      large_render_request.write_int32 (vorder);
      large_render_request.begin_large_parameter ();
      for (int i = 0; i < data.length; i++)
        large_render_request.write_int8 (data [i]);
      large_render_request.write_pad (RequestOutputStream.pad(data.length));
    }
  }


  // glx render opcode 147 - map grid1d
  /**
   * @see <a href="glMapGrid1d.html">glMapGrid1d</a>
   */
  public void map_grid1d (int un, double u1, double u2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 147, 24);
      o.write_double (u1);
      o.write_double (u2);
      o.write_int32 (un);
    }
  }


  // glx render opcode 148 - map grid1f
  /**
   * @see <a href="glMapGrid1f.html">glMapGrid1f</a>
   */
  public void map_grid1f (int un, float u1, float u2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 148, 16);
      o.write_float (u1);
      o.write_float (u2);
      o.write_int32 (un);
    }
  }


  // glx render opcode 149 - map grid2d
  /**
   * @see <a href="glMapGrid2d.html">glMapGrid2d</a>
   */
  public void map_grid2d (int un, double u1, double u2, int vn, double v1,
                          double v2) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 149, 44);
      o.write_double (u1);
      o.write_double (u2);
      o.write_double (v1);
      o.write_double (v2);
      o.write_int32 (un);
      o.write_int32 (vn);
    }
  }


  // glx render opcode 150 - map grid2f
  /**
   * @see <a href="glMapGrid2f.html">glMapGrid2f</a>
   */
  public void map_grid2f (int un, float u1, float u2, int vn, float v1,
                          float v2) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 150, 28);
      o.write_int32 (un);
      o.write_float (u1);
      o.write_float (u2);
      o.write_int32 (vn);
      o.write_float (v1);
      o.write_float (v2);
    }
  }


  // glx render opcode 151 - eval coord1dv
  /**
   * @see <a href="glEvalCoord1d.html">glEvalCoord1d</a>
   */
  public void eval_coord1d (double u) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 151, 12);
      o.write_double (u);
    }
  }


  // glx render opcode 152 - eval coord1df
  /**
   * @see <a href="glEvalCoord1f.html">glEvalCoord1f</a>
   */
  public void eval_coord1f (float u) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 152, 8);
      o.write_float (u);
    }
  }


  // glx render opcode 153 - eval coord2dv
  /**
   * @see <a href="glEvalCoord2d.html">glEvalCoord2d</a>
   */
  public void eval_coord2d (double u, double v) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 153, 20);
      o.write_double (u);
      o.write_double (v);
    }
  }


  // glx render opcode 154 - eval coord2df
  /**
   * @see <a href="glEvalCoord2f.html">glEvalCoord2f</a>
   */
  public void eval_coord2f (float u, float v) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 154, 12);
      o.write_float (u);
      o.write_float (v);
    }
  }


  // glx render opcode 155 - eval mesh1
  /**
   * @see <a href="glEvalMesh1.html">glEvalMesh1</a>
   */
  public void eval_mesh1 (int mode, int i1, int i2) {
    render_3i (155, mode, i1, i2);
  }


  // glx render opcode 156 - eval point1
  /**
   * @see <a href="glEvalPoint1.html">glEvalPoint1</a>
   */
  public void eval_point1 (int i) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 156, 8);
      o.write_int32 (i);
    }
  }


  // glx render opcode 157 - eval mesh2
  /**
   * @see <a href="glEvalMesh2.html">glEvalMesh2</a>
   */
  public void eval_mesh2 (int mode, int i1, int i2, int j1, int j2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 157, 24);
      o.write_int32 (mode);
      o.write_int32 (i1);
      o.write_int32 (i2);
      o.write_int32 (j1);
      o.write_int32 (j2);
    }
  }


  // glx render opcode 158 - eval point2
  /**
   * @see <a href="glEvalPoint2.html">glEvalPoint2</a>
   */
  public void eval_point2 (int i, int j) {
    render_2i (158, i, j);
  }


  // glx render opcode 159 - alpha function
  /**
   * @see <a href="glAlphaFunc.html">glAlphaFunc</a>
   */
  public void alpha_func (int func, int ref) {
    render_2i (159, func, ref);
  }


  // glx render opcode 160 - blend function
  /**
   * @see <a href="glAlphaFunc.html">glBlendFunc</a>
   */
  public void blend_func (int sfactor, int dfactor) {
    render_2i (160, sfactor, dfactor);
  }


  // glx render opcode 161 - logic op
  /**
   * @see <a href="glLogicOp.html">glLogicOp</a>
   */
  public void logic_op (int opcode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 161, 8);
      o.write_int32 (opcode);
    }
  }


  // glx render opcode 162 - stencil function
  /**
   * @see <a href="glStencilFunc.html">glStencilFunc</a>
   */
  public void stencil_func (int func, int ref, int mask) {
    render_3i (162, func, ref, mask);
  }


  // glx render opcode 163 - stencil op
  /**
   * @see <a href="glStencilOp.html">glStencilOp</a>
   */
  public void stencil_op (int fail, int zfail, int zpass) {
    render_3i (163, fail, zfail, zpass);
  }


  // glx render opcode 164 - depth function
  /**
   * @see <a href="glDepthFunc.html">glDepthFunc</a>
   */
  public void depth_func (int func) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 164, 8);
      o.write_int32 (func);
    }
  }


  // glx render opcode 165 - pixel zoom
  /**
   * @see <a href="glPixelZoom.html">glPixelZoom</a>
   */
  public void pixel_zoom (float xfactor, float yfactor) {
    render_2f (165, xfactor, yfactor);
  }


  // glx render opcode 166 - pixel transferf
  /**
   * @see <a href="glPixelTransferf.html">glPixelTransferf</a>
   */
  public void pixel_transferf (int pname, float param) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 166, 12);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 167 - pixel transferi
  /**
   * @see <a href="glPixelTransferi.html">glPixelTransferi</a>
   */
  public void pixel_transferi (int pname, int param) {
    render_2i (167, pname, param);
  }


  // glx render opcode 171 - read buffer
  /**
   * @see <a href="glReadBuffer.html">glReadBuffer</a>
   */
  public void read_buffer (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 171, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 172 - copy pixels
  /**
   * @see <a href="glCopyPixels.html">glCopyPixels</a>
   */
  public void copy_pixels (int x, int y, int width, int height, int type) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 172, 24);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (height);
      o.write_int32 (type);
    }
  } 


  // glx render opcode 173 - draw pixels
  /**
   * @see <a href="glDrawPixels.html">glDrawPixels</a>
   */
  public void draw_pixels (int width, int height, int format, int type,
                           byte[] pixels) {
    
    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 173, 40, pixels.length);
      large_render_request.write_int8 ((byte) 0);  // swap bytes
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_pad (2);

      // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.write_int32 (width);
      large_render_request.write_int32 (height);
      large_render_request.write_int32 (format);
      large_render_request.write_int32 (type);

      large_render_request.begin_large_parameter ();
      for (int i = 0; i < pixels.length; i++)
        large_render_request.write_int8 (pixels [i]);
      large_render_request.write_pad (RequestOutputStream.pad(pixels.length));
    }
  }


  // glx render opcode 174 - depth range
  /**
   * @see <a href="glDepthRange.html">glDepthRange</a>
   */
  public void depth_range (double near, double far) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 174, 20);
      o.write_double (near);
      o.write_double (far);
    }
  }


  // glx render opcode 175 - frustum
  /**
   * @see <a href="glFrustum.html">glFrustum</a>
   */
  public void frustum (double left, double right, double bottom, double top,
                       double near, double far) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 175, 52);
      o.write_double (left);
      o.write_double (right);
      o.write_double (bottom);
      o.write_double (top);
      o.write_double (near);
      o.write_double (far);
    }
  }


  // glx render opcode 176 - load identity
  /**
   * @see <a href="glLoadIdentity.html">glLoadIdentity</a>
   */
  public void load_identity () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 176, 4);
    }
  }


  // glx render opcode 177 - load matrixf
  /**
   * @see <a href="glLoadMatrixf.html">glLoadMatrixf</a>
   */
  public void load_matrixf (float [] matrix) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 177, 68);
      for (int i = 0; i < 16; i++)
        o.write_float (matrix [i]);
    }
  }


  // glx render opcode 178 - load matrixd
  /**
   * @see <a href="glLoadMatrixd.html">glLoadMatrixd</a>
   */
  public void load_matrixd (double [] matrix) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 178, 132);
      for (int i = 0; i < 16; i++)
        o.write_double (matrix [i]);
    }
  }


  // glx render opcode 179 - matrix mode
  /**
   * @see <a href="glMatrixMode.html">glMatrixMode</a>
   */
  public void matrix_mode (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 179, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 180 - mult matrixf
  /**
   * @see <a href="glMultMatrixf.html">glMultMatrixf</a>
   */
  public void mult_matrixf (float [] matrix) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 180, 68);
      for (int i = 0; i < 16; i++)
        o.write_float (matrix [i]);
    }
  } 


  // glx render opcode 181 - mult matrixd
  /**
   * @see <a href="glMultMatrixd.html">glMultMatrixd</a>
   */
  public void mult_matrixd (double [] matrix) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 181, 132);
      for (int i = 0; i < 16; i++)
        o.write_double (matrix [i]);
    }
  } 

  
  // glx render opcode 182 - ortho
  /**
   * @see <a href="glOrtho.html">glOrtho</a>
   */
  public void ortho (double left, double right, double bottom, double top,
                     double near, double far) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 182, 52);
      o.write_double (left);
      o.write_double (right);
      o.write_double (bottom);
      o.write_double (top);
      o.write_double (near);
      o.write_double (far);
    }
  }


  // glx render opcode 183 - pop matrix
  /**
   * @see <a href="glPopMatrix.html">glPopMatrix</a>
   */
  public void pop_matrix () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 183, 4);
    }
  }


  // glx render opcode 184 - push matrix
  /**
   * @see <a href="glPushMatrix.html">glPushMatrix</a>
   */
  public void push_matrix () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 184, 4);
    }
  }


  // glx render opcode 185 - rotated
  /**
   * @see <a href="glRotated.html">glRotated</a>
   */
  public void rotated (double angle, double x, double y, double z) {
    render_4d (185, angle, x, y, z);
  } 


  // glx render opcode 186 - rotatef
  /**
   * @see <a href="glRotatef.html">glRotatef</a>
   */
  public void rotatef (float angle, float x, float y, float z) {
    render_4f (186, angle, x, y, z);
  } 


  // glx render opcode 187 - scaled
  /**
   * @see <a href="glScaled.html">glScaled</a>
   */
  public void scaled (double x, double y, double z) {
    render_3d (187, x, y, z);
  } 


  // glx render opcode 188 - scalef
  /**
   * @see <a href="glScalef.html">glScalef</a>
   */
  public void scalef (float x, float y, float z) {
    render_3f (188, x, y, z);
  } 


  // glx render opcode 189 - translated
  /**
   * @see <a href="glTranslated.html">glTranslated</a>
   */
  public void translated (double x, double y, double z) {
    render_3d (189, x, y, z);
  } 


  // glx render opcode 190 - translatef
  /**
   * @see <a href="glTranslatef.html">glTranslatef</a>
   */
  public void translatef (float x, float y, float z) {
    render_3f (190, x, y, z);
  } 


  // glx render opcode 191 - viewport
  /**
   * @see <a href="glViewport.html">glViewport</a>
   */
  public void viewport (int x, int y, int width, int height) {
    render_4i (191, x, y, width, height);
  } 


  // glx render opcode 193 - draw arrays
  /**
   * @see <a href="glDrawArrays.html">glDrawArrays</a>
   */
  public void draw_arrays (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 193, 16);
      o.write_int32 (mode);
    }
  }



  // glx render opcode 194 - indexubv
  /**
   * @see <a href="glIndexub.html">glIndexub</a>
   */
  public void indexub (boolean c) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 194, 8);
      o.write_bool (c);
      o.write_pad (3);
    }
  }


  // glx render opcode 196 - copy color sub table
  /**
   * @see <a href="glCopyColorSubTable.html">glCopyColorSubTable</a>
   */
  public void copy_color_sub_table (int target, int start, int x, int y,
                                    int width) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 196, 24);
      o.write_int32 (target);
      o.write_int32 (start);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
    }
  } 


  // glx render opcode 197 - active texture arb
  /**
   * @see <a href="glActiveTextureARB.html">glActiveTextureARB</a>
   */
  public void active_texture_arb (int texture) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 197, 8);
      o.write_int32 (texture);
    }
  }
  
  
  // glx render opcode 198 - multi-texture coord1dv arb
  /**
   * @see <a href="glMultiTexCoord1dARB.html">glMultiTexCoord1dARB</a>
   */
  public void multi_tex_coord1d_arb (int target, double s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 198, 16);
      o.write_int32 (target);
      o.write_double (s);
    }
  } 


  // glx render opcode 199 - multi-texture coord1fv arb
  /**
   * @see <a href="glMultiTexCoord1fARB.html">glMultiTexCoord1fARB</a>
   */
  public void multi_tex_coord1f_arb (int target, float s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 199, 12);
      o.write_int32 (target);
      o.write_float (s);
    }
  } 


  // glx render opcode 200 - multi-texture coord1iv arb
  /**
   * @see <a href="glMultiTexCoord1iARB.html">glMultiTexCoord1iARB</a>
   */
  public void multi_tex_coord1i_arb (int target, int s) {
    render_2i (200, target, s);
  } 


  // glx render opcode 201 - multi-texture coord1sv arb
  /**
   * @see <a href="glMultiTexCoord1fARB.html">glMultiTexCoord1fARB</a>
   */
  public void multi_tex_coord1s_arb (int target, int s) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 201, 12);
      o.write_int32 (target);
      o.write_int16 (s);
      o.write_pad (2);
    }
  } 


  // glx render opcode 202 - multi-texture coord2dv arb
  /**
   * @see <a href="glMultiTexCoord2dARB.html">glMultiTexCoord2dARB</a>
   */
  public void multi_tex_coord2d_arb (int target, double s, double t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 202, 24);
      o.write_int32 (target);
      o.write_double (s);
      o.write_double (t);
    }
  } 


  // glx render opcode 203 - multi-texture coord2fv arb
  /**
   * @see <a href="glMultiTexCoord2fARB.html">glMultiTexCoord2fARB</a>
   */
  public void multi_tex_coord2f_arb (int target, float s, float t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 203, 16);
      o.write_int32 (target);
      o.write_float (s);
      o.write_float (t);
    }
  } 


  // glx render opcode 204 - multi-texture coord2iv arb
  /**
   * @see <a href="glMultiTexCoord2iARB.html">glMultiTexCoord2iARB</a>
   */
  public void multi_tex_coord2i_arb (int target, int s, int t) {
    render_3i (204, target, s, t);
  } 


  // glx render opcode 205 - multi-texture coord2sv arb
  /**
   * @see <a href="glMultiTexCoord2fARB.html">glMultiTexCoord2fARB</a>
   */
  public void multi_tex_coord2s_arb (int target, int s, int t) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 205, 12);
      o.write_int32 (target);
      o.write_int16 (s);
      o.write_int16 (t);
    }
  } 


  // glx render opcode 206 - multi-texture coord3dv arb
  /**
   * @see <a href="glMultiTexCoord3dARB.html">glMultiTexCoord3dARB</a>
   */
  public void multi_tex_coord3d_arb (int target, double s, double t,
                                     double r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 206, 32);
      o.write_int32 (target);
      o.write_double (s);
      o.write_double (t);
      o.write_double (r);
    }
  } 


  // glx render opcode 207 - multi-texture coord3fv arb
  /**
   * @see <a href="glMultiTexCoord3fARB.html">glMultiTexCoord3fARB</a>
   */
  public void multi_tex_coord3f_arb (int target, float s, float t, float r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 207, 20);
      o.write_int32 (target);
      o.write_float (s);
      o.write_float (t);
      o.write_float (r);
    }
  } 


  // glx render opcode 208 - multi-texture coord3iv arb
  /**
   * @see <a href="glMultiTexCoord3iARB.html">glMultiTexCoord3iARB</a>
   */
  public void multi_tex_coord3i_arb (int target, int s, int t, int r) {
    render_4i (208, target, s, t, r);
  } 


  // glx render opcode 209 - multi-texture coord3sv arb
  /**
   * @see <a href="glMultiTexCoord3fARB.html">glMultiTexCoord3fARB</a>
   */
  public void multi_tex_coord3s_arb (int target, int s, int t, int r) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 209, 16);
      o.write_int32 (target);
      o.write_int16 (s);
      o.write_int16 (t);
      o.write_int16 (r);
      o.write_pad (2);
    }
  } 


  // glx render opcode 210 - multi-texture coord4dv arb
  /**
   * @see <a href="glMultiTexCoord4dARB.html">glMultiTexCoord4dARB</a>
   */
  public void multi_tex_coord4d_arb (int target, double s, double t, 
                                     double r, double q) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 210, 40);
      o.write_int32 (target);
      o.write_double (s);
      o.write_double (t);
      o.write_double (r);
      o.write_double (q);
    }
  } 


  // glx render opcode 211 - multi-texture coord4fv arb
  /**
   * @see <a href="glMultiTexCoord4fARB.html">glMultiTexCoord4fARB</a>
   */
  public void multi_tex_coord4f_arb (int target, float s, float t, 
                                     float r, float q) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 211, 24);
      o.write_int32 (target);
      o.write_float (s);
      o.write_float (t);
      o.write_float (r);
      o.write_float (q);
    }
  } 


  // glx render opcode 212 - multi-texture coord4iv arb
  /**
   * @see <a href="glMultiTexCoord4iARB.html">glMultiTexCoord4iARB</a>
   */
  public void multi_tex_coord4i_arb (int target, int s, int t, 
    int r, int q) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 212, 24);
      o.write_int32 (target);
      o.write_int32 (s);
      o.write_int32 (t);
      o.write_int32 (r);
      o.write_int32 (q);
    }
  } 


  // glx render opcode 213 - multi-texture coord4sv arb
  /**
   * @see <a href="glMultiTexCoord4fARB.html">glMultiTexCoord4fARB</a>
   */
  public void multi_tex_coord4s_arb (int target, int s, int t, 
                                     int r, int q) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 213, 16);
      o.write_int32 (target);
      o.write_int16 (s);
      o.write_int16 (t);
      o.write_int16 (r);
      o.write_int16 (q);
    }
  } 


  // glx render opcode 2054 - color table parameterfv
  /**
   * @see <a href="glColorTableParameterf.html">
   * glColorTableParameterf</a>
   */
  public void color_table_parameterfv (int target, int pname, 
    float [] params) {

    int n = 0;

    switch (pname) {
    case COLOR_TABLE_SCALE:     // fall through
    case COLOR_TABLE_BIAS: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 2054, 12+4*n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++) {
        o.write_float (params[i]);
      }
    }
  }



  // glx render opcode 2055 - color table parameteriv
  /**
   * @see <a href="glColorTableParameteri.html">
   * glColorTableParameterf</a>
   */
  public void color_table_parameteriv (int target, int pname, 
    int [] params) {

    int n = 0;

    switch (pname) {
    case COLOR_TABLE_SCALE:     // fall through
    case COLOR_TABLE_BIAS: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 2055, 12+4*n);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++) {
        o.write_int32 (params[i]);
      }
    }
  }


  // glx render opcode 2056 - copy color table
  /**
   * @see <a href="glCopyColorTable.html">glCopyColorTable</a>
   */
  public void copy_color_table (int target, int internal_format, 
    int x, int y, int width) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 2056, 24);
      o.write_int32 (target);
      o.write_int32 (internal_format);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
    }
  } 


  // glx render opcode 4096 - blend color
  /**
   * @see <a href="glBlendColor.html">glBlendColor</a>
   */
  public void blend_color (float red, float green, 
    float blue, float alpha) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4096, 20);
      o.write_float (red);
      o.write_float (green);    
      o.write_float (blue);
      o.write_float (alpha);
    }
  }


  // glx render opcode 4097 - blend equation
  /**
   * @see <a href="glBlendEquation.html">glBlendEquation</a>
   */
  public void blend_equation (int mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4097, 8);
      o.write_int32 (mode);
    }
  }


  // glx render opcode 4098 - polygon offset
  /**
   * @see <a href="glPolygonOffset.html">glPolygonOffset</a>
   */
  public void polygon_offset (float factor, float units) {
    // TODO 1.3: opcode = 192
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4098, 12);
      o.write_float (factor);
      o.write_float (units);
    }
  }


  // glx render opcode 4100 - tex subimage 2d
  /**
   * @see <a href="glTexSubimage2d.html">glTexSubimage2D</a>
   */
  public void tex_subimage_2d (int target, int level, 
    int xoffset, int yoffset, int width, int height,
    int format, int type, byte[] pixels) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      large_render_request.begin(o, 4100, 60, pixels.length);
      large_render_request.write_int8 ((byte) 0);  // swap bytes
      large_render_request.write_bool (false); // java = msb = !lsb_first
      large_render_request.write_pad (2);

    // FIXME work with other cases??
      large_render_request.write_int32 (0);  // row len
      large_render_request.write_int32 (0);  // skip rows
      large_render_request.write_int32 (0);  // skip pixels
      large_render_request.write_int32 (1);  // alignment

      large_render_request.write_int32 (target);
      large_render_request.write_int32 (level);
      large_render_request.write_int32 (xoffset);
      large_render_request.write_int32 (yoffset);
      large_render_request.write_int32 (width);
      large_render_request.write_int32 (height);
      large_render_request.write_int32 (format);
      large_render_request.write_int32 (type);
      large_render_request.write_pad(4);

      large_render_request.begin_large_parameter ();
      for (int i = 0; i < pixels.length; i++)
        large_render_request.write_int8 (pixels [i]);
      large_render_request.write_pad (RequestOutputStream.pad(pixels.length));
    }
  }


  // glx render opcode 4103 - convolution parameterf
  /**
   * @see <a href="glConvolutionParameterf.html">
   * glConvolutionParameterf</a>
   */
  public void convolution_parameterf (int target, 
    int pname, float param) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4103, 16);
      o.write_int32 (target);
      o.write_int32 (pname);
      o.write_float (param);
    }
  }


  // glx render opcode 4104 - convolution parameterfv
  /**
   * @see <a href="glConvolutionParameterf.html">
   * glConvolutionParameterf</a>
   */
  public void convolution_parameterf (int target, int pname, 
    float [] params) {

    int n = 0;

    switch (pname) {
    case CONVOLUTION_BORDER_COLOR: // fall through
    case CONVOLUTION_FORMAT:    // fall through
    case CONVOLUTION_WIDTH:   // fall through
    case CONVOLUTION_HEIGHT:    // fall through
    case MAX_CONVOLUTION_WIDTH: // fall through
    case MAX_CONVOLUTION_HEIGHT: n = 1; break;
    case CONVOLUTION_FILTER_SCALE: // fall through
    case CONVOLUTION_FILTER_BIAS: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4104, 12+4*n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_float (params[i]);
    }
  }


  // glx render opcode 4105 - convolution parameteri
  /**
   * @see <a href="glConvolutionParameteri.html">
   * glConvolutionParameteri</a>
   */
  public void convolution_parameteri (int target, 
    int pname, int param) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4105, 16);
      o.write_int32 (target);
      o.write_int32 (pname);
      o.write_int32 (param);
    }
  }


  // glx render opcode 4106 - convolution parameteriv
  /**
   * @see <a href="glConvolutionParameteri.html">
   * glConvolutionParameteri</a>
   */
  public void convolution_parameteri (int target, int pname, 
    int [] params) {

    int n = 0;

    switch (pname) {
    case CONVOLUTION_BORDER_COLOR: // fall through
    case CONVOLUTION_FORMAT:    // fall through
    case CONVOLUTION_WIDTH:   // fall through
    case CONVOLUTION_HEIGHT:    // fall through
    case MAX_CONVOLUTION_WIDTH: // fall through
    case MAX_CONVOLUTION_HEIGHT: n = 1; break;
    case CONVOLUTION_FILTER_SCALE: // fall through
    case CONVOLUTION_FILTER_BIAS: n = 4; break;
    }

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4106, 12+4*n);
      o.write_int32 (target);
      o.write_int32 (pname);
      for (int i = 0; i < n; i++)
        o.write_int32 (params[i]);
    }
  }


  // glx render opcode 4107 - copy convolution filter1d
  /**
   * @see <a href="glCopyConvolutionFilter1d.html">
   * glCopyConvolutionFilter1d</a>
   */
  public void copy_convolution_filter1d (int target, int internal_format, 
    int x, int y, int width) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4107, 24);
      o.write_int32 (target);
      o.write_int32 (internal_format);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
    }
  }


  // glx render opcode 4108 - copy convolution filter2d
  /**
   * @see <a href="glCopyConvolutionFilter1d.html">
   * glCopyConvolutionFilter1d</a>
   */
  public void copy_convolution_filter2d (int target, int internal_format, 
    int x, int y, int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4108, 28);
      o.write_int32 (target);
      o.write_int32 (internal_format);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (height);
    }
  }


  // glx render opcode 4110 - histogram
  /**
   * @see <a href="glHistogram.html">
   * glHistogram</a>
   */
  public void histogram (int target, int width, int internal_format, 
    boolean sink) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4110, 20);
      o.write_int32 (target);
      o.write_int32 (width);
      o.write_int32 (internal_format);
      o.write_bool (sink);
      o.write_pad (3);
    }
  }


  // glx render opcode 4111 - minmax
  /**
   * @see <a href="glMinmax.html">
   * glMinmax</a>
   */
  public void minmax (int target, int internal_format, boolean sink) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4111, 16);
      o.write_int32 (target);
      o.write_int32 (internal_format);
      o.write_bool(sink);
      o.write_pad (3);
    }
  }


  // glx render opcode 4112 - reset histogram
  /**
   * @see <a href="glResetHistogram.html">glResetHistogram</a>
   */
  public void reset_histogram (int target) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4112, 8);
      o.write_int32 (target);
    }
  }


  // glx render opcode 4113 - reset minmax
  /**
   * @see <a href="glResetMinmax.html">glResetMinmax</a>
   */
  public void reset_minmax (int target) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4113, 8);
      o.write_int32 (target);
    }
  }


  // glx render opcode 4117 - bind texture
  /**
   * @see <a href="glBindTexture.html">glBindTexture</a>
   */
  public void bind_texture (int target, int texture) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4117, 12);
      o.write_int32 (target);
      o.write_int32 (texture);
    }
  }


  // glx render opcode 4118 - prioritize textures
  /**
   * @see <a href="glPrioritizeTextures.html">glPrioritizeTextures</a>
   */
  public void prioritize_textures (int [] textures, float [] priorities) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4118, 8+textures.length*2*8);
      for (int i = 0; i < textures.length; i++)
        o.write_int32 (textures[i]);
      for (int i = 0; i < priorities.length; i++)
        o.write_float (priorities[i]);
    }
  }


  // glx render opcode 4119 - copy texture image 1d
  /**
   * @see <a href="glCopyTexImage1D.html">glCopyTexImage1D</a>
   */
  public void copy_texture_image_1d (int target, int level, 
    int internal_format, int x, int y, int width, int border) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4119, 32);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (internal_format);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (border);
    }
  } 


  // glx render opcode 4120 - copy texture image 2d
  /**
   * @see <a href="glCopyTexImage2D.html">glCopyTexImage2D</a>
   */
  public void copy_texture_image_2d (int target, int level, 
    int internal_format, int x, int y, int width, int height, int border) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4120, 26);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (internal_format);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (height);
      o.write_int32 (border);
    }
  } 


  // glx render opcode 4121 - copy texture sub image 1d
  /**
   * @see <a href="glCopyTexSubImage1D.html">glCopyTexSubImage1D</a>
   */
  public void copy_texture_sub_image_1d (int target, int level, 
    int xoffset, int x, int y, int width) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4121, 28);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (xoffset);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
    }
  } 


  // glx render opcode 4122 - copy texture sub image 2d
  /**
   * @see <a href="glCopyTexSubImage2D.html">glCopyTexSubImage2D</a>
   */
  public void copy_texture_sub_image_2d (int target, int level, 
    int xoffset, int yoffset, int x, int y, int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4122, 36);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (xoffset);
      o.write_int32 (yoffset);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (height);
    }
  } 


  // glx render opcode 4123 - copy texture sub image3d
  /**
   * @see <a href="glCopyTexSubImage3D.html">glCopyTexSubImage3D</a>
   */
  public void copy_texture_sub_image3d (int target, int level, 
    int xoffset, int yoffset, int zoffset,
    int x, int y, int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, 4123, 40);
      o.write_int32 (target);
      o.write_int32 (level);
      o.write_int32 (xoffset);
      o.write_int32 (yoffset);
      o.write_int32 (zoffset);
      o.write_int32 (x);
      o.write_int32 (y);
      o.write_int32 (width);
      o.write_int32 (height);
    }
  } 


  /**
   * Enable or disable server-side GL capability.
   *
   * @see #enable(int)
   * @see #disable(int)
   */
  public void capability (int capability, boolean enable) {
    if (enable) enable (capability);
    else disable (capability);
  }


  /**
   * @see #color3d(double, double, double)
   * @see <a href="GLColor3dv.html">GLColor3dv</a>
   */
  public void color3dv (double [] v) {
    color3d (v [0], v [1], v [2]);
  }


  /**
   * @see #color3f(float, float, float)
   * @see <a href="GLColor3fv.html">GLColor3fv</a>
   */
  public void color3fv (float [] v) {
    color3f (v [0], v [1], v [2]);
  }


  /**
   * @see #color3i(int, int, int)
   * @see <a href="GLColor3iv.html">GLColor3iv</a>
   */
  public void color3iv (int [] v) {
    color3i (v [0], v [1], v [2]);
  }


  /**
   * @see #color3s(int, int, int)
   * @see <a href="GLColor3sv.html">GLColor3sv</a>
   */
  public void color3sv (int [] v) {
    color3s (v [0], v [1], v [2]);
  }


  /**
   * @see #color3ub(byte, byte, byte)
   * @see <a href="GLColor3ubv.html">GLColor3ubv</a>
   */
  public void color3ubv (byte [] v) {
    color3ub (v [0], v [1], v [2]);
  }


  /**
   * @see #color3ui(int, int, int)
   * @see <a href="GLColor3uiv.html">GLColor3uiv</a>
   */
  public void color3uiv (int [] v) {
    color3ui (v [0], v [1], v [2]);
  }


  /**
   * @see #color3us(int, int, int)
   * @see <a href="GLColor3usv.html">GLColor3usv</a>
   */
  public void color3usv (int [] v) {
    color3s (v [0], v [1], v [2]);
  }


  /**
   * @see #color4d(double, double, double, double)
   * @see <a href="GLColor4d.html">GLColor4d</a>
   */
  public void color4dv (double [] v) {
    color4d (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #color4f(float, float, float, float)
   * @see <a href="GLColor4f.html">GLColor4f</a>
   */
  public void color4fv (float [] v) {
    color4f (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #color4i(int, int, int, int)
   * @see <a href="GLColor4i.html">GLColor4i</a>
   */
  public void color4iv (int [] v) {
    color4i (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #color4s(int, int, int, int)
   * @see <a href="GLColor4s.html">GLColor4s</a>
   */
  public void color4sv (int [] v) {
    color4s (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #color4ui(int, int, int, int)
   * @see <a href="GLColor4ui.html">GLColor4ui</a>
   */
  public void color4uiv (int [] v) {
    color4ui (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #color4us(int, int, int, int)
   * @see <a href="GLColor4us.html">GLColor4us</a>
   */
  public void color4usv (int [] v) {
    color4us (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #error()
   */ 
  public String error_string () {
    switch (error ()) {
    case NO_ERROR: return "no-error";
    case INVALID_ENUM: return "invalid-enum";
    case INVALID_VALUE: return "invalid-value";
    case INVALID_OPERATION: return "invalid-operation";
    case STACK_OVERFLOW: return "stack-overflow";
    case STACK_UNDERFLOW: return "stack-underflow";
    case OUT_OF_MEMORY: return "out-of-memory";
    default: return null;
    }
  }


  /**
   * @see #multi_tex_coord1d_arb(int, double)
   * @see <a href="GLMultiTexCoord1dvARB.html">
   * GLMultiTexCoord1dvARB</a>
   */
  public void multi_tex_coord1dv_arb (int target, double [] v) {
    multi_tex_coord1d_arb (target, v [0]);
  }


  /**
   * @see #multi_tex_coord1f_arb(int, float)
   * @see <a href="GLMultiTexCoord1fvARB.html">
   * GLMultiTexCoord1fvARB</a>
   */
  public void multi_tex_coord1fv_arb (int target, float [] v) {
    multi_tex_coord1f_arb (target, v [0]);
  }


  /**
   * @see #multi_tex_coord1i_arb(int, int)
   * @see <a href="GLMultiTexCoord1ivARB.html">
   * GLMultiTexCoord1ivARB</a>
   */
  public void multi_tex_coord1iv_arb (int target, int [] v) {
    multi_tex_coord1i_arb (target, v [0]);
  }


  /**
   * @see #multi_tex_coord1s_arb(int, int)
   * @see <a href="GLMultiTexCoord1svARB.html">
   * GLMultiTexCoord1svARB</a>
   */
  public void multi_tex_coord1sv_arb (int target, int [] v) {
    multi_tex_coord1s_arb (target, v [0]);
  }

  /**
   * @see #multi_tex_coord2d_arb(int, double, double)
   * @see <a href="GLMultiTexCoord2dvARB.html">
   * GLMultiTexCoord2dvARB</a>
   */
  public void multi_tex_coord2dv (int target, double [] v) {
    multi_tex_coord2d_arb (target, v [0], v [1]);
  }


  /**
   * @see #multi_tex_coord2f_arb(int, float, float)
   * @see <a href="GLMultiTexCoord2fvARB.html">
   * GLMultiTexCoord2fvARB</a>
   */
  public void multi_tex_coord2fv_arb (int target, float [] v) {
    multi_tex_coord2f_arb (target, v [0], v [1]);
  }


  /**
   * @see #multi_tex_coord2i_arb(int, int, int)
   * @see <a href="GLMultiTexCoord2ivARB.html">
   * GLMultiTexCoord2ivARB</a>
   */
  public void multi_tex_coord2iv_arb (int target, int [] v) {
    multi_tex_coord2i_arb (target, v [0], v [1]);
  }


  /**
   * @see #multi_tex_coord2s_arb(int, int, int)
   * @see <a href="GLMultiTexCoord2svARB.html">
   * GLMultiTexCoord2svARB</a>
   */
  public void multi_tex_coord2sv_arb (int target, int [] v) {
    multi_tex_coord2s_arb (target, v [0], v [1]);
  }


  /**
   * @see #multi_tex_coord3d_arb(int, double, double, double)
   * @see <a href="GLMultiTexCoord3dvARB.html">
   * GLMultiTexCoord3dvARB</a>
   */
  public void multi_tex_coord3dv_arb (int target, double [] v) {
    multi_tex_coord3d_arb (target, v [0], v [1], v [2]);
  }


  /**
   * @see #multi_tex_coord3f_arb(int, float, float, float)
   * @see <a href="GLMultiTexCoord3fvARB.html">
   * GLMultiTexCoord3fvARB</a>
   */
  public void multi_tex_coord3fv_arb (int target, float [] v) {
    multi_tex_coord3f_arb (target, v [0], v [1], v [2]);
  }


  /**
   * @see #multi_tex_coord3i_arb(int, int, int, int)
   * @see <a href="GLMultiTexCoord3ivARB.html">
   * GLMultiTexCoord3ivARB</a>
   */
  public void multi_tex_coord3iv_arb (int target, int [] v) {
    multi_tex_coord3i_arb (target, v [0], v [1], v [2]);
  }


  /**
   * @see #multi_tex_coord3s_arb(int, int, int, int)
   * @see <a href="GLMultiTexCoord3svARB.html">
   * GLMultiTexCoord3svARB</a>
   */
  public void multi_tex_coord3sv_arb (int target, int [] v) {
    multi_tex_coord3s_arb (target, v [0], v [1], v [2]);
  }


  /**
   * @see #multi_tex_coord4d_arb(int, double, double, double, double)
   * @see <a href="GLMultiTexCoord4dARB.html">
   * GLMultiTexCoord4dARB</a>
   */
  public void multi_tex_coord4dv_arb (int target, double [] v) {
    multi_tex_coord4d_arb (target, v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #multi_tex_coord4f_arb(int, float, float, float, float)
   * @see <a href="GLMultiTexCoord4fARB.html">
   * GLMultiTexCoord4fARB</a>
   */
  public void multi_tex_coord4fv_arb (int target, float [] v) {
    multi_tex_coord4f_arb (target, v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #multi_tex_coord4i_arb(int, int, int, int, int)
   * @see <a href="GLMultiTexCoord4iARB.html">
   * GLMultiTexCoord4iARB</a>
   */
  public void multi_tex_coord4iv_arb (int target, int [] v) {
    multi_tex_coord4i_arb (target, v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #multi_tex_coord4s_arb(int, int, int, int, int)
   * @see <a href="GLMultiTexCoord4sARB.html">
   * GLMultiTexCoord4sARB</a>
   */
  public void multi_tex_coord4sv_arb (int target, int [] v) {
    multi_tex_coord4s_arb (target, v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #normal3b(boolean, boolean, boolean)
   * @see <a href="GLNormal3bv.html">GLNormal3bv</a>
   */
  public void normal3bv (boolean [] v) {
    normal3b (v [0], v [1], v [2]);
  }


  /**
   * @see #normal3d(double, double, double)
   * @see <a href="GLNormal3dv.html">GLNormal3dv</a>
   */
  public void normal3dv (double [] v) {
    normal3d (v [0], v [1], v [2]);
  }


  /**
   * @see #normal3f(float, float, float)
   * @see <a href="GLNormal3fv.html">GLNormal3fv</a>
   */
  public void normal3fv (float [] v) {
    normal3f (v [0], v [1], v [2]);
  }


  /**
   * @see #normal3i(int, int, int)
   * @see <a href="GLNormal3iv.html">GLNormal3iv</a>
   */
  public void normal3iv (int [] v) {
    normal3i (v [0], v [1], v [2]);
  }


  /**
   * @see #normal3s(int, int, int)
   * @see <a href="GLNormal3sv.html">GLNormal3sv</a>
   */
  public void normal3sv (int [] v) {
    normal3s (v [0], v [1], v [2]);
  }


  /**
   * @see #raster_pos2d(double, double)
   * @see <a href="GLRasterPos2dv.html">GLRasterPos2dv</a>
   */
  public void raster_pos2dv (double [] v) {
    raster_pos2d (v [0], v [1]);
  }


  /**
   * @see #raster_pos2f(float, float)
   * @see <a href="GLRasterPos2fv.html">GLRasterPos2fv</a>
   */
  public void raster_pos2fv (float [] v) {
    raster_pos2f (v [0], v [1]);
  }


  /**
   * @see #raster_pos2i(int, int)
   * @see <a href="GLRasterPos2iv.html">GLRasterPos2iv</a>
   */
  public void raster_pos2iv (int [] v) {
    raster_pos2i (v [0], v [1]);
  }


  /**
   * @see #raster_pos2s(int, int)
   * @see <a href="GLRasterPos2sv.html">GLRasterPos2sv</a>
   */
  public void raster_pos2sv (int [] v) {
    raster_pos2s (v [0], v [1]);
  }


  /**
   * @see #raster_pos3d(double, double, double)
   * @see <a href="GLRasterPos3dv.html">GLRasterPos3dv</a>
   */
  public void raster_pos3dv (double [] v) {
    raster_pos3d (v [0], v [1], v [2]);
  }


  /**
   * @see #raster_pos3f(float, float, float)
   * @see <a href="GLRasterPos3fv.html">GLRasterPos3fv</a>
   */
  public void raster_pos3fv (float [] v) {
    raster_pos3f (v [0], v [1], v [2]);
  }


  /**
   * @see #raster_pos3i(int, int, int)
   * @see <a href="GLRasterPos3iv.html">GLRasterPos3iv</a>
   */
  public void raster_pos3iv (int [] v) {
    raster_pos3i (v [0], v [1], v [2]);
  }


  /**
   * @see #raster_pos3s(int, int, int)
   * @see <a href="GLRasterPos3sv.html">GLRasterPos3sv</a>
   */
  public void raster_pos3sv (int [] v) {
    raster_pos3s (v [0], v [1], v [2]);
  }


  /**
   * @see #raster_pos4d(double, double, double, double)
   * @see <a href="GLRasterPos4d.html">GLRasterPos4d</a>
   */
  public void raster_pos4dv (double [] v) {
    raster_pos4d (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #raster_pos4f(float, float, float, float)
   * @see <a href="GLRasterPos4f.html">GLRasterPos4f</a>
   */
  public void raster_pos4fv (float [] v) {
    raster_pos4f (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #raster_pos4i(int, int, int, int)
   * @see <a href="GLRasterPos4i.html">GLRasterPos4i</a>
   */
  public void raster_pos4iv (int [] v) {
    raster_pos4i (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #raster_pos4s(int, int, int, int)
   * @see <a href="GLRasterPos4s.html">GLRasterPos4s</a>
   */
  public void raster_pos4sv (int [] v) {
    raster_pos4s (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #rectd(double, double, double, double)
   * @see <a href="glRectdv.html">glRectdv</a>
   */
  public void rectdv (double [] v1, double [] v2) {
    rectd (v1 [0], v1 [1], v2 [0], v2 [1]);
  }


  /**
   * @see #rectf(float, float, float, float)
   * @see <a href="glRectfv.html">glRectfv</a>
   */
  public void rectfv (float [] v1, float [] v2) {
    rectf (v1 [0], v1 [1], v2 [0], v2 [1]);
  }


  /**
   * @see #recti(int, int, int, int)
   * @see <a href="glRectiv.html">glRectiv</a>
   */
  public void rectiv (int [] v1, int [] v2) {
    recti (v1 [0], v1 [1], v2 [0], v2 [1]);
  }


  /**
   * @see #rects(int, int, int, int)
   * @see <a href="glRectsv.html">glRectsv</a>
   */
  public void rectsv (int [] v1, int [] v2) {
    rects (v1 [0], v1 [1], v2 [0], v2 [1]);
  }


  /**
   * @see #vertex2d(double, double)
   * @see <a href="GLVertex2dv.html">GLVertex2dv</a>
   */
  public void vertex2dv (double [] v) {
    vertex2d (v [0], v [1]);
  }


  /**
   * @see #vertex2f(float, float)
   * @see <a href="GLVertex2fv.html">GLVertex2fv</a>
   */
  public void vertex2fv (float [] v) {
    vertex2f (v [0], v [1]);
  }


  /**
   * @see #vertex2i(int, int)
   * @see <a href="GLVertex2iv.html">GLVertex2iv</a>
   */
  public void vertex2iv (int [] v) {
    vertex2i (v [0], v [1]);
  }


  /**
   * @see #vertex2s(int, int)
   * @see <a href="GLVertex2sv.html">GLVertex2sv</a>
   */
  public void vertex2sv (int [] v) {
    vertex2s (v [0], v [1]);
  }


  /**
   * @see #vertex3d(double, double, double)
   * @see <a href="GLVertex3dv.html">GLVertex3dv</a>
   */
  public void vertex3dv (double [] v) {
    vertex3d (v [0], v [1], v [2]);
  }


  /**
   * @see #vertex3f(float, float, float)
   * @see <a href="GLVertex3fv.html">GLVertex3fv</a>
   */
  public void vertex3fv (float [] v) {
    vertex3f (v [0], v [1], v [2]);
  }


  /**
   * @see #vertex3i(int, int, int)
   * @see <a href="GLVertex3iv.html">GLVertex3iv</a>
   */
  public void vertex3iv (int [] v) {
    vertex3i (v [0], v [1], v [2]);
  }


  /**
   * @see #vertex3s(int, int, int)
   * @see <a href="GLVertex3sv.html">GLVertex3sv</a>
   */
  public void vertex3sv (int [] v) {
    vertex3s (v [0], v [1], v [2]);
  }


  /**
   * @see #vertex4d(double, double, double, double)
   * @see <a href="GLVertex4d.html">GLVertex4d</a>
   */
  public void vertex4dv (double [] v) {
    vertex4d (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #vertex4f(float, float, float, float)
   * @see <a href="GLVertex4f.html">GLVertex4f</a>
   */
  public void vertex4fv (float [] v) {
    vertex4f (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #vertex4i(int, int, int, int)
   * @see <a href="GLVertex4i.html">GLVertex4i</a>
   */
  public void vertex4iv (int [] v) {
    vertex4i (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #vertex4s(int, int, int, int)
   * @see <a href="GLVertex4s.html">GLVertex4s</a>
   */
  public void vertex4sv (int [] v) {
    vertex4s (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #tex_coord1d(double)
   * @see <a href="GLTexCoord1dv.html">GLTexCoord1dv</a>
   */
  public void tex_coord1dv (double [] v) {
    tex_coord1d (v [0]);
  }


  /**
   * @see #tex_coord1f(float)
   * @see <a href="GLTexCoord1fv.html">GLTexCoord1fv</a>
   */
  public void tex_coord1fv (float [] v) {
    tex_coord1f (v [0]);
  }


  /**
   * @see #tex_coord1i(int)
   * @see <a href="GLTexCoord1iv.html">GLTexCoord1iv</a>
   */
  public void tex_coord1iv (int [] v) {
    tex_coord1i (v [0]);
  }


  /**
   * @see #tex_coord1s(int)
   * @see <a href="GLTexCoord1sv.html">GLTexCoord1sv</a>
   */
  public void tex_coord1sv (int [] v) {
    tex_coord1s (v [0]);
  }

  /**
   * @see #tex_coord2d(double, double)
   * @see <a href="GLTexCoord2dv.html">GLTexCoord2dv</a>
   */
  public void tex_coord2dv (double [] v) {
    tex_coord2d (v [0], v [1]);
  }


  /**
   * @see #tex_coord2f(float, float)
   * @see <a href="GLTexCoord2fv.html">GLTexCoord2fv</a>
   */
  public void tex_coord2fv (float [] v) {
    tex_coord2f (v [0], v [1]);
  }


  /**
   * @see #tex_coord2i(int, int)
   * @see <a href="GLTexCoord2iv.html">GLTexCoord2iv</a>
   */
  public void tex_coord2iv (int [] v) {
    tex_coord2i (v [0], v [1]);
  }


  /**
   * @see #tex_coord2s(int, int)
   * @see <a href="GLTexCoord2sv.html">GLTexCoord2sv</a>
   */
  public void tex_coord2sv (int [] v) {
    tex_coord2s (v [0], v [1]);
  }


  /**
   * @see #tex_coord3d(double, double, double)
   * @see <a href="GLTexCoord3dv.html">GLTexCoord3dv</a>
   */
  public void tex_coord3dv (double [] v) {
    tex_coord3d (v [0], v [1], v [2]);
  }


  /**
   * @see #tex_coord3f(float, float, float)
   * @see <a href="GLTexCoord3fv.html">GLTexCoord3fv</a>
   */
  public void tex_coord3fv (float [] v) {
    tex_coord3f (v [0], v [1], v [2]);
  }


  /**
   * @see #tex_coord3i(int, int, int)
   * @see <a href="GLTexCoord3iv.html">GLTexCoord3iv</a>
   */
  public void tex_coord3iv (int [] v) {
    tex_coord3i (v [0], v [1], v [2]);
  }


  /**
   * @see #tex_coord3s(int, int, int)
   * @see <a href="GLTexCoord3sv.html">GLTexCoord3sv</a>
   */
  public void tex_coord3sv (int [] v) {
    tex_coord3s (v [0], v [1], v [2]);
  }


  /**
   * @see #tex_coord4d(double, double, double, double)
   * @see <a href="GLTexCoord4d.html">GLTexCoord4d</a>
   */
  public void tex_coord4dv (double [] v) {
    tex_coord4d (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #tex_coord4f(float, float, float, float)
   * @see <a href="GLTexCoord4f.html">GLTexCoord4f</a>
   */
  public void tex_coord4fv (float [] v) {
    tex_coord4f (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #tex_coord4i(int, int, int, int)
   * @see <a href="GLTexCoord4i.html">GLTexCoord4i</a>
   */
  public void tex_coord4iv (int [] v) {
    tex_coord4i (v [0], v [1], v [2], v [3]);
  }


  /**
   * @see #tex_coord4s(int, int, int, int)
   * @see <a href="GLTexCoord4s.html">GLTexCoord4s</a>
   */
  public void tex_coord4sv (int [] v) {
    tex_coord4s (v [0], v [1], v [2], v [3]);
  }


  private void flush_render_request () {
    RequestOutputStream o = display.out;
    if (o.index > 0 && o.opcode () == 1) {
      o.update_length ();
      o.flush ();
    }
  }

//  private Enum read_enum (Request request) {
//    Data reply = display.read_reply (request);
//    int n = reply.read4 (12);
//    
//    if (n == 0) return null;
//    if (n == 1) return new Enum (reply, 16, 1);
//    return new Enum (reply, 32, n);
//  }


  public boolean support (int major, int minor) {
    String version_all = string (VERSION);
    int to = version_all.indexOf (' ');
    if (to == -1) to = version_all.length ();
    String version_number = version_all.substring (0, to);
    
    String [] versions = gnu.util.Misc.tokenize (version_number, ".");
    if (versions.length < 2) return false; // invalid format

    int major0 = Integer.parseInt (versions [0]);
    int minor0 = Integer.parseInt (versions [1]);

    return major0 == major && minor0 >= minor;
  }


  public String toString () {
    return "#GL"
      + "\n  vendor: " + string (VENDOR)
      + "\n  renderer: " + string (RENDERER)
      + "\n  version: " + string (VERSION)
      + "\n  extensions: " + string (EXTENSIONS);
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes one int-like parameters (a 4 bytes) and returns
   * a FLOAT32 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   *
   * @return the returned FLOAT32 array
   */
  private float [] get_fv1 (int opcode, int par1)
  {
    RequestOutputStream o = display.out;
    float [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 3);
      o.write_int32 (tag);
      o.write_int32 (par1);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new float [n];
        if (n == 1) {
          ret [0] = in.read_float32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_float32 ();
        }
      }
    }
    return ret;
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes two int-like parameters (a 4 bytes) and returns
   * a FLOAT32 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   * @param par2 the second parameter
   *
   * @return the returned FLOAT32 array
   */
  private float [] get_fv2 (int opcode, int par1, int par2)
  {
    RequestOutputStream o = display.out;
    float [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 4);
      o.write_int32 (tag);
      o.write_int32 (par1);
      o.write_int32 (par2);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new float [n];
        if (n == 1) {
          ret [0] = in.read_float32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_float32 ();
        }
      }
    }
    return ret;
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes two int-like parameters (a 4 bytes) and returns
   * a FLOAT64 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   *
   * @return the returned FLOAT64 array
   */
  private double [] get_dv1 (int opcode, int par1)
  {
    RequestOutputStream o = display.out;
    double [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 3);
      o.write_int32 (tag);
      o.write_int32 (par1);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new double [n];
        if (n == 1) {
          ret [0] = in.read_float64 ();
          in.skip (8);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_float64 ();
        }
      }
    }
    return ret;
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes two int-like parameters (a 4 bytes) and returns
   * a FLOAT64 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   * @param par2 the second parameter
   *
   * @return the returned FLOAT64 array
   */
  private double [] get_dv2 (int opcode, int par1, int par2)
  {
    RequestOutputStream o = display.out;
    double [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 4);
      o.write_int32 (tag);
      o.write_int32 (par1);
      o.write_int32 (par2);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new double [n];
        if (n == 1) {
          ret [0] = in.read_float64 ();
          in.skip (8);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_float64 ();
        }
      }
    }
    return ret;
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes one int-like parameters (a 4 bytes) and returns
   * a INT32 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   *
   * @return the returned FLOAT32 array
   */
  private int [] get_iv1 (int opcode, int par1)
  {
    RequestOutputStream o = display.out;
    int [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 3);
      o.write_int32 (tag);
      o.write_int32 (par1);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new int [n];
        if (n == 1) {
          ret [0] = in.read_int32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_int32 ();
        }
      }
    }
    return ret;
  }

  /**
   * A generic function for a common request pattern in GLX. This sends
   * a request that takes two int-like parameters (a 4 bytes) and returns
   * a INT32 array.
   *
   * @param opcode the opcode
   * @param par1 the first parameter
   * @param par2 the second parameter
   *
   * @return the returned FLOAT32 array
   */
  private int [] get_iv2 (int opcode, int par1, int par2)
  {
    RequestOutputStream o = display.out;
    int [] ret;
    synchronized (o) {
      o.begin_request (glx.major_opcode, opcode, 4);
      o.write_int32 (tag);
      o.write_int32 (par1);
      o.write_int32 (par2);
      ResponseInputStream in = display.in;
      synchronized (in) {
        in.skip (12);
        int n = in.read_int32 ();
        ret = new int [n];
        if (n == 1) {
          ret [0] = in.read_int32 ();
          in.skip (12);
        } else {
          in.skip (16);
          for (int i = 0; i < n; i++)
            ret [i] = in.read_int32 ();
        }
      }
    }
    return ret;
  }

  private void render_2f (int opcode, float p1, float p2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 12);
      o.write_float (p1);
      o.write_float (p2);
    }
  }

  private void render_2i (int opcode, int p1, int p2) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 12);
      o.write_int32 (p1);
      o.write_int32 (p2);
    }
  }

  private void render_3d (int opcode, double p1, double p2, double p3) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 28);
      o.write_double (p1);
      o.write_double (p2);
      o.write_double (p3);
    }
  }

  private void render_3f (int opcode, float p1, float p2, float p3) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 16);
      o.write_float (p1);
      o.write_float (p2);
      o.write_float (p3);
    }
  }

  private void render_3i (int opcode, int p1, int p2, int p3) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 16);
      o.write_int32 (p1);
      o.write_int32 (p2);
      o.write_int32 (p3);
    }
  }

  private void render_3s (int opcode, int p1, int p2, int p3) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 12);
      o.write_int16 (p1);
      o.write_int16 (p2);
      o.write_int16 (p3);
      o.write_pad (2);
    }
  }

  private void render_4d(int opcode, double p1, double p2, double p3,
                         double p4) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 36);
      o.write_double (p1);
      o.write_double (p2);
      o.write_double (p3);
      o.write_double (p4);
    }
  }

  private void render_4f(int opcode, float p1, float p2, float p3,
                         float p4) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 20);
      o.write_float (p1);
      o.write_float (p2);
      o.write_float (p3);
      o.write_float (p4);
    }
  }

  private void render_4i(int opcode, int p1, int p2, int p3, int p4) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 20);
      o.write_int32 (p1);
      o.write_int32 (p2);
      o.write_int32 (p3);
      o.write_int32 (p4);
    }
  }

  private void render_4s(int opcode, int p1, int p2, int p3, int p4) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      begin_render_request (o, opcode, 12);
      o.write_int16 (p1);
      o.write_int16 (p2);
      o.write_int16 (p3);
      o.write_int16 (p4);
    }
  }

  public void write_float32 (byte[] buffer, int index, float f) {
    int v = Float.floatToIntBits (f);
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  private void write_double (byte[] buffer, int index, double d) {
    long v = Double.doubleToLongBits (d);
    
    buffer [index] = (byte) (v >> 56);
    index++;
    buffer [index] = (byte) (v >> 48);
    index++;
    buffer [index] = (byte) (v >> 40);
    index++;
    buffer [index] = (byte) (v >> 32);
    index++;
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }
}

