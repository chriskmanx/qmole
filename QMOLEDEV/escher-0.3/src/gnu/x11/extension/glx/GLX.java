package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/**
 * OpenGL GLX Extension. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/glx-1.3-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/glx-1.3-protocol.ps.gz"
 * >protocol</a>).
 *
 * <p>Note XFree86 4.0.x only implements GLX 1.2 instead GLX 1.3.
 */
public class GLX extends gnu.x11.extension.Extension 
  implements gnu.x11.extension.ErrorFactory,
  gnu.x11.extension.EventFactory {

  public static final String [] MINOR_OPCODE_STRINGS = {
    // GLX commands
    "",                         // 0
    "Render",                   // 1
    "RenderLarge",              // 2
    "CreateContext",            // 3
    "DestroyContext",           // 4
    "MakeCurrent",              // 5
    "IsDirect",                 // 6
    "QueryVersion",             // 7
    "WaitGL",                   // 8
    "WaitX",                    // 9
    "CopyContext",              // 10
    "SwapBuffers",              // 11
    "UseXFont",                 // 12
    "CreateGLXPixmap",          // 13
    "GetVisualConfigs",         // 14
    "DestroyGLXPixmap",         // 15
    "VendorPrivate",            // 16
    "VendorPrivateWithReply",   // 17
    "QueryExtensionsString",    // 18
    "QueryServerString",        // 19
    "ClientInfo",               // 20
    "GetFBConfigs",             // 21
    "CreatePixmap",             // 22
    "DestroyPixmap",            // 23
    "CreateNewContext",         // 24
    "QueryContext",             // 25
    "MakeContextCurrent",       // 26
    "CreatePbuffer",            // 27
    "DestroyPbuffer",           // 28
    "GetDrawableAttributes",    // 29
    "ChangeDrawableAttributes", // 30
    "CreateWindow",             // 31
    "DestroyWindow",            // 32

    "",                         // 33
    "",                         // 34
    "",                         // 35
    "",                         // 36
    "",                         // 37
    "",                         // 38
    "",                         // 39
    "",                         // 40
    "",                         // 41
    "",                         // 42
    "",                         // 43
    "",                         // 44
    "",                         // 45
    "",                         // 46
    "",                         // 47
    "",                         // 48
    "",                         // 49
    "",                         // 50
    "",                         // 51
    "",                         // 52
    "",                         // 53
    "",                         // 54
    "",                         // 55
    "",                         // 56
    "",                         // 57
    "",                         // 58
    "",                         // 59
    "",                         // 60
    "",                         // 61
    "",                         // 62
    "",                         // 63
    "",                         // 64
    "",                         // 65
    "",                         // 66
    "",                         // 67
    "",                         // 68
    "",                         // 69
    "",                         // 70
    "",                         // 71
    "",                         // 72
    "",                         // 73
    "",                         // 74
    "",                         // 75
    "",                         // 76
    "",                         // 77
    "",                         // 78
    "",                         // 79
    "",                         // 80
    "",                         // 81
    "",                         // 82
    "",                         // 83
    "",                         // 84
    "",                         // 85
    "",                         // 86
    "",                         // 87
    "",                         // 88
    "",                         // 89
    "",                         // 90
    "",                         // 91
    "",                         // 92
    "",                         // 93
    "",                         // 94
    "",                         // 95
    "",                         // 96
    "",                         // 97
    "",                         // 98
    "",                         // 99
    "",                         // 100

    // GL non-rendering commands
    "NewList",                  // 101
    "EndList",                  // 102
    "DeleteLists",              // 103
    "GenLists",                 // 104
    "FeedbackBuffer",           // 105
    "SelectBuffer",             // 106
    "RenderMode",               // 107
    "Finish",                   // 108
    "PixelStoref",              // 109
    "PixelStorei",              // 110
    "ReadPixels",               // 111
    "GetBooleanv",              // 112
    "GetClipPlane",             // 113
    "GetDoublev",               // 114
    "GetError",                 // 115
    "GetFloatv",                // 116
    "GetIntegerv",              // 117
    "GetLightfv",               // 118
    "GetLightiv",               // 119
    "GetMapdv",                 // 120
    "GetMapfv",                 // 121
    "GetMapiv",                 // 122
    "GetMaterialfv",            // 123
    "GetMaterialiv",            // 124
    "GetPixelMapfv",            // 125
    "GetPixelMapuiv",           // 126
    "GetPixelMapusv",           // 127
    "GetPolygonStipple",        // 128
    "GetString",                // 129
    "GetTexEnvfv",              // 130
    "GetTexEnviv",              // 131
    "GetTexGendv",              // 132
    "GetTexGenfv",              // 133
    "GetTexGeniv",              // 134
    "GetTexImage",              // 135
    "GetTexParameterfv",        // 136
    "GetTexParameteriv",        // 137
    "GetTexLevelParameterfv",   // 138
    "GetTexLevelParameteriv",   // 139
    "IsEnabled",                // 140
    "IsList",                   // 141
    "Flush",                    // 142
    "AreTexturesResident",      // 143
    "DeleteTextures",           // 144
    "GenTextures",              // 145
    "IsTexture",                // 146
    "GetColorTable",            // 147
    "GetColorTableParameterfv", // 148
    "GetColorTableParameteriv", // 149
    "GetConvolutionFilter",     // 150
    "GetConvolutionParameterfv", // 151
    "GetConvolutionParameteriv", // 152
    "GetSeparableFilter",       // 153
    "GetHistogram",             // 154
    "GetHistogramParameterfv",  // 155
    "GetHistogramParameteriv",  // 156
    "GetMinmax",                // 157
    "GetMinmaxParameterfv",     // 158
    "GetMinmaxParameteriv",     // 159
  };


  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 2;


  public int server_major_version, server_minor_version;
  private VisualConfig [] [] visual_configs_cache;


  // glx opcode 7 - get version
  /**
   * @see <a href="glXQueryVersion.html">glXQueryVersion</a>
   */
  public GLX (gnu.x11.Display display) 
    throws gnu.x11.extension.NotFoundException { 

    super (display, "GLX", MINOR_OPCODE_STRINGS, 13, 1);

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 7, 3);
      o.write_int32 (CLIENT_MAJOR_VERSION);
      o.write_int32 (CLIENT_MINOR_VERSION);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply(o);
        i.skip (8);
        server_major_version = i.read_int32 ();
        server_minor_version = i.read_int32 ();
        i.skip (16);
      }
    }

    send_client_info ();
    visual_configs_cache = new VisualConfig [display.screens.length] [];    
  }


  // glx opcode 14 - get visual configs
  public VisualConfig [] visual_configs (int screen_no) {
    if (visual_configs_cache [screen_no] != null) 
      return visual_configs_cache [screen_no];

    RequestOutputStream o = display.out;
    VisualConfig [] vcs;
    synchronized (o) {
      o.begin_request (major_opcode, 14, 2);
      o.write_int32 (screen_no);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply(o);
        i.skip (4);
        int n = i.read_int32 ();
        int visual_count = i.read_int32 ();
        int property_count = (i.read_int32 () - 18) / 2;
        i.skip (16);
        assert n == visual_count * property_count;
        vcs = new VisualConfig [visual_count];
        for (int index = 0; index < visual_count; index++) {
          vcs [index] = new VisualConfig (i, property_count);
        }
        visual_configs_cache [screen_no] = vcs;
      }
    }
    return vcs;
  }
    

  public final static int VENDOR = 0x1;
  public final static int VERSION = 0x2;
  public final static int EXTENSIONS = 0x3;


  // glx opcode 19 - query server string
  /**
   * @param name valid:
   * {@link #VENDOR},
   * {@link #VERSION},
   * {@link #EXTENSIONS}
   * 
   * @see <a href="glXQueryServerString.html">glXQueryServerString</a>
   */
  public String server_string (int screen_no, int name) {
    String server_string;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 19, 3);
      o.write_int32 (screen_no);
      o.write_int32 (name);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (12);
        int len = i.read_int32 ();
        i.skip (16);
        server_string = i.read_string8 (len);
        i.pad (len);
      }
    }
    return server_string;
  }


  // TODO
  private static final String CLIENT_EXTENSION_STRING =
    "GL_EXT_abgr"
    + " GL_EXT_blend_color"
    + " GL_EXT_blend_func_separate"
    + " GL_EXT_blend_logic_op"
    + " GL_EXT_blend_minmax"
    + " GL_EXT_blend_subtract";

// GL_ARB_multitexture
// GL_ARB_texture_cube_map
// GL_ARB_tranpose_matrix
// GL_EXT_clip_volume_hint
// GL_EXT_compiled_vertex_array
// GL_EXT_histogram
// GL_EXT_packed_pixels
// GL_EXT_paletted_texture
// GL_EXT_point_parameters
// GL_EXT_polygon_offset
// GL_EXT_rescale_normal
// GL_EXT_shared_texture_palette
// GL_EXT_stencil_wrap
// GL_EXT_texture3D
// GL_EXT_texture_env_add
// GL_EXT_texture_env_combine
// GL_EXT_texture_object
// GL_EXT_texture_lod_bias
// GL_EXT_vertex_array
// GL_HP_occlusion_test
// GL_INGR_blend_func_separate
// GL_MESA_window_pos
// GL_MESA_resize_buffers
// GL_NV_texgen_reflection
// GL_PGI_misc_hints
// GL_SGI_color_matrix
// GL_SGI_color_table
// GL_SGIS_pixel_texture
// GL_SGIS_texture_edge_clamp
// GL_SGIX_pixel_texture


  // glx opcode 20 - client info
  private void send_client_info () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      int n = CLIENT_EXTENSION_STRING.length ();
      int p = RequestOutputStream.pad (n);
      o.begin_request (major_opcode, 20, 4 + (n + p) / 4);
      o.write_int32 (CLIENT_MAJOR_VERSION);
      o.write_int32 (CLIENT_MINOR_VERSION);
      o.write_int32 (CLIENT_EXTENSION_STRING.length ());
      o.write_string8 (CLIENT_EXTENSION_STRING);
      o.skip (p);
      o.send ();
    }
  }


  // glx opcode 21 - get fb configs
  public int[] fb_configs (int screen_no) { // TODO 1.3
    int[] props;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 21, 2);
      o.write_int32 (screen_no);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.skip (4);
        int n = i.read_int32 ();
        // TODO: Build more useful object from this data.
        int num_fb_configs = i.read_int32 ();
        int num_props = i.read_int32 ();
        i.skip (16);
        props = new int[n];
        for (int idx = 0; idx < n; idx++) {
          props[idx] = i.read_int32 ();
        }
      }
    }
    return props;
  }


  /**
   * @see GL#GL(GLX, int, int, GL)
   */
  public GL create_context (int visual_id, int screen_no, GL share_list) {    
    return new GL (this, visual_id, screen_no, share_list, true);
  }


  public gnu.x11.Error build (Display display, int code, int seq_no, int bad,
                              int minor_opcode, int major_opcode) {

    return new Error (display, code - first_error, seq_no, bad, minor_opcode,
                      major_opcode);
  }


  public gnu.x11.event.Event build (Display display, 
    ResponseInputStream i, int code) {

    // only one extension event
    return new PbufferClobberEvent (display, i);
  }


  /**
   * @see #visual_config(int, VisualConfig, boolean)
   */
  public VisualConfig visual_config (VisualConfig template) {
    return visual_config (display.default_screen_no, template, true);
  }


  /**
   * @see <a href="glXChooseVisual.html">glXChooseVisual</a>
   */
  public VisualConfig visual_config (int screen_no, 
    VisualConfig template, boolean must) {

    VisualConfig [] vcs = visual_configs (screen_no);
    for (int i=0; i<vcs.length; i++)
      if (vcs [i].match (template)) return vcs [i];
      
    if (!must) return null;
    throw new java.lang.Error ("No matching: " + template);
  }


  /**
   * @see GLX#server_string(int, int)
   */
  public String more_string () {
    int screen = display.default_screen_no;

    // TODO output like `glxinfo'

    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version
      + "\n  vendor: " + server_string (screen, VENDOR)
      + "\n  extensions: " + server_string (screen, EXTENSIONS);      
  }


  public boolean support (int major, int minor) {
    return server_major_version == major 
      && server_minor_version >= minor;
  }
}
