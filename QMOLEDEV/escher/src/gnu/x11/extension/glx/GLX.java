package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;

/**
 * OpenGL GLX Extension. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/glx-1.3-library.ps.gz" >here</a> (<a
 * href=
 * "http://escher.sourceforge.net/etc/specification/glx-1.3-protocol.ps.gz"
 * >protocol</a>).
 * 
 * <p>
 * Note XFree86 4.0.x only implements GLX 1.2 instead GLX 1.3.
 */
public class GLX extends gnu.x11.extension.Extension implements
        gnu.x11.extension.ErrorFactory, gnu.x11.extension.EventFactory {

    /**
     * The minimum OpenGL version supported by Escher.
     * 
     * This constant can be meaningless depending on your use, as Escher
     * supports some extension from later GLX versions. You should query
     * the server to get the GLX version in use in the connection associated
     * with the current display. Also, You should query both server and client
     * to get a full list of all the supported commands and extension.
     * 
     * @see #queryExtensionsString(int)
     * @see #queryServerString(int, int)
     * @see #queryVersion()
     */
    public static final GLXVersion SUPPORTED_GLX_VERSION = new GLXVersion(1, 2);
    private final GLXVersion glxVersion;
    
    /**
     * Separator used to separate extensions in the extension strings.
     */
    private static final String EXT_SEPARATOR = " ";

    /**
     * List all the extension implemented by Escher.
     * 
     * This is to be considered deprecated. Use the GLXExtension enum instead. 
     */
    @Deprecated
    private static final String CLIENT_EXTENSION_STRING =
        "GL_EXT_abgr"                   + EXT_SEPARATOR +
        "GL_EXT_blend_color"            + EXT_SEPARATOR +
        "GL_EXT_blend_func_separate "   + EXT_SEPARATOR +
        "GL_EXT_blend_logic_op"         + EXT_SEPARATOR +
        "GL_EXT_blend_minmax"           + EXT_SEPARATOR +
        "GL_EXT_blend_subtract"         + EXT_SEPARATOR +
        "GL_ARB_vertex_program";

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

    
    public static final String[] MINOR_OPCODE_STRINGS = {
    // GLX commands
            "", // 0
            "Render", // 1
            "RenderLarge", // 2
            "CreateContext", // 3
            "DestroyContext", // 4
            "MakeCurrent", // 5
            "IsDirect", // 6
            "QueryVersion", // 7
            "WaitGL", // 8
            "WaitX", // 9
            "CopyContext", // 10
            "SwapBuffers", // 11
            "UseXFont", // 12
            "CreateGLXPixmap", // 13
            "GetVisualConfigs", // 14
            "DestroyGLXPixmap", // 15
            "VendorPrivate", // 16
            "VendorPrivateWithReply", // 17
            "QueryExtensionsString", // 18
            "QueryServerString", // 19
            "ClientInfo", // 20
            "GetFBConfigs", // 21
            "CreatePixmap", // 22
            "DestroyPixmap", // 23
            "CreateNewContext", // 24
            "QueryContext", // 25
            "MakeContextCurrent", // 26
            "CreatePbuffer", // 27
            "DestroyPbuffer", // 28
            "GetDrawableAttributes", // 29
            "ChangeDrawableAttributes", // 30
            "CreateWindow", // 31
            "DestroyWindow", // 32

            "", // 33
            "", // 34
            "", // 35
            "", // 36
            "", // 37
            "", // 38
            "", // 39
            "", // 40
            "", // 41
            "", // 42
            "", // 43
            "", // 44
            "", // 45
            "", // 46
            "", // 47
            "", // 48
            "", // 49
            "", // 50
            "", // 51
            "", // 52
            "", // 53
            "", // 54
            "", // 55
            "", // 56
            "", // 57
            "", // 58
            "", // 59
            "", // 60
            "", // 61
            "", // 62
            "", // 63
            "", // 64
            "", // 65
            "", // 66
            "", // 67
            "", // 68
            "", // 69
            "", // 70
            "", // 71
            "", // 72
            "", // 73
            "", // 74
            "", // 75
            "", // 76
            "", // 77
            "", // 78
            "", // 79
            "", // 80
            "", // 81
            "", // 82
            "", // 83
            "", // 84
            "", // 85
            "", // 86
            "", // 87
            "", // 88
            "", // 89
            "", // 90
            "", // 91
            "", // 92
            "", // 93
            "", // 94
            "", // 95
            "", // 96
            "", // 97
            "", // 98
            "", // 99
            "", // 100

            // GL non-rendering commands
            "NewList", // 101
            "EndList", // 102
            "DeleteLists", // 103
            "GenLists", // 104
            "FeedbackBuffer", // 105
            "SelectBuffer", // 106
            "RenderMode", // 107
            "Finish", // 108
            "PixelStoref", // 109
            "PixelStorei", // 110
            "ReadPixels", // 111
            "GetBooleanv", // 112
            "GetClipPlane", // 113
            "GetDoublev", // 114
            "GetError", // 115
            "GetFloatv", // 116
            "GetIntegerv", // 117
            "GetLightfv", // 118
            "GetLightiv", // 119
            "GetMapdv", // 120
            "GetMapfv", // 121
            "GetMapiv", // 122
            "GetMaterialfv", // 123
            "GetMaterialiv", // 124
            "GetPixelMapfv", // 125
            "GetPixelMapuiv", // 126
            "GetPixelMapusv", // 127
            "GetPolygonStipple", // 128
            "GetString", // 129
            "GetTexEnvfv", // 130
            "GetTexEnviv", // 131
            "GetTexGendv", // 132
            "GetTexGenfv", // 133
            "GetTexGeniv", // 134
            "GetTexImage", // 135
            "GetTexParameterfv", // 136
            "GetTexParameteriv", // 137
            "GetTexLevelParameterfv", // 138
            "GetTexLevelParameteriv", // 139
            "IsEnabled", // 140
            "IsList", // 141
            "Flush", // 142
            "AreTexturesResident", // 143
            "DeleteTextures", // 144
            "GenTextures", // 145
            "IsTexture", // 146
            "GetColorTable", // 147
            "GetColorTableParameterfv", // 148
            "GetColorTableParameteriv", // 149
            "GetConvolutionFilter", // 150
            "GetConvolutionParameterfv", // 151
            "GetConvolutionParameteriv", // 152
            "GetSeparableFilter", // 153
            "GetHistogram", // 154
            "GetHistogramParameterfv", // 155
            "GetHistogramParameteriv", // 156
            "GetMinmax", // 157
            "GetMinmaxParameterfv", // 158
            "GetMinmaxParameteriv", // 159
    };
    
    private VisualConfig[][] visualConfigs;

    /**
     * Initialize a new GLX context to the given display.
     */
    public GLX(gnu.x11.Display display)
        throws gnu.x11.extension.NotFoundException {

        super(display, "GLX", MINOR_OPCODE_STRINGS, 13, 1);

        int major = 0;
        int minor = 0;
        
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            
            // set only once
            o.setGLXMajorOpcode(majorOpcode);
            
            o.beginGLXRequest(GLXCommand.GLXQueryVersion);
            o.writeInt32(SUPPORTED_GLX_VERSION.major);
            o.writeInt32(SUPPORTED_GLX_VERSION.minor);
            
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                major = i.readInt32();
                minor = i.readInt32();
                this.glxVersion = new GLXVersion(major, minor);
                
                i.skip(16);
            }
        }

        send_client_info();
        
        // get a listing of visual configs and store them for later use
        
        visualConfigs = new VisualConfig[display.getScreens().length][];
    }

    /**
     * Get an array of supported Visual Configurations.
     * 
     * GLX opcode 14.
     * @deprecated Use {@link #getVisualConfigs(int)} instead
     */
    @Deprecated
    public VisualConfig[] visual_configs(int screenNo) {

        if (visualConfigs[screenNo] != null)
            return visualConfigs[screenNo];

        RequestOutputStream o = display.getResponseOutputStream();
        VisualConfig[] vcs;
        synchronized (o) {
            o.beginGLXRequest(GLXCommand.GLXGetVisualConfigs);
            o.writeInt32(screenNo);
            
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(4);
                int n = i.readInt32();
                int visual_count = i.readInt32();
                int property_count = i.readInt32();
                i.skip(16);
                assert n == visual_count * property_count;
                if (property_count > 18) {
                    property_count = (property_count - 18) / 2;
                }
                vcs = new VisualConfig[visual_count];
                for (int index = 0; index < visual_count; index++) {
                    vcs[index] = new VisualConfig(i, property_count);
                }
                visualConfigs[screenNo] = vcs;
            }
        }
        return vcs;
    }

    /**
     * Get an array of supported Visual Configurations on the given screen.
     * 
     * GLX opcode 14.
     */
    public VisualConfig[] getVisualConfigs(int screenNo) {
        
        return this.visual_configs(screenNo);
    }

    public final static int VENDOR = 0x1;
    public final static int VERSION = 0x2;
    public final static int EXTENSIONS = 0x3;

    /**
     * Returns a string describing some aspect of the server's GLX extension.
     * 
     * glx opcode 19.
     * 
     * @deprecated use {@link #queryServerString(int, int)} instead
     * 
     * @param name
     *                valid: {@link #VENDOR}, {@link #VERSION},
     *                {@link #EXTENSIONS}
     * 
     * @see <a
     *      href="http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/glx/xqueryserverstring.html">glXQueryServerString</a>
     */
    @Deprecated
    public String server_string(int screen_no, int name) {

        String server_string;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginGLXRequest(GLXCommand.GLXQueryServerString);
            o.writeInt32(screen_no);
            o.writeInt32(name);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(12);
                int len = i.readInt32();
                i.skip(16);
                server_string = i.readString8(len);
                i.pad(len);
            }
        }
        return server_string;
    }

    /**
     * Returns a string describing some aspect of the server's GLX extension.
     * 
     * glx opcode 19.
     * 
     * @param name
     *                valid: {@link #VENDOR}, {@link #VERSION},
     *                {@link #EXTENSIONS}
     * 
     * @see <a
     *      href="http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/glx/xqueryserverstring.html">glXQueryServerString</a>
     */
    public String queryServerString(int screenNo, int name) {

        return this.server_string(screenNo, name);
    }

    public String getClentString (int screenNo, int name)
    {
        // TODO
        return "";
    }
    
    /**
     * glXQueryVersion returns the major and minor version numbers
     * of the GLX extension implemented by the server associated
     * with connection dpy. Implementations with the same major
     * version number are upward compatible, meaning that the
     * implementation with the higher minor number is a superset of
     * the version with the lower minor number.
     * 
     * <a href='http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/glx/xqueryversion.html'>glXQueryVersion</a>
     */
    public GLXVersion queryVersion() {
        
        return this.glxVersion;
    }
    
    /**
     * Returns a string describing which GLX extensions are supported on
     * the connection. The string contains a space-separated list of 
     * extension names (The extension names themselves never contain 
     * spaces). If there are no extensions to GLX, then the empty string is 
     * returned. 
     * 
     * glx opcode 18.
     *  
     * @see <a
     *      href="http://www.opengl.org/sdk/docs/man/xhtml/glXQueryExtensionsString.xml">
     *      glXQueryExtensionsString
     *      </a>
     */
    public String queryExtensionsString(int screenNo) {
              
        // It seems that this is not a real GLX command, so
        // we just add a list of "known" extensions to the list
        // returned by the server.
        //
        // This is more or less how MESA implements this routine:
        //
        // " An extension is supported if the client-side (i.e., libGL)
        // supports it and the "server" supports it.
        // In this case that means that either the true server supports
        // it or it is only for direct-rendering and the direct rendering
        // driver supports it.
        // 
        // If the display is not capable of direct rendering, then the
        // extension is enabled if and only if the client-side library and
        // the server support it."
        
        String serverExtensions =
            this.queryServerString(screenNo, GLX.EXTENSIONS);
        
        // we maintain a list of known exentions depending on the version
        // of OpenGL actually implemented by the server.
        String serverVersion = this.queryServerString(screenNo, GLX.VERSION);
        
        // FIXME: this is not correct, we don't want CLIENT_EXTENSION_STRING
        // here, see the notice above.
        return serverVersion.trim() + EXT_SEPARATOR +
               serverExtensions.trim() + EXT_SEPARATOR + 
               CLIENT_EXTENSION_STRING;
    }
    
    // glx opcode 20 - client info
    private void send_client_info() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            int n = CLIENT_EXTENSION_STRING.length();
            int p = RequestOutputStream.pad(n);
            o.beginRequest(majorOpcode, 20, 4 + (n + p) / 4);
            o.writeInt32(this.SUPPORTED_GLX_VERSION.major);
            o.writeInt32(this.SUPPORTED_GLX_VERSION.minor);
            o.writeInt32(CLIENT_EXTENSION_STRING.length());
            o.writeString8(CLIENT_EXTENSION_STRING);
            o.skip(p);
            o.send();
        }
    }

    // glx opcode 21 - get fb configs
    public int[] fb_configs(int screen_no) { // TODO 1.3

        int[] props;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(majorOpcode, 21, 2);
            o.writeInt32(screen_no);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.skip(4);
                int n = i.readInt32();
                // TODO: Build more useful object from this data.
                int num_fb_configs = i.readInt32();
                int num_props = i.readInt32();
                i.skip(16);
                props = new int[n];
                for (int idx = 0; idx < n; idx++) {
                    props[idx] = i.readInt32();
                }
            }
        }
        return props;
    }

    /**
     * @see GL#GL(GLX, int, int, GL)
     */
    public GL create_context(int visual_id, int screen_no, GL share_list) {

        /* Sending direct==true here leads to a bad crasher. */
        return new GL(this, visual_id, screen_no, share_list, false);
    }

    public gnu.x11.Error build(Display display, int code, int seq_no, int bad,
                               int minor_opcode, int major_opcode) {

        return new Error(display, code - firstError, seq_no, bad,
                         minor_opcode, major_opcode);
    }

    public gnu.x11.event.Event build(Display display, ResponseInputStream i,
                                     int code) {

        // only one extension event
        return new PbufferClobberEvent(display, i);
    }
    
    /**
     * glXChooseVisual returns a XVisualInfo describing the visual that best
     * meets the template XVisualInfo specification, or null, if not matching
     * configuration can be found.
     * 
     * @see <a href="http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/glx/xchoosevisual.html">glXChooseVisual</a>
     */
    public XVisualInfo chooseVisual(int screenNo, int [] attributeList) {
        
        // get a list of all visuals
        VisualConfig[] vcs = getVisualConfigs(screenNo);
        
        // create a template object from the attributeList
        VisualConfig template = new VisualConfig(attributeList);
        VisualConfig bestMatch = null;
        
        // see if we can eliminate some visuals
        for (VisualConfig config : vcs) {
            if (config.compatible(template) &&
                ((bestMatch == null) || (config.compare(bestMatch) < 0))) {
                bestMatch = config;
            }
        }
        
        // then build an XVisualInfo object from the attribute list
        if (bestMatch != null) {
            XVisualInfo visual = new XVisualInfo();
            visual.setID(bestMatch.getVisualID());
            return visual; 
        }
        
        return null;
    }

    // NOTE: as of 2008-06-25 we simply give away the full visual_config
    // family of methods.
    // Being it quite sensitive method call, we expect some code to break.
    // Please, just use chooseVisual and getVisualConfigs instead.
//   /**
//    * @see <a href="http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/glx/xchoosevisual.html">glXChooseVisual</a>
//    * @deprecated Use {@link #chooseVisual(VisualConfig)} instead
//    */
//    @Deprecated
//    public VisualConfig visual_config(VisualConfig template) {
//        return visual_config(display.default_screen_no, template, true);
//    }
//    
//    /** 
//     * @deprecated Use {@link #chooseVisual(int, VisualConfig)} instead
//     */
//    @Deprecated
//    public VisualConfig visual_config(int screen_no, VisualConfig template,
//                                      boolean must) {
//        
//        // we usually try to be compatible with the old code, but
//        // this method, like all the visual_config are proven to be completely
//        // broken, so we need to break compatibilty in this case.
//        throw new NotImplementedException();
//        
////        VisualConfig[] vcs = getVisualConfigs(screen_no);
////        
////        for (int i = 0; i < vcs.length; i++) {
////            if (vcs[i].match(template)) {
////                return vcs[i];
////            }
////        }
////        
////        if (!must)
////            return null;
////        
////      throw new java.lang.Error("No matching: " + template);
//    }

    /**
     * @see GLX#server_string(int, int)
     */
    public String moreString() {

        int screen = display.getDefaultScreenNumber();

        // TODO output like `glxinfo'

        return "\n  client-version: " + SUPPORTED_GLX_VERSION.major + "."
                + SUPPORTED_GLX_VERSION.minor + "\n  server-version: "
                + glxVersion.major + "." + glxVersion.minor
                + "\n  vendor: " + server_string(screen, VENDOR)
                + "\n  extensions: " + server_string(screen, EXTENSIONS);
    }

    public boolean support(int major, int minor) {

        return glxVersion.major == major && glxVersion.minor >= minor;
    }
}
