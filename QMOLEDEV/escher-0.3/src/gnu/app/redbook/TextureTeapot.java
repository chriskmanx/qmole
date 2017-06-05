package gnu.app.redbook;

import gnu.x11.extension.glx.GL;
import gnu.x11.Data;


/**
 * Texture map teapot. To demonstrate how to draw a texture mapped teapot
 * with automatically generated texture coordinates. The texture is
 * rendered as stripes on the teapot. Initially, the object is drawn with
 * texture coordinates based upon the object coordinates of the vertex and
 * distance from the plane x = 0. Modified from <code>texgen.c</code>.
 *
 * <ul>
 * <li>To use eye-coordinate, press 'e' or 'E'.
 * <li>To use object-coordinate, press 'o' or 'O'.
 * <li>To use a slanted plane, press 's' or 'S'.
 * <li>To use a yz-plane, press 'x' or 'X'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TextureTeapot.gif">
 * screenshot 4</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TextureTeapot.help">
 * help output</a>
 */
public class TextureTeapot extends gnu.x11.extension.glx.Application {
  private static final int IMAGE_SIZE = 32;
  private static byte[] IMAGE;
  private static final byte [] IMAGE0 = new byte [IMAGE_SIZE * 4];
  private static final float [] TEXTURE_SLANTED = {1.0f, 1.0f, 1.0f, 0.0f};
  private static final float [] TEXTURE_YZ = {1.0f, 0.0f, 0.0f, 0.0f};

  private boolean support_bind;
  private int texture;
  private int texture_plane = GL.OBJECT_PLANE;
  private float [] texture_coeff = TEXTURE_YZ;


  public TextureTeapot (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "texture map teapot",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo use eye-coordinate, press 'e' or 'E'."
      + "\nTo use object-coordinate, press 'o' or 'O'."
      + "\nTo use a slanted plane, press 's' or 'S'."
      + "\nTo use a yz-plane, press 'x' or 'X'.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (256, 256);
    support_bind = gl.support (1, 1) && glx.support (1, 3);

    gl.cull_face (GL.BACK);
    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.CULL_FACE);
    gl.enable (GL.DEPTH_TEST);
    gl.enable (GL.NORMALIZE);
    gl.front_face (GL.CW);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);

    init_image ();
    init_light ();
    init_texture ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);    
    gl.push_matrix ();
    gl.rotatef (45.0f, 0.0f, 0.0f, 1.0f);
    if (support_bind) gl.bind_texture (GL.TEXTURE_1D, texture);
    glut.solid_teapot (2.0);
    gl.pop_matrix ();
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 'e':                   // fall through
    case 'E': 
      gl.tex_geni (GL.S, GL.TEXTURE_GEN_MODE, GL.EYE_LINEAR);
      texture_plane = GL.EYE_PLANE;
      break;

    case 'o':                   // fall through
    case 'O':
      gl.tex_geni (GL.S, GL.TEXTURE_GEN_MODE, GL.OBJECT_LINEAR);
      texture_plane = GL.OBJECT_PLANE;
      break;

    case 's':                   // fall through
    case 'S': texture_coeff = TEXTURE_SLANTED; break;
    case 'x':                   // fall through
    case 'X': texture_coeff = TEXTURE_YZ; break;
    default: return;
    }

    gl.tex_genfv (GL.S, texture_plane, texture_coeff);
    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();

    double wh = (float) width / (float) height;
    double hw = (float) height / (float) width;
    
    if (width <= height)
      gl.ortho (-3.5, 3.5, -3.5*hw, 3.5*hw, -3.5, 3.5);
    else
      gl.ortho (-3.5*wh, 3.5*wh, -3.5, 3.5, -3.5, 3.5);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
  }


  private void init_image () {
    for (int i=0; i<IMAGE_SIZE; i++) {
      IMAGE0 [i * 4 + 0] = (i <= 4) ? (byte) 255 : 0;
      IMAGE0 [i * 4 + 1] = (i >= 4) ? (byte) 255 : 0;
      IMAGE0 [i * 4 + 2] = (byte) 0;
      IMAGE0 [i * 4 + 3] = (byte) 255;
    }

    IMAGE = IMAGE0;
  }


  private void init_light () {
    gl.enable (GL.LIGHT0);
    gl.enable (GL.LIGHTING);
    gl.materialf (GL.FRONT, GL.SHININESS, 64.0f);
  }


  private void init_texture () {
   gl.enable (GL.TEXTURE_1D);
   gl.enable (GL.TEXTURE_GEN_S);

    if (support_bind) {
      texture = gl.gen_textures (1) [0];
      gl.bind_texture (GL.TEXTURE_2D, texture);
    }

    gl.tex_parameteri (GL.TEXTURE_1D, GL.TEXTURE_WRAP_S, GL.REPEAT);
    gl.tex_parameteri (GL.TEXTURE_1D, GL.TEXTURE_MAG_FILTER, GL.LINEAR);
    gl.tex_parameteri (GL.TEXTURE_1D, GL.TEXTURE_MIN_FILTER, GL.LINEAR);
    
    int format = support_bind ? GL.RGBA : 4;
    gl.tex_image_1d (GL.TEXTURE_1D, 0, format, IMAGE_SIZE,
      0, GL.RGBA, GL.UNSIGNED_BYTE, IMAGE);

    gl.tex_envf (GL.TEXTURE_ENV, GL.TEXTURE_ENV_MODE, GL.MODULATE);
    gl.tex_geni (GL.S, GL.TEXTURE_GEN_MODE, GL.OBJECT_LINEAR);
    gl.tex_genfv (GL.S, texture_plane, texture_coeff);
  }


  public static void main (String [] args) {
    new TextureTeapot (args).exec ();
  }
}
