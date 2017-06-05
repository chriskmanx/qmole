package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Texture map a checkerboard image onto two rectangles with subimage. To
 * texture subimage, press 's' or 'S'. To reset press 'r' or 'R'. Modified
 * from <code>texsub.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/SubimageTexture.gif">
 * screenshot 2</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/SubimageTexture.help">
 * help output</a>
 */
public class SubimageTexture extends gnu.x11.extension.glx.Application {
  private boolean support_bind;
  private int texture;  


  public SubimageTexture (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "wrap checkerboard texture",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo texture subimage, press 's' or 'S'."
      + "\nTo reset, press 'r' or 'R'.");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (250, 250);
    support_bind = gl.support (1, 1) && glx.support (1, 3);

    gl.enable (GL.DEPTH_TEST);
    gl.shade_model (GL.FLAT);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);
    init_texture ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);    
    gl.enable (GL.TEXTURE_2D);
    gl.tex_envf (GL.TEXTURE_ENV, GL.TEXTURE_ENV_MODE, GL.DECAL);
    if (support_bind) gl.bind_texture (GL.TEXTURE_2D, texture);

    gl.begin (GL.QUADS);


    gl.tex_coord2f (0.0f, 0.0f); 
    gl.vertex3f (-2.0f, -1.0f, 0.0f);
    gl.tex_coord2f (0.0f, 1.0f);
    gl.vertex3f (-2.0f, 1.0f, 0.0f);
    gl.tex_coord2f (1.0f, 1.0f);
    gl.vertex3f (0.0f, 1.0f, 0.0f);
    gl.tex_coord2f (1.0f, 0.0f);
    gl.vertex3f (0.0f, -1.0f, 0.0f);

    gl.tex_coord2f (0.0f, 0.0f);
    gl.vertex3f (1.0f, -1.0f, 0.0f);
    gl.tex_coord2f (0.0f, 1.0f);
    gl.vertex3f (1.0f, 1.0f, 0.0f);
    gl.tex_coord2f (1.0f, 1.0f); 
    gl.vertex3f (2.41421f, 1.0f, -1.41421f);
    gl.tex_coord2f (1.0f, 0.0f); 
    gl.vertex3f (2.41421f, -1.0f, -1.41421f);

    gl.end ();

    gl.swap_buffers (window);
    gl.disable (GL.TEXTURE_2D);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case 's':
    case 'S':      
      if (support_bind) gl.bind_texture (GL.TEXTURE_2D, texture);
      gl.tex_subimage_2d (GL.TEXTURE_2D, 0, 12, 44, 
        Checkerboard.SUB_SIZE, Checkerboard.SUB_SIZE,
        GL.RGBA, GL.UNSIGNED_BYTE, Checkerboard.SUB_PIXELS);
      break;

    case 'r':
    case 'R':
      if (support_bind) gl.bind_texture (GL.TEXTURE_2D, texture);
      tex_image_2d ();
      break;

    default: return;
    }

    mark_window_dirty ();
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    double wh = (float) width / (float) height;
    glu.perspective (60.0, wh, 1.0, 30.0);
    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    gl.translatef (0.0f, 0.0f, -3.6f);
  }


  private void init_texture () {
    if (support_bind) {
      texture = gl.gen_textures (1) [0];
      gl.bind_texture (GL.TEXTURE_2D, texture);
    }

    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_WRAP_S, GL.REPEAT);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_WRAP_T, GL.REPEAT);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_MAG_FILTER, 
      GL.NEAREST);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_MIN_FILTER, 
      GL.NEAREST);

    tex_image_2d ();
  }


  private void tex_image_2d () {
    int format = support_bind ? GL.RGBA : 4;
    gl.tex_image_2d (GL.TEXTURE_2D, 0, format, Checkerboard.SIZE, 
      Checkerboard.SIZE, 0, GL.RGBA, GL.UNSIGNED_BYTE, 
      Checkerboard.PIXELS);
  }


  public static void main (String [] args) {
    new SubimageTexture (args).exec ();
  }
}
