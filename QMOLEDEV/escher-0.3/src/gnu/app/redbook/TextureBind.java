package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Test texture binding by creating and managing two textures. Modified from <code>texbind.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TextureBind.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/TextureBind.help">
 * help output</a>
 */
public class TextureBind extends gnu.x11.extension.glx.Application {
  private int [] textures;  


  public TextureBind (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "texture binding",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;

    visual_config.set_depth_size (1);
    init_window (250, 250);

    if (!gl.support (1, 1) || !glx.support (1, 3))
      throw new Error ("Texture binding unsupported");

    gl.enable (GL.DEPTH_TEST);
    gl.shade_model (GL.FLAT);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);
    init_texture ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT | GL.DEPTH_BUFFER_BIT);    

    gl.bind_texture (GL.TEXTURE_2D, textures [0]);
    gl.begin (GL.QUADS);
    gl.tex_coord2f (0.0f, 0.0f); 
    gl.vertex3f (-2.0f, -1.0f, 0.0f);
    gl.tex_coord2f (0.0f, 1.0f);
    gl.vertex3f (-2.0f, 1.0f, 0.0f);
    gl.tex_coord2f (1.0f, 1.0f);
    gl.vertex3f (0.0f, 1.0f, 0.0f);
    gl.tex_coord2f (1.0f, 0.0f);
    gl.vertex3f (0.0f, -1.0f, 0.0f);
    gl.end ();

    gl.bind_texture (GL.TEXTURE_2D, textures [1]);
    gl.begin (GL.QUADS);
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
    gl.enable (GL.TEXTURE_2D);
    textures = gl.gen_textures (2);

    gl.bind_texture (GL.TEXTURE_2D, textures [0]);
    init_texture_parameter ();
    gl.tex_image_2d (GL.TEXTURE_2D, 0, GL.RGBA, Checkerboard.SIZE, 
      Checkerboard.SIZE, 0, GL.RGBA, GL.UNSIGNED_BYTE, 
      Checkerboard.PIXELS);

    gl.bind_texture (GL.TEXTURE_2D, textures [1]);
    init_texture_parameter ();
    gl.tex_envf (GL.TEXTURE_ENV, GL.TEXTURE_ENV_MODE, GL.DECAL); // vs. above
    gl.tex_image_2d (GL.TEXTURE_2D, 0, GL.RGBA, Checkerboard.SIZE, 
      Checkerboard.SIZE, 0, GL.RGBA, GL.UNSIGNED_BYTE, 
      Checkerboard.RED_PIXELS);
  }


  private void init_texture_parameter () {
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_WRAP_S, GL.CLAMP);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_WRAP_T, GL.CLAMP);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_MAG_FILTER, 
      GL.NEAREST);
    gl.tex_parameteri (GL.TEXTURE_2D, GL.TEXTURE_MIN_FILTER, 
      GL.NEAREST);
  }


  public static void main (String [] args) {
    new TextureBind (args).exec ();
  }
}
