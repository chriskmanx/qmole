package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;
import gnu.x11.Data;


/**
 * Test ABGR extension. Modified from <code>abgr.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html"> opengl
 * sample</a> by SGI.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/ABGR.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/ABGR.help">
 * help output</a>
 */
public class ABGR extends gnu.x11.extension.glx.Application {
  private static final int IMAGE_SIZE = 128;
  private static byte[] IMAGE;
  private static final byte [] IMAGE0 = new byte [IMAGE_SIZE * IMAGE_SIZE * 4];


  public ABGR (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "abgr extension",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (400, 400);
    
    gl.disable (GL.DITHER);
    gl.clear_color (0.0f, 0.0f, 0.0f, 1.0f);
    init_image ();
  }


  private void draw_texture () {
    gl.tex_image_2d (GL.TEXTURE_2D, 0, 3, IMAGE_SIZE, IMAGE_SIZE, 0, GL.ABGR_EXT,
      GL.UNSIGNED_BYTE, IMAGE);

    gl.tex_parameterf (GL.TEXTURE_2D, GL.TEXTURE_WRAP_S, GL.REPEAT);
    gl.tex_parameterf (GL.TEXTURE_2D, GL.TEXTURE_WRAP_T, GL.REPEAT);
    gl.tex_parameterf (GL.TEXTURE_2D, GL.TEXTURE_MIN_FILTER, GL.NEAREST);

    gl.tex_envf (GL.TEXTURE_ENV, GL.TEXTURE_ENV_MODE, GL.DECAL);
    gl.enable (GL.TEXTURE_2D);

    gl.begin (GL.POLYGON);
    gl.tex_coord2f (1.0f, 1.0f); gl.vertex3f (-0.2f, 0.8f, -100.0f);
    gl.tex_coord2f (0.0f, 1.0f); gl.vertex3f (-0.8f, 0.8f, -2.0f);
    gl.tex_coord2f (0.0f, 0.0f); gl.vertex3f (-0.8f, 0.2f, -2.0f);
    gl.tex_coord2f (1.0f, 0.0f); gl.vertex3f (-0.2f, 0.2f, -100.0f);
    gl.end ();

    gl.tex_image_2d (GL.TEXTURE_2D, 0, 3, IMAGE_SIZE, IMAGE_SIZE, 0, GL.RGBA,
      GL.UNSIGNED_BYTE, IMAGE);

    gl.begin (GL.POLYGON);
    gl.tex_coord2f (1.0f, 1.0f); gl.vertex3f (0.8f, 0.8f, -2.0f);
    gl.tex_coord2f (0.0f, 1.0f); gl.vertex3f (0.2f, 0.8f, -100.0f);
    gl.tex_coord2f (0.0f, 0.0f); gl.vertex3f (0.2f, 0.2f, -100.0f);
    gl.tex_coord2f (1.0f, 0.0f); gl.vertex3f (0.8f, 0.2f, -2.0f);
    gl.end ();

    gl.disable (GL.TEXTURE_2D);
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.raster_pos3f (-0.8f, -0.8f, -1.5f);
    gl.draw_pixels (IMAGE_SIZE, IMAGE_SIZE, GL.ABGR_EXT, GL.UNSIGNED_BYTE, IMAGE);

    gl.raster_pos3f (0.2f, -0.8f, -1.5f);
    gl.draw_pixels (IMAGE_SIZE, IMAGE_SIZE, GL.RGBA, GL.UNSIGNED_BYTE, IMAGE);

    draw_texture ();       
    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    glu.perspective (60.0, 1.0, 0.1, 1000.0);
    gl.matrix_mode (GL.MODELVIEW);
  }


  private void init_image () {
    int band = IMAGE_SIZE/4;

    for (int i=0; i<band; i++)
      for (int j=0; j<IMAGE_SIZE; j++) {
             
        IMAGE0 [(i + 0*band) * IMAGE_SIZE * 4 + j * 4 + 0] = (byte) 0xff;
        IMAGE0 [(i + 0*band) * IMAGE_SIZE * 4 + j * 4 + 1] = (byte) 0x00;
        IMAGE0 [(i + 0*band) * IMAGE_SIZE * 4 + j * 4 + 2] = (byte) 0x00;
        IMAGE0 [(i + 0*band) * IMAGE_SIZE * 4 + j * 4 + 3] = (byte) 0xff;

        IMAGE0 [(i + 1*band) * IMAGE_SIZE * 4 + j * 4 + 0] = (byte) 0xff;
        IMAGE0 [(i + 1*band) * IMAGE_SIZE * 4 + j * 4 + 1] = (byte) 0x00;
        IMAGE0 [(i + 1*band) * IMAGE_SIZE * 4 + j * 4 + 2] = (byte) 0xff;
        IMAGE0 [(i + 1*band) * IMAGE_SIZE * 4 + j * 4 + 3] = (byte) 0x00;

        IMAGE0 [(i + 2*band) * IMAGE_SIZE * 4 + j * 4 + 0] = (byte) 0xff;
        IMAGE0 [(i + 2*band) * IMAGE_SIZE * 4 + j * 4 + 1] = (byte) 0xff;
        IMAGE0 [(i + 2*band) * IMAGE_SIZE * 4 + j * 4 + 2] = (byte) 0x00;
        IMAGE0 [(i + 2*band) * IMAGE_SIZE * 4 + j * 4 + 3] = (byte) 0x00;

        IMAGE0 [(i + 3*band) * IMAGE_SIZE * 4 + j * 4 + 0] = (byte) 0x00;
        IMAGE0 [(i + 3*band) * IMAGE_SIZE * 4 + j * 4 + 1] = (byte) 0xff;
        IMAGE0 [(i + 3*band) * IMAGE_SIZE * 4 + j * 4 + 2] = (byte) 0x00;
        IMAGE0 [(i + 3*band) * IMAGE_SIZE * 4 + j * 4 + 3] = (byte) 0xff;
      }

    IMAGE = IMAGE0;
  }


  public static void main (String [] args) {
    new ABGR (args).exec ();
  }
}
