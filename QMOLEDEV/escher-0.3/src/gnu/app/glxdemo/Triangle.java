package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;
import gnu.x11.keysym.Misc;


/**
 * Draw a triangle with various attributes. Modified from
 * <code>tri.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html"> opengl
 * sample</a> by SGI.
 *
 * <ul>
 * <li>To toggle anti-aliasing, press '0'.
 * <li>To draw polygon with point, press '1'.
 * <li>To draw polygon with line, press '2'.
 * <li>To draw polygon with fill, press '3'.
 * <li>To draw with solid, press '4'.
 * <li>To draw with line, press '5'.
 * <li>To draw with point, press '6'.
 * <li>To toggle culling, press '7'.
 * <li>To toggle winding, press '8'.
 * <li>To toggle cull-front, press '9'.
 * <li>To toggle showing bottom, press 'b' or 'B'.
 * <li>To toggle dithering, press 'd' or 'D'.
 * <li>To toggle showing outline, press 'o' or 'O'.
 * <li>To toggle showing vertex, press 'v' or 'V'.
 * <li>To toggle shading, press 's' or 'S'.
 * <li>To zoom in, press 'z'.
 * <li>To zoom out, press 'Z'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Triangle.gif">
 * screenshot 12</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Triangle.help">
 * help output</a>
 */
public class Triangle extends gnu.x11.extension.glx.Application {
  private static final int SOLID = 0;
  private static final int LINE = 1;
  private static final int POINT = 2;

  private static final float [] BOX_A = {-100.0f, -100.0f, 0.0f};
  private static final float [] BOX_B = {100.0f, -100.0f, 0.0f};
  private static final float [] BOX_C = {100.0f, 100.0f, 0.0f};
  private static final float [] BOX_D = {-100.0f, 100.0f, 0.0f};

  private static final float [] POINT0 = {-125.0f, -80.0f, 0.0f};
  private static final float [] POINT1 = {-125.0f, 80.0f, 0.0f};
  private static final float [] POINT2 = {172.0f, 0.0f, 0.0f};
    

  private boolean anti_aliasing = false;
  private boolean culling = false;
  private boolean cull_front = false;
  private boolean dithering = true;
  private boolean shading = true;
  private boolean show_bottom = true;
  private boolean show_outline = true;
  private boolean show_vertex = true;
  private boolean winding = false;

  private int draw_state = SOLID;
  private float zoom = 1.0f;
  private float z_angle = 90.0f;  


  public Triangle (String [] args) {
    super (args, KEYBOARD_BIT);

    about ("0.1", "triangle",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle anti-aliasing, press '0'."
      + "\nTo draw polygon with point, press '1'."
      + "\nTo draw polygon with line, press '2'."
      + "\nTo draw polygon with fill, press '3'."
      + "\nTo draw with solid, press '4'."
      + "\nTo draw with line, press '5'."
      + "\nTo draw with point, press '6'."
      + "\nTo toggle culling, press '7'."
      + "\nTo toggle winding, press '8'."
      + "\nTo toggle cull-front, press '9'."
      + "\nTo toggle showing bottom, press 'b' or 'B'."
      + "\nTo toggle dithering, press 'd' or 'D'."
      + "\nTo toggle showing outline, press 'o' or 'O'."
      + "\nTo toggle showing vertex, press 'v' or 'V'."
      + "\nTo toggle shading, press 's' or 'S'."
      + "\nTo zoom in, press 'z'."
      + "\nto zoom out, press 'Z'.");

    if (help_option) return;
    init_window (600, 300);

    gl.enable (GL.SCISSOR_TEST);
    gl.line_stipple (1, 0xf0f0);
    gl.blend_func (GL.SRC_ALPHA, GL.ONE);
  }


  private void draw_bottom () {
    if (!show_bottom) return;

    gl.push_matrix ();

    gl.scalef (zoom, zoom, zoom);
    gl.rotatef (z_angle, 0.0f, 0.0f, 1.0f);
    gl.color3f (0.0f, 0.0f, 1.0f);

    switch (draw_state) {
    case SOLID: gl.begin (GL.POLYGON); break;
    case LINE: gl.begin (GL.LINE_LOOP); break;
    case POINT: gl.begin (GL.POINTS); break;
    }

    gl.vertex3fv (POINT0);
    gl.vertex3fv (POINT1);
    gl.vertex3fv (POINT2);
    gl.end ();
    
    draw_vertex ();
    gl.pop_matrix ();
  }


  private void draw_box () {
    gl.begin (GL.LINE_LOOP);
    gl.vertex3fv (BOX_A);
    gl.vertex3fv (BOX_B);
    gl.vertex3fv (BOX_C);
    gl.vertex3fv (BOX_D);
    gl.end ();
  }


  private void draw_prism () {
    init_prism_start ();

    switch (draw_state) {
    case SOLID: gl.begin (GL.POLYGON); break;
    case LINE: gl.begin (GL.LINE_LOOP); break;
    case POINT: gl.begin (GL.POINTS); break;
    }

    gl.color3f (1.0f, 0.0f, 0.0f);
    gl.vertex3fv (POINT0);

    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.vertex3fv (POINT1);

    gl.rectf (POINT2[0]-2, POINT2[1]-2, POINT2[0]+2, POINT2[1]+2);
    gl.vertex3fv (POINT2);

    gl.end ();
    init_prism_end ();
  }
    

  private void draw_vertex () {
    if (!show_vertex) return;

    gl.color3f (1.0f, 0.0f, 0.0f);
    gl.rectf (POINT0[0]-2, POINT0[1]-2, POINT0[0]+2, POINT0[1]+2);

    gl.color3f (0.0f, 1.0f, 0.0f);
    gl.rectf (POINT1[0]-2, POINT1[1]-2, POINT1[0]+2, POINT1[1]+2);

    gl.color3f (0.0f, 0.0f, 1.0f);
    gl.rectf (POINT2[0]-2, POINT2[1]-2, POINT2[0]+2, POINT2[1]+2);
  }


  private void draw_outline () {
    if (!show_outline) return;

    gl.color3f (1.0f, 1.0f, 1.0f);

    gl.begin (GL.LINE_LOOP);
    gl.vertex3fv (POINT0);
    gl.vertex3fv (POINT1);
    gl.vertex3fv (POINT2);
    gl.end ();
  }    


  protected void handle_expose () {
    init_view1 ();
    init_render ();
    draw_box ();
    draw_bottom ();

    init_view2 ();
    gl.push_matrix ();
    draw_prism ();
    draw_outline ();  
    gl.pop_matrix ();  
 
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case '0': anti_aliasing = !anti_aliasing; break;
    case '1': gl.polygon_mode (GL.FRONT_AND_BACK, GL.POINT); break;
    case '2': gl.polygon_mode (GL.FRONT_AND_BACK, GL.LINE); break;
    case '3': gl.polygon_mode (GL.FRONT_AND_BACK, GL.FILL); break;
    case '4': draw_state = POINT; break;
    case '5': draw_state = LINE; break;
    case '6': draw_state = SOLID; break;
    case '7': culling = !culling; break;
    case '8': winding = !winding; break;
    case '9': cull_front = !cull_front; break;
    case 'b':                   // fall through
    case 'B': show_bottom = !show_bottom; break;
    case 'd':                   // fall through
    case 'D': dithering = !dithering; break;
    case 'o':                   // fall through
    case 'O': show_outline = !show_outline; break;
    case 'v':                   // fall through
    case 'V': show_vertex = !show_vertex; break;
    case 's':                   // fall through
    case 'S': shading = !shading; break;
    case 'Z': zoom *= 0.75f; break;
    case Misc.LEFT: z_angle += 0.5f; break;
    case Misc.RIGHT: z_angle -= 0.5f; break;

    case 'z': 
      zoom /= 0.75f; 
      if (zoom > 10.0f) zoom = 10.0f;
      break;
      
    default: return;
    }

    mark_window_dirty ();
  }


  private void init_prism_start () {
    gl.scalef (zoom, zoom, zoom);
    gl.rotatef (z_angle, 0,0,1);

    gl.point_size (10);
    gl.line_width (5);
    gl.enable (GL.POINT_SMOOTH);
    gl.enable (GL.LINE_STIPPLE);
    gl.blend_func (GL.SRC_ALPHA, GL.ONE_MINUS_SRC_ALPHA);
  }


  private void init_prism_end () {
    gl.point_size (1);
    gl.line_width (1);
    gl.disable (GL.POINT_SMOOTH);
    gl.disable (GL.LINE_STIPPLE);
    gl.blend_func (GL.ONE, GL.ZERO);
  }
    

  private void init_render () {
    gl.clear (GL.COLOR_BUFFER_BIT);
    gl.color3f (0.0f, 1.0f, 0.0f);

    gl.capability (GL.BLEND, anti_aliasing);
    gl.capability (GL.CULL_FACE, culling);
    gl.capability (GL.DITHER, dithering);
    gl.capability (GL.POLYGON_SMOOTH, anti_aliasing);

    if (anti_aliasing) gl.blend_func (GL.SRC_ALPHA, GL.ONE);
    gl.cull_face (cull_front ? GL.FRONT : GL.BACK);    
    gl.front_face (winding ? GL.CCW : GL.CW);
    gl.shade_model (shading ? GL.SMOOTH : GL.FLAT);
  }

  
  private void init_view1 () {
    gl.viewport (0, 0, window.width, window.height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    glu.ortho_2d (-175, 175, -175, 175);
    gl.matrix_mode (GL.MODELVIEW);
    gl.scissor (0, 0, window.width, window.height);
  }


  private void init_view2 () {
    float factor = 1.0f / 2.0f / 175.0f * (175.0f - 100.0f);
    int scale_x = (int) ((window.width - 20.0f) * factor + 10);
    int scale_y = (int) ((window.height - 20.0f) * factor + 10);
    int scale_width = (int) (window.width - 2.0f * scale_x);
    int scale_height = (int) (window.height - 2.0f * scale_y);

    gl.viewport (scale_x, scale_y, scale_width, scale_height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    glu.ortho_2d (-100, 100, -100, 100);
    gl.matrix_mode (GL.MODELVIEW);
    gl.scissor (scale_x, scale_y, scale_width, scale_height);
  }


  public static void main (String [] args) {
    new Triangle (args).exec ();
  }
}
