package gnu.app.glxdemo;

import gnu.x11.extension.glx.GL;


/**
 * Draw primitive shapes. Modified from <code>prim.c</code> in <a href=
 * "http://trant.sgi.com/opengl/examples/samples/samples.html">
 * opengl sample</a> by SGI.
 *
 * <ul>
 * <li>To toggle smooth shading, press '1'.
 * <li>To toggle polygon filling, press '2'.
 * <li>To rotate color mask, press '3'.
 * </ul>
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Primitive.gif">
 * screenshot 10</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/glxdemo/Primitive.help">
 * help output</a>
 */
public class Primitive extends gnu.x11.extension.glx.Application {
  private static final int COLUMN_COUNT = 4;
  private static final int GAP = 10;
  private static final int ROW_COUNT = 3;
  private static final int BITMAP_WIDTH = 48;
  private static final int BITMAP_HEIGHT = 13;


  private static final byte [] BITMAP = {
    (byte) 0x00, (byte) 0x03, (byte) 0x00, 
    (byte) 0x00, (byte) 0x00, (byte) 0x00, 
    (byte) 0x7f, (byte) 0xfb, (byte) 0xff, 
    (byte) 0xff, (byte) 0xff, (byte) 0x01,
    (byte) 0x7f, (byte) 0xfb, (byte) 0xff, 
    (byte) 0xff, (byte) 0xff, (byte) 0x01, 
    (byte) 0x00, (byte) 0x03, (byte) 0x00, 
    (byte) 0x00, (byte) 0x00, (byte) 0x00,
    (byte) 0x3e, (byte) 0x8f, (byte) 0xb7, 
    (byte) 0xf9, (byte) 0xfc, (byte) 0x01, 
    (byte) 0x63, (byte) 0xdb, (byte) 0xb0, 
    (byte) 0x8d, (byte) 0x0d, (byte) 0x00,
    (byte) 0x63, (byte) 0xdb, (byte) 0xb7, 
    (byte) 0x8d, (byte) 0x0d, (byte) 0x00, 
    (byte) 0x63, (byte) 0xdb, (byte) 0xb6, 
    (byte) 0x8d, (byte) 0x0d, (byte) 0x00,
    (byte) 0x63, (byte) 0x8f, (byte) 0xf3, 
    (byte) 0xcc, (byte) 0x0d, (byte) 0x00, 
    (byte) 0x63, (byte) 0x00, (byte) 0x00, 
    (byte) 0x0c, (byte) 0x4c, (byte) 0x0a,
    (byte) 0x63, (byte) 0x00, (byte) 0x00, 
    (byte) 0x0c, (byte) 0x4c, (byte) 0x0e, 
    (byte) 0x63, (byte) 0x00, (byte) 0x00, 
    (byte) 0x8c, (byte) 0xed, (byte) 0x0e,
    (byte) 0x3e, (byte) 0x00, (byte) 0x00, 
    (byte) 0xf8, (byte) 0x0c, (byte) 0x00, 
  };


  private static final float [] [] COLORS = {
    {0.0f, 0.0f, 0.0f},
    {1.0f, 0.0f, 0.0f},
    {0.0f, 1.0f, 0.0f},
    {1.0f, 1.0f, 0.0f},
    {0.0f, 0.0f, 1.0f},
    {1.0f, 0.0f, 1.0f},
    {0.0f, 1.0f, 1.0f},
    {1.0f, 1.0f, 1.0f},
  };


  private int box_width, box_height;
  private int color_rotation;
  private boolean polygon_fill = true;
  private boolean smooth_shade = true;
  int [] [] vertex_position = new int [8] [2];


  public Primitive (String [] args) {
    super (args, KEYBOARD_BIT | RESIZE_BIT);

    about ("0.1", "primitive shapes",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/",
      "\nTo toggle smooth shading, press '1'."
      + "\nTo toggle polygon filling, press '2'."
      + "\nTo rotate color mask, press '3'.");

    if (help_option) return;
    init_window (600, 300);

    gl.blend_func (GL.SRC_ALPHA, GL.ZERO);
  }


  private float center (int i) { return i + 0.5f; }


  private void draw_bitmap () {
    gl.begin (GL.LINES);

    gl.color3fv (COLORS [1]);
    gl.vertex2i (-box_width/2, 0);
    gl.vertex2i (box_width/2, 0);
    gl.vertex2i (0, -box_height/2);
    gl.vertex2i (0, box_height/2);

    gl.color3fv (COLORS [2]);
    gl.vertex2i (0, -3);
    gl.vertex2i (0, -3+BITMAP_HEIGHT);

    gl.color3fv (COLORS [3]);
    gl.vertex2i (0, -3);
    gl.vertex2i (BITMAP_WIDTH, -3);
    gl.end ();

    gl.color3fv (COLORS [4]);

    gl.pixel_storei (GL.UNPACK_LSB_FIRST, GL.TRUE);
    gl.pixel_storei (GL.UNPACK_ALIGNMENT, 1);

    gl.raster_pos2i (0, 0);
    gl.bitmap (BITMAP_WIDTH, BITMAP_HEIGHT, 0, 3, 0.0f, 0.0f, BITMAP);
  }


  private void draw_box () {
    gl.color3fv (COLORS [1]);
    gl.vertex2f (center (-box_width/4), center (-box_height/4));

    gl.color3fv (COLORS [2]);
    gl.vertex2f (center (-box_width/4), center (box_height/4));

    gl.color3fv (COLORS [3]);
    gl.vertex2f (center (box_width/4), center (box_height/4));

    gl.color3fv (COLORS [4]);
    gl.vertex2f (center (box_width/4), center (-box_height/4));
  }

  
  private void draw_fan () {
    int y0 = -box_height/4;
    int y1 = y0 + box_height/2/3;
    int y2 = y1 + box_height/2/3;
    int y3 = box_height/4;
    int x0 = -box_width/4;
    int x1 = x0 + box_width/2/3;
    int x2 = x1 + box_width/2/3;
    int x3 = box_width/4;

    vertex_position [0] [0] = x0;
    vertex_position [0] [1] = y1;
    vertex_position [1] [0] = x0;
    vertex_position [1] [1] = y2;
    vertex_position [2] [0] = x1;
    vertex_position [2] [1] = y3;
    vertex_position [3] [0] = x2;
    vertex_position [3] [1] = y3;
    vertex_position [4] [0] = x3;
    vertex_position [4] [1] = y2;
    vertex_position [5] [0] = x3;
    vertex_position [5] [1] = y1;
    vertex_position [6] [0] = x2;
    vertex_position [6] [1] = y0;
    vertex_position [7] [0] = x1;
    vertex_position [7] [1] = y0;

    for (int i=0; i<8; i++) {
      gl.color3fv (COLORS [7-i]);
      gl.vertex2iv (vertex_position [i]);
    }
  }


  private void draw_line () {
    gl.push_matrix ();

    gl.translatef (-12.0f, 0.0f, 0.0f);
    for (int i=1; i<8; i++) {
      gl.color3fv (COLORS [i]);
      gl.begin (GL.LINES);
      gl.vertex2i (-box_width/4, -box_height/4);
      gl.vertex2i (box_width/4, box_height/4);
      gl.end ();
      gl.translatef (4.0f, 0.0f, 0.0f);
    }

    gl.pop_matrix ();

    gl.color3fv (COLORS [7]);    
    gl.begin (GL.LINES);
    gl.vertex2i (0, 0);
    gl.end ();
  }


  private void draw_line_strip () {
    gl.begin (GL.LINE_STRIP);
    draw_box ();
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.LINE_STRIP);
    gl.vertex2i (0, 0);
    gl.end ();
  }


  private void draw_loop () {
    gl.begin (GL.LINE_LOOP);
    draw_box ();
    gl.end ();

    gl.enable (GL.LOGIC_OP);
    gl.logic_op (GL.XOR);

    gl.enable (GL.BLEND);
    gl.blend_func (GL.ONE, GL.ONE);

    gl.color3fv (COLORS [5]);
    gl.begin (GL.LINE_LOOP);
    gl.vertex2f (center (-box_width/8), center (-box_height/8));
    gl.vertex2f (center (-box_width/8), center (box_height/8));
    gl.end ();

    gl.begin (GL.LINE_LOOP);
    gl.vertex2f (center (-box_width/8), center (box_height/8+5));
    gl.vertex2f (center (box_width/8), center (box_height/8+5));
    gl.end ();

    gl.disable (GL.LOGIC_OP);
    gl.disable (GL.BLEND);

    gl.color3fv (COLORS [6]);
    gl.begin (GL.POINTS);
    gl.vertex2i (0, 0);
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.LINE_LOOP);
    gl.vertex2i (0, 0);
    gl.end ();
  }

        
  private void draw_point () {
    gl.begin (GL.POINTS);

    for (int i=1; i<8; i++) {
      int j = i * 2;
      gl.color3fv (COLORS [i]);

      gl.vertex2i (-j, -j);
      gl.vertex2i (-j, 0);
      gl.vertex2i (-j, j);
      gl.vertex2i (0, j);
      gl.vertex2i (j, j);
      gl.vertex2i (j, 0);
      gl.vertex2i (j, -j);
      gl.vertex2i (0, -j);
    }
    gl.end ();
  }

  
  private void draw_polygon () {
    gl.begin (GL.POLYGON);
    draw_fan ();
    gl.end ();    

    gl.color3fv (COLORS [7]);
    gl.begin (GL.POLYGON);
    gl.vertex2i (0, 0);
    gl.vertex2i (100, 100);
    gl.end ();
  }


  private void draw_quad () {
    gl.begin (GL.QUADS);
    draw_quad_internal ();
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.QUADS);
    gl.vertex2i (0, 0);
    gl.vertex2i (100, 100);
    gl.vertex2i (-100, 100);
    gl.end ();
  }

  private void draw_quad_internal () {
    gl.color3fv (COLORS [1]);
    gl.vertex2i (-box_width/4, -box_height/4);
    gl.color3fv (COLORS [2]);
    gl.vertex2i (-box_width/4, box_height/4);
    gl.color3fv (COLORS [3]);
    gl.vertex2i (0, -box_height/4);
    gl.color3fv (COLORS [4]);
    gl.vertex2i (0, box_height/4);
    gl.color3fv (COLORS [5]);
    gl.vertex2i (box_width/4, -box_height/4);
    gl.color3fv (COLORS [6]);
    gl.vertex2i (box_width/4, box_height/4);
  }
    
    
  private void draw_quad_strip () {
    gl.begin (GL.QUAD_STRIP);
    draw_quad_internal ();
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.QUAD_STRIP);
    gl.vertex2i (0, 0);
    gl.vertex2i (100, 100);
    gl.vertex2i (-100, 100);
    gl.end ();
  }


  private void draw_rectangle () {
    gl.color3fv (COLORS [5]);
    gl.recti (-box_width/4, -box_height/4, box_width/4, box_height/4);
  }


  private void draw_triangle () {
    gl.begin (GL.TRIANGLES);
    gl.color3fv (COLORS [1]);
    gl.vertex2i (-box_width/4, -box_height/4);
    gl.color3fv (COLORS [2]);
    gl.vertex2i (-box_width/8, -box_height/16);
    gl.color3fv (COLORS [3]);
    gl.vertex2i (box_width/8, -box_height/16);

    gl.color3fv (COLORS [4]);
    gl.vertex2i (-box_width/4, box_height/4);
    gl.color3fv (COLORS [5]);
    gl.vertex2i (-box_width/8, box_height/16);
    gl.color3fv (COLORS [6]);
    gl.vertex2i (box_width/8, box_height/16);
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.TRIANGLES);
    gl.vertex2i (0, 0);
    gl.vertex2i (-100, 100);
    gl.end ();
  }


  private void draw_triangle_fan () {
    gl.begin (GL.TRIANGLE_FAN);
    gl.color3fv (COLORS [7]);
    gl.vertex2i (0, 0);
    draw_fan ();
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.TRIANGLE_FAN);
    gl.vertex2i (0, 0);
    gl.vertex2i (-100, 100);
    gl.end ();
  }


  private void draw_triangle_strip () {
    gl.begin (GL.TRIANGLE_STRIP);
    gl.color3fv (COLORS [1]);
    gl.vertex2i (-box_width/4, -box_height/4);
    gl.color3fv (COLORS [2]);
    gl.vertex2i (-box_width/4, box_height/4);
    gl.color3fv (COLORS [3]);
    gl.vertex2i (0, -box_height/4);
    gl.color3fv (COLORS [4]);
    gl.vertex2i (0, box_height/4);
    gl.color3fv (COLORS [5]);
    gl.vertex2i (box_width/4, -box_height/4);
    gl.color3fv (COLORS [6]);
    gl.vertex2i (box_width/4, box_height/4);
    gl.end ();

    gl.color3fv (COLORS [7]);
    gl.begin (GL.TRIANGLE_STRIP);
    gl.vertex2i (0, 0);
    gl.vertex2i (-100, 100);
    gl.end ();
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.viewport (0, 0, window.width, window.height);
    gl.disable (GL.SCISSOR_TEST);

    gl.push_attrib (GL.COLOR_BUFFER_BIT);

    gl.color_mask (true, true, true, true);
    gl.index_mask (0xff);
    gl.clear_color (0.0f, 0.0f, 0.0f, 0.0f);
    gl.clear (GL.COLOR_BUFFER_BIT);

    gl.pop_attrib ();

    gl.shade_model (smooth_shade ? GL.SMOOTH : GL.FLAT);
    gl.polygon_mode (GL.FRONT_AND_BACK, polygon_fill ? GL.FILL : GL.LINE);

    viewport (0, 0); draw_point ();
    viewport (0, 1); draw_line ();
    viewport (0, 2); draw_line_strip ();
    viewport (0, 3); draw_loop ();

    viewport (1, 0); draw_bitmap ();
    viewport (1, 1); draw_triangle_fan ();
    viewport (1, 2); draw_triangle ();
    viewport (1, 3); draw_triangle_strip ();

    viewport (2, 0); draw_rectangle ();
    viewport (2, 1); draw_polygon ();
    viewport (2, 2); draw_quad ();
    viewport (2, 3); draw_quad_strip ();
       
    gl.swap_buffers (window);
  }


  protected void handle_keyboard (int key, int state, int x, int y) {
    switch (key) {
    case '1': smooth_shade = !smooth_shade; break;
    case '2': polygon_fill = !polygon_fill; break;
    case '3': rotate_color_mask (); break;
    default: return;
    }

    mark_window_dirty ();
  }

  
  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();   
    glu.ortho_2d (-width/2, width/2, -height/2, height/2);
    gl.matrix_mode (GL.MODELVIEW);
  }


  private void rotate_color_mask () {
    color_rotation = (color_rotation + 1) % 4;

    switch (color_rotation) {
    case 0:
      gl.color_mask (true, true, true, true);
      gl.index_mask (0xff);
      break;

    case 1:
      gl.color_mask (false, true, true, true);
      gl.index_mask (0xfe);
      break;

    case 2:
      gl.color_mask (true, false, true, true);
      gl.index_mask (0xfd);
      break;

    case 3:
      gl.color_mask (true, true, false, true);
      gl.index_mask (0xfb);
      break;
    }
  }


  private void viewport (int row, int column) {
    box_width = (window.width - (COLUMN_COUNT + 1) * GAP) 
      / COLUMN_COUNT;
    box_height = (window.height - (ROW_COUNT + 1) * GAP)
      / ROW_COUNT;

    int x = GAP + column * (box_width + GAP);
    int y = GAP + row * (box_height + GAP);

    gl.viewport (x, y, box_width, box_height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    gl.ortho (-box_width/2, box_width/2,
      -box_height/2, box_height/2, 0.0, 1.0);
    gl.matrix_mode (GL.MODELVIEW);

    gl.enable (GL.SCISSOR_TEST);
    gl.scissor (x, y, box_width, box_height);
  }


  public static void main (String [] args) {
    new Primitive (args).exec ();
  }
}
