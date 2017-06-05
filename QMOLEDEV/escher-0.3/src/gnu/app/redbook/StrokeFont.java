package gnu.app.redbook;

import gnu.x11.extension.glx.GL;


/**
 * Draw stroke (vector) font. To demonstrate display list. Modified from
 * <code>stroke.c</code>.
 *
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/StrokeFont.gif">
 * screenshot</a>
 * 
 * @see <a href="../../../../etc/screenshot/gnu/app/redbook/StrokeFont.help">
 * help output</a>
 */
public class StrokeFont extends gnu.x11.extension.glx.Application {
  private static final String LINE1 = "A SPARE SERAPE APPEARS AS";
  private static final String LINE2 = "APES PREPARE RARE PEPPERS";

  private static final int POINT = 1;
  private static final int STROKE = 2;
  private static final int END = 3;

  private static final int [] [] LETTER_A = {
   {0, 0, POINT}, {0, 9, POINT}, {1, 10, POINT}, 
   {4, 10, POINT}, {5, 9, POINT}, {5, 0, STROKE}, 
   {0, 5, POINT}, {5, 5, END}
  };


  private static final int [] [] LETTER_E = {
   {5, 0, POINT}, {0, 0, POINT}, {0, 10, POINT}, 
   {5, 10, STROKE}, {0, 5, POINT}, {4, 5, END}
  };


  private static final int [] [] LETTER_P = {
   {0, 0, POINT}, {0, 10, POINT},  {4, 10, POINT}, 
   {5, 9, POINT}, {5, 6, POINT}, {4, 5, POINT}, 
   {0, 5, END}
  };


  private static final int [] [] LETTER_R = {
   {0, 0, POINT}, {0, 10, POINT},  {4, 10, POINT}, 
   {5, 9, POINT}, {5, 6, POINT}, {4, 5, POINT}, 
   {0, 5, STROKE}, {3, 5, POINT}, {5, 0, END}
  };


  private static final int [] [] LETTER_S = {
   {0, 1, POINT}, {1, 0, POINT}, {4, 0, POINT}, 
   {5, 1, POINT}, {5, 4, POINT}, {4, 5, POINT}, 
   {1, 5, POINT}, {0, 6, POINT}, {0, 9, POINT}, 
   {1, 10, POINT}, {4, 10, POINT}, {5, 9, END}
  };
    

  public StrokeFont (String [] args) {
    super (args, RESIZE_BIT);

    about ("0.1", "stroke font",
      "Stephen Tse <stephent@sfu.ca>",
      "http://escher.sourceforge.net/");

    if (help_option) return;
    init_window (440, 120);

    gl.shade_model (GL.FLAT);
    init_letters ();
  }


  private void draw_letter (int [] [] letter) {
   gl.begin (GL.LINE_STRIP);

   for (int i=0; i<letter.length; i++) {
     int [] instruction = letter [i];
     int x = instruction [0];
     int y = instruction [1];
     int type = instruction [2];
     gl.vertex2f (x, y);

     switch (type) {
     case POINT:
       break;

     case STROKE: {
       gl.end ();
       gl.begin (GL.LINE_STRIP);
       break;

     } case END: {
       gl.end ();
       gl.translatef (8.0f, 0.0f, 0.0f);
       return;
     }
     }
   }
  }


  protected void handle_expose () {
    gl.clear (GL.COLOR_BUFFER_BIT);

    // line 1
    gl.push_matrix ();   
    gl.scalef (2.0f, 2.0f, 2.0f);
    gl.translatef (10.0f, 30.0f, 0.0f);
    gl.call_lists (GL.BYTE, LINE1.getBytes ());
    gl.pop_matrix ();

    // line 2
    gl.push_matrix ();
    gl.scalef (2.0f, 2.0f, 2.0f);
    gl.translatef (10.0f, 13.0f, 0.0f);
    gl.call_lists (GL.BYTE, LINE2.getBytes ());
    gl.pop_matrix ();

    gl.swap_buffers (window);
  }


  protected void handle_resize (int width, int height) {
    gl.viewport (0, 0, width, height);
    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();
    glu.ortho_2d (0.0, width, 0.0, height);
  }


  private void init_letters () {
    int base = gl.gen_lists (128);
    gl.list_base (base);

    gl.new_list (base+'A', GL.COMPILE); 
    draw_letter (LETTER_A); 
    gl.end_list ();

    gl.new_list (base+'E', GL.COMPILE);
    draw_letter (LETTER_E);
    gl.end_list ();

    gl.new_list (base+'P', GL.COMPILE);
    draw_letter (LETTER_P);
    gl.end_list ();

    gl.new_list (base+'R', GL.COMPILE);
    draw_letter (LETTER_R); 
    gl.end_list ();

    gl.new_list (base+'S', GL.COMPILE); 
    draw_letter (LETTER_S);
    gl.end_list ();

    gl.new_list (base+' ', GL.COMPILE); 
    gl.translatef (8.0f, 0.0f, 0.0f);
    gl.end_list ();
  }


  public static void main (String [] args) {
    new StrokeFont (args).exec ();
  }
}
