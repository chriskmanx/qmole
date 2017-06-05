package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.x11.ResponseInputStream;


/** GLX visual configuration. */ 
public class VisualConfig {
  public static final int VISUAL_BIT = 1<<0;
  public static final int CLASS_BIT = 1<<1;
  public static final int RGBA_BIT = 1<<2;
  public static final int RED_SIZE_BIT = 1<<3;
  public static final int GREEN_SIZE_BIT = 1<<4;
  public static final int BLUE_SIZE_BIT = 1<<5;
  public static final int ALPHA_SIZE_BIT = 1<<6;
  public static final int ACCUM_RED_SIZE_BIT = 1<<7;
  public static final int ACCUM_GREEN_SIZE_BIT = 1<<8;
  public static final int ACCUM_BLUE_SIZE_BIT = 1<<9;
  public static final int ACCUM_ALPHA_SIZE_BIT = 1<<10;
  public static final int DOUBLE_BUFFER_BIT = 1<<11;
  public static final int STERO_BIT = 1<<12;
  public static final int BUFFER_SIZE_BIT = 1<<13;
  public static final int DEPTH_SIZE_BIT = 1<<14;
  public static final int STENCIL_SIZE_BIT = 1<<15;
  public static final int AUX_BUFFERS_BIT = 1<<16;
  public static final int LEVEL_BIT = 1<<17;


  public int bitmask;
  public int count;

  private int visual_id;
  private int clazz;
  private boolean rgba;
  private int red_size;
  private int green_size;
  private int blue_size;
  private int alpha_size;
  private int accum_red_size;
  private int accum_green_size;
  private int accum_blue_size;
  private int accum_alpha_size;
  private boolean double_buffer;
  private boolean stero;
  private int buffer_size;
  private int depth_size;
  private int stencil_size;
  private int aux_buffers;
  private int level;

  private int[] more_prop_types;
  private int[] more_prop_values;

  /** Writing. */
  public VisualConfig () {
    
  }


  /** Reading. */
  public VisualConfig (ResponseInputStream i, int count) {
    this.count = count;
    visual_id = i.read_int32 ();
    clazz = i.read_int32 ();
    rgba = i.read_int32 () == 1;
    red_size = i.read_int32 ();
    green_size = i.read_int32 ();
    blue_size = i.read_int32 ();
    alpha_size = i.read_int32 ();
    accum_red_size = i.read_int32 ();
    accum_green_size = i.read_int32 ();
    accum_blue_size = i.read_int32 ();
    accum_alpha_size = i.read_int32 ();
    double_buffer = i.read_int32 () == 1;
    stero = i.read_int32 () == 1;
    buffer_size = i.read_int32 ();
    depth_size = i.read_int32 ();
    stencil_size = i.read_int32 ();
    aux_buffers = i.read_int32 ();
    level = i.read_int32 ();

    more_prop_types = new int[count];
    more_prop_values = new int[count];
    for (int index = 0; index < count; index++) {
      more_prop_types[index] = i.read_int32 ();
      more_prop_values[index] = i.read_int32 ();
    }
  }    


  //-- reading

  public int visual_id () { return visual_id; }
  public int clazz () { return clazz; }
  public boolean rgba  () { return rgba; }
  public int red_size () { return red_size; }
  public int green_size () { return green_size; }
  public int blue_size () { return blue_size; }
  public int alpha_size () { return alpha_size; }
  public int accum_red_size () { return accum_red_size; }
  public int accum_green_size () { return accum_green_size; }
  public int accum_blue_size () { return accum_blue_size; }
  public int accum_alpha_size () { return accum_alpha_size; }
  public boolean double_buffer  () { return double_buffer; }
  public boolean stero  () { return stero; }
  public int buffer_size () { return buffer_size; }
  public int depth_size () { return depth_size; }
  public int stencil_size () { return stencil_size; }
  public int aux_buffers () { return aux_buffers; }
  public int level () { return level; }


  public int more_property_type (int i) { return more_prop_types[i]; }
  public int more_property_value (int i) { return more_prop_values[i]; }


  //-- writing

  public void set_visual_id (int i) { visual_id = i; set (0); }
  public void set_clazz (int i) { clazz = i; set (1); }
  public void set_rgba () { set (2); }
  public void set_red_size (int i) { red_size = i; set (3); }
  public void set_green_size (int i) { green_size = i; set (4); }
  public void set_blue_size (int i) { blue_size = i; set (5); }
  public void set_alpha_size (int i) { alpha_size = i; set (6); }
  public void set_accum_red_size (int i) { accum_red_size = i; set (7); }
  public void set_accum_green_size (int i) { accum_green_size = i; set (8); }
  public void set_accum_blue_size (int i) { accum_blue_size = i; set (9); }
  public void set_accum_alpha_size (int i) { accum_alpha_size = i; set (10); }
  public void set_double_buffer () { set (11); }
  public void set_stero () { set (12); }
  public void set_buffer_size (int i) { buffer_size = i; set (13); }
  public void set_depth_size (int i) { depth_size = i; set (14); }
  public void set_stencil_size (int i) { stencil_size = i; set (15); }
  public void set_aux_buffers (int i) { aux_buffers = i; set (16); }
  public void set_level (int i) { level = i; set (17); }


  public void clear () { bitmask = 0; }
  public int length () { return 4 * count; }


  public void set_accum_rgb_size (int i) {
    set_accum_red_size (i);
    set_accum_green_size (i);
    set_accum_blue_size (i);
  }


  public boolean match (VisualConfig template) { // TODO
    if ((template.bitmask & VISUAL_BIT) != 0 // exact
      && template.visual_id () != visual_id ()) return false;

    if ((template.bitmask & CLASS_BIT) != 0 // exact
      && template.clazz () != clazz ()) return false;

    if ((template.bitmask & RGBA_BIT) != 0 // exact
      && !rgba ()) return false;

    if ((template.bitmask & RED_SIZE_BIT) != 0 // larger
      && template.red_size () > red_size ()) return false;

    if ((template.bitmask & GREEN_SIZE_BIT) != 0 // larger
      && template.green_size () > green_size ()) return false;

    if ((template.bitmask & BLUE_SIZE_BIT) != 0 // larger
      && template.blue_size () > blue_size ()) return false;

    if ((template.bitmask & ALPHA_SIZE_BIT) != 0 // larger
      && template.alpha_size () > alpha_size ()) return false;

    if ((template.bitmask & ACCUM_RED_SIZE_BIT) != 0 // larger
      && template.accum_red_size () > accum_red_size ()) return false;

    if ((template.bitmask & ACCUM_GREEN_SIZE_BIT) != 0 // larger
      && template.accum_green_size () > accum_green_size ()) return false;

    if ((template.bitmask & ACCUM_BLUE_SIZE_BIT) != 0 // larger
      && template.accum_blue_size () > accum_blue_size ()) return false;

    if ((template.bitmask & ACCUM_ALPHA_SIZE_BIT) != 0 // larger
      && template.accum_alpha_size () > accum_alpha_size ()) return false;

    if ((template.bitmask & DOUBLE_BUFFER_BIT) != 0 // exact
      && !double_buffer ()) return false;

    if ((template.bitmask & STERO_BIT) != 0 // exact
      && !stero ()) return false;

    if ((template.bitmask & BUFFER_SIZE_BIT) != 0 // larger
      && template.buffer_size () > buffer_size ()) return false;

    if ((template.bitmask & DEPTH_SIZE_BIT) != 0 // larger
      && template.depth_size () > depth_size ()) return false;

    if ((template.bitmask & STENCIL_SIZE_BIT) != 0 // larger
      && template.stencil_size () > stencil_size ()) return false;

    if ((template.bitmask & AUX_BUFFERS_BIT) != 0 // larger
      && template.aux_buffers () > aux_buffers ()) return false;

    if ((template.bitmask & LEVEL_BIT) != 0 // exact
      && template.level () == level ()) return false;

    return true;
  }  

      
  public String toString () {
    return "#VisualConfig"
      + "\n  visual-id: " + visual_id ()
      + "\n  class: " + clazz ()
      + "\n  rgba: " + rgba ()
      + "\n  red-size: " + red_size ()
      + "\n  green-size: " + green_size ()
      + "\n  blue-size: " + blue_size ()
      + "\n  alpha-size: " + alpha_size ()
      + "\n  accum-red-size: " + accum_red_size ()
      + "\n  accum-green-size: " + accum_green_size ()
      + "\n  accum-blue-size: " + accum_blue_size ()
      + "\n  accum-alpha-size: " + accum_alpha_size ()
      + "\n  double-buffer: " + double_buffer ()
      + "\n  stero: " + stero ()
      + "\n  buffer-size: " + buffer_size ()
      + "\n  depth-size: " + depth_size ()
      + "\n  stencil-size: " + stencil_size ()
      + "\n  aux-buffers: " + aux_buffers ()
      + "\n  level: " + level ()
      + "\n  property-count: " + count;
  }     


  private void set (int i) { bitmask |= 1<<i; }
}


  
